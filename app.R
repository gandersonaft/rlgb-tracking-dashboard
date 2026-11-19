# RLGB radio tracking dashboard. Data come from data/rlgb.rds, rebuilt once a day by build_data.R after the 12:15 UTC decode.
library(shiny); library(bslib); library(dplyr); library(ggplot2); library(plotly); library(DT); library(tidyr); library(leaflet)

d <- readRDS("data/rlgb.rds")
stn_cols <- c("RLGB-1" = "#1b9e77", "RLGB-2" = "#d95f02", "RLGB-3" = "#7570b3")

fish <- d$rel |>
  mutate(fish = sprintf("T%02d", tag)) |>
  left_join(d$events |> group_by(tag) |>
              summarise(events = n(), bursts = sum(bursts_decoded), first_seen = min(start_utc), last_seen = max(start_utc),
                        stations = paste(sort(unique(station)), collapse = ", "), best_db = max(sig_max_db, na.rm = TRUE)),
            by = "tag") |>
  mutate(status = case_when(!is.na(events) ~ "Heard at a station",
                            handheld ~ "Handheld only",
                            TRUE ~ "Never heard"),
         status = factor(status, c("Heard at a station", "Handheld only", "Never heard"))) |>
  arrange(tag)

ev_days <- range(as.Date(d$events$start_utc))
days_all <- seq(ev_days[1], max(ev_days), by = 'day')
latest <- d$power |> group_by(station) |> slice_max(hour_utc, n = 1, with_ties = FALSE) |> ungroup()

ui <- page_navbar(
  title = "RLGB radio tracking", theme = bs_theme(version = 5, preset = "flatly"),
  nav_panel("Fish",
    layout_columns(fill = FALSE,
      value_box("Heard at a station", paste0(sum(fish$status == "Heard at a station"), " of ", nrow(fish)), theme = "success"),
      value_box("Handheld only", sum(fish$status == "Handheld only"), theme = "warning"),
      value_box("Never heard", sum(fish$status == "Never heard"), theme = "secondary"),
      value_box("Latest decoded day (UTC)", d$last_day)),
    card(card_header("Every fish"), DTOutput("fish_tbl")),
    card(card_header("Detections by time"), plotlyOutput("timeline", height = 420))),
  nav_panel("Stations",
    layout_columns(fill = FALSE, !!!lapply(seq_len(nrow(latest)), function(i)
      value_box(latest$station[i], sprintf("%.0f%%", latest$soc_est_pct_end[i]), 
                sprintf("%.2f V, %.1f C at %s", latest$batt_v_mean[i], latest$temp_c_mean[i], format(latest$hour_utc[i], "%d %b %H:%M")),
                theme = "primary"))),
    card(card_header("Battery charge estimate (%)"), plotlyOutput("soc", height = 300)),
    card(card_header("Solar energy per day (Wh) and peak panel power (W)"), plotlyOutput("solar", height = 300)),
    card(card_header("Battery temperature (C); the charger appears to stop below 5 C"), plotlyOutput("temp", height = 300))),
  nav_panel("Map",
    layout_sidebar(
      sidebar = sidebar(width = 300,
        sliderInput("days", "Dates (UTC)", min = ev_days[1], max = max(ev_days), value = range(ev_days), timeFormat = "%d %b", step = 1, animate = TRUE),
        selectInput("fish_sel", "Fish", c("All fish" = "all", setNames(as.character(fish$tag), fish$fish))),
        helpText("Fish are shown at the receiver that heard them; a receiver hears fish within roughly 100 m to a few hundred metres. ",
                 "With one fish selected, the line joins the release site to each receiver in time order. Handheld records are not mapped.")),
      leafletOutput("map", height = 560),
      card(card_header("Detections in the selected dates"), DTOutput("map_tbl")))),
  nav_panel("Data",
    card(card_header("Station reports"), DTOutput("status_tbl")),
    card(card_header("Receiver activity: pulses per day"), plotlyOutput("pulses", height = 300)),
    p(class = "text-muted small", paste("Built", d$built_utc, "from the nightly decoder packages in the private repo. Only decoder-confirmed events",
      "after each fish's release are shown; handheld records come from the release log. Bench beacons (T21-T23) are excluded."))))

server <- function(input, output, session) {
  output$fish_tbl <- renderDT({
    t <- fish |> transmute(Fish = fish, Released = release_date_utc, Reach = release_reach, Status = status,
                           `First heard` = format(first_seen, "%d %b %H:%M"), `Last heard` = format(last_seen, "%d %b %H:%M"),
                           Station = stations, Events = events, Bursts = bursts, `Best dB` = round(best_db, 1))
    datatable(t, rownames = FALSE, options = list(pageLength = 20, dom = "t")) |>
      formatStyle("Status", backgroundColor = styleEqual(c("Heard at a station", "Handheld only", "Never heard"),
                                                         c("#d4edda", "#fff3cd", "#e9ecef")))
  })
  output$timeline <- renderPlotly({
    e <- d$events |> mutate(fish = factor(sprintf("T%02d", tag), sprintf("T%02d", 20:1)))
    p <- ggplot(e, aes(start_utc, fish, colour = station, size = bursts_decoded,
                       text = sprintf("%s %s<br>%s to %s<br>%d bursts, best %.1f dB", fish, station, format(start_utc, "%d %b %H:%M"),
                                      format(end_utc, "%H:%M"), as.integer(bursts_decoded), sig_max_db))) +
      geom_point(alpha = .6) + scale_colour_manual(values = stn_cols) + scale_size_area(max_size = 6, guide = "none") +
      scale_y_discrete(drop = FALSE) + labs(x = NULL, y = NULL, colour = NULL) + theme_minimal()
    ggplotly(p, tooltip = "text")
  })
  output$soc <- renderPlotly(ggplotly(ggplot(d$power, aes(hour_utc, soc_est_pct_end, colour = station)) + geom_line() +
    scale_colour_manual(values = stn_cols) + labs(x = NULL, y = "%", colour = NULL) + theme_minimal()))
  output$solar <- renderPlotly({
    s <- d$power |> mutate(day = as.Date(hour_utc)) |> group_by(station, day) |>
      summarise(wh = sum(pv_w_mean * minutes / 60, na.rm = TRUE), peak = max(pv_w_max, na.rm = TRUE), .groups = "drop")
    s$peak[!is.finite(s$peak)] <- NA
    ggplotly(ggplot(s, aes(day, wh, colour = station, group = station, text = sprintf("%s %s<br>%.0f Wh, peak %.0f W", station, day, wh, peak))) +
      geom_line() + geom_point() + scale_colour_manual(values = stn_cols) + labs(x = NULL, y = "Wh per day", colour = NULL) + theme_minimal(), tooltip = "text")
  })
  output$temp <- renderPlotly(ggplotly(ggplot(d$power, aes(hour_utc, temp_c_mean, colour = station)) + geom_line() +
    geom_hline(yintercept = 5, linetype = 2, colour = "grey40") + scale_colour_manual(values = stn_cols) +
    labs(x = NULL, y = "C", colour = NULL) + theme_minimal()))
  ev_sel <- reactive({
    e <- d$events |> filter(as.Date(start_utc) >= input$days[1], as.Date(start_utc) <= input$days[2])
    if (input$fish_sel != "all") e <- e |> filter(tag == as.integer(input$fish_sel))
    e |> mutate(fish = sprintf("T%02d", tag))
  })
  output$map <- renderLeaflet({
    leaflet() |> addProviderTiles(providers$OpenStreetMap) |>
      fitBounds(min(d$stations$lon) - .01, min(d$stations$lat) - .01, max(d$stations$lon) + .01, max(d$stations$lat) + .01)
  })
  observe({
    req(input$map_zoom)   # wait until the map has been drawn (it is on a hidden tab at first)
    e <- ev_sel()
    per <- e |> group_by(station, fish) |> summarise(b = sum(bursts_decoded), f = min(start_utc), l = max(start_utc), .groups = "drop") |>
      mutate(line = sprintf("%s: %s bursts, %s to %s", fish, format(b, big.mark = ","), format(f, "%d %b"), format(l, "%d %b")))
    st <- d$stations |> left_join(per |> group_by(station) |> summarise(n_fish = n(), txt = paste(line, collapse = "<br>")), by = "station") |>
      mutate(n_fish = coalesce(n_fish, 0L), txt = ifelse(is.na(txt), "No fish in these dates", txt))
    rel_sites <- d$rel |> filter(!is.na(release_lat)) |> group_by(release_lat, release_lon, release_reach) |>
      summarise(fish = paste(sprintf("T%02d", tag), collapse = ", "), .groups = "drop")
    m <- leafletProxy("map") |> clearMarkers() |> clearShapes() |>
      addCircleMarkers(data = rel_sites, lng = ~release_lon, lat = ~release_lat, radius = 6, color = "#6c757d", fillOpacity = .8, weight = 2,
                       popup = ~paste0("<b>Release site</b> (", release_reach, ")<br>", fish), label = ~paste("Release:", release_reach)) |>
      addCircleMarkers(data = st, lng = ~lon, lat = ~lat, radius = ~8 + 4 * sqrt(n_fish), color = ~unname(stn_cols[station]), fillOpacity = .6, weight = 3,
                       popup = ~paste0("<b>", station, "</b><br>", n_fish, " fish<br>", txt), label = ~paste0(station, ": ", n_fish, " fish"),
                       labelOptions = labelOptions(noHide = TRUE, direction = "right", offset = c(12, 0)))
    if (input$fish_sel != "all" && nrow(e) > 0) {
      r1 <- d$rel |> filter(tag == as.integer(input$fish_sel))
      path <- e |> arrange(start_utc) |> inner_join(d$stations, by = "station") |> filter(station != lag(station, default = "")) |> select(lon, lat)
      if (nrow(r1) && !is.na(r1$release_lat)) path <- bind_rows(tibble(lon = r1$release_lon, lat = r1$release_lat), path)
      if (nrow(path) > 1) m <- m |> addPolylines(lng = path$lon, lat = path$lat, color = "#c0392b", weight = 3, dashArray = "6")
    }
  })
  output$map_tbl <- renderDT({
    t <- ev_sel() |> group_by(Fish = fish, Station = station, Day = as.Date(start_utc)) |>
      summarise(Events = n(), Bursts = sum(bursts_decoded), From = format(min(start_utc), "%H:%M"), To = format(max(end_utc), "%H:%M"), `Best dB` = round(max(sig_max_db), 1), .groups = "drop") |>
      arrange(desc(Day), Fish)
    datatable(t, rownames = FALSE, options = list(pageLength = 8, dom = "tp"))
  })
  output$status_tbl <- renderDT(datatable(d$status |> transmute(Station = station, `Reported (UTC)` = reported_utc, `Last power line` = last_power_line),
                                          rownames = FALSE, options = list(dom = "t", scrollX = TRUE)))
  output$pulses <- renderPlotly({
    s <- d$sysh |> mutate(day = as.Date(hour_utc)) |> group_by(station, day) |> summarise(pulses = sum(pulses, na.rm = TRUE), .groups = "drop")
    ggplotly(ggplot(s, aes(day, pulses, colour = station)) + geom_line() + scale_y_log10() + scale_colour_manual(values = stn_cols) +
      labs(x = NULL, y = "pulses (log)", colour = NULL) + theme_minimal())
  })
}
shinyApp(ui, server)
