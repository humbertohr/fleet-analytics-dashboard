# ==============================================================================
# mod_fuel.R — Fuel & Efficiency
# Diesel market price series, realized fuel economy by class, fuel cost per
# mile trend vs market price, load vs economy physics, CO2 footprint
# ==============================================================================

fuelUI <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(7, 5),
      card(full_screen = TRUE,
           card_header("Diesel Market Price vs. Realized Fuel Cost per Mile"),
           plotly::plotlyOutput(ns("price_cpm"), height = 320)),
      card(full_screen = TRUE,
           card_header("Realized Fuel Economy by Equipment Class (mpg)"),
           plotly::plotlyOutput(ns("mpg_class"), height = 320))
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(full_screen = TRUE,
           card_header("Payload Utilization vs. Fuel Economy (the physics check)"),
           plotly::plotlyOutput(ns("load_econ"), height = 300)),
      card(full_screen = TRUE,
           card_header("CO2 Footprint — Monthly and Cumulative"),
           plotly::plotlyOutput(ns("co2"), height = 300))
    )
  )
}

fuelServer <- function(id, filtered, fuel_prices) {
  moduleServer(id, function(input, output, session) {

    output$price_cpm <- plotly::renderPlotly({
      t <- filtered()
      req(nrow(t) > 0)
      fp <- fuel_prices %>%
        filter(date >= min(t$trip_start), date <= max(t$trip_start)) %>%
        mutate(month_date = lubridate::floor_date(date, "month")) %>%
        group_by(month_date) %>%
        summarise(price = mean(diesel_usd_per_gallon), .groups = "drop")
      cpm <- t %>% group_by(month_date) %>%
        summarise(fuel_cpm = sum(fuel_cost_usd) / sum(total_miles), .groups = "drop")
      plotly::plot_ly() %>%
        plotly::add_lines(data = fp, x = ~month_date, y = ~price,
                  name = "Diesel ($/gal)",
                  line = list(color = PAL$warn, width = 2.5),
                  hovertemplate = "$%{y:.2f}/gal<extra></extra>") %>%
        plotly::add_lines(data = cpm, x = ~month_date, y = ~fuel_cpm, yaxis = "y2",
                  name = "Fuel cost ($/mi)",
                  line = list(color = PAL$primary, width = 2.5),
                  hovertemplate = "$%{y:.2f}/mi<extra></extra>") %>%
        plotly::layout(
          xaxis = list(title = ""),
          yaxis = list(title = "Diesel $/gal", tickformat = "$.2f"),
          yaxis2 = list(overlaying = "y", side = "right",
                        title = "Fuel $/mi", tickformat = "$.2f",
                        gridcolor = "rgba(0,0,0,0)")) %>%
        plt(margin = list(l = 60, r = 60, t = 30, b = 45))
    })

    output$mpg_class <- plotly::renderPlotly({
      d <- filtered() %>%
        mutate(mpg = total_km / fuel_liters * 2.352)
      req(nrow(d) > 0)
      if (nrow(d) > 6000) d <- d %>% slice_sample(n = 6000)
      p <- plotly::plot_ly()
      for (vt in names(PAL$vehicle_type)) {
        ds <- d %>% filter(vehicle_type == vt)
        if (nrow(ds) == 0) next
        p <- p %>% plotly::add_boxplot(data = ds, x = ~vehicle_type, y = ~mpg,
              name = vt, marker = list(color = PAL$vehicle_type[[vt]]),
              line = list(color = PAL$vehicle_type[[vt]]),
              fillcolor = "rgba(0,0,0,0)")
      }
      p %>% plotly::layout(xaxis = list(title = ""),
                    yaxis = list(title = "Miles per gallon")) %>%
        plt(legend = FALSE)
    })

    output$load_econ <- plotly::renderPlotly({
      d <- filtered() %>%
        mutate(mpg = total_km / fuel_liters * 2.352)
      req(nrow(d) > 0)
      if (nrow(d) > 3000) d <- d %>% slice_sample(n = 3000)
      plotly::plot_ly(d, x = ~utilization_out_pct, y = ~mpg,
              type = "scatter", mode = "markers",
              color = ~vehicle_type, colors = PAL$vehicle_type,
              marker = list(size = 5, opacity = 0.45),
              hovertemplate = "Load %{x:.0f}% · %{y:.1f} mpg<extra></extra>") %>%
        plotly::layout(xaxis = list(title = "Outbound payload used (%)"),
                       yaxis = list(title = "Trip fuel economy (mpg)")) %>%
        plt()
    })

    output$co2 <- plotly::renderPlotly({
      d <- filtered() %>%
        group_by(month_date) %>%
        summarise(co2_t = sum(co2_kg) / 1000, .groups = "drop") %>%
        arrange(month_date) %>%
        mutate(cum_t = cumsum(co2_t))
      req(nrow(d) > 0)
      plotly::plot_ly(d) %>%
        plotly::add_bars(x = ~month_date, y = ~co2_t, name = "Monthly (t)",
                 marker = list(color = PAL$muted),
                 hovertemplate = "%{y:,.0f} t<extra></extra>") %>%
        plotly::add_lines(x = ~month_date, y = ~cum_t, yaxis = "y2",
                  name = "Cumulative (t)",
                  line = list(color = PAL$good, width = 2.5),
                  hovertemplate = "%{y:,.0f} t cumulative<extra></extra>") %>%
        plotly::layout(
          xaxis = list(title = ""),
          yaxis = list(title = "Tonnes CO2 / month"),
          yaxis2 = list(overlaying = "y", side = "right",
                        title = "Cumulative", gridcolor = "rgba(0,0,0,0)")) %>%
        plt(margin = list(l = 55, r = 60, t = 30, b = 45))
    })
  })
}
