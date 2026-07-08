# ==============================================================================
# mod_operations.R — Trips & Routes
# Segment mix over time, lane profitability, dispatch cadence heatmap,
# on-time service trend, load utilization by segment
# ==============================================================================

operationsUI <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(7, 5),
      card(full_screen = TRUE,
           card_header("Monthly Trip Volume by Service Segment"),
           plotly::plotlyOutput(ns("segment_mix"), height = 320)),
      card(full_screen = TRUE,
           card_header("Top Lanes by Volume (with revenue per loaded mile)"),
           plotly::plotlyOutput(ns("top_lanes"), height = 320))
    ),
    layout_columns(
      col_widths = c(5, 7),
      card(full_screen = TRUE,
           card_header("Dispatch Cadence — Weekday x Month"),
           plotly::plotlyOutput(ns("heatmap"), height = 300)),
      card(full_screen = TRUE,
           card_header("On-Time Delivery by Segment"),
           plotly::plotlyOutput(ns("on_time"), height = 300))
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(full_screen = TRUE,
           card_header("Outbound Payload Utilization by Segment"),
           plotly::plotlyOutput(ns("load_util"), height = 300)),
      card(full_screen = TRUE,
           card_header("Backhaul Coverage & Deadhead Trend"),
           plotly::plotlyOutput(ns("backhaul"), height = 300))
    )
  )
}

operationsServer <- function(id, filtered) {
  moduleServer(id, function(input, output, session) {

    seg_levels <- c("Local", "Regional", "Mid-range", "Long-haul")

    output$segment_mix <- plotly::renderPlotly({
      d <- filtered() %>%
        count(month_date, segment) %>%
        mutate(segment = factor(segment, levels = seg_levels))
      req(nrow(d) > 0)
      p <- plotly::plot_ly()
      for (s in seg_levels) {
        ds <- d %>% filter(segment == s)
        if (nrow(ds) == 0) next
        p <- p %>% plotly::add_bars(data = ds, x = ~month_date, y = ~n, name = s,
                                    marker = list(color = PAL$segment[[s]]))
      }
      p %>% plotly::layout(barmode = "stack",
                           xaxis = list(title = ""),
                           yaxis = list(title = "Trips")) %>% plt()
    })

    output$top_lanes <- plotly::renderPlotly({
      d <- filtered() %>%
        group_by(destination) %>%
        summarise(trips = n(),
                  rpm = sum(revenue_usd) / (sum(loaded_km) / 1.60934),
                  .groups = "drop") %>%
        arrange(desc(trips)) %>% head(12) %>%
        mutate(destination = factor(destination, levels = rev(destination)))
      req(nrow(d) > 0)
      plotly::plot_ly(d, y = ~destination, x = ~trips, type = "bar",
              orientation = "h",
              marker = list(color = PAL$primary),
              customdata = ~rpm,
              hovertemplate = paste0(
                "%{y}<br>Trips: %{x:,}<br>",
                "Rev / loaded mile: $%{customdata:.2f}<extra></extra>")) %>%
        plotly::layout(xaxis = list(title = "Trips"),
                       yaxis = list(title = "")) %>%
        plt(legend = FALSE, margin = list(l = 130, r = 15, t = 20, b = 40))
    })

    output$heatmap <- plotly::renderPlotly({
      d <- filtered() %>%
        count(weekday, month) %>%
        tidyr::complete(weekday, month, fill = list(n = 0))
      req(nrow(d) > 0)
      plotly::plot_ly(d, x = ~month, y = ~weekday, z = ~n, type = "heatmap",
              colors = colorRampPalette(c(PAL$surface, PAL$primary))(64),
              hovertemplate = "%{x} / %{y}: %{z} dispatches<extra></extra>",
              showscale = FALSE) %>%
        plotly::layout(xaxis = list(title = ""), yaxis = list(title = "",
                       autorange = "reversed")) %>%
        plt(legend = FALSE, margin = list(l = 45, r = 10, t = 15, b = 40))
    })

    output$on_time <- plotly::renderPlotly({
      d <- filtered() %>%
        group_by(month_date, segment) %>%
        summarise(on_time = mean(on_time), n = n(), .groups = "drop") %>%
        filter(n >= 5) %>%
        mutate(segment = factor(segment, levels = seg_levels))
      req(nrow(d) > 0)
      p <- plotly::plot_ly()
      for (s in seg_levels) {
        ds <- d %>% filter(segment == s)
        if (nrow(ds) == 0) next
        p <- p %>% plotly::add_lines(data = ds, x = ~month_date, y = ~on_time,
              name = s, line = list(color = PAL$segment[[s]], width = 2),
              hovertemplate = "%{y:.1%}<extra></extra>")
      }
      p %>%
        plotly::layout(
          xaxis = list(title = ""),
          yaxis = list(title = "On-time %", tickformat = ".0%",
                       range = c(0.7, 1.02)),
          shapes = list(list(type = "line", x0 = 0, x1 = 1, xref = "paper",
                             y0 = 0.95, y1 = 0.95,
                             line = list(color = PAL$muted, dash = "dot")))) %>%
        plt()
    })

    output$load_util <- plotly::renderPlotly({
      d <- filtered() %>%
        mutate(segment = factor(segment, levels = seg_levels))
      req(nrow(d) > 0)
      if (nrow(d) > 6000) d <- d %>% slice_sample(n = 6000)
      p <- plotly::plot_ly()
      for (s in seg_levels) {
        ds <- d %>% filter(segment == s)
        if (nrow(ds) == 0) next
        p <- p %>% plotly::add_boxplot(data = ds, x = ~segment,
              y = ~utilization_out_pct, name = s,
              marker = list(color = PAL$segment[[s]]),
              line = list(color = PAL$segment[[s]]),
              fillcolor = "rgba(0,0,0,0)")
      }
      p %>% plotly::layout(xaxis = list(title = ""),
                    yaxis = list(title = "Outbound payload used (%)",
                                 range = c(0, 105))) %>%
        plt(legend = FALSE)
    })

    output$backhaul <- plotly::renderPlotly({
      d <- filtered() %>%
        group_by(month_date) %>%
        summarise(backhaul = mean(has_backhaul),
                  deadhead = sum(deadhead_km) / sum(total_km),
                  .groups = "drop")
      req(nrow(d) > 0)
      plotly::plot_ly(d) %>%
        plotly::add_lines(x = ~month_date, y = ~backhaul, name = "Backhaul coverage",
                  line = list(color = PAL$good, width = 2.5),
                  hovertemplate = "%{y:.1%}<extra></extra>") %>%
        plotly::add_lines(x = ~month_date, y = ~deadhead, name = "Deadhead miles",
                  line = list(color = PAL$bad, width = 2.5),
                  hovertemplate = "%{y:.1%}<extra></extra>") %>%
        plotly::layout(xaxis = list(title = ""),
                       yaxis = list(title = "", tickformat = ".0%")) %>%
        plt()
    })
  })
}
