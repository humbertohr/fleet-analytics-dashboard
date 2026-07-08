# ==============================================================================
# mod_overview.R — Executive Overview
# KPI strip (with period-over-period deltas), revenue vs cost trend,
# monthly volume, cost structure, margin by service segment
# ==============================================================================

overviewUI <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(2, 2, 2, 2, 2, 2), fill = FALSE,
      uiOutput(ns("kpi_revenue")),
      uiOutput(ns("kpi_or")),
      uiOutput(ns("kpi_cpm")),
      uiOutput(ns("kpi_ontime")),
      uiOutput(ns("kpi_util")),
      uiOutput(ns("kpi_deadhead"))
    ),
    layout_columns(
      col_widths = c(7, 5),
      card(full_screen = TRUE,
           card_header("Revenue vs. Operating Cost by Month"),
           plotly::plotlyOutput(ns("rev_cost"), height = 330)),
      card(full_screen = TRUE,
           card_header("Cost Structure"),
           plotly::plotlyOutput(ns("cost_structure"), height = 330))
    ),
    layout_columns(
      col_widths = c(7, 5),
      card(full_screen = TRUE,
           card_header("Dispatched Trips & Total Miles by Month"),
           plotly::plotlyOutput(ns("volume"), height = 300)),
      card(full_screen = TRUE,
           card_header("Net Margin by Service Segment"),
           plotly::plotlyOutput(ns("margin_seg"), height = 300))
    )
  )
}

overviewServer <- function(id, filtered, previous, fleet_days) {
  moduleServer(id, function(input, output, session) {

    # ---- KPI computations (current vs prior equal-length period) ----
    kpis <- reactive({
      cur <- filtered(); prev <- previous()
      calc <- function(d, days) {
        if (nrow(d) == 0) return(NULL)
        list(
          revenue  = sum(d$revenue_usd),
          or       = sum(d$total_cost_usd) / sum(d$revenue_usd),
          cpm      = sum(d$total_cost_usd) / sum(d$total_miles),
          on_time  = mean(d$on_time),
          util     = sum(d$duty_days) / days,       # duty days / available days
          deadhead = sum(d$deadhead_km) / sum(d$total_km)
        )
      }
      list(cur = calc(cur, fleet_days()$cur), prev = calc(prev, fleet_days()$prev))
    })

    box <- function(field, title, fmt, dir_good, note = NULL) {
      renderUI({
        k <- kpis()
        if (is.null(k$cur)) return(kpi_box(title, "—"))
        delta <- if (!is.null(k$prev)) pct_change(k$cur[[field]], k$prev[[field]]) else NULL
        kpi_box(title, fmt(k$cur[[field]]), delta, dir_good, note)
      })
    }

    output$kpi_revenue  <- box("revenue",  "Revenue",           fmt_usd_k,              +1)
    output$kpi_or       <- box("or",       "Operating Ratio",   function(x) fmt_pct(x), -1,
                               "cost / revenue")
    output$kpi_cpm      <- box("cpm",      "Cost per Mile",     function(x) fmt_usd(x, .01), -1)
    output$kpi_ontime   <- box("on_time",  "On-Time Delivery",  fmt_pct,                +1)
    output$kpi_util     <- box("util",     "Fleet Utilization", fmt_pct,                +1,
                               "duty days / available days")
    output$kpi_deadhead <- box("deadhead", "Deadhead Miles",    fmt_pct,                -1,
                               "empty / total miles")

    # ---- Revenue vs cost trend ----
    output$rev_cost <- plotly::renderPlotly({
      d <- filtered() %>%
        group_by(month_date) %>%
        summarise(Revenue = sum(revenue_usd), Cost = sum(total_cost_usd),
                  .groups = "drop") %>%
        mutate(Margin = Revenue - Cost)
      req(nrow(d) > 0)
      plotly::plot_ly(d) %>%
        plotly::add_bars(x = ~month_date, y = ~Revenue, name = "Revenue",
                         marker = list(color = PAL$primary)) %>%
        plotly::add_bars(x = ~month_date, y = ~Cost, name = "Operating cost",
                         marker = list(color = PAL$surface2,
                                       line = list(color = PAL$border, width = 1))) %>%
        plotly::add_lines(x = ~month_date, y = ~Margin, name = "Net margin",
                          yaxis = "y2",
                          line = list(color = PAL$warn, width = 2.5)) %>%
        plotly::layout(
          barmode = "group",
          yaxis  = list(title = "USD", tickformat = "$.2s"),
          yaxis2 = list(overlaying = "y", side = "right", title = "Margin",
                        tickformat = "$.2s", gridcolor = "rgba(0,0,0,0)",
                        zeroline = TRUE, zerolinecolor = PAL$border),
          xaxis  = list(title = "")) %>%
        plt(margin = list(l = 55, r = 60, t = 30, b = 45))
    })

    # ---- Cost structure donut ----
    output$cost_structure <- plotly::renderPlotly({
      d <- filtered()
      req(nrow(d) > 0)
      s <- tibble(
        component = c("Fuel", "Driver pay", "Tolls & misc", "Overhead allocation"),
        usd = c(sum(d$fuel_cost_usd), sum(d$driver_pay_usd),
                sum(d$tolls_misc_usd), sum(d$overhead_alloc_usd)))
      plotly::plot_ly(s, labels = ~component, values = ~usd, type = "pie",
              hole = 0.62, sort = FALSE,
              marker = list(colors = c(PAL$warn, PAL$primary, PAL$muted, PAL$surface2),
                            line = list(color = PAL$bg, width = 2)),
              textinfo = "label+percent",
              textfont = list(color = PAL$text),
              hovertemplate = "%{label}: %{value:$,.0f}<extra></extra>") %>%
        plt(legend = FALSE, margin = list(l = 15, r = 15, t = 25, b = 15)) %>%
        plotly::layout(annotations = list(list(
          text = paste0("<b>", fmt_usd_k(sum(s$usd)), "</b><br>",
                        "<span style='font-size:11px;color:", PAL$muted,
                        "'>total cost</span>"),
          showarrow = FALSE, font = list(size = 17, color = PAL$text))))
    })

    # ---- Volume ----
    output$volume <- plotly::renderPlotly({
      d <- filtered() %>%
        group_by(month_date) %>%
        summarise(Trips = n(), Miles = sum(total_miles), .groups = "drop")
      req(nrow(d) > 0)
      plotly::plot_ly(d) %>%
        plotly::add_bars(x = ~month_date, y = ~Miles, name = "Miles",
                         marker = list(color = PAL$primary_d)) %>%
        plotly::add_lines(x = ~month_date, y = ~Trips, name = "Trips", yaxis = "y2",
                          line = list(color = PAL$good, width = 2.5)) %>%
        plotly::layout(
          yaxis  = list(title = "Miles", tickformat = ".2s"),
          yaxis2 = list(overlaying = "y", side = "right", title = "Trips",
                        gridcolor = "rgba(0,0,0,0)"),
          xaxis  = list(title = "")) %>%
        plt(margin = list(l = 55, r = 55, t = 30, b = 45))
    })

    # ---- Margin by segment ----
    output$margin_seg <- plotly::renderPlotly({
      d <- filtered() %>%
        group_by(segment) %>%
        summarise(margin = sum(margin_usd), .groups = "drop") %>%
        mutate(segment = factor(segment,
               levels = c("Local", "Regional", "Mid-range", "Long-haul")))
      req(nrow(d) > 0)
      plotly::plot_ly(d, x = ~segment, y = ~margin, type = "bar",
              marker = list(color = ifelse(d$margin >= 0, PAL$good, PAL$bad)),
              hovertemplate = "%{x}: %{y:$,.0f}<extra></extra>") %>%
        plotly::layout(xaxis = list(title = ""),
                       yaxis = list(title = "Net margin (USD)", tickformat = "$.2s")) %>%
        plt(legend = FALSE)
    })
  })
}
