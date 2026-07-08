# ==============================================================================
# mod_maintenance.R — Maintenance & Downtime
# Spend and downtime KPIs, preventive vs corrective trend, category Pareto,
# maintenance cost per km vs vehicle age (lifecycle curve), work order log
# ==============================================================================

maintenanceUI <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(3, 3, 3, 3), fill = FALSE,
      uiOutput(ns("kpi_cost")),
      uiOutput(ns("kpi_downtime")),
      uiOutput(ns("kpi_corr_share")),
      uiOutput(ns("kpi_cost_per_mile"))
    ),
    layout_columns(
      col_widths = c(7, 5),
      card(full_screen = TRUE,
           card_header("Monthly Maintenance Spend — Preventive vs. Corrective"),
           plotly::plotlyOutput(ns("monthly"), height = 310)),
      card(full_screen = TRUE,
           card_header("Downtime Pareto by System Category"),
           plotly::plotlyOutput(ns("pareto"), height = 310))
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(full_screen = TRUE,
           card_header("Corrective Cost per 1,000 km vs. Vehicle Age"),
           plotly::plotlyOutput(ns("aging"), height = 300)),
      card(full_screen = TRUE,
           card_header("Work Order Log"),
           DT::DTOutput(ns("wo_table")))
    )
  )
}

maintenanceServer <- function(id, maint_filtered, filtered, vehicles) {
  moduleServer(id, function(input, output, session) {

    output$kpi_cost <- renderUI({
      m <- maint_filtered()
      kpi_box("Maintenance Spend",
              if (nrow(m)) fmt_usd_k(sum(m$cost_usd)) else "—",
              note = paste(nrow(m), "work orders"))
    })
    output$kpi_downtime <- renderUI({
      m <- maint_filtered()
      kpi_box("Fleet Downtime",
              if (nrow(m)) paste(fmt_num(sum(m$downtime_days)), "days") else "—",
              note = "vehicle-days out of service")
    })
    output$kpi_corr_share <- renderUI({
      m <- maint_filtered()
      kpi_box("Corrective Share of Spend",
              if (nrow(m)) fmt_pct(sum(m$cost_usd[m$maintenance_type == "Corrective"]) /
                                   sum(m$cost_usd)) else "—",
              note = "lower is healthier")
    })
    output$kpi_cost_per_mile <- renderUI({
      m <- maint_filtered(); t <- filtered()
      val <- if (nrow(m) && nrow(t) && sum(t$total_miles) > 0)
        fmt_usd(sum(m$cost_usd) / sum(t$total_miles), 0.01) else "—"
      kpi_box("Maintenance Cost / Mile", val, note = "spend / fleet miles")
    })

    output$monthly <- plotly::renderPlotly({
      d <- maint_filtered() %>%
        group_by(month_date, maintenance_type) %>%
        summarise(cost = sum(cost_usd), .groups = "drop")
      req(nrow(d) > 0)
      p <- plotly::plot_ly()
      for (mt in names(PAL$maint)) {
        ds <- d %>% filter(maintenance_type == mt)
        if (nrow(ds) == 0) next
        p <- p %>% plotly::add_bars(data = ds, x = ~month_date, y = ~cost, name = mt,
                    marker = list(color = PAL$maint[[mt]]),
                    hovertemplate = "%{y:$,.0f}<extra></extra>")
      }
      p %>% plotly::layout(barmode = "stack", xaxis = list(title = ""),
                    yaxis = list(title = "USD", tickformat = "$.2s")) %>% plt()
    })

    output$pareto <- plotly::renderPlotly({
      d <- maint_filtered() %>%
        filter(maintenance_type == "Corrective") %>%
        group_by(category) %>%
        summarise(downtime = sum(downtime_days), .groups = "drop") %>%
        arrange(desc(downtime)) %>%
        mutate(cum_pct = cumsum(downtime) / sum(downtime),
               category = factor(category, levels = category))
      req(nrow(d) > 0)
      plotly::plot_ly(d) %>%
        plotly::add_bars(x = ~category, y = ~downtime, name = "Downtime days",
                 marker = list(color = PAL$bad)) %>%
        plotly::add_lines(x = ~category, y = ~cum_pct, yaxis = "y2",
                  name = "Cumulative %",
                  line = list(color = PAL$warn, width = 2.5)) %>%
        plotly::layout(
          xaxis = list(title = "", tickangle = -30),
          yaxis = list(title = "Downtime days"),
          yaxis2 = list(overlaying = "y", side = "right", tickformat = ".0%",
                        range = c(0, 1.05), gridcolor = "rgba(0,0,0,0)")) %>%
        plt(margin = list(l = 55, r = 55, t = 30, b = 80))
    })

    output$aging <- plotly::renderPlotly({
      km_by_vehicle <- filtered() %>%
        group_by(vehicle_id) %>%
        summarise(km = sum(total_km), .groups = "drop")
      d <- maint_filtered() %>%
        filter(maintenance_type == "Corrective") %>%
        group_by(vehicle_id) %>%
        summarise(corr_cost = sum(cost_usd), .groups = "drop") %>%
        inner_join(km_by_vehicle, by = "vehicle_id") %>%
        inner_join(vehicles %>% select(vehicle_id, vehicle_type, vehicle_age_years),
                   by = "vehicle_id") %>%
        filter(km > 0) %>%
        mutate(cost_per_1k_km = corr_cost / (km / 1000))
      req(nrow(d) > 2)
      fit <- tryCatch(loess(cost_per_1k_km ~ vehicle_age_years, data = d,
                            span = 0.9), error = function(e) NULL)
      p <- plotly::plot_ly(d, x = ~vehicle_age_years, y = ~cost_per_1k_km,
              type = "scatter", mode = "markers",
              color = ~vehicle_type, colors = PAL$vehicle_type,
              marker = list(size = 8, opacity = 0.75,
                            line = list(color = PAL$bg, width = 1)),
              customdata = ~vehicle_id,
              hovertemplate = paste0("%{customdata}<br>Age %{x:.1f} yr · ",
                                     "$%{y:.0f}/1k km<extra></extra>"))
      if (!is.null(fit)) {
        xs <- seq(min(d$vehicle_age_years), max(d$vehicle_age_years), length.out = 50)
        p <- p %>% plotly::add_lines(x = xs, y = predict(fit, xs), name = "Trend",
                     line = list(color = PAL$text, width = 2, dash = "dot"),
                     inherit = FALSE)
      }
      p %>% plotly::layout(xaxis = list(title = "Vehicle age (years)"),
                    yaxis = list(title = "Corrective $ / 1,000 km")) %>% plt()
    })

    output$wo_table <- DT::renderDT({
      maint_filtered() %>%
        left_join(vehicles %>% select(vehicle_id, vehicle_type), by = "vehicle_id") %>%
        transmute(`Work order` = work_order_id, Date = event_date,
                  Vehicle = vehicle_id, Class = vehicle_type,
                  Type = maintenance_type, Category = category,
                  Cost = cost_usd, `Downtime (days)` = downtime_days) %>%
        arrange(desc(Date)) %>%
        dark_dt(page_len = 8) %>%
        DT::formatCurrency("Cost", digits = 0)
    })
  })
}
