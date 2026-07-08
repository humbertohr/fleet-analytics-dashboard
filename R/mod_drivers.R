# ==============================================================================
# mod_drivers.R — Drivers
# Workload leaders, service-risk scatter (miles vs on-time), active headcount
# trend, tenure profile, full performance leaderboard
# ==============================================================================

driversUI <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(6, 6),
      card(full_screen = TRUE,
           card_header("Top 15 Drivers by Miles in Period"),
           plotly::plotlyOutput(ns("top_miles"), height = 330)),
      card(full_screen = TRUE,
           card_header("Service Risk — Miles vs. On-Time Rate"),
           plotly::plotlyOutput(ns("risk"), height = 330))
    ),
    layout_columns(
      col_widths = c(7, 5),
      card(full_screen = TRUE,
           card_header("Active Driver Headcount Over Time"),
           plotly::plotlyOutput(ns("headcount"), height = 280)),
      card(full_screen = TRUE,
           card_header("Tenure Profile — Current Roster"),
           plotly::plotlyOutput(ns("tenure"), height = 280))
    ),
    card(full_screen = TRUE,
         card_header("Driver Performance Leaderboard"),
         DT::DTOutput(ns("leaderboard")))
  )
}

driversServer <- function(id, filtered, drivers, as_of_date) {
  moduleServer(id, function(input, output, session) {

    drv_summary <- reactive({
      filtered() %>%
        group_by(driver_id) %>%
        summarise(trips = n(), miles = sum(total_miles),
                  revenue = sum(revenue_usd),
                  on_time = mean(on_time),
                  avg_load = mean(load_out_kg),
                  duty_days = sum(duty_days),
                  .groups = "drop") %>%
        left_join(drivers %>%
                    select(driver_id, driver_name, cdl_class, hire_date,
                           employment_status),
                  by = "driver_id")
    })

    output$top_miles <- plotly::renderPlotly({
      d <- drv_summary() %>% arrange(desc(miles)) %>% head(15) %>%
        mutate(driver_name = factor(driver_name, levels = rev(driver_name)))
      req(nrow(d) > 0)
      plotly::plot_ly(d, y = ~driver_name, x = ~miles, type = "bar",
              orientation = "h", marker = list(color = PAL$primary),
              customdata = ~trips,
              hovertemplate = "%{y}<br>%{x:,.0f} mi · %{customdata} trips<extra></extra>") %>%
        plotly::layout(xaxis = list(title = "Miles"), yaxis = list(title = "")) %>%
        plt(legend = FALSE, margin = list(l = 140, r = 15, t = 15, b = 40))
    })

    output$risk <- plotly::renderPlotly({
      d <- drv_summary() %>% filter(trips >= 10)
      req(nrow(d) > 0)
      plotly::plot_ly(d, x = ~miles, y = ~on_time, type = "scatter", mode = "markers",
              marker = list(
                size = 9, opacity = 0.8,
                color = ifelse(d$on_time < 0.88 & d$miles > median(d$miles),
                               PAL$bad, PAL$primary),
                line = list(color = PAL$bg, width = 1)),
              customdata = ~driver_name,
              hovertemplate = paste0("%{customdata}<br>%{x:,.0f} mi · ",
                                     "on-time %{y:.1%}<extra></extra>")) %>%
        plotly::layout(
          xaxis = list(title = "Miles in period"),
          yaxis = list(title = "On-time rate", tickformat = ".0%"),
          shapes = list(list(type = "line", x0 = 0, x1 = 1, xref = "paper",
                             y0 = 0.9, y1 = 0.9,
                             line = list(color = PAL$muted, dash = "dot"))),
          annotations = list(list(
            x = 1, xref = "paper", y = 0.9, xanchor = "right", yanchor = "bottom",
            text = "90% service floor", showarrow = FALSE,
            font = list(color = PAL$muted, size = 11)))) %>%
        plt(legend = FALSE)
    })

    output$headcount <- plotly::renderPlotly({
      months <- seq.Date(min(filtered()$month_date, na.rm = TRUE),
                         max(filtered()$month_date, na.rm = TRUE), by = "month")
      req(length(months) > 0)
      d <- tibble(month_date = months) %>%
        rowwise() %>%
        mutate(active = sum(drivers$hire_date <= month_date &
                 (is.na(drivers$exit_date) | drivers$exit_date >= month_date))) %>%
        ungroup()
      plotly::plot_ly(d, x = ~month_date, y = ~active, type = "scatter",
              mode = "lines", fill = "tozeroy",
              line = list(color = PAL$primary, width = 2.5),
              fillcolor = "rgba(94,155,211,0.18)",
              hovertemplate = "%{y} active drivers<extra></extra>") %>%
        plotly::layout(xaxis = list(title = ""),
                       yaxis = list(title = "Active drivers")) %>%
        plt(legend = FALSE)
    })

    output$tenure <- plotly::renderPlotly({
      d <- drivers %>%
        filter(employment_status == "Active") %>%
        mutate(tenure_yr = as.numeric(as_of_date - hire_date) / 365.25,
               bucket = cut(tenure_yr, breaks = c(0, 1, 3, 5, 8, Inf),
                            labels = c("< 1 yr", "1-3 yr", "3-5 yr",
                                       "5-8 yr", "8+ yr"))) %>%
        count(bucket)
      plotly::plot_ly(d, x = ~bucket, y = ~n, type = "bar",
              marker = list(color = PAL$primary),
              hovertemplate = "%{x}: %{y} drivers<extra></extra>") %>%
        plotly::layout(xaxis = list(title = ""),
                       yaxis = list(title = "Drivers")) %>%
        plt(legend = FALSE)
    })

    output$leaderboard <- DT::renderDT({
      drv_summary() %>%
        transmute(
          Driver = driver_name, CDL = cdl_class, Status = employment_status,
          Trips = trips, Miles = round(miles), `Duty days` = duty_days,
          Revenue = round(revenue),
          `On-time` = paste0(round(on_time * 100, 1), "%"),
          `Avg outbound load (kg)` = round(avg_load)) %>%
        arrange(desc(Miles)) %>%
        dark_dt() %>%
        DT::formatCurrency("Revenue", digits = 0) %>%
        DT::formatRound(c("Miles", "Avg outbound load (kg)"), digits = 0)
    })
  })
}
