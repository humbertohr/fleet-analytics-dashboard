# ==============================================================================
# mod_fleet.R — Fleet
# Composition & status, age vs accumulated mileage, cost per mile by class,
# highest / lowest utilized units, per-vehicle summary table
# ==============================================================================

fleetUI <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(4, 8),
      card(full_screen = TRUE,
           card_header("Fleet Composition & Status"),
           plotly::plotlyOutput(ns("composition"), height = 320)),
      card(full_screen = TRUE,
           card_header("Vehicle Age vs. Accumulated Mileage"),
           plotly::plotlyOutput(ns("age_mileage"), height = 320))
    ),
    layout_columns(
      col_widths = c(5, 7),
      card(full_screen = TRUE,
           card_header("Cost per Mile by Equipment Class"),
           plotly::plotlyOutput(ns("cpm_type"), height = 300)),
      card(full_screen = TRUE,
           card_header("Vehicle Utilization — Duty Days in Period (top & bottom 10)"),
           plotly::plotlyOutput(ns("veh_util"), height = 300))
    ),
    card(full_screen = TRUE,
         card_header("Per-Vehicle Operating Summary"),
         DT::DTOutput(ns("veh_table")))
  )
}

fleetServer <- function(id, filtered, vehicles) {
  moduleServer(id, function(input, output, session) {

    veh_summary <- reactive({
      filtered() %>%
        group_by(vehicle_id) %>%
        summarise(
          trips = n(),
          miles = sum(total_miles),
          duty_days = sum(duty_days),
          revenue = sum(revenue_usd),
          cost = sum(total_cost_usd),
          cpm = cost / miles,
          mpg = mean(total_km / fuel_liters) * 2.352,
          on_time = mean(on_time),
          last_odometer_km = max(odometer_km),
          .groups = "drop") %>%
        left_join(vehicles %>%
                    select(vehicle_id, brand, model, vehicle_type,
                           vehicle_age_years, status),
                  by = "vehicle_id")
    })

    output$composition <- plotly::renderPlotly({
      d <- vehicles %>% count(vehicle_type, status)
      status_cols <- c("Active" = PAL$good, "In Shop" = PAL$warn,
                       "Retired" = PAL$muted)
      p <- plotly::plot_ly()
      for (s in names(status_cols)) {
        ds <- d %>% filter(status == s)
        if (nrow(ds) == 0) next
        p <- p %>% plotly::add_bars(data = ds, x = ~vehicle_type, y = ~n, name = s,
                            marker = list(color = status_cols[[s]]))
      }
      p %>% plotly::layout(barmode = "stack", xaxis = list(title = ""),
                    yaxis = list(title = "Units")) %>% plt()
    })

    output$age_mileage <- plotly::renderPlotly({
      d <- veh_summary()
      req(nrow(d) > 0)
      plotly::plot_ly(d, x = ~vehicle_age_years, y = ~last_odometer_km / 1000,
              type = "scatter", mode = "markers",
              color = ~vehicle_type, colors = PAL$vehicle_type,
              size = ~trips, sizes = c(6, 26),
              marker = list(opacity = 0.75),
              customdata = ~paste0(vehicle_id, " · ", brand, " ", model),
              hovertemplate = paste0("%{customdata}<br>Age: %{x:.1f} yr<br>",
                                     "Odometer: %{y:,.0f}k km<extra></extra>")) %>%
        plotly::layout(xaxis = list(title = "Vehicle age (years)"),
                       yaxis = list(title = "Odometer (thousand km)")) %>%
        plt()
    })

    output$cpm_type <- plotly::renderPlotly({
      d <- filtered() %>%
        group_by(vehicle_type) %>%
        summarise(
          Fuel = sum(fuel_cost_usd) / sum(total_miles),
          `Driver pay` = sum(driver_pay_usd) / sum(total_miles),
          `Tolls & misc` = sum(tolls_misc_usd) / sum(total_miles),
          Overhead = sum(overhead_alloc_usd) / sum(total_miles),
          .groups = "drop") %>%
        tidyr::pivot_longer(-vehicle_type, names_to = "component", values_to = "cpm")
      req(nrow(d) > 0)
      comp_cols <- c("Fuel" = PAL$warn, "Driver pay" = PAL$primary,
                     "Tolls & misc" = PAL$muted, "Overhead" = PAL$surface2)
      p <- plotly::plot_ly()
      for (cc in names(comp_cols)) {
        ds <- d %>% filter(component == cc)
        p <- p %>% plotly::add_bars(data = ds, x = ~vehicle_type, y = ~cpm, name = cc,
                    marker = list(color = comp_cols[[cc]]),
                    hovertemplate = "$%{y:.2f}/mi<extra></extra>")
      }
      p %>% plotly::layout(barmode = "stack", xaxis = list(title = ""),
                    yaxis = list(title = "USD per mile", tickformat = "$.2f")) %>%
        plt()
    })

    output$veh_util <- plotly::renderPlotly({
      d <- veh_summary() %>% arrange(desc(duty_days))
      req(nrow(d) > 0)
      d <- bind_rows(head(d, 10) %>% mutate(grp = "Top 10"),
                     tail(d, 10) %>% mutate(grp = "Bottom 10")) %>%
        distinct(vehicle_id, .keep_all = TRUE) %>%
        mutate(vehicle_id = factor(vehicle_id, levels = rev(vehicle_id)))
      plotly::plot_ly(d, y = ~vehicle_id, x = ~duty_days, type = "bar",
              orientation = "h",
              marker = list(color = ifelse(d$grp == "Top 10",
                                           PAL$primary, PAL$bad)),
              customdata = ~vehicle_type,
              hovertemplate = "%{y} (%{customdata})<br>%{x} duty days<extra></extra>") %>%
        plotly::layout(xaxis = list(title = "Duty days in period"),
                       yaxis = list(title = "")) %>%
        plt(legend = FALSE, margin = list(l = 75, r = 15, t = 15, b = 40))
    })

    output$veh_table <- DT::renderDT({
      veh_summary() %>%
        transmute(
          Vehicle = vehicle_id, Unit = paste(brand, model), Class = vehicle_type,
          Status = status, `Age (yr)` = vehicle_age_years, Trips = trips,
          Miles = round(miles), `Duty days` = duty_days,
          Revenue = round(revenue), `Cost/mi` = round(cpm, 2),
          MPG = round(mpg, 1), `On-time` = paste0(round(on_time * 100, 1), "%"),
          `Odometer (km)` = round(last_odometer_km)) %>%
        arrange(desc(Revenue)) %>%
        dark_dt() %>%
        DT::formatCurrency("Revenue", digits = 0) %>%
        DT::formatCurrency("Cost/mi", digits = 2) %>%
        DT::formatRound(c("Miles", "Odometer (km)"), digits = 0)
    })
  })
}
