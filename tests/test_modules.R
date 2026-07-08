# ==============================================================================
# tests/test_modules.R — headless smoke test
# Renders every output of every module against the real dataset.
# Run from project root:  Rscript tests/test_modules.R
# ==============================================================================
suppressPackageStartupMessages({
  library(shiny); library(bslib); library(dplyr); library(tidyr)
  library(lubridate); library(plotly); library(DT); library(scales); library(htmltools)
})
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)

vehicles    <- readRDS("data/vehicles.rds")
drivers     <- readRDS("data/drivers.rds")
maintenance <- readRDS("data/maintenance.rds")
fuel_prices <- readRDS("data/fuel_prices.rds")
trips <- readRDS("data/trips.rds") %>%
  left_join(vehicles %>% select(vehicle_id, vehicle_type), by = "vehicle_id")

AS_OF <- max(trips$trip_start)
d1 <- AS_OF - 364; d0 <- d1 - 365

cur  <- reactive(trips %>% filter(trip_start >= d1, trip_start <= AS_OF))
prev <- reactive(trips %>% filter(trip_start >= d0, trip_start < d1))
fdays <- reactive(list(cur = 365 * 89, prev = 365 * 89))
mnt <- reactive(maintenance %>% filter(event_date >= d1, event_date <= AS_OF))

touch_all <- function(module, label, ...) {
  testServer(module, args = list(...), {
    outs <- ls(session$output, all.names = FALSE)
    # renderUI/renderDT/renderPlotly names are registered lazily; probe known ids
    for (o in outputs_to_touch) {
      res <- tryCatch({ session$output[[o]]; "OK" },
                      error = function(e) paste("FAIL:", conditionMessage(e)))
      cat(sprintf("  [%s] %s$%s\n", ifelse(res == "OK", "OK", "!!"), label, o))
      if (res != "OK") cat("      ", res, "\n")
    }
  })
}

run <- function(module, label, ids, ...) {
  cat(label, "\n")
  args <- list(...)
  testServer(module, args = args, {
    for (o in ids) {
      res <- tryCatch({ invisible(output[[o]]); "OK" },
                      error = function(e) paste0("FAIL: ", conditionMessage(e)))
      cat(sprintf("  [%s] %s\n", ifelse(res == "OK", "OK", "!!"), o))
      if (res != "OK") cat("       ", res, "\n")
    }
  })
}

run(overviewServer, "OVERVIEW",
    c("kpi_revenue","kpi_or","kpi_cpm","kpi_ontime","kpi_util","kpi_deadhead",
      "rev_cost","cost_structure","volume","margin_seg"),
    filtered = cur, previous = prev, fleet_days = fdays)

run(operationsServer, "OPERATIONS",
    c("segment_mix","top_lanes","heatmap","on_time","load_util","backhaul"),
    filtered = cur)

run(fleetServer, "FLEET",
    c("composition","age_mileage","cpm_type","veh_util","veh_table"),
    filtered = cur, vehicles = vehicles)

run(driversServer, "DRIVERS",
    c("top_miles","risk","headcount","tenure","leaderboard"),
    filtered = cur, drivers = drivers, as_of_date = AS_OF)

run(maintenanceServer, "MAINTENANCE",
    c("kpi_cost","kpi_downtime","kpi_corr_share","kpi_cost_per_mile",
      "monthly","pareto","aging","wo_table"),
    maint_filtered = mnt, filtered = cur, vehicles = vehicles)

run(fuelServer, "FUEL",
    c("price_cpm","mpg_class","load_econ","co2"),
    filtered = cur, fuel_prices = fuel_prices)

cat("DATA EXPLORER\n")
testServer(dataServer, args = list(filtered = cur, maint_filtered = mnt,
    vehicles = vehicles, drivers = drivers, fuel_prices = fuel_prices), {
  for (ds in c("trips","vehicles","drivers","maintenance","fuel_prices")) {
    session$setInputs(dataset = ds)
    res <- tryCatch({ invisible(output$table); "OK" },
                    error = function(e) paste0("FAIL: ", conditionMessage(e)))
    cat(sprintf("  [%s] table:%s\n", ifelse(res == "OK", "OK", "!!"), ds))
    if (res != "OK") cat("       ", res, "\n")
  }
})

cat("\nAll module outputs exercised.\n")
