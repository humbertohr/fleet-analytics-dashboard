# ==============================================================================
# FLEET ANALYTICS — SYNTHETIC DATA GENERATION ENGINE
# ------------------------------------------------------------------------------
# Generates a realistic, internally-consistent operational dataset for a
# Denver, CO based regional carrier ("Summit Line Logistics"):
#
#   1. vehicles     — fleet master (acquisition, retirement, odometer, cost)
#   2. drivers      — driver roster with multi-year tenure and turnover
#   3. trips        — round-trip dispatch records w/ physical fuel model,
#                     revenue per loaded mile, deadhead, HOS-based duration,
#                     driver double-booking prevention, on-time performance
#   4. maintenance  — preventive (mileage-interval) + corrective (age hazard)
#                     work orders with downtime and cost by system category
#   5. fuel_prices  — daily diesel price series (random walk + seasonality)
#
# Business-logic guarantees (data quality contract):
#   * No trip precedes vehicle acquisition or driver hire date
#   * No trip after a vehicle's retirement date or a driver's exit date
#   * No driver is assigned to two overlapping trips (no double-booking)
#   * Loads never exceed rated payload capacity
#   * Fuel cost = liters consumed x market price on the trip date (physical
#     model: distance / fuel economy, load + winter penalties), never a
#     hard-coded $/liter assumption
#   * Preventive maintenance triggers on odometer intervals; corrective
#     failure rates increase with vehicle age and accumulated mileage
#
# Anchored to a fixed "as of" date for full reproducibility.
# Author: DataMexLabs | Humberto Hernandez Renteria
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lubridate)
})

set.seed(2026)
options(scipen = 999)

# ------------------------------------------------------------------------------
# 0. GLOBAL CONFIGURATION
# ------------------------------------------------------------------------------
CFG <- list(
  as_of_date      = as.Date("2026-07-07"),   # fixed anchor (reproducible)
  history_years   = 2,                        # trip history window
  n_vehicles      = 100,
  n_drivers       = 150,                      # ~1.5 drivers per power unit
                                              # (covers multi-day trips + turnover)
  home_base       = "Denver, CO",
  km_per_mile     = 1.60934,
  diesel_kg_co2_l = 2.68,                     # kg CO2 per liter of diesel
  hos_drive_hrs   = 10                        # max driving hours per duty day
)
CFG$window_start <- CFG$as_of_date - years(CFG$history_years)

# ------------------------------------------------------------------------------
# 1. DAILY DIESEL PRICE SERIES (USD per liter)
#    Bounded random walk around ~$1.00/L (~$3.80/gal) with mild winter
#    seasonality — replaces the original per-trip uniform draw, which made
#    fuel prices uncorrelated across trips on the same day.
# ------------------------------------------------------------------------------
generate_fuel_prices <- function(start_date, end_date) {
  dates <- seq.Date(start_date, end_date, by = "day")
  n     <- length(dates)
  shocks <- rnorm(n, mean = 0, sd = 0.004)          # daily volatility
  walk   <- cumsum(shocks)
  walk   <- walk - seq(0, tail(walk, 1), length.out = n) * 0.5  # soft mean reversion
  seasonal <- 0.035 * cos(2 * pi * (yday(dates) - 15) / 365)    # winter premium
  price <- 1.00 + walk + seasonal
  price <- pmin(pmax(price, 0.86), 1.18)            # plausible bounds
  tibble(
    date = dates,
    diesel_usd_per_liter = round(price, 3),
    diesel_usd_per_gallon = round(price * 3.78541, 2)
  )
}

# ------------------------------------------------------------------------------
# 2. VEHICLES
# ------------------------------------------------------------------------------
generate_vehicles <- function(n = CFG$n_vehicles) {

  # Class specs grounded in real equipment categories:
  #   Van       = Class 2 cargo van (final mile / LTL)
  #   Box Truck = Class 5-6 straight truck (regional distribution)
  #   Truck     = Class 8 tractor (regional & long-haul)
  specs <- tribble(
    ~vehicle_type, ~payload_min, ~payload_max, ~kml_min, ~kml_max, ~price_min, ~price_max,
    "Van",              900,        1600,        6.4,      8.1,      42000,      62000,
    "Box Truck",       4500,        9000,        3.6,      4.7,      75000,     115000,
    "Truck",          18000,       21500,        2.5,      3.1,     145000,     195000
  )
  # kml = km per liter baseline (empty-to-average load, flat terrain)
  # Class 8 payload reflects ~44k lb max cargo on an 80k lb GVW combination

  # Brand/model catalogs matched to the correct class
  catalog <- list(
    "Van" = list(
      Ford      = c("Transit 350"),
      Mercedes  = c("Sprinter 2500", "Sprinter 3500"),
      RAM       = c("ProMaster 3500")
    ),
    "Box Truck" = list(
      Isuzu      = c("NPR-HD", "NRR", "FTR"),
      Hino       = c("L6", "L7"),
      Freightliner = c("M2 106"),
      Mitsubishi = c("Fuso FE160")
    ),
    "Truck" = list(
      Freightliner = c("Cascadia 126"),
      Volvo        = c("VNL 760", "VNL 860"),
      Kenworth     = c("T680"),
      Peterbilt    = c("579"),
      International = c("LT625")
    )
  )

  type <- sample(c("Van", "Truck", "Box Truck"), n, replace = TRUE,
                 prob = c(0.30, 0.40, 0.30))

  # Acquisition: fleet built up over 12 years, heavier buying in recent years
  acq_days_ago <- round(rbeta(n, 1.5, 1.9) * 365.25 * 12)
  acquisition_date <- CFG$as_of_date - acq_days_ago
  manufacture_year <- year(acquisition_date) - sample(0:1, n, TRUE, prob = c(.7, .3))

  brand <- character(n); model <- character(n)
  payload_kg <- numeric(n); kml_base <- numeric(n); purchase_price <- numeric(n)

  for (i in seq_len(n)) {
    sp <- specs[specs$vehicle_type == type[i], ]
    cat_i    <- catalog[[type[i]]]
    brand[i] <- sample(names(cat_i), 1)
    model[i] <- sample(cat_i[[brand[i]]], 1)
    payload_kg[i]     <- round(runif(1, sp$payload_min, sp$payload_max) / 50) * 50
    kml_base[i]       <- round(runif(1, sp$kml_min, sp$kml_max), 2)
    purchase_price[i] <- round(runif(1, sp$price_min, sp$price_max) / 500) * 500
  }

  # Colorado-style plates: ABC-123
  plates <- replicate(n, paste0(
    paste0(sample(LETTERS, 3, TRUE), collapse = ""), "-",
    sample(100:999, 1)))
  while (any(duplicated(plates))) {
    dup <- which(duplicated(plates))
    plates[dup] <- replicate(length(dup), paste0(
      paste0(sample(LETTERS, 3, TRUE), collapse = ""), "-", sample(100:999, 1)))
  }

  age_years <- as.numeric(CFG$as_of_date - acquisition_date) / 365.25

  # Status & retirement: retirement probability rises with age;
  # retired units get an explicit retirement_date (trips must stop there)
  p_retire <- pmin(0.04 + pmax(age_years - 6, 0) * 0.13, 0.70)
  retired  <- runif(n) < p_retire
  retirement_date <- as.Date(rep(NA, n))
  retirement_date[retired] <- acquisition_date[retired] +
    round(runif(sum(retired), 0.75, 0.95) * age_years[retired] * 365.25)

  status <- ifelse(retired, "Retired", "Active")
  # A slice of the active fleet is in the shop "today" (set after maintenance gen)

  # Odometer at acquisition (some units bought used)
  bought_used <- runif(n) < 0.25
  initial_odometer_km <- ifelse(bought_used, round(runif(n, 40000, 220000), -2), 0)

  tibble(
    vehicle_id = sprintf("VH-%04d", seq_len(n)),
    plate_number = plates,
    brand, model,
    manufacture_year,
    vehicle_type = type,
    payload_capacity_kg = payload_kg,
    fuel_economy_kml_base = kml_base,
    purchase_price_usd = purchase_price,
    acquisition_date,
    retirement_date,
    status,
    initial_odometer_km,
    vehicle_age_years = round(age_years, 1)
  ) %>% arrange(vehicle_id)
}

# ------------------------------------------------------------------------------
# 3. DRIVERS — multi-year tenure with realistic turnover
#    (fixes the original: 30 drivers all hired in the last 12 months could not
#     have driven trips from two years ago)
# ------------------------------------------------------------------------------
generate_drivers <- function(n = CFG$n_drivers) {
  first_names <- c("James","Michael","John","Robert","David","William","Richard",
                   "Joseph","Thomas","Christopher","Charles","Daniel","Matthew","Anthony","Mark",
                   "Steven","Andrew","Donald","Joshua","Paul","Kevin","Kenneth","Brian","Timothy",
                   "Ronald","Jason","George","Edward","Jeffrey","Jacob")
  
  last_names <- c("Smith","Johnson","Williams","Brown","Jones","Garcia","Miller",
                  "Davis","Rodriguez","Martinez","Hernandez","Lopez","Wilson","Anderson","Thomas",
                  "Taylor","Moore","Jackson","Martin","Lee","Perez","Thompson","White","Harris",
                  "Sanchez","Clark","Ramirez","Lewis","Robinson","Walker","Young","Allen","King",
                  "Wright","Scott","Torres","Nguyen","Hill","Flores","Green","Adams","Nelson",
                  "Baker","Hall","Rivera","Campbell","Mitchell","Carter","Roberts","Turner")

  nm <- character(0)
  while (length(nm) < n) {
    cand <- paste(sample(first_names, n, TRUE), sample(last_names, n, TRUE))
    nm <- unique(c(nm, cand))
  }
  nm <- nm[seq_len(n)]

  # Hire dates spread over 12 years (fleet-age aligned), mild recent skew
  hire_days_ago <- round(rbeta(n, 1.3, 1.7) * 365.25 * 12)
  hire_date <- CFG$as_of_date - hire_days_ago

  # Turnover: a large share of ever-hired drivers eventually leave
  # (driver turnover is a structural reality in trucking);
  # exit date uniform within tenure; recent hires mostly still active
  tenure_days <- as.numeric(CFG$as_of_date - hire_date)
  p_left <- pmin(tenure_days / 365.25 * 0.095, 0.70)
  left <- runif(n) < p_left
  exit_date <- as.Date(rep(NA, n))
  exit_date[left] <- hire_date[left] +
    round(runif(sum(left), 90, pmax(tenure_days[left] - 1, 91)))
  exit_date <- pmin(exit_date, CFG$as_of_date - 1, na.rm = FALSE)

  cdl_class <- sample(c("Class A", "Class B", "Class C"), n, TRUE,
                      prob = c(0.55, 0.30, 0.15))
  # Latent quality traits drive per-driver KPI variation in trips
  tibble(
    driver_id = sprintf("DRV-%04d", seq_len(n)),
    driver_name = nm,
    cdl_class,
    hire_date,
    exit_date,
    employment_status = ifelse(is.na(exit_date), "Active", "Separated"),
    pay_per_mile_usd = round(runif(n, 0.52, 0.72), 2),
    reliability_score = round(rbeta(n, 12, 1.6), 3), # drives on-time probability
    activity_weight   = rgamma(n, shape = 3, rate = 1) # some drivers run more
  )
}

# ------------------------------------------------------------------------------
# 4. DESTINATION NETWORK (from Denver, CO)
# ------------------------------------------------------------------------------
destination_network <- function() {
  tribble(
    ~city,                  ~one_way_miles, ~segment,
    "Colorado Springs, CO",   70,  "Local",
    "Boulder, CO",            30,  "Local",
    "Fort Collins, CO",       65,  "Local",
    "Greeley, CO",            60,  "Local",
    "Pueblo, CO",            115,  "Local",
    "Vail, CO",              100,  "Local",
    "Cheyenne, WY",          100,  "Local",
    "Grand Junction, CO",    245,  "Regional",
    "Durango, CO",           330,  "Regional",
    "Albuquerque, NM",       450,  "Regional",
    "Santa Fe, NM",          385,  "Regional",
    "Salt Lake City, UT",    525,  "Regional",
    "Rapid City, SD",        390,  "Regional",
    "Wichita, KS",           430,  "Regional",
    "Omaha, NE",             540,  "Regional",
    "Kansas City, MO",       600,  "Regional",
    "Amarillo, TX",          425,  "Regional",
    "Phoenix, AZ",           820,  "Mid-range",
    "Dallas, TX",            780,  "Mid-range",
    "Minneapolis, MN",       860,  "Mid-range",
    "St. Louis, MO",         860,  "Mid-range",
    "Austin, TX",            925,  "Mid-range",
    "Chicago, IL",          1000,  "Mid-range",
    "Houston, TX",          1020,  "Mid-range",
    "Las Vegas, NV",         750,  "Mid-range",
    "Los Angeles, CA",      1015,  "Mid-range",
    "Portland, OR",         1245,  "Long-haul",
    "Seattle, WA",          1315,  "Long-haul",
    "San Francisco, CA",    1270,  "Long-haul",
    "Nashville, TN",        1160,  "Long-haul",
    "Atlanta, GA",          1400,  "Long-haul",
    "New York City, NY",    1780,  "Long-haul",
    "Miami, FL",            2100,  "Long-haul"
  ) %>%
    mutate(one_way_km = round(one_way_miles * CFG$km_per_mile))
}

# Which lanes can a given vehicle class serve? Vans do final-mile/local work;
# box trucks run local + regional; tractors run everything but favor distance.
segment_probs <- function(vehicle_type) {
  switch(vehicle_type,
    "Van"       = c(Local = .82, Regional = .18, `Mid-range` = 0,   `Long-haul` = 0),
    "Box Truck" = c(Local = .45, Regional = .45, `Mid-range` = .10, `Long-haul` = 0),
    "Truck"     = c(Local = .08, Regional = .40, `Mid-range` = .32, `Long-haul` = .20)
  )
}

# Revenue per loaded mile by segment (2024-26 blended contract/spot),
# discounted by equipment class (smaller equipment bills lower linehaul)
rate_per_mile <- function(segment, vehicle_type) {
  base <- switch(segment,
    "Local"     = runif(1, 3.00, 3.60),
    "Regional"  = runif(1, 2.40, 2.95),
    "Mid-range" = runif(1, 2.10, 2.55),
    "Long-haul" = runif(1, 1.90, 2.35))
  mult <- switch(vehicle_type, "Van" = 0.62, "Box Truck" = 0.82, "Truck" = 1.00)
  base * mult
}

avg_speed_kmh <- function(segment) {
  switch(segment, "Local" = 42, "Regional" = 78, "Mid-range" = 85, "Long-haul" = 88)
}

# ------------------------------------------------------------------------------
# 5. TRIPS — sequential per-vehicle dispatch simulation
# ------------------------------------------------------------------------------
generate_trips <- function(vehicles, drivers, fuel_prices) {

  dest <- destination_network()
  price_lookup <- setNames(fuel_prices$diesel_usd_per_liter,
                           as.character(fuel_prices$date))

  all_trips <- vector("list", nrow(vehicles))

  for (v in seq_len(nrow(vehicles))) {
    veh <- vehicles[v, ]

    # Vehicle's operating window inside the 2-year history
    win_start <- max(veh$acquisition_date + 7, CFG$window_start)
    win_end   <- min(coalesce(veh$retirement_date, CFG$as_of_date) - 1,
                     CFG$as_of_date - 1)
    if (win_start >= win_end) { all_trips[[v]] <- NULL; next }

    sprob <- segment_probs(veh$vehicle_type)
    cursor <- win_start + sample(0:6, 1)
    rows <- list(); k <- 0

    while (cursor < win_end) {
      seg <- sample(names(sprob), 1, prob = sprob)
      d   <- dest %>% filter(segment == seg) %>% slice_sample(n = 1)

      # --- Round-trip structure: outbound loaded, backhaul secured ~72% ---
      has_backhaul <- runif(1) < 0.72
      total_km     <- 2 * d$one_way_km
      loaded_km    <- d$one_way_km + if (has_backhaul) d$one_way_km else 0
      deadhead_km  <- total_km - loaded_km

      # --- Load: heavier for longer segments, capped at rated payload ---
      util_range <- switch(seg,
        "Local"     = c(0.30, 0.70),
        "Regional"  = c(0.45, 0.88),
        "Mid-range" = c(0.55, 0.95),
        "Long-haul" = c(0.60, 0.97))
      out_util  <- runif(1, util_range[1], util_range[2])
      load_out  <- round(min(out_util * veh$payload_capacity_kg,
                             veh$payload_capacity_kg) / 10) * 10
      load_back <- if (has_backhaul)
        round(runif(1, 0.35, 0.85) * veh$payload_capacity_kg / 10) * 10 else 0

      # --- Duration under HOS: driving hrs -> duty days (round trip) ---
      drive_hrs <- total_km / avg_speed_kmh(seg) +
        runif(1, 0.5, 2.5)                          # dock/dwell time
      duty_days <- max(1, ceiling(drive_hrs / CFG$hos_drive_hrs))
      trip_end  <- cursor + (duty_days - 1)
      if (trip_end > win_end) break

      # --- Physical fuel model ---
      # baseline economy degraded by load (up to +18% burn at full payload,
      # averaged over both legs) and winter ops out of Denver (+6% Dec-Feb)
      avg_util_trip <- (out_util + if (has_backhaul) load_back / veh$payload_capacity_kg else 0) / 2
      load_penalty  <- 1 + 0.18 * avg_util_trip
      winter        <- if (month(cursor) %in% c(12, 1, 2)) 1.06 else 1.00
      eff_kml       <- veh$fuel_economy_kml_base / (load_penalty * winter) *
                       runif(1, 0.95, 1.05)          # route/driver variance
      fuel_liters   <- total_km / eff_kml
      ppl           <- price_lookup[[as.character(cursor)]]
      fuel_cost     <- fuel_liters * ppl

      # --- Revenue on loaded miles only ---
      loaded_miles  <- loaded_km / CFG$km_per_mile
      out_miles     <- d$one_way_miles
      rpm           <- rate_per_mile(seg, veh$vehicle_type)
      revenue       <- out_miles * rpm +
        if (has_backhaul) d$one_way_miles * rpm * runif(1, 0.72, 0.88) else 0

      total_miles <- total_km / CFG$km_per_mile

      k <- k + 1
      rows[[k]] <- tibble(
        vehicle_id   = veh$vehicle_id,
        trip_start   = cursor,
        trip_end     = trip_end,
        duty_days    = duty_days,
        origin       = CFG$home_base,
        destination  = d$city,
        segment      = seg,
        one_way_km   = d$one_way_km,
        total_km     = total_km,
        loaded_km    = loaded_km,
        deadhead_km  = deadhead_km,
        has_backhaul = has_backhaul,
        load_out_kg  = load_out,
        load_back_kg = load_back,
        utilization_out_pct = round(load_out / veh$payload_capacity_kg * 100, 1),
        fuel_liters  = round(fuel_liters, 1),
        diesel_usd_per_liter = ppl,
        fuel_cost_usd    = round(fuel_cost, 2),
        revenue_usd      = round(revenue, 2),
        rate_per_loaded_mile = round(revenue / loaded_miles, 2),
        total_miles      = round(total_miles, 1)
      )

      # Idle gap before next dispatch (fleet utilization ~65-80%),
      # with retail peak season (Oct-Dec) busier and Jan-Feb slower
      gap <- sample(0:4, 1, prob = c(.38, .30, .17, .10, .05))
      m <- month(trip_end)
      if (m %in% 10:12 && gap > 0 && runif(1) < 0.5) gap <- gap - 1
      if (m %in% 1:2  && runif(1) < 0.35) gap <- gap + 1
      cursor <- trip_end + 1 + gap
    }
    all_trips[[v]] <- bind_rows(rows)
  }

  trips <- bind_rows(all_trips) %>% arrange(trip_start, vehicle_id)
  trips$trip_id <- sprintf("TRP-%06d", seq_len(nrow(trips)))

  # ---- Driver assignment: greedy by date, no double-booking ----------------
  drv <- drivers
  busy_until <- setNames(rep(as.Date("1900-01-01"), nrow(drv)), drv$driver_id)
  assigned <- character(nrow(trips))

  for (i in seq_len(nrow(trips))) {
    ts <- trips$trip_start[i]; te <- trips$trip_end[i]
    ok <- which(drv$hire_date <= ts &
                (is.na(drv$exit_date) | drv$exit_date >= te) &
                busy_until[drv$driver_id] < ts)
    if (length(ok) == 0) {
      # No qualified driver free -> load goes uncovered (real dispatch
      # constraint). The trip is dropped rather than double-booking a driver.
      assigned[i] <- NA_character_
      next
    }
    pick <- if (length(ok) == 1) ok else
      sample(ok, 1, prob = drv$activity_weight[ok])
    assigned[i] <- drv$driver_id[pick]
    busy_until[assigned[i]] <- te
  }
  trips$driver_id <- assigned
  n_uncovered <- sum(is.na(trips$driver_id))
  message(sprintf("  Uncovered loads dropped (no qualified driver free): %d of %d (%.1f%%)",
                  n_uncovered, nrow(trips), 100 * n_uncovered / nrow(trips)))
  trips <- trips %>% filter(!is.na(driver_id))
  trips$trip_id <- sprintf("TRP-%06d", seq_len(nrow(trips)))  # re-sequence IDs

  # ---- Driver-dependent economics & service performance --------------------
  trips <- trips %>%
    left_join(drv %>% select(driver_id, pay_per_mile_usd, reliability_score),
              by = "driver_id") %>%
    mutate(
      driver_pay_usd  = round(total_miles * pay_per_mile_usd, 2),
      tolls_misc_usd  = round(total_miles * runif(n(), 0.04, 0.10), 2),
      overhead_alloc_usd = round(total_miles * 0.42, 2),  # insurance/admin/depr.
      total_cost_usd  = round(fuel_cost_usd + driver_pay_usd +
                              tolls_misc_usd + overhead_alloc_usd, 2),
      margin_usd      = round(revenue_usd - total_cost_usd, 2),
      # On-time: driver reliability x distance risk x winter risk
      p_on_time = pmin(0.995,
        reliability_score * 1.065 -
        (segment == "Long-haul") * 0.05 -
        (segment == "Mid-range") * 0.03 -
        (month(trip_start) %in% c(12, 1, 2)) * 0.04),
      on_time = runif(n()) < p_on_time,
      co2_kg  = round(fuel_liters * CFG$diesel_kg_co2_l, 1)
    ) %>%
    select(-p_on_time, -pay_per_mile_usd, -reliability_score) %>%
    mutate(
      year      = year(trip_start),
      month     = month(trip_start, label = TRUE, abbr = TRUE),
      month_date = floor_date(trip_start, "month"),
      quarter   = paste0(year(trip_start), "-Q", quarter(trip_start)),
      weekday   = wday(trip_start, label = TRUE, abbr = TRUE, week_start = 1)
    )

  # ---- Odometer: cumulative km per vehicle ---------------------------------
  trips <- trips %>%
    group_by(vehicle_id) %>%
    arrange(trip_start, .by_group = TRUE) %>%
    mutate(cum_km = cumsum(total_km)) %>%
    ungroup() %>%
    left_join(vehicles %>% select(vehicle_id, initial_odometer_km), by = "vehicle_id") %>%
    mutate(odometer_km = initial_odometer_km + cum_km) %>%
    select(-initial_odometer_km) %>%
    arrange(trip_start, trip_id)

  trips
}

# ------------------------------------------------------------------------------
# 6. MAINTENANCE — preventive by odometer interval, corrective by age hazard
# ------------------------------------------------------------------------------
generate_maintenance <- function(vehicles, trips) {

  pm_interval_km <- c("Van" = 25000, "Box Truck" = 22000, "Truck" = 20000)
  pm_cost_range  <- list("Van" = c(180, 350), "Box Truck" = c(300, 620),
                         "Truck" = c(420, 950))

  corrective_catalog <- tribble(
    ~category,        ~w,   ~cost_lo, ~cost_hi, ~down_lo, ~down_hi,
    "Brakes",         .18,     400,     1500,      1,        3,
    "Tires",          .20,     350,     1400,      1,        2,
    "Electrical",     .13,     200,     1200,      1,        3,
    "Cooling System", .10,     300,     1800,      1,        4,
    "Suspension",     .10,     450,     2600,      2,        5,
    "Engine",         .10,     900,     6500,      3,       10,
    "Transmission",   .06,    1600,     8500,      4,       12,
    "Aftertreatment / DPF", .08, 550,   3600,      1,        5,
    "Body / Cab",     .05,     250,     2000,      1,        4
  )

  events <- list(); k <- 0

  for (v in seq_len(nrow(vehicles))) {
    veh <- vehicles[v, ]
    vt  <- trips %>% filter(vehicle_id == veh$vehicle_id) %>% arrange(trip_start)
    if (nrow(vt) == 0) next

    # ---- Preventive: every PM interval of accumulated km ----
    interval <- pm_interval_km[[veh$vehicle_type]] * runif(1, 0.92, 1.08)
    next_pm  <- ceiling(vt$odometer_km[1] / interval) * interval
    for (r in seq_len(nrow(vt))) {
      while (vt$odometer_km[r] >= next_pm) {
        k <- k + 1
        cr <- pm_cost_range[[veh$vehicle_type]]
        events[[k]] <- tibble(
          vehicle_id = veh$vehicle_id,
          event_date = vt$trip_end[r] + 1,
          maintenance_type = "Preventive",
          category = "Scheduled Service (PM)",
          odometer_km = round(next_pm),
          cost_usd = round(runif(1, cr[1], cr[2]), 2),
          downtime_days = sample(c(0.5, 1), 1, prob = c(.6, .4))
        )
        next_pm <- next_pm + interval
      }
    }

    # ---- Corrective: Poisson count, rate rises with age & mileage ----
    yrs_in_window <- as.numeric(
      min(coalesce(veh$retirement_date, CFG$as_of_date), CFG$as_of_date) -
      max(veh$acquisition_date, CFG$window_start)) / 365.25
    if (yrs_in_window <= 0) next
    base_rate <- 0.9 + 0.22 * veh$vehicle_age_years +
                 max(vt$odometer_km) / 400000
    n_corr <- rpois(1, lambda = base_rate * yrs_in_window)
    if (n_corr > 0) {
      idx <- sample(seq_len(nrow(vt)), min(n_corr, nrow(vt)))
      for (j in idx) {
        cc <- corrective_catalog %>% slice_sample(n = 1, weight_by = w)
        sev <- rbeta(1, 2, 3)                     # severity skewed low
        k <- k + 1
        events[[k]] <- tibble(
          vehicle_id = veh$vehicle_id,
          event_date = vt$trip_end[j] + sample(0:2, 1),
          maintenance_type = "Corrective",
          category = cc$category,
          odometer_km = round(vt$odometer_km[j]),
          cost_usd = round(cc$cost_lo + sev * (cc$cost_hi - cc$cost_lo), 2),
          downtime_days = round(cc$down_lo + sev * (cc$down_hi - cc$down_lo), 1)
        )
      }
    }
  }

  bind_rows(events) %>%
    filter(event_date <= CFG$as_of_date) %>%
    arrange(event_date) %>%
    mutate(work_order_id = sprintf("WO-%05d", row_number()),
           month_date = floor_date(event_date, "month")) %>%
    relocate(work_order_id)
}

# ------------------------------------------------------------------------------
# 7. BUILD, VALIDATE, EXPORT
# ------------------------------------------------------------------------------
message("Generating fuel price series ...")
fuel_prices <- generate_fuel_prices(CFG$window_start - 30, CFG$as_of_date)

message("Generating vehicles ...")
vehicles <- generate_vehicles()

message("Generating drivers ...")
drivers <- generate_drivers()

message("Simulating trips (this is the slow part) ...")
trips <- generate_trips(vehicles, drivers, fuel_prices)

message("Generating maintenance work orders ...")
maintenance <- generate_maintenance(vehicles, trips)

# Flag a realistic slice of active fleet as "In Shop" as of today, driven by
# a recent open corrective work order
recent_wo <- maintenance %>%
  filter(maintenance_type == "Corrective",
         event_date >= CFG$as_of_date - 10) %>%
  distinct(vehicle_id) %>% pull(vehicle_id)
vehicles <- vehicles %>%
  mutate(status = ifelse(status == "Active" & vehicle_id %in% recent_wo,
                         "In Shop", status))

# ---- Data quality contract ---------------------------------------------------
check <- function(label, expr) {
  ok <- isTRUE(expr)
  cat(sprintf("  [%s] %s\n", ifelse(ok, "PASS", "FAIL"), label))
  if (!ok) stop("Data quality check failed: ", label)
}
vt <- trips %>% left_join(vehicles, by = "vehicle_id")
dt <- trips %>% left_join(drivers,  by = "driver_id")
overlap <- trips %>% group_by(driver_id) %>% arrange(trip_start, .by_group = TRUE) %>%
  mutate(prev_end = lag(trip_end)) %>%
  filter(!is.na(prev_end), trip_start <= prev_end) %>% nrow()

cat("\nDATA QUALITY CONTRACT\n---------------------\n")
check("No trip before vehicle acquisition",
      all(vt$trip_start >= vt$acquisition_date))
check("No trip after vehicle retirement",
      all(is.na(vt$retirement_date) | vt$trip_end < vt$retirement_date))
check("No trip before driver hire date",
      all(dt$trip_start >= dt$hire_date))
check("No trip after driver exit date",
      all(is.na(dt$exit_date) | dt$trip_end <= dt$exit_date))
check("No driver double-booking", overlap == 0)
check("No load exceeds payload capacity",
      all(vt$load_out_kg <= vt$payload_capacity_kg &
          vt$load_back_kg <= vt$payload_capacity_kg))
check("Fuel cost = liters x market price (within rounding)",
      max(abs(trips$fuel_cost_usd -
              trips$fuel_liters * trips$diesel_usd_per_liter)) < 1)
check("No missing values in trips",
      !anyNA(trips %>% select(-month)))

# ---- Summary ------------------------------------------------------------------
cat("\nDATASET SUMMARY\n---------------\n")
cat("Vehicles:   ", nrow(vehicles), " | Active:", sum(vehicles$status == "Active"),
    "| In Shop:", sum(vehicles$status == "In Shop"),
    "| Retired:", sum(vehicles$status == "Retired"), "\n")
cat("Drivers:    ", nrow(drivers), " | Active:",
    sum(drivers$employment_status == "Active"), "\n")
cat("Trips:      ", format(nrow(trips), big.mark = ","),
    "| ", format(min(trips$trip_start)), "to", format(max(trips$trip_start)), "\n")
cat("Work orders:", format(nrow(maintenance), big.mark = ","), "\n")
cat("Revenue:     $", format(round(sum(trips$revenue_usd)), big.mark = ","), "\n")
cat("Total cost:  $", format(round(sum(trips$total_cost_usd)), big.mark = ","), "\n")
cat("Op. ratio:  ", round(sum(trips$total_cost_usd) / sum(trips$revenue_usd) * 100, 1), "%\n")
cat("Cost/mile:  $", round(sum(trips$total_cost_usd) / sum(trips$total_miles), 2), "\n")
cat("Rev/mile:   $", round(sum(trips$revenue_usd) / sum(trips$total_miles), 2), "\n")
cat("Deadhead %: ", round(sum(trips$deadhead_km) / sum(trips$total_km) * 100, 1), "%\n")
cat("On-time %:  ", round(mean(trips$on_time) * 100, 1), "%\n")

# ---- Export --------------------------------------------------------------------
# Run from the project root: Rscript data-raw/generate_fleet_data.R
out_dir <- "data"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

saveRDS(vehicles,    file.path(out_dir, "vehicles.rds"))
saveRDS(drivers,     file.path(out_dir, "drivers.rds"))
saveRDS(trips,       file.path(out_dir, "trips.rds"))
saveRDS(maintenance, file.path(out_dir, "maintenance.rds"))
saveRDS(fuel_prices, file.path(out_dir, "fuel_prices.rds"))

write.csv(vehicles,    file.path(out_dir, "vehicles.csv"),    row.names = FALSE)
write.csv(drivers,     file.path(out_dir, "drivers.csv"),     row.names = FALSE)
write.csv(trips,       file.path(out_dir, "trips.csv"),       row.names = FALSE)
write.csv(maintenance, file.path(out_dir, "maintenance.csv"), row.names = FALSE)

cat("\nAll tables exported to", normalizePath(out_dir), "\n")
