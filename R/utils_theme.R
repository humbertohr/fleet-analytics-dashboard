# ==============================================================================
# utils_theme.R — design tokens, formatting, and shared plot styling
# ==============================================================================

# ---- Design tokens -----------------------------------------------------------
PAL <- list(
  bg        = "#0F1822",
  surface   = "#17232F",
  surface2  = "#1D2B3A",
  border    = "#26374A",
  text      = "#DCE6EE",
  muted     = "#8CA0B3",
  primary   = "#5E9BD3",   # steel blue — brand accent
  primary_d = "#3D6E9E",
  good      = "#46A882",
  warn      = "#D9A441",
  bad       = "#C95C54",
  grid      = "#22303F"
)

# Categorical ramps (consistent meaning across every tab)
PAL$vehicle_type <- c("Truck" = "#5E9BD3", "Box Truck" = "#D9A441", "Van" = "#46A882")
PAL$segment <- c("Local" = "#46A882", "Regional" = "#5E9BD3",
                 "Mid-range" = "#D9A441", "Long-haul" = "#C95C54")
PAL$maint <- c("Preventive" = "#5E9BD3", "Corrective" = "#C95C54")

app_theme <- function() {
  bslib::bs_theme(
    version = 5,
    bg = PAL$bg, fg = PAL$text, primary = PAL$primary,
    secondary = PAL$surface2, success = PAL$good,
    warning = PAL$warn, danger = PAL$bad,
    base_font    = bslib::font_google("Inter", wght = c(400, 500, 600),
                                      local = FALSE),
    heading_font = bslib::font_google("Barlow Semi Condensed",
                                      wght = c(500, 600, 700), local = FALSE),
    "card-bg" = PAL$surface,
    "border-color" = PAL$border,
    "navbar-bg" = "#0B121A"
  )
}

# ---- Number formatting ---------------------------------------------------------
fmt_usd  <- function(x, acc = 1) scales::dollar(x, accuracy = acc, big.mark = ",")
fmt_usd_k <- function(x) {
  dplyr::case_when(
    abs(x) >= 1e6 ~ paste0("$", scales::comma(x / 1e6, accuracy = 0.1), "M"),
    abs(x) >= 1e3 ~ paste0("$", scales::comma(x / 1e3, accuracy = 1), "K"),
    TRUE          ~ scales::dollar(x, accuracy = 1))
}
fmt_num  <- function(x, acc = 1) scales::comma(x, accuracy = acc)
fmt_pct  <- function(x, acc = 0.1) scales::percent(x, accuracy = acc)

# ---- KPI value box with period-over-period delta -------------------------------
# `direction_good`: +1 if an increase is good (revenue), -1 if bad (cost per mile)
kpi_box <- function(title, value, delta = NULL, direction_good = 1, note = NULL) {
  delta_ui <- NULL
  if (!is.null(delta) && is.finite(delta)) {
    up   <- delta >= 0
    good <- (delta * direction_good) >= 0
    delta_ui <- htmltools::span(
      style = paste0("color:", if (good) PAL$good else PAL$bad,
                     "; font-size:0.85rem; font-weight:600;"),
      paste0(if (up) "\u25B2 " else "\u25BC ", fmt_pct(abs(delta))),
      htmltools::span(" vs prior period",
        style = paste0("color:", PAL$muted, "; font-weight:400;"))
    )
  }
  bslib::value_box(
    title = title,
    value = htmltools::span(value, style = "font-variant-numeric: tabular-nums;"),
    delta_ui,
    if (!is.null(note)) htmltools::span(note,
      style = paste0("color:", PAL$muted, "; font-size:0.78rem;")),
    theme = bslib::value_box_theme(bg = PAL$surface, fg = PAL$text)
  )
}

pct_change <- function(cur, prev) {
  if (is.na(prev) || prev == 0) return(NA_real_)
  (cur - prev) / abs(prev)
}

# ---- Plotly styling -------------------------------------------------------------
plt <- function(p, legend = TRUE, margin = list(l = 55, r = 15, t = 30, b = 45)) {
  p %>%
    plotly::layout(
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(0,0,0,0)",
      font   = list(family = "Inter, sans-serif", color = PAL$text, size = 12),
      margin = margin,
      hoverlabel = list(bgcolor = PAL$surface2,
                        bordercolor = PAL$border,
                        font = list(color = PAL$text)),
      xaxis  = list(gridcolor = PAL$grid, zerolinecolor = PAL$grid,
                    linecolor = PAL$border, title = list(font = list(color = PAL$muted))),
      yaxis  = list(gridcolor = PAL$grid, zerolinecolor = PAL$grid,
                    linecolor = PAL$border, title = list(font = list(color = PAL$muted))),
      legend = if (legend) list(orientation = "h", y = 1.12, x = 0,
                                font = list(color = PAL$muted)) else NULL,
      showlegend = legend
    ) %>%
    plotly::config(displayModeBar = FALSE)
}

# ---- DT styling ------------------------------------------------------------------
dark_dt <- function(df, page_len = 10, ...) {
  DT::datatable(
    df, rownames = FALSE, class = "compact stripe hover",
    options = list(pageLength = page_len, dom = "ftip",
                   language = list(search = "Filter:")),
    ...
  )
}
