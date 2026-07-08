# ==============================================================================
# mod_data.R — Data Explorer
# Browse any base table with the global filters applied where relevant,
# and download the current view as CSV
# ==============================================================================

dataUI <- function(id) {
  ns <- NS(id)
  card(
    card_header(
      div(class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
        span("Data Explorer"),
        div(class = "d-flex gap-2 align-items-center",
          selectInput(ns("dataset"), NULL,
            choices = c("Trips (filtered)" = "trips",
                        "Vehicles" = "vehicles",
                        "Drivers" = "drivers",
                        "Maintenance (filtered)" = "maintenance",
                        "Diesel prices" = "fuel_prices"),
            width = "230px"),
          downloadButton(ns("download"), "Download CSV",
                         class = "btn-sm btn-outline-primary"))
      )
    ),
    DT::DTOutput(ns("table"))
  )
}

dataServer <- function(id, filtered, maint_filtered, vehicles, drivers, fuel_prices) {
  moduleServer(id, function(input, output, session) {

    current <- reactive({
      switch(input$dataset,
        trips       = filtered() %>% select(-month_date),
        vehicles    = vehicles,
        drivers     = drivers %>% select(-activity_weight, -reliability_score),
        maintenance = maint_filtered() %>% select(-month_date),
        fuel_prices = fuel_prices)
    })

    output$table <- DT::renderDT({
      dark_dt(current(), page_len = 12)
    })

    output$download <- downloadHandler(
      filename = function() paste0("fleet_", input$dataset, "_",
                                   format(Sys.Date(), "%Y%m%d"), ".csv"),
      content = function(file) write.csv(current(), file, row.names = FALSE)
    )
  })
}
