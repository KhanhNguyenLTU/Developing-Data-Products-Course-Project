library(shiny)

# Plotting 
library(ggplot2)
library(rCharts)
library(data.table)
library(reshape2)
library(dplyr)
library(markdown)


source("helper.R", local = TRUE)

# Load data
dt <- fread('storm.csv') %>% mutate(EVTYPE = tolower(EVTYPE))
evtypes <- sort(unique(dt$EVTYPE))

# Shiny server 
shinyServer(function(input, output, session) {
  
  # Define and initialize reactive values
  values <- reactiveValues()
  values$evtypes <- evtypes
  
  # Create event type checkbox
  output$evtypeControls <- renderUI({
    checkboxGroupInput('evtypes', 'Event types', evtypes, selected=values$evtypes)
  })
  
  # Add observers on clear and select all buttons
  observe({
    if(input$clear_all == 0) return()
    values$evtypes <- c()
  })
  
  observe({
    if(input$select_all == 0) return()
    values$evtypes <- evtypes
  })
  
  # Preapre datasets
  
  dt.agg <- reactive({
    aggregate_by_state(dt, input$range[1], input$range[2], input$evtypes)
  })

  dt.agg.year <- reactive({
    aggregate_by_year(dt, input$range[1], input$range[2], input$evtypes)
  })
  
  dataTable <- reactive({
    prepare_downolads(dt.agg())
  })
  
  # Render Plots
  

  # Events by year
  output$eventsByYear <- renderChart({
    plot_events_by_year(dt.agg.year())
  })
  
  # Population impact by year
  output$populationImpact <- renderChart({
    plot_impact_by_year(
      dt = dt.agg.year() %>% select(Year, Injuries, Fatalities),
      dom = "populationImpact",
      yAxisLabel = "Affected",
      desc = TRUE
    )
  })
  
  # Economic impact by year
  output$economicImpact <- renderChart({
    plot_impact_by_year(
      dt = dt.agg.year() %>% select(Year, Crops, Property),
      dom = "economicImpact",
      yAxisLabel = "Total damage (Million USD)"
    )
  })
  
  # Render data table and create download handler
  output$table <- renderDataTable(
    {dataTable()}, options = list(bFilter = FALSE, iDisplayLength = 50))
  
  output$downloadData <- downloadHandler(
    filename = 'data.csv',
    content = function(file) {
      write.csv(dataTable(), file, row.names=FALSE)
    }
  )
})
