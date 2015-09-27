library(shiny)
library(ggplot2)
library(rCharts)
library(ggvis)
library(data.table)
library(reshape2)
library(dplyr)
library(markdown)
library(mapproj)
library(maps)
source("some_functions.R", local = TRUE)

states_map <- map_data("state")
dt <- fread('data_minimized.csv') %>% mutate(EVTYPE = tolower(EVTYPE))
evtypes <- sort(unique(dt$EVTYPE))
shinyServer(function(input, output, session) {
  values <- reactiveValues()
  values$evtypes <- evtypes
  output$evtypeControls <- renderUI({
    checkboxGroupInput('evtypes', 'Event types', evtypes, selected=values$evtypes)
  })
  observe({
    if(input$clear_all == 0) return()
    values$evtypes <- c()
  })
  observe({
    if(input$select_all == 0) return()
    values$evtypes <- evtypes
  })
  dt.agg <- reactive({
    aggregate_by_state(dt, input$range[1], input$range[2], input$evtypes)
  })
  dt.agg.year <- reactive({
    aggregate_by_year(dt, input$range[1], input$range[2], input$evtypes)
  })
  dataTable <- reactive({
    prepare_downloads(dt.agg())
  })
  output$populationImpactByState <- renderPlot({
    print(plot_impact_by_state (
      dt = compute_affected(dt.agg(), input$populationCategory),
      states_map = states_map, 
      year_min = input$range[1],
      year_max = input$range[2],
      title = "Population impact from %d - to %d in number of affected people",
      fill = "Affected"
    ))
  })
  output$economicImpactByState <- renderPlot({
    print(plot_impact_by_state(
      dt = compute_damages(dt.agg(), input$economicCategory),
      states_map = states_map, 
      year_min = input$range[1],
      year_max = input$range[2],
      title = "Economical impact from %d - to %d in Million of USD($))",
      fill = "Damages"
    ))
  })
  output$eventsByYear <- renderChart({
    plot_events_by_year(dt.agg.year())
  })
  output$populationImpact <- renderChart({
    plot_impact_by_year(
      dt = dt.agg.year() %>% select(Year, Injuries, Fatalities),
      dom = "populationImpact",
      yAxisLabel = "Affected",
      desc = TRUE
    )
  })
  output$economicImpact <- renderChart({
    plot_impact_by_year(
      dt = dt.agg.year() %>% select(Year, Crops, Property),
      dom = "economicImpact",
      yAxisLabel = "Total damage in Million of USD($)"
    )
  })
  output$table <- renderDataTable(
    {dataTable()}, options = list(bFilter = FALSE, iDisplayLength = 50))
  
  output$downloadData <- downloadHandler(
    filename = 'data.csv',
    content = function(file) {
      write.csv(dataTable(), file, row.names=FALSE)
    }
  )
})