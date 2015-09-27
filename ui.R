
library(shiny)

library(rCharts)

shinyUI(
  navbarPage("U.S. National Storm database (1950 - 2011)",
             tabPanel("Exploratory Analytics",
                      sidebarPanel(
                        sliderInput("range", 
                                    "Range:", 
                                    min = 1950, 
                                    max = 2011, 
                                    value = c(1995, 2011),
                                    format="####"),
                        uiOutput("evtypeControls"),
                        actionButton(inputId = "clear_all", label = "Clear selection", icon = icon("square")),
                        actionButton(inputId = "select_all", label = "Select all", icon = icon("check-square"))
                      ),
                      
                      mainPanel(
                        tabsetPanel(
                          
                          # Data 
                          tabPanel(p(icon("table"), "Data"),
                                   dataTableOutput(outputId="table"),
                                   downloadButton('downloadData', 'Download')
                          ),
                          # by year
                          tabPanel(p(icon("line-chart"), "Visualization"),
                                   h4('Number of events by year', align = "center"),
                                   showOutput("eventsByYear", "nvd3"),
                                   h4('Population impact by year', align = "center"),
                                   showOutput("populationImpact", "nvd3"),
                                   h4('Economic impact by year', align = "center"),
                                   showOutput("economicImpact", "nvd3")
                          )
                        )
                      )
                      
             ),
             
             tabPanel("Instructions",
                      mainPanel(
                        includeMarkdown("about.md")
                      )
             )
  )
)