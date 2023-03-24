library(shiny)
library(dplyr)
library(ggplot2)
library(DT)

iris_data = iris

iris_summary <- iris_data %>%
  group_by(Species) %>%
  summarise_all(mean)

summary_ui <- function(id) {
  ns = NS(id)
  DTOutput(ns("summary_table"))
}

summary_server <- function(id, data_raw, data_summary) {
  
  moduleServer(id, function(input, output, session) {
    output$summary_table <- renderDT(datatable(data_summary))
    
    observeEvent(input$summary_table_rows_selected, {
      
      selected_species <- data_raw[input$summary_table_rows_selected, ]$Species
      selected_data <- data_raw[data_raw$Species == selected_species, ]
      
      showModal(modalDialog(
        
        fluidPage(
          fluidRow(
            column(6, DTOutput("selected_table")),
            column(6, plotOutput("selected_plot"))
          )
        )
        
      ))
      
      output$selected_table <- renderDT(datatable(selected_data))
      output$selected_plot <- renderPlot(
        selected_data %>%
          ggplot(aes(x = Sepal.Length, y = Sepal.Width)) +
          geom_bar(stat = "identity")
      )
    })  
  })
}

ui <- fluidPage(
  titlePanel("Iris Dataset"),
  sidebarLayout(
    sidebarPanel(),
    mainPanel(
      summary_ui("summary")
    )
  )
)

server <- function(input, output, session) {
    summary_server("summary", iris_data, iris_summary)
}

shinyApp(ui, server)
