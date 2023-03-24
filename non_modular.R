library(shiny)
library(dplyr)
library(ggplot2)
library(DT)

iris_data = iris

iris_summary <- iris_data %>%
  group_by(Species) %>%
  summarise_all(mean)

ui <- fluidPage(
  titlePanel("Iris Dataset"),
  sidebarLayout(
    sidebarPanel(),
    mainPanel(
      DTOutput("table")
    )
  )
)

server <- function(input, output) {
  output$table <- renderDT(datatable(iris_summary))
  
  observeEvent(input$table_rows_selected, {
    selected_species <- iris_data[input$table_rows_selected, ]$Species
    selected_data <- iris_data[iris_data$Species == selected_species, ]
    
    showModal(modalDialog(
      fluidPage(
        fluidRow(
          column(6, DTOutput("selected_table")),
          column(6, plotOutput("plot"))
        )
      )
      
    ))
    output$selected_table <- renderDT(datatable(selected_data))
    output$plot <- renderPlot(
      selected_data %>%
        ggplot(aes(x = Sepal.Length, y = Sepal.Width)) +
        geom_bar(stat = "identity")
    )
  })
  
}

shinyApp(ui, server)

