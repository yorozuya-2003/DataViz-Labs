df <- read.csv("KernelLanguages.csv", stringsAsFactors = FALSE)

library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Kernel Languages"),
  sidebarLayout(
    sidebarPanel(
      selectInput("language", "Select Language:", choices = c("All", unique(df$DisplayName))),
      dataTableOutput("table")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Kernel Languages (Programming Languages)", plotOutput("barplot")),
        tabPanel("Proportion of Notebooks", plotOutput("piechart")),
        tabPanel("Kernel Languages\n(Programming Languages + Notebook Proportion)", plotOutput("stackedbarplot"))
      )
    )
  )
)

server <- function(input, output) {
  filtered_data <- reactive({
    if (input$language == "All") {
      df
    } else {
      df[df$DisplayName == input$language, ]
    }
  })
  output$barplot <- renderPlot({
    ggplot(filtered_data(), aes(x = DisplayName)) +
      geom_bar() +
      labs(title = "")
  })
  output$piechart <- renderPlot({
    ggplot(filtered_data(), aes(x = factor(IsNotebook), fill = factor(IsNotebook))) +
      geom_bar() +
      labs(title = "")
  })
  output$stackedbarplot <- renderPlot({
    ggplot(filtered_data(), aes(x = DisplayName, fill = factor(IsNotebook))) +
      geom_bar() +
      labs(title = "") +
      theme(axis.text.x = element_text(hjust = 1))
  })
  output$table <- renderDataTable({
    filtered_data()
  })
}

shinyApp(ui = ui, server = server)
