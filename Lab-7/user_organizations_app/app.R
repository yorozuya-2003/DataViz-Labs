data <- read.csv("UserOrganizations.csv", stringsAsFactors = FALSE)
data$JoinDate <- as.Date(data$JoinDate, format = "%m/%d/%Y")

library(shiny)
library(dplyr)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Cumulative Joinings for Organizations by Dates"),
  sidebarLayout(
    sidebarPanel(
      selectInput("org_id", "Select Organization ID", choices = unique(data$OrganizationId)),
      br(),
      tableOutput("user_table")
    ),
    mainPanel(
      plotOutput("join_plot")
    )
  )
)

server <- function(input, output) {
  filtered_data <- reactive({
    data %>% filter(OrganizationId == input$org_id) %>% arrange(JoinDate)
  })
  cumulative_joins <- reactive({
    df <- filtered_data()
    df$CumulativeJoins <- 1:nrow(df)
    df
  })
  output$user_table <- renderTable({
    req(input$org_id)
    filtered_users <- data %>%
      filter(OrganizationId == input$org_id) %>%
      select(UserId)
    filtered_users
  })
  output$join_plot <- renderPlot({
    ggplot(cumulative_joins(), aes(x = JoinDate, y = CumulativeJoins)) +
      geom_line() +
      labs(title = paste("Cumulative Joinings for Organization ID", input$org_id),
           x = "Date",
           y = "Cumulative Joinings")
  })
}

shinyApp(ui = ui, server = server)
