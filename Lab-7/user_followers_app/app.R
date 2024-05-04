library(dplyr)

data <- read.csv("UserFollowers.csv", stringsAsFactors = FALSE)
# slicing data so that server can handle
data <- data %>% slice(1:10000)
data$CreationDate <- as.Date(data$CreationDate, format = "%m/%d/%Y")

library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("User Following and Followers Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("userid", "Select User ID:", choices = unique(data$UserId))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Following Plot", plotOutput("following_plot")),
        tabPanel("Followers Plot", plotOutput("followers_plot")),
        tabPanel("Following List", uiOutput("following_table")),
        tabPanel("Followers List", uiOutput("followers_table"))
      )
    )
  )
)

server <- function(input, output) {
  output$following_plot <- renderPlot({
    req(input$userid)
    user_data <- data %>%
      filter(UserId == input$userid)
    user_data <- user_data %>%
      arrange(CreationDate) %>%
      mutate(cumulative_following = cumsum(row_number()))
    ggplot(user_data, aes(x = CreationDate, y = cumulative_following)) +
      geom_line() +
      labs(title = "Cumulative Following Count Over Time",
           x = "Date",
           y = "Cumulative Following Count")
  })
  
  output$followers_plot <- renderPlot({
    req(input$userid)
    user_data <- data %>%
      filter(FollowingUserId == input$userid)
    user_data <- user_data %>%
      arrange(CreationDate) %>%
      mutate(cumulative_followers = cumsum(row_number()))
    ggplot(user_data, aes(x = CreationDate, y = cumulative_followers)) +
      geom_line() +
      labs(title = "Cumulative Followers Count Over Time",
           x = "Date",
           y = "Cumulative Followers Count")
  })
  
  output$following_table <- renderUI({
    req(input$userid)
    following <- data %>%
      filter(UserId == input$userid) %>%
      select(FollowingUserId)
    table <- dataTableOutput("following_table_output")
    output$following_table_output <- renderDataTable({
      following
    })
    table
  })
  
  output$followers_table <- renderUI({
    req(input$userid)
    followers <- data %>%
      filter(FollowingUserId == input$userid) %>%
      select(UserId)
    table <- dataTableOutput("followers_table_output")
    output$followers_table_output <- renderDataTable({
      followers
    })
    table
  })
}

shinyApp(ui = ui, server = server)
