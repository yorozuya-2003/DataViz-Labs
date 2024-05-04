library(shiny)
library(ggplot2)
library(plotly)

df <- read.csv("thyroid.csv")
class_choices <- unique(df$class)
feature_labels <- c("T3-Resin" = "t3_resin",
                    "Total Thyroxin" = "serum_thyroxin",
                    "Total Triiodothyronine" = "serum_triiodothyronine",
                    "Basal TSH" = "basal_tsh",
                    "Max TSH Difference" = "max_tsh_difference")

ui <- fluidPage(
  titlePanel("Thyroid Data Visualization"),
  sidebarLayout(
    sidebarPanel(
      style = "position:fixed;width:inherent",
      selectInput("plot_type", "Select Plot Type", choices = c("Histogram", "Scatterplot", "Boxplot", "Violin Plot")),
      conditionalPanel(
        condition = "input.plot_type == 'Histogram'",
        hr(),
        fluidRow(
          column(6, sliderInput("binSize_t3_resin", "Bin Size (T3-Resin)", min = 1, max = 25, value = 1)),
          column(6, colourpicker::colourInput("color_t3_resin", "Color (T3-Resin)", value = "#20B2AA"))
        ),
        fluidRow(
          column(6, sliderInput("binSize_serum_thyroxin", "Bin Size (Total Thyroxin)", min = 1, max = 25, value = 1)),
          column(6, colourpicker::colourInput("color_serum_thyroxin", "Color (Total Thyroxin)", value = "#20B2AA"))
        ),
        fluidRow(
          column(6, sliderInput("binSize_serum_triiodothyronine", "Bin Size (Total Triiodothyronine)", min = 1, max = 25, value = 1)),
          column(6, colourpicker::colourInput("color_serum_triiodothyronine", "Color (Total Triiodothyronine)", value = "#20B2AA"))
        ),
        fluidRow(
          column(6, sliderInput("binSize_basal_tsh", "Bin Size (Basal TSH)", min = 1, max = 25, value = 1)),
          column(6, colourpicker::colourInput("color_basal_tsh", "Color (Basal TSH)", value = "#20B2AA"))
        ),
        fluidRow(
          column(6, sliderInput("binSize_max_tsh_difference", "Bin Size (Max TSH Difference)", min = 1, max = 25, value = 1)),
          column(6, colourpicker::colourInput("color_max_tsh_difference", "Color (Max TSH Difference)", value = "#20B2AA"))
        )
      ),
      conditionalPanel(
        condition = "input.plot_type == 'Scatterplot'",
        hr(),
        fluidRow(
          column(6, selectInput("x_axis", "X-Axis", choices = feature_labels)),
          column(6, selectInput("y_axis", "Y-Axis", choices = rev(feature_labels))),
        ),
        uiOutput("class_inputs")
      ),
      conditionalPanel(
        condition = "input.plot_type == 'Boxplot' || input.plot_type == 'Violin Plot'",
        hr(),
        selectInput("boxplot_feature", "Select Feature", choices = feature_labels)
      )
    ),
    mainPanel(
      conditionalPanel(
        condition = "input.plot_type == 'Histogram'",
        plotlyOutput("plot_t3_resin"),
        plotlyOutput("plot_serum_thyroxin"),
        plotlyOutput("plot_serum_triiodothyronine"),
        plotlyOutput("plot_basal_tsh"),
        plotlyOutput("plot_max_tsh_difference")
      ),
      conditionalPanel(
        condition = "input.plot_type == 'Scatterplot'",
        plotlyOutput("scatter_plot")
      ),
      conditionalPanel(
        condition = "input.plot_type == 'Boxplot'",
        plotlyOutput("box_plot")
      ),
      conditionalPanel(
        condition = "input.plot_type == 'Violin Plot'",
        plotlyOutput("violin_plot")
      )
    )
  )
)

server <- function(input, output, session) {
  initial_colors <- c("#fde725", "#21918c", "#440154")
  
  observe({
    output$class_inputs <- renderUI({
      class_inputs <- lapply(seq_along(class_choices), function(i) {
        fluidRow(
          column(6, colourpicker::colourInput(inputId = paste0("color_class_", i), label = paste("Color ( Class", class_choices[i], ")", sep = " "), value = initial_colors[i])),
          column(6, sliderInput(inputId = paste0("point_size_", i), label = paste("Point Size ( Class", class_choices[i], ")", sep = " "), min = 1, max = 10, value = 5)),
          column(6, selectInput(inputId = paste0("point_shape_", i), label = paste("Point Shape ( Class", class_choices[i], ")", sep = " "), choices = c("circle", "square", "triangle"))),
          hr(),
        )
      })
      do.call(tagList, class_inputs)
    })
  })
  
  generateHistogram <- function(df, x_var, bin_size, color, x_label, title) {
    p <- ggplot(df, aes_string(x = x_var)) +
      geom_histogram(binwidth = bin_size, fill = color, color = "black") +
      labs(title = title, x = x_label, y = "Frequency") +
      theme_minimal()
    return(p)
  }
  
  generateScatterplot <- function(df, x_var, y_var) {
    x_label <- names(feature_labels)[match(x_var, feature_labels)]
    y_label <- names(feature_labels)[match(y_var, feature_labels)]
    
    p <- ggplot(df, aes_string(x = x_var, y = y_var)) +
      geom_point(aes(color = factor(class), size = factor(class), shape = factor(class))) +
      scale_color_manual(values = sapply(seq_along(class_choices), function(i) {
        if (!is.null(input[[paste0("color_class_", i)]])) {
          return(input[[paste0("color_class_", i)]])
        } else {
          return(initial_colors[i])
        }
      })) +
      scale_size_manual(values = sapply(seq_along(class_choices), function(i) as.numeric(input[[paste0("point_size_", i)]]))) +
      scale_shape_manual(values = sapply(seq_along(class_choices), function(i) {
        if (!is.null(input[[paste0("point_shape_", i)]])) {
          shape <- input[[paste0("point_shape_", i)]]
          if (shape == "triangle") {
            return(17)
          } else {
            return(shape)
          }
        } else {
          return(16)
        }
      })) +
      labs(title = "Scatterplot (Click on any class in the legend to filter it out)", x = x_label, y = y_label) +
      theme_minimal()
    
    return(p)
  }
  
  generateBoxplot <- function(df, feature) {
    feature_label <- names(feature_labels)[match(feature, feature_labels)]
    
    p <- ggplot(df, aes_string(x = "factor(class)", y = feature, fill = "factor(class)")) +
      geom_boxplot() +
      labs(title = paste("Boxplot of", feature_label, "by Class (Click on any class in the legend to filter it out)"), x = "Class", y = feature_label) +
      theme_minimal()
    
    return(p)
  }
  
  generateViolinplot <- function(df, feature) {
    feature_label <- names(feature_labels)[match(feature, feature_labels)]
    
    p <- ggplot(df, aes_string(x = "factor(class)", y = feature, fill = "factor(class)")) +
      geom_violin() +
      labs(title = paste("Violin Plot of", feature_label, "by Class (Click on any class in the legend to filter it out)"), x = "Class", y = feature_label) +
      theme_minimal()
    
    return(p)
  }
  
  output$scatter_plot <- renderPlotly({
    p <- generateScatterplot(df, input$x_axis, input$y_axis)
    ggplotly(p)
  })
  
  output$plot_t3_resin <- renderPlotly({
    p <- generateHistogram(df, "t3_resin", input$binSize_t3_resin, input$color_t3_resin, "T3-Resin", "Histogram of T3-Resin")
    ggplotly(p)
  })
  
  output$plot_serum_thyroxin <- renderPlotly({
    p <- generateHistogram(df, "serum_thyroxin", input$binSize_serum_thyroxin, input$color_serum_thyroxin, "Total Thyroxin", "Histogram of Total Thyroxin")
    ggplotly(p)
  })
  
  output$plot_serum_triiodothyronine <- renderPlotly({
    p <- generateHistogram(df, "serum_triiodothyronine", input$binSize_serum_triiodothyronine, input$color_serum_triiodothyronine, "Total Triiodothyronine", "Histogram of Total Triiodothyronine")
    ggplotly(p)
  })
  
  output$plot_basal_tsh <- renderPlotly({
    p <- generateHistogram(df, "basal_tsh", input$binSize_basal_tsh, input$color_basal_tsh, "Basal TSH", "Histogram of Basal TSH")
    ggplotly(p)
  })
  
  output$plot_max_tsh_difference <- renderPlotly({
    p <- generateHistogram(df, "max_tsh_difference", input$binSize_max_tsh_difference, input$color_max_tsh_difference, "Max TSH Difference", "Histogram of Max TSH Difference")
    ggplotly(p)
  })
  
  output$box_plot <- renderPlotly({
    p <- generateBoxplot(df, input$boxplot_feature)
    ggplotly(p)
  })
  
  output$violin_plot <- renderPlotly({
    p <- generateViolinplot(df, input$boxplot_feature)
    ggplotly(p)
  })
}

shinyApp(ui = ui, server = server)
