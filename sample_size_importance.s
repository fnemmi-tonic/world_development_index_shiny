library(shiny)
library(ggplot2)
library(tidyverse)

population_size <- 2000
population <- rnorm(2000, mean = 100, sd = 30)

ui <- fluidPage(
  numericInput(inputId = "draw", 
               label = "size of your sample",
               value = 10, min = 2, max = population_size),
  radioButtons(inputId = "plot_type", "choose your plot", choices = list(hist = "hist", boxplot = "boxplot", 
                                                                         density = "density")),
  plotOutput(outputId = "plot"),
  verbatimTextOutput(outputId = "stat"))

server <- function(input,output){
  data <- reactive({sample(population, input$draw, replace = FALSE)})
  output$plot <- renderPlot({df <- data_frame(value = c(population, data()),
                                              population = c(rep("whole", population_size), rep("sample", input$draw)))
  base_layer <- ggplot(data = df, aes(x = value, group = population))
  if (input$plot_type == "hist") {
    base_layer + geom_histogram(aes(y =..count../sum(..count..), fill = population), alpha = .5, position = "identity") } else if (input$plot_type == "boxplot") {
      ggplot(data = df, aes(y = value, x = population)) + geom_boxplot() } else {base_layer + geom_density(aes(fill = population),
                                                                                                       alpha = .5)}
  })
  output$stat <- renderPrint({rbind(summary(population), summary(data()))})
  
  }

shinyApp(ui = ui, server = server)
