library(shiny)

ui <- fluidPage(
  sliderInput(inputId = "num",
              label = "Choose a number",
              value = 25, min = 1, max = 100),
  textInput(inputId = "mytext", label = "Title",
            value = ""),
  plotOutput(outputId = "hist"),
  plotOutput(outputId = "hist2"),
  plotOutput(outputId = "scat")
  )

server <- function(input,output){
  output$hist <- renderPlot({ title <- input$mytext
    hist(rnorm(input$num), main = title) })
  output$hist2 <- renderPlot({ title <- input$mytext
  hist(rnorm(input$num), main = title) })
  output$scat <- renderPlot({ title <- input$mytext
  plot(rnorm(input$num),rnorm(input$num), main = title) })
  
}

shinyApp(ui = ui, server = server)

