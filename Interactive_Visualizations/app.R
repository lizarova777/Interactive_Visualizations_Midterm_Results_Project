# Interactive Visualization

library(shiny)
library(ggplot2)

ui <- fluidPage(
  sliderInput(inputId = "num", label = "Choose a number", 
              value = 1, min = 1, max = 100), plotOutput("gghist"))

server <- function(input, output) {
  output$gghist <- renderPlot({
    DG <- data.frame(rnorm(input$num))
    ggplot(DG, aes(rnorm(input$num))) + geom_histogram()
  })
}

shinyApp(ui = ui, server = server)