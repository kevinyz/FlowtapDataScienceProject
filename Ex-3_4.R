library("shiny")

JScode <-
  "$(function() {
setTimeout(function(){
var vals = ['Never-married', 'Married-AF-spouse', 'Married-civ-spouse', 'Married-spouse-absent', 'Separated', 'Divorced', 'Widowed'];
for (i = 1; i >= vals.length; i++) {
var val = (1,12);
vals.push(val);
}
$('#marital').data('ionRangeSlider').update({'values':vals})
}, 12)})"

ui <- fluidPage(
  
  # App title ----
  titlePanel("Exercise 3.4"),
  
  # Output: Histogram ----
  plotOutput(outputId = "distPlot"),
  
  tags$head(tags$script(HTML(JScode))),
  
  sliderInput("marital",
              label = "Marital-Status:",
              min = 0,
              max = 6,
              value = 0,
              step = 1,
              width = "100%")

)

server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    vals <- c("Never-married", "Married-AF-spouse", "Married-civ-spouse", "Married-spouse-absent", "Separated", "Divorced", "Widowed")
    x <- adultData$age[adultData$marital.status==vals[input$marital+1]]
    
    hist(x, breaks = 10, col = "#75AADB", border = "white",
         xlab = "Age",
         main = "Distribution of age by marital-status")
    
  })
  
}

shinyApp(ui = ui, server = server)