#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("De-Moivre-Laplace Central Limit Theorem"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("size",
                     "Number of trials:",
                     min = 1,
                     max = 100,
                     value = 10),
         sliderInput("prob",
                     "Probability:",
                     min = 0,
                     max = 1,
                     value = 0.1)
      ),
      
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      n <- input$size
      p <- input$prob
      x <- dbinom(seq(0,n,length.out=n+1),n,p)
      
      # draw the histogram with the specified number of bins
      barplot(x, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

