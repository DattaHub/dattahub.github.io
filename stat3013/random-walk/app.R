#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(dplyr)
library(shiny)
library(sde)

ui <- fluidPage(
    tags$style("* { font-family: Arial; }"),
    titlePanel("Random Walk"),
    sidebarLayout(
        sidebarPanel(
            p("This plot shows a simulated random walk of a special kind"),
            p("This is called a Geometric Brownian Motion"),
            p("S(t) = x * exp((r-sigma^2/2)*t + sigma*B(t))"),
            p("starting at position x at time t0"),
            numericInput("seed","Random Seed",123),
            sliderInput("paths",
                        "Paths",
                        min = 1,
                        max = 100,
                        value = 1),
            sliderInput("start",
                        "Starting Value (x)",
                        0,10,0.1,0.1),
            sliderInput("r",
                        "Expected Return (r)",-0.1, 0.1, 0, 0.001),
            sliderInput("sigma",
                        "Sigma: volatility of the Brownian Motion",0.001, 1, 0.01, 0.001),
            sliderInput("periods",
                        "Periods",10, 1000, 200, 10),
            p("Contact for source code for this app: ", a("Jyotishka Datta", target="_blank", href="mailto:jd033@uark.edu"))
        ),
        mainPanel(plotOutput("plot"))
    )
)


server <- function(input, output) {
    output$plot <- renderPlot({
        set.seed(input$seed)
        mat <- sapply(seq_len(input$paths), function(i){
            sde::GBM(input$start, input$r, input$sigma, 1, input$periods)
        })
        matplot(mat, type = "l", lty = 1, main = "Random Walks") })
}

# Run the application 
shinyApp(ui = ui, server = server)
