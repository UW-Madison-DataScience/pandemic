library(shiny)
source("pandemic.R")

# Define UI for miles per gallon app ----
ui <- pageWithSidebar(
  
  # App title ----
  headerPanel("Cases and Beds"),
  
  # Sidebar panel for inputs ----
  sidebarPanel(
    
    # Input: Slider for initial settings and rates
    sliderInput("confirmed0", "Confirmed:", 0, 100, 10),
    sliderInput("doubling", "Days to Double:", 4, 10, 6),
    sliderInput("hospitalizing", "Percent Hospitalized:", 0, 100, 10),
    sliderInput("hidden", "Hidden per Confirmed:", 1, 10, 4),
    sliderInput("bedmax", "Maximum Hospital Beds:", 0, 5000, 1000),
    selectInput("scale", "Plot Scale:", c("raw","geometric"))
    
  ),
  
  # Main panel for displaying outputs ----
  mainPanel(
    
    # Output
    plotOutput(outputId = "main_plot", height = "300px"),
    
    textOutput(outputId = "main_text"),
    uiOutput("url")
    
  )
)

# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
  
  output$main_text <- renderText("Based on article by Liz Specht STAT (10 March 2020)")

  url <- a("Simple Math, Alarming Answers", 
           href="https://www.statnews.com/2020/03/10/simple-math-alarming-answers-covid-19/")
  output$url <- renderUI({
    tagList("URL link:", url)
  })

  output$main_plot <- renderPlot({
    
    dat <- double_cases(input$doubling, 
                        input$actual,
                        input$hidden,
                        input$hospitalizing)
    
    p <- ggplot(dat) +
      aes(dates, Count / 1000, group = Cases, col = Cases) +
      geom_hline(yintercept = input$bedmax / 1000, col = "black", size = 2) +
      geom_line(size = 2) +
      ylim(0, 5 * input$bedmax / 1000) +
      ggtitle("Thousands of Cases")
    
    if(input$scale == "geometric") {
      p <- p +
        scale_y_log10()
    } else {
      p <- p +
        ylim(0, 5 * input$bedmax / 1000)
    }
    p
  })
}

shinyApp(ui, server)