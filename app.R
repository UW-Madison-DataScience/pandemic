library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)

double_cases <- function(confirmed0, doubling, actual, hidden, hospitalizing,
                         bedmax) {
  case0 <- confirmed0 * hidden
  tibble(
    dates = seq(as.Date("2020-03-16"), as.Date("2020-06-01"), by="days")) %>%
    mutate(days = seq_along(dates) - 1,
           Actual = case0 * 2 ^ (days / doubling),
           Confirmed = Actual / hidden,
           Hospitalized = Actual / hospitalizing,
           Max_Beds = bedmax) %>%
    pivot_longer(Actual:Max_Beds,
                 names_to = "Cases",
                 values_to = "Count")
}

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
    uiOutput("url"),
    textOutput(outputId = "extra_text"),
    uiOutput("sourceurl")
    
  )
)

# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
  
  output$main_text <- renderText("Based on article by Liz Specht STAT (10 March 2020)")

  url <- a("Simple Math, Alarming Answers", 
           href="https://www.statnews.com/2020/03/10/simple-math-alarming-answers-covid-19/")
  output$url <- renderUI({
    tagList("Article URL:", url)
  })

  sourceurl <- a("https://github.com/byandell/pandemic", 
           href="https://github.com/byandell/pandemic")
  output$sourceurl <- renderUI({
    tagList("Source URL:", sourceurl)
  })
  
  output$main_plot <- renderPlot({
    
    dat <- double_cases(input$confirmed0,
                        input$doubling, 
                        input$actual,
                        input$hidden,
                        input$hospitalizing,
                        input$bedmax)

    p <- ggplot(dat) +
      aes(dates, Count / 1000, group = Cases, col = Cases) +
      geom_line(size = 2) +
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
  
  output$extra_text <- renderText(
  "Consider a community with ~500K people and ~2K beds. On 16 March 2020, there are 10 cases, which might represent 40 actual cases. 
  Consider a doubling every 6 days. Say 10% need hospitalization, which is only 4. By 1 April, that could jump to 60 confirmed cases, 250 actual cases, and 25 needing hospital beds. Doesn’t seem so bad, but the next jump is huge.
  By 1 May, there would be over 2000 confirmed cases of 8000+ actual cases, with 800+ needing hospitalization.
  Still manageable. However, by 1 June, there would be 70,000+ confirmed cases, ~300K actual cases (over half of the community),
  and need for almost 30K beds. Put another way, by the first week of May, community would need 1-2K beds.
  Slowing the days to double by social distancing buys time. Adjust sliders to view."
  )

}

shinyApp(ui, server)
