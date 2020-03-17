library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)

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

real_cases_state <- function() {
  dirpath <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series"

  bind_rows(
    confirmed = read_csv(
      file.path(dirpath, "time_series_19-covid-Confirmed.csv")),
    death = read_csv(
      file.path(dirpath, "time_series_19-covid-Deaths.csv")),
    recovered = read_csv(
      file.path(dirpath, "time_series_19-covid-Recovered.csv")),
    .id = "Type") %>%
    rename(State = "Province/State",
           Region = "Country/Region") %>%
    pivot_longer(-(Type:Long), names_to = "Date", values_to = "Count") %>%
    mutate(Date = as.POSIXct(Date, format="%m/%d/%y")) %>%
    mutate(State = ifelse(str_detect(State, ","), str_remove(State, "^.*, "), State),
           State = ifelse(State %in% state.name, 
                          state.abb[match(State, state.name)], 
                          State)) %>%
    group_by(Type, State, Region, Date) %>%
    summarize(Count = sum(Count)) %>%
    ungroup
}
real_cases <- function(cases_state) {
cases_state %>%
    group_by(Type, Region, Date) %>%
    summarize(Count = sum(Count)) %>%
    ungroup
}

cases_state <- real_cases_state()
cases <- real_cases(cases_state)
regions <- sort(unique(cases$Region))

# Define UI for miles per gallon app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Pandemic Cases"),
  
  # Sidebar panel for inputs ----
  tabsetPanel(
    tabPanel("Simulation",
      sidebarLayout(
        sidebarPanel(
        # Input: Slider for initial settings and rates
        sliderInput("confirmed0", "Confirmed:", 0, 100, 10),
        sliderInput("doubling", "Days to Double:", 4, 10, 6),
        sliderInput("hospitalizing", "Percent Hospitalized:", 0, 100, 10),
        sliderInput("hidden", "Hidden per Confirmed:", 1, 10, 4),
        sliderInput("bedmax", "Maximum Hospital Beds:", 0, 5000, 1000),
        selectInput("scale", "Plot Scale:", c("raw","geometric"))
        ),

        mainPanel(
          plotOutput(outputId = "main_plot"),
          textOutput(outputId = "extra_text")
        )
      )
    ),
    tabPanel("Real Cases",
      sidebarLayout(
        sidebarPanel(
          conditionalPanel(
            condition = 'input.states == "Countries"',
            selectInput("country", "Countries:", 
                        regions,
                        c("US", "China", "Iran", "Italy", "Korea, South"),
                        multiple = TRUE)
          ),
          conditionalPanel(
            condition = 'input.states == "States"',
            selectInput("state", "States:", 
                        state.abb,
                        c("WI","MI","IL","IA"),
                        multiple = TRUE)
          ),
          radioButtons("states", "", c("States","Countries")),
          selectInput("casetypes", "Cases:", c("confirmed","death","recovered")),
          selectInput("realscale", "Plot Scale:", c("raw","geometric"))
        ),
        mainPanel(
          plotOutput(outputId = "case_plot"))
      )
    ),
    tabPanel("Reference",
      textOutput("main_text"),
      uiOutput("url"),
      textOutput("space"),
      textOutput("jhudata"),
      uiOutput("jhusource"),
      textOutput("space2"),
      uiOutput("sourceurl")
    )
  )
)

# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
  
  output$main_text <- renderText("Simulation based on article by Liz Specht STAT (10 March 2020)")

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
  "Consider a community with ~500K people and ~2K beds. On 16 March 2020, there were 10 confirmed cases, which might represent 40 actual cases. 
  Consider a doubling every 6 days. Say 10% need hospitalization, which is only 4 at that time. By 1 April, that could jump to 60 confirmed cases, 250 actual cases, and 25 needing hospital beds. Doesn’t seem so bad, but the next jump is huge.
  By 1 May, there would be over 2000 confirmed cases of 8000+ actual cases, with 800+ needing hospitalization.
  Still manageable. However, by 1 June, there would be 70,000+ confirmed cases, ~300K actual cases (over half of the community),
  and need for almost 30K beds. Put another way, by the first week of May, community would need 1-2K beds.
  Slowing the days to double by social distancing buys time. Adjust sliders to view changes."
  )

  output$case_plot <- renderPlot({
    req(input$states, input$casetypes)
    if(input$states == "States") {
      req(input$state)
      unitnames <- input$state
      p <- ggplot(cases_state %>% 
                    filter(Type == input$casetypes, Count > 0, 
                           Region == "US", State %in% input$state))
    } else {
      req(input$country)
      unitnames <- input$country
      p <- ggplot(cases %>% 
                    filter(Type == input$casetypes, Count > 0,
                           Region %in% input$country))
    }
    p <- p +
      aes(Date, Count) +
      geom_line(size = 2) +
      ggtitle(paste(input$casetypes, "cases"))
    
    if(length(unitnames) > 1) {
      if(input$states == "States") {
        p <- p +
          aes(col = State)
        
      } else {
        p <- p +
          aes(col = Region)
      }
    } else {
      p <- p +
        theme(legend.position = "none")
    }
    if(input$realscale == "geometric") {
      p <- p +
        scale_y_log10()
    }
    p
  })

  output$space <- renderText(" ")
  output$space2 <- renderText(" ")
  output$jhudata <- renderText(
    "Real cases come from Johns Hopkins U Center for Systems Science and Engineering."
  )
  sourcejhu <- a("https://github.com/CSSEGISandData/COVID-19", 
                 href="https://github.com/CSSEGISandData/COVID-19")
  output$jhusource <- renderUI({
    tagList("Data URL:", sourcejhu)
  })
}

shinyApp(ui, server)
