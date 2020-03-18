library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)

double_cases <- function(Confirmed0, doubling, actual, hidden, hospitalizing,
                         bedmax) {
  case0 <- Confirmed0 * hidden
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

real_cases_county <- function() {
  dirpath <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series"

  bind_rows(
    Confirmed = read_csv(
      file.path(dirpath, "time_series_19-covid-Confirmed.csv"),
      col_types = cols()),
    Death = read_csv(
      file.path(dirpath, "time_series_19-covid-Deaths.csv"),
      col_types = cols()),
    Recovered = read_csv(
      file.path(dirpath, "time_series_19-covid-Recovered.csv"),
      col_types = cols()),
    .id = "Type") %>%
    rename(State = "Province/State",
           Region = "Country/Region") %>%
    pivot_longer(-(Type:Long), names_to = "Date", values_to = "Count") %>%
    mutate(Date = as.POSIXct(Date, format="%m/%d/%y")) %>%
    mutate(County = "All",
           County = ifelse(str_detect(State, ","), str_remove(State, ",.*$"), County),
           State = ifelse(str_detect(State, ","), str_remove(State, "^.*, "), State),
           State = ifelse(State %in% state.name, 
                          state.abb[match(State, state.name)], 
                          State)) %>%
    group_by(Type, County, State, Region, Date) %>%
    summarize(Count = sum(Count)) %>%
    ungroup %>%
    weight_date()
}

weight_date <- function(cases_state) {
  # Add weights skewed toward most recent dates.
  skew <- 20
  sweight <- as.numeric(unique(cases_state$Date))
  mweight <- min(sweight)
  eweight <- as.numeric(sweight - mweight)
  sweight <- max(eweight)
  eweight <- max(exp(skew * eweight / sweight))
  
  # Add weights to states
  cases_state %>%
    mutate(Weight = as.numeric(Date - mweight) / sweight) %>%
    mutate(Weight = exp(skew * Weight) / eweight)
}

real_cases_state <- function(cases_state) {
  # Sum within Regions
  cases_state %>%
    group_by(Type, Region, State, Date, Weight) %>%
    summarize(Count = sum(Count)) %>%
    ungroup
}

real_cases <- function(cases_state) {
  # Sum within Regions
  cases_state %>%
    group_by(Type, Region, Date, Weight) %>%
    summarize(Count = sum(Count)) %>%
    ungroup
}

cases_county <- real_cases_county()
cases_state <- real_cases_state(cases_county)
cases_county <- cases_county %>%
  filter(State == "WI")
counties <- sort(unique(cases_county$County))
cases <- real_cases(cases_state)
regions <- sort(unique(cases$Region))
cases_state <- cases_state %>%
  filter(Region == "US")

# Testing in US
test_us <- read_csv("https://covidtracking.com/api/us/daily.csv",
                    col_types = cols()) %>%
  mutate(date = as.Date(as.character(date), "%Y%m%d")) %>%
  pivot_longer(positive:total, names_to = "status", values_to = "count")
test_st <- read_csv("http://covidtracking.com/api/states/daily.csv",
                    col_types = cols()) %>%
  mutate(date = as.Date(as.character(date), "%Y%m%d")) %>%
  pivot_longer(positive:total, names_to = "status", values_to = "count")

##########################################################################33

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
        sliderInput("Confirmed0", "Confirmed:", 0, 100, 10),
        sliderInput("doubling", "Days to Double:", 1, 10, 6, 1),
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
          radioButtons("states", "", c("States","Countries"), inline = TRUE),
          selectInput("casetypes", "Cases:", c("Confirmed","Death","Recovered")),
          selectInput("realscale", "Plot Scale:", c("raw","geometric")),
          checkboxInput("predict", "Add predict lines?", FALSE)
        ),
        mainPanel(
          plotOutput(outputId = "case_plot"),
          textOutput("latest"),
          tableOutput("fitcase"))
      )
    ),
    tabPanel("Testing",
      plotOutput("testplot"),
      radioButtons("testgroup", "", c("States","US"), inline = TRUE),
      conditionalPanel(
        condition = 'input.testgroup == "States"',
        selectInput("teststate", "States:", 
                    state.abb,
                    c("WI","MI","IL","IA"),
                    multiple = TRUE)
      ),
      selectInput("testscale", "Plot Scale:", c("raw","geometric")),
      textOutput("testinfo")
    ),
    tabPanel("References",
      textOutput("main_text"),
      uiOutput("url"),
      textOutput("space"),
      textOutput("jhudata"),
      uiOutput("jhusource"),
      uiOutput("testsource"),
      textOutput("space2"),
      uiOutput("sourceurl"),
      uiOutput("dslist")
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
    
    dat <- double_cases(input$Confirmed0,
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
    suppressWarnings(p)
  })
  
  output$extra_text <- renderText(
  "Consider a community with ~500K people and ~2K beds. Suppose on 16 March 2020, there were 10 confirmed cases, which might represent 40 actual cases. 
  Consider a doubling every 6 days. Say 10% need hospitalization, which is only 4 at that time. By 1 April, that could jump to 60 confirmed cases, 250 actual cases, and 25 needing hospital beds. Doesn’t seem so bad, but the next jump is huge.
  By 1 May, there would be over 2000 confirmed cases of 8000+ actual cases, with 800+ needing hospitalization.
  Still manageable. However, by 1 June, there would be 70,000+ confirmed cases, ~300K actual cases (over half of the community),
  and need for almost 30K beds. Put another way, by the first week of May, community would need 1-2K beds.
  Slowing the days to double by social distancing buys time. Adjust sliders to view changes."
  )

  # Plot real cases.
  output$case_plot <- renderPlot({
    req(input$states, input$casetypes)
    p <- ggplot(cases_reactive() %>% 
                  filter(Count > 0, Type == input$casetypes))
    p <- p +
      aes(Date, Count) +
      geom_line(size = 2) +
      ggtitle(paste(input$casetypes, "cases"))
    
    
    if(length(units_reactive()) > 1) {
      if(input$states == "States") {
        p <- p +
          aes(col = State, z = State)
        
      } else {
        p <- p +
          aes(col = Region, z = Region)
      }
    } else {
      p <- p +
        theme(legend.position = "none")
    }

    if(isTruthy(input$predict)) {
      if(input$states == "States") {
        p <- p + 
          geom_smooth(method="glm", mapping = aes(weight = Weight), se = FALSE,
                      formula = y ~ x,
                      method.args = list(family = "poisson"),
                      linetype = "dashed")
      } else {
        p <- p + 
          geom_smooth(method="glm", mapping = aes(weight = Weight), se = FALSE,
                      formula = y ~ x,
                      method.args = list(family = "poisson"),
                      linetype = "dashed")
      }
    }
    if(input$realscale == "geometric") {
      p <- p +
        scale_y_log10()
    }
    suppressWarnings(p)
  })
  
  units_reactive <- reactive({
    switch(
      req(input$states),
      States = {
        sort(req(input$state))
      },
      Countries = {
        sort(req(input$country))
      })
  })
  cases_reactive <- reactive({
    switch(
      req(input$states),
      States = {
        cases_state %>% 
          filter(Region == "US", 
                 State %in% units_reactive())
      },
      Countries = {
        cases %>% 
          filter(Region %in% units_reactive())
      })
  })
  
  output$latest <- renderText({as.character(max(cases_reactive()$Date))})
  # Fit line for real cases.
  output$fitcase <- renderTable({
    req(input$states, input$casetypes)
    
    # Get estimate doubling rate
    if(length(units_reactive() > 1)) {
      switch(
        req(input$states),
        States = {
          form <- formula(Count ~ Date * State)
        },
        Countries = {
          form <- formula(Count ~ Date * Region)
        })
      fit <- glm(form, cases_reactive(),
          weight = Weight, family = "poisson")
      coefs <- coef(fit)
      coefs <- coefs[str_detect(names(coefs), "Date")]
      coefs[-1] <- coefs[-1] + coefs[1]
      names(coefs) <- units_reactive()
    } else {
      form <- formula(Count ~ Date)
      fit <- glm(form, cases_reactive(),
                 weight = Weight, family = "poisson")
      coefs <- coef(fit)
      coefs <- coefs[str_detect(names(coefs), "Date")]
      names(coefs) <- units_reactive()
    }
    doubling <- log(2) / coefs / 86400
    
    # Get last date.
    if(input$states == "States") {
      cases_reactive() %>% 
        filter(Date == max(Date)) %>%
        arrange(State) %>%
        select(Type, Region, State, Count) %>%
        mutate(Count = as.integer(Count)) %>%
        pivot_wider(names_from = Type, values_from = Count) %>%
        mutate(Doubling = doubling)
    } else {
      cases_reactive() %>% 
        filter(Date == max(Date)) %>%
        arrange(Region) %>%
        select(Type, Region, Count) %>%
        mutate(Count = as.integer(Count)) %>%
        pivot_wider(names_from = Type, values_from = Count) %>%
        mutate(Doubling = doubling)
    }
  })
  
  output$testplot <- renderPlot({
    req(input$teststate)
    switch(
        req(input$testgroup),
        States = {
          p <- ggplot(test_st %>% 
                        filter(count > 0 & !is.na(count),
                               state %in% input$teststate)) +
            aes(date, count, col = state) +
            facet_wrap(~ status, scales = "free_y")
        },
        US = {
          p <- ggplot(test_us) +
            aes(date, count, col = status)
        })
    p <- p +
      geom_line(size = 2)
    if(input$testscale == "geometric") {
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
    tagList("JHU CSSE Data URL:", sourcejhu)
  })
  output$testinfo <- renderText(
    "Test data compiled by COVID Testing Project."
  )
  sourcetest <- a("https://covidtracking.com/api/", 
                 href="https://covidtracking.com/api/")
  output$testsource <- renderUI({
    tagList("COVID Testing Project URL:", sourcetest)
  })
  
  dslist <- a("https://datascience.wisc.edu/covid19", 
                 href="https://datascience.wisc.edu/covid19")
  output$dslist <- renderUI({
    tagList("Data Science Links:", dslist)
  })
}

shinyApp(ui, server)
