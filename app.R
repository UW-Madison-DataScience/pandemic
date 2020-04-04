library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(ggrepel)

double_cases <- function(Confirmed0, doubling, actual, hidden, hospitalizing,
                         bedmax) {
  case0 <- Confirmed0 * hidden
  tibble(
    dates = seq(as.Date("2020-03-16"), as.Date("2020-06-01"), by="days")) %>%
    mutate(days = seq_along(dates) - 1,
           Actual = case0 * 2 ^ (days / doubling),
           Confirmed = Actual / hidden,
           Hospitalized = Actual * hospitalizing / 100,
           Max_Beds = bedmax) %>%
    pivot_longer(Actual:Max_Beds,
                 names_to = "Cases",
                 values_to = "Count")
}

real_cases_county <- function() {
  # These data were modified 3/22 to only have one line per region and changed names.
  # Province/State is no longer meaningful for USA.
  dirpath <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series"

  bind_rows(
    Confirmed = read_csv(
      file.path(dirpath, "time_series_covid19_confirmed_global.csv"),
      col_types = cols()),
    Death = read_csv(
      file.path(dirpath, "time_series_covid19_deaths_global.csv"),
      col_types = cols()),
    Recovered = read_csv(
      file.path(dirpath, "time_series_covid19_recovered_global.csv"),
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
                          State),
           Region = ifelse(Region == "US", "USA", Region)) %>%
    group_by(Type, County, State, Region, Date) %>%
    summarize(Count = sum(Count, na.rm = TRUE)) %>%
    ungroup %>%
    weight_date()
}

real_cases_cds <- function() {
  # data downloaded from "https://coronadatascraper.com/timeseries-tidy.csv"
  # takes too long in the shiny app to download the data each time (34 seconds).  Need to 
  #  manually update the file in the application for this one to work
  dirpath <- "data/timeseries-tidy.csv"
  if(!file.exists(dirpath)) {
    dirpath <- "https://coronadatascraper.com/timeseries-tidy.csv"
  }
  read.csv(dirpath) %>%
    filter(city == "") %>% # remove any city level data
    filter(type %in% c("cases", "deaths", "recovered")) %>% # also have active, growthFactor for some
    select(type, county, state, country, date, value, population) %>% 
    mutate_at(vars(type, county, state, country), as.character) %>% 
    mutate(date = as.POSIXct(as.character(date), format="%Y-%m-%d")) %>% 
    mutate(county = paste0(county, ", ", state)) %>% 
    rename(
      Type = type,
      County = county,
      State = state,
      Region = country,
      Date = date,
      Count = value
    ) %>% 
    mutate(Type = case_when(
      Type == "cases" ~ "Confirmed",
      Type == "deaths" ~ "Death",
      Type == "recovered" ~ "Recovered",
      TRUE ~ NA_character_
    )) %>%
    as_tibble() %>%
    filter(State != "") %>%
    mutate(Region = ifelse(Region == "United States", "USA", Region),
           Region = ifelse(Region == "iso1:US", "USA", Region),
           State = str_remove(State, "^iso2:US-"),
           State = ifelse(State %in% state.name, 
                          state.abb[match(State, state.name)], 
                          State)) %>% 
    weight_date()
}

real_cases_county_cds <- function(cases_cds) {
  cases_cds %>% 
    filter(County != "" & Region == "USA") %>%
    filter(!str_detect(County, "^,"),
           !str_detect(County, "unassign")) %>%
    mutate(County = ifelse(State %in% state.abb, 
                           str_replace(County, 
                                       state.name[match(State, state.abb)],
                                       state.abb[match(State, state.abb)]), 
                           County))
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
    summarize(Count = sum(Count, na.rm = TRUE)) %>%
    ungroup
}

real_cases <- function(cases_state) {
  # Sum within Regions
  cases_state %>%
    group_by(Type, Region, Date, Weight) %>%
    summarize(Count = sum(Count, na.rm = TRUE)) %>%
    ungroup
}

# JHU Data. Only has State for a few countries.
cases_county <- real_cases_county()
cases_state <- real_cases_state(cases_county)
cases <- real_cases(cases_state)

# Coronadatascraper Data. Using this for State and County
cases_state <- real_cases_cds()
cases_county <- real_cases_county_cds(cases_state) 
counties <- sort(unique(cases_county$County))
regions <- sort(unique(cases$Region))
# For now, only look at USA. Open up once figure out how to do counties.
cases_state <- real_cases_state(cases_state)
regions_cds <- sort(unique(cases_state$Region))

# Testing in USA
test_us <- read_csv("https://covidtracking.com/api/us/daily.csv",
                    col_types = cols()) %>%
  mutate(date = as.Date(as.character(date), "%Y%m%d")) %>%
  select(date, positive:pending, recovered, death, hospitalized, total) %>%
  pivot_longer(positive:total, names_to = "status", values_to = "count") %>%
  filter(status != "death")
test_st <- read_csv("http://covidtracking.com/api/states/daily.csv",
                    col_types = cols()) %>%
  mutate(date = as.Date(as.character(date), "%Y%m%d")) %>%
  select(date, state, positive:pending, recovered, death, hospitalized, total) %>%
  pivot_longer(positive:total, names_to = "status", values_to = "count") %>%
  filter(status != "death")

##########################################################################33

# Define UI for miles per gallon app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Pandemic Cases"),
  
  # Sidebar panel for inputs ----
  tabsetPanel(
    tabPanel("Real Cases",
      sidebarLayout(
        sidebarPanel(
          h4("Select data to view from the options below."),
          radioButtons("states", "", c("Counties", "States","Countries"), inline = TRUE, selected = "States"),
          conditionalPanel(
            condition = 'input.states == "Countries"',
            selectInput("country", "Countries:", 
                        regions,
                        c("USA", "Iran", "Italy", "Korea, South"),
                        multiple = TRUE)
          ),
          conditionalPanel(
            condition = 'input.states != "Countries"',
            tagList(
              selectInput("country_cds", "Countries:", 
                          regions_cds,
                          c("USA"),
                          multiple = TRUE),
              uiOutput("states_region")
            )
          ),
          conditionalPanel(
            condition = 'input.states == "Counties"',
            uiOutput("counties_states")
          ),
          selectInput("casetypes", "Case Type:", c("Confirmed","Death","Recovered")),
          selectInput("realscale", "Plot Scale:", c("raw","geometric")),
          checkboxInput("predict", "Add predict lines?", FALSE),
          hr(),
          textOutput("latest")
        ),
        mainPanel(
          plotOutput(outputId = "case_plot"),
          tableOutput("fitcase"),
          uiOutput("onep3"))
      )
    ),
    tabPanel("Testing",
      plotOutput("testplot"),
      radioButtons("testgroup", "", c("States","USA"), inline = TRUE),
      conditionalPanel(
        condition = 'input.testgroup == "States"',
        selectInput("teststate", "State:", 
                    state.abb,
                    c("WI","MI","IL","IA"),
                    multiple = TRUE)
      ),
      selectInput("testscale", "Plot Scale:", c("raw","geometric")),
      textOutput("testinfo")
    ),
    tabPanel("Simulations",
             sidebarLayout(
               sidebarPanel(
                 # Input: Slider for initial settings and rates
                 sliderInput("Confirmed0", "Confirmed:", 0, 100, 10),
                 sliderInput("doubling", "Days to Double:", 0, 100, 6, 1),
                 sliderInput("hospitalizing", "Percent Hospitalized:", 0, 100, 10),
                 sliderInput("hidden", "Hidden per Confirmed:", 1, 10, 4),
                 sliderInput("bedmax", "Maximum Hospital Beds:", 0, 5000, 1000),
                 selectInput("scale", "Plot Scale:", c("raw","geometric"))
               ),
               
               mainPanel(
                 plotOutput(outputId = "main_plot"),
                 uiOutput("simpenn"),
                 uiOutput("simharv"),
                 textOutput("space5"),
                 textOutput(outputId = "extra_text")
               )
             )
    ),
    tabPanel("References",
      textOutput("summary"),
      
      # Real Cases
      textOutput("space1"),
      textOutput("summary2"),
      textOutput("jhudata"),
      uiOutput("jhusource"),
      uiOutput("cdsdata"),
      
      # Testing
      textOutput("space2"),
      textOutput("summary3"),
      uiOutput("testsource"),
      textOutput("summary4"),
  
      # Simulations    
      textOutput("space"),
      textOutput("summary1"),
      textOutput("main_text"),
      uiOutput("url"),
      uiOutput("pennmed"),
      uiOutput("harvard"),
      textOutput("agents"),
      
      textOutput("space3"),
      uiOutput("pointacres"),
      
      textOutput("space4"),
      uiOutput("sourceurl"),
      uiOutput("epilist"),
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

  sourceurl <- a("github.com/UW-Madison-DataScience/pandemic", 
           href="https://github.com/UW-Madison-DataScience/pandemic")
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
      geom_line(size = 1.5) +
      ggtitle(paste(input$casetypes, "cases"))
    
    latest_date <- max(cases_reactive()$Date)
    if(length(units_reactive()) > 1) {
      if(input$states == "States") {
        p <- p +
          aes(col = State, z = State) +
          ggrepel::geom_label_repel(aes(label = State), color = "black",
                                    data = . %>% filter(Date == latest_date)) +
          theme(legend.position = "none")
      } else if (input$states == "Countries") {
        p <- p +
          aes(col = Region, z = Region) +
          ggrepel::geom_label_repel(aes(label = Region), color = "black",
                                    data = . %>% filter(Date == latest_date)) +
          theme(legend.position = "none")
      } else {
        p <- p + 
          aes(col = County, z = County) + 
          ggrepel::geom_label_repel(aes(label = County), color = "black",
                                    data = . %>% filter(Date == latest_date)) +
          theme(legend.position = "none")
        
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
      Counties = {
        sort(req(input$county))
      },
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
      Counties = {
        cases_county %>% 
          filter(County %in% units_reactive())
      },
      States = {
        cases_state %>% 
          filter(Region %in% input$country_cds, 
                 State %in% units_reactive())
      },
      Countries = {
        cases %>% 
          filter(Region %in% units_reactive())
      })
  })
  
  # States in a Region
  output$states_region <- renderUI({
    req(input$country_cds)
    states_region <- 
      sort(
        unique((
          cases_state %>% 
            filter(Region %in% input$country_cds))$State))
    
    selectInput("state", "States:", 
                states_region,
                c("WI","MI","IL","IA"),
                multiple = TRUE)
  })
  # Counties in a State
  output$counties_states <- renderUI({
    req(input$state, input$country_cds)
    counties_states <- 
      sort(
        unique((
          cases_county %>% 
            filter(Region %in% input$country_cds,
                   State %in% input$state))$County))
    selectInput("county", "Counties:", 
                counties_states,
                c("Dane County, WI","Milwaukee County, WI", "Waukesha County, WI", "Fond du Lac County, WI"),
                multiple = TRUE)
  })
  
  output$latest <- renderText({paste0("Data is current as of ",
                                      as.character(max(cases_reactive()$Date)))})
  # Fit line for real cases.
  output$fitcase <- renderTable({
    req(input$states, input$casetypes)

    # Get estimate doubling rate
    if(length(units_reactive() > 1)) {
      req(units_reactive())
      if(length(units_reactive()) == 1) {
        form <- formula(Count ~ Date)
      } else {
        switch(
          req(input$states),
          Counties = {
            form <- formula(Count ~ Date * County)
          },
          States = {
            form <- formula(Count ~ Date * State)
          },
          Countries = {
            form <- formula(Count ~ Date * Region)
          })
      }
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
    # Rate cannot be negative, but could be really small.
    doubling <- log(2) / pmax(0, coefs) / 86400
    
    # Get last date.
    switch(input$states,
    States = {
      cases_reactive() %>% 
        group_by(Type, State) %>%
        summarize(Count = max(Count, na.rm = TRUE)) %>%
        ungroup %>%
        arrange(State) %>%
        select(Type, State, Count) %>%
        mutate(Count = as.integer(Count)) %>%
        pivot_wider(names_from = Type, values_from = Count) %>%
        mutate(Doubling = doubling)},
    Countries = {
      cases_reactive() %>% 
        group_by(Type, Region) %>%
        summarize(Count = max(Count, na.rm = TRUE)) %>%
        ungroup %>%
        arrange(Region) %>%
        select(Type, Region, Count) %>%
        mutate(Count = as.integer(Count)) %>%
        pivot_wider(names_from = Type, values_from = Count) %>%
        mutate(Doubling = doubling)},
    Counties = {
      cases_reactive() %>% 
        # filter(Type == input$casetypes)
        group_by(Type, County) %>% 
        summarize(Count = max(Count, na.rm = TRUE)) %>% 
        ungroup %>% 
        arrange(County) %>% 
        mutate(Count = as.integer(Count)) %>% 
        pivot_wider(names_from = Type, values_from = Count) %>% 
        mutate(Doubling = doubling)
    })
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
        USA = {
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

  output$space <- renderText("---")
  output$space1 <- renderText("---")
  output$space2 <- renderText("---")
  output$space3 <- renderText("---")
  output$space4 <- renderText("---")
  output$space5 <- renderText("---")
  output$jhudata <- renderText(
    "Real cases (Country level) come from Johns Hopkins U Center for Systems Science and Engineering."
  )
  sourcejhu <- a("github.com/CSSEGISandData/COVID-19", 
                 href="https://github.com/CSSEGISandData/COVID-19")
  output$jhusource <- renderUI({
    tagList("JHU CSSE Data URL:", sourcejhu)
  })
  sourcecds <- a("https://coronadatascraper.com/", href = "https://coronadatascraper.com/#home")
  output$cdsdata <- renderUI({
    tagList("Real cases (State and County level) come from Corona Data Scraper:", sourcecds)
  })
  output$testinfo <- renderText(
    "Test data compiled by COVID Testing Project."
  )
  
  # COVID Testing Project site
  sourcetest <- a("covidtracking.com/api/", 
                 href="https://covidtracking.com/api/")
  output$testsource <- renderUI({
    tagList("COVID Testing Project URL:", sourcetest)
  })
  
  # 1Point3Acres site
  pointacres <- a("coronavirus.1point3acres.com/en", 
                  href="https://coronavirus.1point3acres.com/en")
  output$pointacres <- renderUI({
    tagList("1Point3Acres Real Time County Updates URL:", pointacres)
  })
  output$onep3 <- renderUI({
    tagList("See", pointacres, "for additional county updates.")
  })
  
  # U Penn Medicine
  pennmed <- a("penn-chime.phl.io/", 
                  href="http://penn-chime.phl.io/")
  output$pennmed <- renderUI({
    tagList("See", pennmed, "for better Susceptible-Infected-Recovered (SIR) CHIME simulations from U Penn Medicine.")
  })
  output$simpenn <- renderUI({
    tagList("See", pennmed, "for better Susceptible-Infected-Recovered (SIR) CHIME simulations.")
  })
  
  # Harvard U
  harvard <- a("alhill.shinyapps.io/COVID19seir/", 
               href="https://alhill.shinyapps.io/COVID19seir/")
  output$harvard <- renderUI({
    tagList("See", harvard, "for even better SEIR Hill simulations from Harvard U Hill group.")
  })
  output$simharv <- renderUI({
    tagList("See", harvard, "for even better SEIR Hill simulations.")
  })
  output$agents <- renderText("Gold standard agent-based models are being developed in many places.")
  
  # Yandell files
  epilist <- a("go.wisc.edu/a1832f", 
              href="https://go.wisc.edu/a1832f")
  output$epilist <- renderUI({
    tagList("Epidemiology Models & Data for Coronavirus:", epilist)
  })
  dslist <- a("datascience.wisc.edu/covid19", 
                 href="https://datascience.wisc.edu/covid19")
  output$dslist <- renderUI({
    tagList("COVID-19 Data Science Links:", dslist)
  })
  
  output$summary <- renderText({
    'Our human challenge is how to grasp concepts of exponential growth, latency (lag time) and carrying capacity (of hospitals),
    when all are involved in this unfolding pandemic. Here is a narrative for this app.'})
  output$summary1 <- renderText({
    '"Simulation" tab is based on "Simple Math, Alarming Answers",
    assuming exponential growth of cases,
    and proportionality between number of confirmed cases,
    number of actual cases and number of hospital beds.
    Does our community have enough hospital beds?'})
  output$summary2 <- renderText({
    '"Real Cases" tab draws on Johns Hopkins U CSSE, which many apps are using.
    Doubling time (days) is estimated via Poisson regression with exponential weights
    (heavier weight on recent dates)
    using ONLY confirmed cases.
    Dashed Poisson regression lines added if you click the “Add predict lines?” box.'})
  output$summary3 <- renderText({
    '"Testing" tab reflects new data from COVID Testing Project.'})
  output$summary4 <- renderText({
    'Be sure to look at the "1Point3Acres" site, which compiles real time county data;
    hope to have that here eventually.'})
}

shinyApp(ui, server)
