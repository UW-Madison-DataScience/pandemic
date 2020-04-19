suppressMessages({
  library(shiny)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readr)
  library(ggrepel)
  library(RcppRoll)
  library(countrycode)
})

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

real_cases_jhu <- function() {
  # These data were modified 3/22 to only have one line per unit and changed names.
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
           Country = "Country/Region") %>%
    pivot_longer(-(Type:Long), names_to = "Date", values_to = "Count") %>%
    mutate(Date = as.POSIXct(Date, format="%m/%d/%y")) %>%
    mutate(County = "All",
           County = ifelse(str_detect(State, ","), str_remove(State, ",.*$"), County),
           State = ifelse(str_detect(State, ","), str_remove(State, "^.*, "), State),
           State = ifelse(State %in% state.name, 
                          state.abb[match(State, state.name)], 
                          State),
           Country = ifelse(Country == "US", "USA", Country),
           Country = ifelse(str_detect(Country, "Taiwan"), "Taiwan", Country)) %>%
    group_by(Type, County, State, Country, Date) %>%
    summarize(Count = sum(Count, na.rm = TRUE)) %>%
    ungroup %>%
    # Remove any date for Country with 0 confirmed.
    pivot_wider(names_from = Type, values_from = Count) %>%
    filter(Confirmed > 0) %>%
    pivot_longer(Confirmed:Recovered, names_to = "Type", values_to = "Count") %>%
    filter(!is.na(Count)) %>%
    weight_date()
}

real_cases_cds <- function() {
  # data downloaded from "https://coronadatascraper.com/timeseries-tidy.csv"
  # takes too long in the shiny app to download the data each time (34 seconds).  Need to 
  #  manually update the file in the application for this one to work
  #
  # Note that around 5 April the timeseries format changed with more columns
  # and update of tidy version seems to have broken down.
  # See added line below to "tidy up"
  
  dirpath <- "data/timeseries.csv"
  if(!file.exists(dirpath)) {
    dirpath <- "https://coronadatascraper.com/timeseries.csv"
  }
  # Probably need to rethink this, as there are records for whole country,
  # whole state and county, and they may not add up.
  # Also population is separate for each of these.
  read.csv(dirpath, stringsAsFactors = FALSE) %>%
    filter(city == "") %>% # remove any city level data
    
    mutate(country = ifelse(country == "United States", "USA", country),
           country = ifelse(country == "iso1:US", "USA", country),
           country = ifelse(country == "Metropolitan France", "France", country)) %>%

    pivot_longer(cases:tested, names_to = "type", values_to = "value") %>% # tidy up
    filter(type %in% c("cases", "deaths", "recovered")) %>% # also have active, growthFactor for some
    select(type, level, county, state, country, date, value, population) %>% 
    mutate(date = as.POSIXct(as.character(date), format="%Y-%m-%d")) %>% 
    rename(
      Type = type,
      County = county,
      State = state,
      Country = country,
      Date = date,
      Population = population,
      Count = value
    ) %>% 
    mutate(Type = case_when(
      Type == "cases" ~ "Confirmed",
      Type == "deaths" ~ "Death",
      Type == "recovered" ~ "Recovered",
      TRUE ~ NA_character_
    )) %>%
    as_tibble() %>%
    # Fix USA state names.
    mutate(State = str_remove(State, "^iso2:US-"),
           State = ifelse(State %in% state.name, 
                          state.abb[match(State, state.name)], 
                          State)) %>% 
    weight_date()
}

real_cases_county_cds <- function(cases_cds) {
  cases_cds %>% 
    filter(County != "",
           level == "county") %>%
    filter(!str_detect(County, "^,"),
           !str_detect(County, "unassign")) %>%
    mutate(County = paste0(County, ", ", State),
           County = ifelse(State %in% state.abb, 
                           str_replace(County, 
                                       state.name[match(State, state.abb)],
                                       state.abb[match(State, state.abb)]), 
                           County)) %>%
    select(-level)
}

real_cases_state_cds <- function(cases_cds) {
  cases_cds %>% 
    filter(County == "",
           State != "",
           level == "state") %>%
    select(-level, -County)
}

real_cases_country_cds <- function(cases_cds) {
  cases_cds %>% 
    filter(State == "",
           County == "",
           level == "country") %>%
    select(-level, -County, -State)
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

real_cases_state_jhu <- function(cases_state) {
  # Sum within Countrys
  cases_state %>%
    group_by(Type, Country, State, Date, Weight) %>%
    summarize(Count = sum(Count, na.rm = TRUE)) %>%
    ungroup
}

real_cases_country_jhu <- function(cases_state) {
  # Sum within Countrys
  cases_state %>%
    group_by(Type, Country, Date, Weight) %>%
    summarize(Count = sum(Count, na.rm = TRUE)) %>%
    ungroup
}

# JHU Data. Only has State for a few countries.
cases_county <- real_cases_jhu()
cases_state <- real_cases_state_jhu(cases_county)
cases <- real_cases_country_jhu(cases_state) %>%
  mutate(Continent = 
           countrycode(Country,
            origin = "country.name",
            destination = "continent",
            warn = FALSE),
         Region = 
           countrycode(Country,
                       origin = "country.name",
                       destination = "region",
                       warn = FALSE))
# 5 continents
# 20 regions if used as destination
continents <- distinct(cases, Continent, Region, Country) %>%
  filter(!is.na(Continent)) %>%
  arrange(Continent, Region) %>%
  mutate(Country.Code = countrycode(Country,
                                    origin = "country.name",
                                    destination = "iso3c",
                                    warn = FALSE))

# This does not have population for VAT, ERI, ESH or TWN
population <- read.csv(
  "https://raw.githubusercontent.com/datasets/population/master/data/population.csv",
  stringsAsFactors = FALSE) %>%
  filter(Year == 2018)

continents$Population <- NA
m <- match(continents$Country.Code, population$Country.Code, nomatch = 0)
continents$Population[m > 0] <- population$Value[m]
continents <- continents %>%
  mutate(Population = ifelse(Country.Code == "TWN", 23.78 * 1e6, Population),
         Population = ifelse(Country.Code == "VAT", 825, Population),
         Population = ifelse(Country.Code == "ERI", 3.214 * 1e6, Population),
         Population = ifelse(Country.Code == "ESH", 500000, Population))

continent_names <- sort(unique(continents$Continent))
region_names <- sort(unique(continents$Region))

cases <- cases %>%
  mutate(Population = continents$Population[match(Country, continents$Country)])
cases_region <- cases %>%
  filter(!is.na(Continent)) %>%
  group_by(Type, Continent, Region, Date, Weight) %>%
  summarize(Count = sum(Count),
            Population = sum(Population)) %>%
  ungroup  
cases_continent <- cases_region %>%
  filter(!is.na(Continent)) %>%
  group_by(Type, Continent, Date, Weight) %>%
  summarize(Count = sum(Count),
            Population = sum(Population)) %>%
  ungroup  


# Coronadatascraper Data. Using this for State and County
# Need to sort out County, State and Country better

cases_country_cds <- real_cases_cds() %>%
  mutate(Continent = 
           countrycode(Country,
                       origin = "country.name",
                       destination = "continent",
                       warn = FALSE),
         Region = 
           countrycode(Country,
                       origin = "country.name",
                       destination = "region",
                       warn = FALSE))
cases_county_cds <- real_cases_county_cds(cases_country_cds)
cases_state_cds <- real_cases_state_cds(cases_country_cds)
cases_country_cds <- real_cases_country_cds(cases_country_cds)

counties <- sort(unique(cases_county$County))
countries <- sort(unique(cases$Country))
# For now, use CDS for state and county
cases_state <- cases_state_cds
cases_county <- cases_county_cds

countries_state <- sort(unique(cases_state$Country))
countries_county <- sort(unique(cases_county$Country))

# Testing in USA
read_testing <- function(filename, by_state = FALSE) {
  out <- read_csv(filename, col_types = cols()) %>%
    mutate(date = as.Date(as.character(date), "%Y%m%d"))
  if(by_state) {
    out <- out %>%
      select(date, state, positive:pending, recovered, death, hospitalized, total)
  } else {
    out <- out %>%
      select(date, positive:pending, recovered, death, hospitalized, total)
  }
  out %>%
    pivot_longer(positive:total, names_to = "status", values_to = "count") %>%
    filter(status != "death")
}
test_us <- read_testing("https://covidtracking.com/api/us/daily.csv")
test_st <- read_testing("http://covidtracking.com/api/states/daily.csv", TRUE)

##########################################################################33

# Define UI for miles per gallon app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Pandemic Cases"),
  
  # Sidebar panel for inputs ----
  tabsetPanel(
    tabPanel("Reported Cases",
      sidebarLayout(
        sidebarPanel(
          h4("Select data to view from the options below."),
          radioButtons("states", "", c("Counties", "States","Countries","Regions","Continents"), inline = TRUE, selected = "States"),
          conditionalPanel(
            condition = 'input.states == "Countries" || input.states == "Regions"',
            selectInput("continent", "Continents:",
                        continent_names, "",
                        multiple = TRUE),
            uiOutput("region_cont")
          ),
          conditionalPanel(
            condition = 'input.states == "Countries"',
            uiOutput("country_cont")
          ),
          conditionalPanel(
            condition = 'input.states == "States"',
            selectInput("country_state", "Countries:", 
                        countries_state,
                        c("USA"),
                        multiple = TRUE)
          ),
          conditionalPanel(
            condition = 'input.states == "Counties"',
            selectInput("country_county", "Countries:", 
                        countries_county,
                        c("USA"),
                        multiple = TRUE)
          ),
          conditionalPanel(
            condition = 'input.states == "Counties" || input.states == "States"',
            uiOutput("states_country")
          ),
          conditionalPanel(
            condition = 'input.states == "Counties"',
            uiOutput("counties_states")
          ),
          selectInput("casetypes", "Case Type:", c("Confirmed","Death","Recovered")),
          selectInput("realscale", "Plot Scale:", c("raw","geometric","new_cases")),
          checkboxInput("predict", "Add predict lines?", FALSE),
          checkboxInput("rate", "Count per 100,000?", FALSE),
          conditionalPanel(
            condition = 'input.states != "Continents"',
            uiOutput("showallui")
          ),
          hr(),
          textOutput("latest"),
          uiOutput("newhost")
        ),
        mainPanel(
          plotOutput(outputId = "case_plot"),
          tableOutput("fitcase"),
          uiOutput("onep3"),
          conditionalPanel(
            condition = 'input.realscale == "new_cases"',
            textOutput("newcases")),
          conditionalPanel(
            condition = 'input.states != "Continents"',
            uiOutput("topcasestxt"),
            tableOutput("topcases"))
          )
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
      
      # Reported Cases
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
  
  regions_reactive <- reactive({
    sregions <- region_names
    if(length(input$region)) {
      sregions <- sort(input$region)
    }
    if(length(input$continent)) {
      sregions <- sort(unique((continents %>%
                          filter(Continent %in% input$continent,
                                 Region %in% sregions))$Region))
    }
    sregions
  })

  output$region_cont <- renderUI({
    if(!isTruthy(selected <- input$region)) {
      selected <- NULL
    }
    sregions <- region_names
    if(length(input$continent)) {
      sregions <- sort(unique((continents %>%
                                 filter(Continent %in% input$continent))$Region))
    }
    choices <- sort(unique(c(selected, sregions)))
    selectInput("region", "Regions:",
                choices, selected,
                multiple = TRUE)
  })
  
  output$country_cont <- renderUI({
    if(!isTruthy(selected <- input$country)) {
      selected <- c("USA", "France", "Iran", "Italy", "Spain")
    }
    choices <- sort(unique(c(selected, countries_reactive())))

    selectInput("country", "Countries:", 
                choices,
                selected,
                multiple = TRUE)
  })
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
  
  output$showallui <- renderUI({
    units <- paste0("Show all ", req(input$states), "?")
    checkboxInput("showall", units, FALSE)
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

  # Plot reported cases.
  output$case_plot <- renderPlot({
    req(input$states, input$casetypes)
    if(isTruthy(input$showall)) {
      dat <- allcases_reactive() %>%
        filter(Type == input$casetypes)       
    } else {
      dat <- cases_reactive() %>%
        filter(Type == input$casetypes)
    }
    if(isTruthy(input$rate)) {
      dat <- dat %>%
        mutate(Count = Count * 100000 / Population)
    }
    if(req(input$realscale) == "new_cases") {
      tmpfn <- function(Count) {
        dc <- diff(Count)
        c(rep(first(dc), 2), 
          roll_mean(c(first(Count), dc), 3))
      }
      switch(input$states,
      States = {
        dat <- dat %>% 
          group_by(State) %>%
            mutate(Count = tmpfn(Count)) %>%
          ungroup
      },
      Continents = {
        dat <- dat %>%
          group_by(Continent) %>%
            mutate(Count = tmpfn(Count)) %>%
          ungroup
      },
      Regions = {
        dat <- dat %>% 
          group_by(Region) %>%
          mutate(Count = tmpfn(Count)) %>%
          ungroup
      },
      Countries = {
        dat <- dat %>% 
          group_by(Country) %>%
          mutate(Count = tmpfn(Count)) %>%
          ungroup
      },
      Counties = {
        dat <- dat %>% 
          group_by(County) %>%
          mutate(Count = tmpfn(Count)) %>%
          ungroup
      })
    }
#    dat <- dat %>%
#      filter(Count >= 1)
    
    # Color-blind friendly palette with grey:
    cbPalette <- c("#DDDDDD", 
                   rep_len(c("#E69F00", "#56B4E9", "#009E73",
                             "#F0E442", "#0072B2", "#D55E00", "#CC79A7"),
                           length(units_reactive())))
    if(!isTruthy(input$showall))
      cbPalette <- cbPalette[-1]
    
    p <- ggplot(dat) +
      scale_colour_manual(values=cbPalette)
    
    p <- suppressWarnings(p +
      aes(Date, Count) +
      geom_line(size = 1.5) +
      ggtitle(paste(input$casetypes, "cases")))
    
    if(isTruthy(input$rate)) {
      p <- p + 
        ylab("Count per 100,000")
    }
    
    latest_date <- max(dat$Date)
    if(length(units_reactive()) > 1 | isTruthy(input$showall)) {
      switch(input$states,
      States = {
        p <- p +
          aes(col = colgp, z = State) +
          ggrepel::geom_label_repel(aes(label = colgp), color = "black",
                                    data = . %>% filter(Date == latest_date)) +
          theme(legend.position = "none")
      },
      Continents = {
        p <- p +
          aes(col = colgp, z = Continent) +
          ggrepel::geom_label_repel(aes(label = colgp), color = "black",
                                    data = . %>% filter(Date == latest_date)) +
          theme(legend.position = "none")
      },
      Regions = {
        p <- p +
          aes(col = colgp, z = Region) +
          ggrepel::geom_label_repel(aes(label = colgp), color = "black",
                                    data = . %>% filter(Date == latest_date)) +
          theme(legend.position = "none")
      },
      Countries = {
        p <- p +
          aes(col = colgp, z = Country) +
          ggrepel::geom_label_repel(aes(label = colgp), color = "black",
                                    data = . %>% filter(Date == latest_date)) +
          theme(legend.position = "none")
      },
      Counties = {
        p <- p + 
          aes(col = colgp, z = County) + 
          ggrepel::geom_label_repel(aes(label = colgp), color = "black",
                                    data = . %>% filter(Date == latest_date)) +
          theme(legend.position = "none")
        
      })
    } else {
      p <- p +
        aes(col = "#E69F00") +
        theme(legend.position = "none")
    }

    if(isTruthy(input$predict)) {
      # Set vertical range by data.
      p <- p +
        ylim(min(dat$Count), max(dat$Count))
      
      if(input$states == "States") {
        p <- suppressWarnings(p + 
          geom_smooth(method="glm", mapping = aes(weight = Weight), se = FALSE,
                      formula = round(y) ~ x,
                      method.args = list(family = "poisson"),
                      linetype = "dashed"))
      } else {
        p <- suppressWarnings(p + 
          geom_smooth(method="glm", mapping = aes(weight = Weight), se = FALSE,
                      formula = round(y) ~ x,
                      method.args = list(family = "poisson"),
                      linetype = "dashed"))
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
      },
      Regions = {
        if(!isTruthy(regions <- input$region)) {
          regions <- regions_reactive()
        }
        sort(regions)
      },
      Continents = {
        sort(continent_names)
      })
  }) 
  allcases_reactive <- reactive({
    switch(
      req(input$states),
      Counties = {
        cases_county %>%
          filter(State %in% input$state) %>%
          mutate(colgp = ifelse(County %in% units_reactive(),
                                County,
                                ""))
      },
      States = {
        cases_state %>% 
          filter(Country %in% countries_reactive()) %>%
          mutate(colgp = ifelse(State %in% units_reactive(),
                                State,
                                ""))
      },
      Continents = {
        cases_continent %>% 
          mutate(colgp = Continent)
      },
      Regions = {
        cases_region %>%
          mutate(colgp = ifelse(Region %in% units_reactive(),
                                Region,
                                ""))
      },
      Countries = {
        continents_in <- continent_names
        if(length(input$continent)) {
          continents_in <- input$continent
        }
        cases %>%
          filter(Continent %in% continents_in) %>%
          mutate(colgp = ifelse(Country %in% units_reactive(),
                                Country,
                                ""))
      })
  })
  
  cases_reactive <- reactive({
    switch(
      req(input$states),
      Counties = {
        cases_county %>% 
          filter(County %in% units_reactive()) %>%
          mutate(colgp = County)
      },
      States = {
        cases_state %>% 
          filter(Country %in% countries_reactive(), 
                 State %in% units_reactive()) %>%
          mutate(colgp = State)
      },
      Continents = {
        cases_continent %>% 
          mutate(colgp = Continent)
      },
      Regions = {
        cases_region %>% 
          filter(Region %in% units_reactive()) %>%
          mutate(colgp = Region)
      },
      Countries = {
        cases %>% 
          filter(Country %in% units_reactive()) %>%
          mutate(colgp = Country)
      })
  })
  countries_reactive <- reactive({
    switch(
      req(input$states),
      Countries =,
      Regions = {
        sort((continents %>% 
                filter(Region %in% regions_reactive()))$Country)
      },
      Counties = {
        sort(req(input$country_county))
      },
      States = {
        sort(req(input$country_state))
      })
  })
  
  # States in a Country
  output$states_country <- renderUI({
    states_country <- 
      sort(
        unique((
          cases_state %>% 
            filter(Country %in% countries_reactive()))$State))
    if(isTruthy(input$state)) {
      selected <- input$state
    } else {
      selected <- c("WI","MI","IL","IA")
    }
    states_country <- sort(unique(c(selected, states_country)))
    
    selectInput("state", "States:", 
                states_country,
                selected,
                multiple = TRUE)
  })
  # Counties in a State
  output$counties_states <- renderUI({
    req(input$state)
    counties_states <- 
      sort(
        unique((
          cases_county %>% 
            filter(Country %in% countries_reactive(),
                   State %in% input$state))$County))
    selectInput("county", "Counties:", 
                counties_states,
                c("Dane County, WI","Milwaukee County, WI", "Waukesha County, WI", "Fond du Lac County, WI"),
                multiple = TRUE)
  })
  
  output$latest <- renderText({
    paste0("Current to ",
           as.character(max(cases$Date)),
           " (Country), ",
           as.character(max(cases_state$Date)),
           " (State & County).")})
  
  newhost <- a("data-viz.it.wisc.edu/pandemic", 
                  href="https://data-viz.it.wisc.edu/pandemic/")
  output$newhost <- renderUI({
    tagList("New URL:", newhost)
  })
  
  # Fit line for reported cases.
  output$fitcase <- renderTable({
    req(input$states, input$casetypes)

    # Get estimate doubling time
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
          Continents = {
            form <- formula(Count ~ Date * Continent)
          },
          Regions = {
            form <- formula(Count ~ Date * Region)
          },
          Countries = {
            form <- formula(Count ~ Date * Country)
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
    # Time cannot be negative, but could be really small.
    doubling <- round(log(2) / pmax(0, coefs) / 86400, 1)
    
    dat <- cases_reactive()
    if(isTruthy(input$rate)){
      dat <- dat %>%
        mutate(Count = Count * 100000 / Population)
    }
    
    # Get data from last date.
    switch(input$states,
    States = {
      dat %>% 
        group_by(Type, State) %>%
        summarize(Count = max(Count, na.rm = TRUE)) %>%
        ungroup %>%
        select(Type, State, Count) %>%
        mutate(Count = as.integer(Count)) %>%
        pivot_wider(names_from = Type, values_from = Count) %>%
        mutate(Doubling = doubling) %>%
        arrange(desc(Confirmed))
    },
    Countries = {
      dat %>% 
        group_by(Type, Country) %>%
        summarize(Count = max(Count, na.rm = TRUE)) %>%
        ungroup %>%
        select(Type, Country, Count) %>%
        mutate(Count = as.integer(Count)) %>%
        pivot_wider(names_from = Type, values_from = Count) %>%
        mutate(Doubling = doubling) %>%
        arrange(desc(Confirmed))
    },
    Counties = {
      dat %>% 
        group_by(Type, County) %>% 
        summarize(Count = max(Count, na.rm = TRUE)) %>% 
        ungroup %>% 
        mutate(Count = as.integer(Count)) %>% 
        pivot_wider(names_from = Type, values_from = Count) %>% 
        mutate(Doubling = doubling) %>%
        arrange(desc(Confirmed))
    },
    Regions = {
      dat %>% 
        group_by(Type, Region) %>% 
        summarize(Count = max(Count, na.rm = TRUE)) %>% 
        ungroup %>% 
        mutate(Count = as.integer(Count)) %>% 
        pivot_wider(names_from = Type, values_from = Count) %>% 
        mutate(Doubling = doubling) %>%
        arrange(desc(Confirmed))
    },
    Continents = {
      dat %>% 
        # filter(Type == input$casetypes)
        group_by(Type, Continent) %>% 
        summarize(Count = max(Count, na.rm = TRUE)) %>% 
        ungroup %>% 
        mutate(Count = as.integer(Count)) %>% 
        pivot_wider(names_from = Type, values_from = Count) %>% 
        mutate(Doubling = doubling) %>%
        arrange(desc(Confirmed))
    })
  })
  
  # Show table of top cases
  output$topcases <- renderTable({
    topnum <- 5
    
    dat <- allcases_reactive()
    if(isTruthy(input$rate)){
      dat <- dat %>%
        mutate(Count = Count * 100000 / Population)
    }

    # Get data fro last date.
    switch(input$states,
           States = {
             dat %>% 
               filter(!(State %in% units_reactive())) %>%
               group_by(Type, State) %>%
               summarize(Count = max(Count, na.rm = TRUE)) %>%
               ungroup %>%
               arrange(desc(Count)) %>%
               select(Type, State, Count) %>%
               mutate(Count = as.integer(Count)) %>%
               pivot_wider(names_from = Type, values_from = Count) %>%
               top_n(topnum, Confirmed) %>%
               arrange(desc(Confirmed))
           },
           Regions = {
             dat %>% 
               filter(!(Region %in% units_reactive())) %>%
               group_by(Type, Region) %>%
               summarize(Count = max(Count, na.rm = TRUE)) %>%
               ungroup %>%
               select(Type, Region, Count) %>%
               mutate(Count = as.integer(Count)) %>%
               pivot_wider(names_from = Type, values_from = Count) %>%
               top_n(topnum, Confirmed) %>%
               arrange(desc(Confirmed))
           },
           Countries = {
             dat %>% 
               filter(!(Country %in% units_reactive())) %>%
               group_by(Type, Country) %>%
               summarize(Count = max(Count, na.rm = TRUE)) %>%
               ungroup %>%
               select(Type, Country, Count) %>%
               mutate(Count = as.integer(Count)) %>%
               pivot_wider(names_from = Type, values_from = Count) %>%
               top_n(topnum, Confirmed) %>%
               arrange(desc(Confirmed))
           },
           Counties = {
             dat %>% 
               filter(!(County %in% units_reactive())) %>%
               group_by(Type, County) %>% 
               summarize(Count = max(Count, na.rm = TRUE)) %>% 
               ungroup %>% 
               mutate(Count = as.integer(Count)) %>% 
               pivot_wider(names_from = Type, values_from = Count) %>%
               top_n(topnum, Confirmed) %>%
               arrange(desc(Confirmed))
           })
  })
  output$topcasestxt <- renderUI({
    tagList("Other top", input$states, "confirmed:")
  })

  output$testplot <- renderPlot({
    req(input$teststate)
    switch(
        req(input$testgroup),
        States = {
          p <- ggplot(test_st %>% 
                        filter(state %in% input$teststate)) +
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
    "Reported cases (Country level) come from Johns Hopkins U Center for Systems Science and Engineering."
  )
  sourcejhu <- a("github.com/CSSEGISandData/COVID-19", 
                 href="https://github.com/CSSEGISandData/COVID-19")
  output$jhusource <- renderUI({
    tagList("JHU CSSE Data URL:", sourcejhu)
  })
  sourcecds <- a("https://coronadatascraper.com/", href = "https://coronadatascraper.com/#home")
  output$cdsdata <- renderUI({
    tagList("Reported cases (State and County level) come from Corona Data Scraper:", sourcecds)
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
  output$newcases <- renderText("New cases are averages of last 3-days.")
  
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
    '"Reported Cases" tab draws on Johns Hopkins U CSSE, which many apps are using.
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
