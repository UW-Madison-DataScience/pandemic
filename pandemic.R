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
