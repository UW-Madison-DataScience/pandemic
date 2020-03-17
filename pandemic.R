library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)

double_cases <- function(doubling, actual, hidden, hospitalizing) {
  tibble(
    dates = seq(as.Date("2020-03-16"), as.Date("2020-06-01"), by="days")) %>%
    mutate(days = seq_along(dates) - 1,
           actual = case0 * 2 ^ (days / doubling),
           confirmed = actual / hidden,
           hospitalized = actual / hospitalizing) %>%
    pivot_longer(actual:hospitalized,
                 names_to = "Cases",
                 values_to = "Count")
}
