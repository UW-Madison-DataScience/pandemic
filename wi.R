dirpath <- "https://coronadatascraper.com/timeseries.csv"
library(dplyr)
wi <- read.csv(dirpath, stringsAsFactors = FALSE) %>%
  filter(state == "Wisconsin") %>% # Wisconsin only
  filter(city == "") %>% # remove any city level data
  filter(county != "") # remove state/region aggregate counts
write.csv(wi, "wi.csv", row.names = FALSE, na = "")
 