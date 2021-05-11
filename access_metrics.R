library(tidyverse)
library(RPostgres)
library(odbc)
library(DBI)
library(skimr)
# https://data-viz.it.wisc.edu/cornell-parameter-sweep/

con <- dbConnect(RPostgres::Postgres(),
                 dbname = "test",
                 host = "database-2.clbsgd2qdkby.us-east-1.rds.amazonaws.com",
                 port = 5432,
                 user = "postgres",
                 password = "YxDi7HnjfpBxHKQ")


## Metrics with sim_id and group_number 
metrics <- dbGetQuery(con, 'Select * from metrics') %>% 
  as_tibble() %>% 
  pivot_longer(cols = -c(sim_id, group_number), 
               names_to = "metric_name", 
               values_to = "metric_value")

## Parameters for each simulation 
all_params <-
  tbl(con, "group_params") %>%
  distinct() %>%
  collect() %>% 
  mutate(across(where(bit64::is.integer64), as.numeric))

all_params_wide <-
  all_params %>%
  select(-`_scenario_name`) %>%
  pivot_wider(id_cols = sim_id,
              names_from = group_number,
              values_from = c(everything(), -sim_id, -group_number)) %>%
  mutate(across(everything(), ~ as.factor(.x)))

skim_df <- 
  all_params_wide %>% 
  skimr::skim() %>% 
  filter(factor.n_unique > 1)


## A basic combination of the two with only the parameters that are varied
metrics %>% 
  pivot_wider(names_from = "metric_name", values_from = "metric_value") %>% 
  left_join(all_params_wide %>% 
              select(sim_id, skim_df$skim_variable),
            by = "sim_id")

tmpfn <- function(x, y = "") {
  paste0(y, " ", 100 * signif(as.numeric(as.character(x)), 2), "%")
}
dat <- metrics %>% 
         mutate(metric_name = make.names(metric_name)) %>%
         pivot_wider(names_from = "metric_name", values_from = "metric_value") %>% 
         left_join(all_params_wide %>% 
                     select(sim_id, skim_df$skim_variable),
                   by = "sim_id") %>%
  rename(contact_UG_on = "contact_rate_multiplier_0",
         contact_UG_off = "contact_rate_multiplier_1",
         contact_Grad_res = "contact_rate_multiplier_2",
         contact_Grad_tea = "contact_rate_multiplier_3",
         contact_Staff = "contact_rate_multiplier_4",
         prev_UG_on = "initial_ID_prevalence_0",
         prev_UG_off = "initial_ID_prevalence_1",
         test_UG_on = "test_population_fraction_0",
         test_UG_off = "test_population_fraction_1") %>%
  mutate(test_UG_on = tmpfn(test_UG_on, "test_UG_on"),
         test_UG_off = tmpfn(test_UG_off, "test_UG_off"),
         prev_UG_off = tmpfn(prev_UG_off, "prev_UG_off"),
         prev_UG_on = tmpfn(prev_UG_on, "prev_UG_on"))

ggplot(dat %>%
         filter(contact_UG_on == 1.25,
                contact_UG_off == 1,
                contact_Grad_res == 1,
                contact_Grad_tea == 1,
                contact_Staff == 1)) +
  aes(Time.of.peak.COVID.19.cases, Peak.active.cases,
      col = test_UG_off) +
  geom_point() +
  geom_smooth(se = FALSE) +
  scale_y_log10() +
  facet_grid(test_UG_on ~ prev_UG_on)

fit <- lm(log10(Peak.active.cases) ~
            contact_UG_on + contact_UG_off +
            contact_Grad_res + contact_Grad_tea +
            contact_Staff + 
            test_UG_on + test_UG_off +
            prev_UG_on + prev_UG_off, dat)

fits <- lm(log10(Peak.active.cases) ~
             (contact_UG_on + contact_UG_off +
             contact_Grad_res + contact_Grad_tea +
             contact_Staff + 
             test_UG_on + test_UG_off +
             prev_UG_on + prev_UG_off)^2, dat)
fitss <- step(fits)
anova(fitss)

ggplot(dat %>%
         mutate(contact_UG_on = as.numeric(as.character(contact_UG_on))) %>%
         filter(contact_UG_off == 1,
                contact_Grad_res == 1,
                contact_Grad_tea == 1,
                contact_Staff == 1,
                test_UG_off == "test_UG_off 7.1%")) +
  aes(contact_UG_on, Peak.active.cases,
      col = prev_UG_off) +
  geom_jitter(height = 0, width = 0.05) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_log10() +
  facet_grid(test_UG_on ~ prev_UG_on)

ggplot(dat %>%
#         mutate(contact_UG_on = as.numeric(as.character(contact_UG_on))) %>%
         filter(contact_UG_on == 1,
                contact_UG_off == 1,
                contact_Grad_res == 1,
                contact_Grad_tea == 1,
                contact_Staff == 1,
                test_UG_off == "test_UG_off 7.1%")) +
  aes(Time.of.peak.COVID.19.cases, Peak.active.cases,
      col = prev_UG_off) +
  geom_jitter(height = 0, width = 0.05) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_log10() +
  facet_grid(test_UG_on ~ prev_UG_on)

dat %>%
  mutate(contact_UG_on = as.numeric(as.character(contact_UG_on))) %>%
  filter(contact_UG_off == 1,
         contact_Grad_res == 1,
         contact_Grad_tea == 1,
         contact_Staff == 1,
         test_UG_off == "test_UG_off 7.1%") %>%
  select(Peak.active.cases, contact_UG_on, test_UG_on, prev_UG_on, prev_UG_off)
