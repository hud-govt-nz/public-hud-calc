library(devtools)
install_local("../hud-calc", force = TRUE)

library(tidyverse)
library(hud.calc)

df_base <- calc_test_data()

# Clean names and population adjustments for TAs
df_base %>%
  mutate(
    ta_code = match_ta(ta, "ta_code"),
    ta_short_name = match_ta(ta, "ta_short_name"),
    ta_name = match_ta(ta, "ta_name"),
    pop = match_population(ta, period, "auto"), # Use interpolated population estimate
    per_10k = 10000 * value / pop,
    pop_proj = match_population(ta, period, "auto", method = "interpolate_proj"), # Use 2018 population estimate
    per_10k_proj = 10000 * value / pop_proj,
    pop_2018 = match_population(ta, "2018-01-01", "ta"), # Use 2018 population estimate
    per_10k_2018 = 10000 * value / pop_2018)

# Aggregate TAs into regions and population adjust
df_base %>%
  mutate(region = match_ta(ta, "regc_name")) %>%
  group_by(region, period) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  group_by(region) %>%
  mutate(
    sadj = make_seasonal(value, period),
    pop = match_population(region, period, "regc"),
    per_10k = 10000 * value / pop,
    per_10k_sadj = 10000 * sadj / pop)

# Aggregate miscellaneous regions
SMALL_REGIONS <-
  c("Rest of North Island" = "Northland",
    "Rest of North Island" = "Bay of Plenty" ,
    "Rest of North Island" = "Gisborne" ,
    "Rest of North Island" = "Hawke's Bay" ,
    "Rest of North Island" = "Taranaki" ,
    "Rest of North Island" = "Manawatū-Whanganui" ,
    "Rest of South Island" = "West Coast" ,
    "Rest of South Island" = "Otago" ,
    "Rest of South Island" = "Southland" ,
    "Rest of South Island" = "Tasman" ,
    "Rest of South Island" = "Nelson" ,
    "Rest of South Island" = "Marlborough")

df_base %>%
  mutate(region = match_ta(ta, "regc_short_name")) %>%
  # Aggregate TAs into regions
  group_by(region, period) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  mutate(pop = match_population(region, period, "regc")) %>% # Much faster outside group
  # Rename regions, then aggregate again
  mutate(region = factor(region) %>% fct_recode(!!!SMALL_REGIONS)) %>%
  group_by(region, period) %>%
  summarise(value = sum(value), pop = sum(pop)) %>%
  ungroup()
