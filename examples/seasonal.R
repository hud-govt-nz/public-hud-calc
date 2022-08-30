library(devtools)
install_github("hud-govt-nz/hud-calc", force = TRUE)

library(tidyverse)
library(hud.calc)

df_wide <- hud.calc::sample_wide
df_long <-
  df_wide %>%
  pivot_longer(-period, names_to = "series")

# Mutate per column
df_wide %>%
mutate(
  constr_sadj = make_seasonal(constr, period),
  total_sadj = make_seasonal(total, period))

# Same, but using mutate_at()
df_wide %>%
mutate_at(vars(-period), make_seasonal, .$period)

# Can also work with long formats, but need to group first
df_long %>%
group_by(series) %>%
summarise(
  series = series,
  period = period,
  value = value,
  value_sadj = make_seasonal(value, period))
