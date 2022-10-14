# HUD calculation tools
**CAUTION: This repo is public. Do not include sensitive data or key materials.**

## Installation
You'll need `devtools::install_github` to install the package:
```R
library(devtools)
install_github("hud-govt-nz/hud-calc")
```


## Usage
* `make_seasonal`: One-line seasonal adjustments - interprets dates automagically.
* `match_ta`/`match_talb`/`match_regc`: Turn TAs/TA-local boards/regions into reliable, consistent names or code.
* `get_pop`: Provides population estimate for any month + NZ/region/TA/local board combination.

```R
library(tidyverse)
library(hud.calc)

df_base <- calc_test_data()

# Clean names and population adjustments for TAs
df_base %>%
  mutate(
    ta_code = match_ta(ta, "ta_code"),
    ta_short_name = match_ta(ta, "ta_short_name"),
    ta_name = match_ta(ta, "ta_name"),
    pop = match_population(ta, period, "ta"), # Use interpolated population estimate
    per_10k = 10000 * value / pop,
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
```


## Maintaining this package
If you make changes to this package, you'll need to rerun document from the root directory to update all the R generated files.
```R
library(roxygen2)
roxygenise()
```

I had real problems installing `roxygen2`, because there's a problem with the upstream library `cli`. It's been fixed, but it's not in the CRAN version as of 29-08-2022. You might need the Github version:
```R
library(devtools)
install_github("r-lib/cli")
install_github("r-lib/roxygen2")
library(cli)
library(roxygen2)
roxygenise()
```
