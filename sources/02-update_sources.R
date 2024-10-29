library(tidyverse)
library(readxl)
library(hud.calc)

prep_population <- function(start_date = "1996-01-01", end_date = "2048-01-01") {
  # Manually rewrite if there are any changes in data structure
  pop_proj <-
    prep_population_data(
      src_fn = "sources/subnational-population-projections-2018base-2048-update.xlsx",
      sheets = c("Table 4", "Table 5", "Table 6"),
      col_names =
        c("area_name", "period", "0-14", "15-39", "40-64", "65+", "total",
          "births", "deaths", "natural_inc", "net_migration", "median_age"),
      skip_rows = 6)

  # Manually rewrite if there are any changes in data structure
  pop_est <-
    prep_population_data(
      src_fn = "sources/subnational-population-estimates-at-30-june-2024-provisional.xlsx",
      sheets = c("Table 3", "Table 4"),
      col_names =
        c("area_name", "area_name_alt", "period", "0-14", "15-39", "40-64", "65+", "total",
          "births", "deaths", "natural_inc", "net_migration", "median_age"),
      skip_rows = 6)

  # Combine projection and estimates history (they're the same thing!)
  # TODO: Should stitch together historical estimates properly
  write_csv(pop_proj, "inst/parsed/pop_projections.csv")
  pop_est <-
    pop_proj %>%
    filter(period < min(pop_est$period)) %>%
    rbind(pop_est) %>% arrange(area_type, area_name, period) %>%
    write_csv("inst/parsed/pop_estimates.csv")

  # Method 1: Use nearest 30 June estimate
  pop_est %>%
    mutate(period = as.character(period) %>% parse_date("%Y")) %>% # Treat 30 June data as 1 Jan data, so a simple filldown can fill the whole year with 30 June data
    group_by(area_type, area_name, area_short_name, area_match_name) %>%
    complete(period = seq(as.Date(start_date), as.Date(end_date), by = "month")) %>%
    fill(`0-14`, `15-39`, `40-64`, `65+`, total, median_age, .direction = c("downup")) %>%
    ungroup() %>%
    write_csv("inst/parsed/pop_nearest_est.csv")

  # Method 2: Interpolate projections
  pop_proj %>%
    mutate(period = paste0(as.character(period), "-07-01") %>% parse_date("%Y-%m-%d")) %>% # Treat 30 June data as 1 July data for interpolation
    group_by(area_type, area_name, area_short_name, area_match_name) %>%
    complete(period = seq(as.Date(start_date), as.Date(end_date), by = "month")) %>%
    mutate_at(vars(`0-14`, `15-39`, `40-64`, `65+`, total),
              ~ round(zoo::na.approx(., na.rm = FALSE))) %>%
    mutate_at(vars(median_age),
              ~ round(zoo::na.approx(., na.rm = FALSE), 1)) %>%
    ungroup() %>%
    write_csv("inst/parsed/pop_interpolate_proj.csv")

  # TODO: Should use estimates to adjust projections then interpolate
}

prep_population_data <- function(src_fn, sheets, col_names, skip_rows) {
  sheets %>%
    map(function(sheet_name) {
      df <-
        read_excel(src_fn, sheet_name, skip = skip_rows, col_names = col_names) %>%
        filter(!is.na(period)) # Remove junk rows
      # Somenames are on an alt column
      if ("area_name_alt" %in% col_names) {
        df <-
          df %>%
          mutate(area_name = coalesce(area_name, area_name_alt))
      }
      # Fill names - do this AFTER coalesce
      df <-
        df %>%
        fill(area_name, .direction = c("down"))
      return(df)
    }) %>%
    do.call(rbind, .) %>%
    transmute(
      # Match to known area (REGC/TA/LB) names
      area_type = match_area(area_name, "area_type"),
      area_name = match_area(area_name, "area_name"),
      area_short_name = match_area(area_name, "area_short_name"),
      area_match_name = match_area(area_name, "area_match_name"),
      # Remove provisional flag
      period = sub(" P", "", period),
      # Everything else
      `0-14`, `15-39`, `40-64`, `65+`, `total`, `median_age`) %>%
    # Clean numbers
    mutate_at(
      vars(`0-14`, `15-39`, `40-64`, `65+`, `total`, `median_age`),
      ~replace(., . == "...", NA) %>% as.numeric()) %>%
    unique()
}
