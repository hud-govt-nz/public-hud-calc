#' Get population
#' @name get_population
#' @param method "nearest_est" or "interpolate_proj"
#' @export
get_population <- function(method = "nearest_est") {
  find_path() %>%
    paste0("/parsed/pop_", method, ".csv") %>%
    read_csv(show_col_types = FALSE)
}

#' Population provider
#'
#' For a given set of TA/region/local boards + period, provide monthly total population count.
#' Monthly data is interpolated from population estimates.
#' @name match_population
#' @param area Column of area
#' @param period Column of period, or a single period
#' @param area_type Type of area ("regc", "ta", "akl_board")
#' @param method "nearest_est" or "interpolate_proj"
#' @export
match_population <- function (area, period, area_type = "auto", method = "nearest_est") {
  joined_df <-
    tibble(area, period) %>%
    mutate(period = as.Date(period),
           area_type = area_type)

  if (area_type == "auto") {
    joined_df <-
      joined_df %>%
      mutate(area_type = match_area(area, "area_type"),
             area_match_name = match_area(area, "area_match_name"))
  }
  else if (area_type == "regc") {
    joined_df <-
      joined_df %>%
      mutate(area_match_name = match_regc(area, "regc_match_name"))
  }
  else if (area_type == "ta") {
    joined_df <-
      joined_df %>%
      mutate(area_match_name = match_ta(area, "ta_match_name"))
  }
  else if (area_type == "talb") {
    joined_df <-
      joined_df %>%
      mutate(area_match_name = match_talb(area, "talb_match_name"))
  }
  else {
    stop("I can only match by 'regc', 'ta' or 'talb'! If unsure, use 'auto'.")
  }
  joined_df %>%
    left_join(get_population(method),
              by = c("area_type", "area_match_name", "period")) %>%
    pull(total)
}

prep_population <- function(start_date = "1996-01-01", end_date = "2048-01-01") {
  pop_proj <-
    prep_population_proj() %>%
    select(area_type, area_name, area_short_name, area_match_name,
           period, `0-14`, `15-39`, `40-64`, `65+`, total, median_age) %>%
    write_csv("inst/parsed/pop_projections.csv")
  pop_est <-
    prep_population_est() %>%
    select(area_type, area_name, area_short_name, area_match_name,
           period, `0-14`, `15-39`, `40-64`, `65+`, total, median_age)
  # Combine projection and estimates history (they're the same thing!)
  # TODO: Should stitch together historical estimates properly
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

prep_population_proj <- function() {
  SRC_FN <- "sources/subnational-population-projections-2018base-2048.xlsx"
  SHEETS <-
    c("regc" = "Table 4",
      "ta" = "Table 5",
      "akl_board" = "Table 6")
  COLUMNS <-
    c("area_name", "period", "0-14", "15–39", "40–64", "65+",
      "total", "births", "deaths",
      "natural_inc", "net_migration", "median_age")
  raw_pop_proj <- data.frame()
  for (area_type in names(SHEETS)) {
    read_excel(SRC_FN, SHEETS[area_type], skip = 6, col_names = COLUMNS) %>%
      filter(!is.na(period)) %>%
      fill(area_name, .direction = c("down")) %>%
      mutate_at(vars(-area_name),
                ~replace(., . == "...", NA) %>%
                as.numeric()) %>%
      rbind(raw_pop_proj) -> raw_pop_proj
  }
  # Add regc level NZ data to TA
  raw_pop_proj %>%
    filter(area_name == "New Zealand") %>%
    rbind(raw_pop_proj) %>%
    rename(`15-39` = `15–39`, `40-64` = `40–64`) %>%
    mutate(area_type = match_area(area_name, "area_type"),
           area_name = match_area(area_name, "area_name"),
           area_short_name = match_area(area_name, "area_short_name"),
           area_match_name = match_area(area_name, "area_match_name"))
}

prep_population_est <- function() {
  SRC_FN <- "sources/snpe-at30june21-population-by-broad-age-group.csv"
  read_csv(SRC_FN) %>%
    transmute(area_name = name,
              period = year,
              `0-14` = a0014,
              `15-39` = a1539,
              `40-64` = a4064,
              `65+` = a6500,
              `total` = atot,
              `median_age` = medage) %>%
    mutate(area_type = match_area(area_name, "area_type"),
           area_name = match_area(area_name, "area_name"),
           area_short_name = match_area(area_name, "area_short_name"),
           area_match_name = match_area(area_name, "area_match_name")) %>%
    unique()
}
