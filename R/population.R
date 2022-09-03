#' Get population
#' @name get_population
#' @export
get_population <- function() {
  find_path() %>%
    paste0("/parsed/population.csv") %>%
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
#' @export
match_population <- function (area, period, area_type) {
  if (area_type == "regc") {
    match_name <- match_regc(area, "regc_match_name")
  }
  else if (area_type == "ta") {
    match_name <- match_ta(area, "ta_match_name")
  }
  else {
    stop("I can only match by 'ta' or 'regc'!")
  }
  tibble(area, period) %>%
    mutate(
      period = as.Date(period),
      area_match_name = match_name) %>%
    left_join(get_population() %>% filter(area_type == !!area_type),
              by = c("area_match_name", "period")) %>%
    pull(total)
}


prep_population <- function() {
  SRC_FN <- "sources/subnational-population-projections-2018base-2048.xlsx"
  SHEETS <-
    c("regc" = "Table 4",
      "ta" = "Table 5",
      "akl_board" = "Table 6")
  COLUMNS <-
    c("area_name", "period", "0-14", "15–39", "40–64", "65+",
      "total", "births", "deaths",
      "natural_inc", "net_migration", "median_age")
  raw_pop_df <- data.frame()
  for (area_type in names(SHEETS)) {
    read_excel(SRC_FN, SHEETS[area_type], skip = 6, col_names = COLUMNS) %>%
      mutate(area_type = area_type) %>%
      filter(!is.na(period)) %>%
      fill(area_name, .direction = c("down")) %>%
      mutate(
        area_name = str_replace(area_name, "\\s*\\(\\d+\\)\\s*$", ""),
        area_short_name = str_replace(area_name, "\\s*(region|city|district|territory|local board area)\\s*$", ""),
        area_match_name = area_short_name %>%
                          tolower() %>%
                          str_replace_all("ā", "a") %>%
                          str_replace_all("ē", "e") %>%
                          str_replace_all("ī", "i") %>%
                          str_replace_all("ō", "o") %>%
                          str_replace_all("ū", "u")) %>%
      mutate_at(vars(-area_type,
                     -area_name,
                     -area_short_name,
                     -area_match_name),
                ~replace(., . == "...", NA) %>%
                as.numeric()) %>%
      rbind(raw_pop_df) -> raw_pop_df
  }
  raw_pop_df %>%
    # Add regc level NZ data to TA
    filter(area_name == "New Zealand") %>%
    mutate(area_type = "ta") %>%
    rbind(raw_pop_df) %>%
    # Interpolate to monthly level
    mutate(period = as.character(period) %>% parse_date("%Y")) %>%
    group_by(area_type, area_name, area_short_name, area_match_name) %>%
    complete(period =  seq(as.Date("1996-01-01"), as.Date("2048-01-01"), by = "month")) %>%
    mutate_at(vars(-area_type,
                   -area_name,
                   -area_short_name,
                   -area_match_name,
                   -period),
              zoo::na.approx, na.rm = FALSE) %>%
    ungroup() %>%
    write_csv("inst/parsed/population.csv")
}
