check_freshness <- function() {
  est_df <-
    find_path() %>%
    paste0("/parsed/pop_estimates.csv") %>%
    read_csv(show_col_types = FALSE)

  warn_date <- 
    paste0(max(est_df$period) + 1, "-10-30") %>%
    as.Date()

  if (Sys.Date() > warn_date) {
    warning("hud-calc population data is out-of-date! Follow the update instructions at hud-govt-nz/hud-calc.")
  }
}

#' Get population
#' @name get_population
#' @param method "nearest_est" or "interpolate_proj"
#' @export
get_population <- function(method = "nearest_est") {
  check_freshness()

  base_df <-
    find_path() %>%
    paste0("/parsed/pop_", method, ".csv") %>%
    read_csv(show_col_types = FALSE)

  # Repeat the island/national level data for each area_type
  base_df %>%
    rbind(base_df %>%
          filter(area_type %in% c("island", "nz")) %>%
          mutate(area_type = "regc")) %>%
    rbind(base_df %>%
          filter(area_type %in% c("island", "nz")) %>%
          mutate(area_type = "ta")) %>%
    rbind(base_df %>%
          filter(area_type %in% c("island", "nz")) %>%
          mutate(area_type = "talb"))
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
  check_freshness()

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