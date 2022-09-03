#' TA matcher
#'
#' Matches TA details by ta_code or ta_match_name. Does a lot of cleaning and
#' simplification to resolve potential issues with name matches.
#' @name get_ta
#' @param src_ta Column of target TAs
#' @param out_col Name of column to extract
#' @param match_col Name of column to match against ("ta_match_name" or "ta_code")
#' @export
get_ta <- function(src_ta, out_col = "ta_code", match_col = "ta_match_name") {
  ta_df <-
    find_path() %>%
    paste0("/parsed/ta.csv") %>%
    read_csv(show_col_types = FALSE)
  if (match_col == "ta_match_name") {
    joined_df <-
      tibble(src_ta) %>%
      mutate(ta_match_name = scrub(src_ta)) %>%
      left_join(ta_df, by = match_col)
  }
  else if (match_col == "ta_code") {
    joined_df <-
      tibble(src_ta) %>%
      mutate(ta_code = src_ta) %>%
      left_join(ta_df, by = match_col)
  }
  else {
    stop("I can only match by TA code or TA name!")
  }
  # Check unmatched
  unmatched <- filter(joined_df, is.na(ta_code)) %>% unique() %>% pull(src_ta)
  if (length(unmatched) > 0) {
    warning("Unmatched TAs - ", paste(unmatched, collapse = ", "))
  }
  pull(joined_df, out_col)
}


#' Region matcher
#'
#' Matches region details by regc_code or regc_match_name. Does a lot of cleaning and
#' simplification to resolve potential issues with name matches.
#' @name get_regc
#' @param src_regc Column of target regions
#' @param out_col Name of column to extract
#' @param match_col Name of column to match against ("regc_match_name" or "regc_code")
#' @export
get_regc <- function(src_regc, out_col = "regc_code", match_col = "regc_match_name") {
  regc_df <-
    find_path() %>%
    paste0("/parsed/regc.csv") %>%
    read_csv(show_col_types = FALSE)
  if (match_col == "regc_match_name") {
    joined_df <-
      tibble(src_regc) %>%
      mutate(regc_match_name = scrub(src_regc)) %>%
      left_join(regc_df, by = match_col)
  }
  else if (match_col == "regc_code") {
    joined_df <-
      tibble(src_regc) %>%
      mutate(regc_code = src_regc) %>%
      left_join(regc_df, by = match_col)
  }
  else {
    stop("I can only match by regc code or regc name!")
  }
  # Check unmatched
  unmatched <- filter(joined_df, is.na(regc_code)) %>% unique() %>% pull(src_regc)
  if (length(unmatched) > 0) {
    warning("Unmatched regions - ", paste(unmatched, collapse = ", "))
  }
  pull(joined_df, out_col)
}


# Scrubs names to make them easier to match
scrub <- function(raw) {
  raw %>%
    tolower() %>%
    str_replace("tauranga district/tauranga city", "tauranga") %>%
    str_replace("total", "new zealand") %>%
    str_replace(" (district|region|city|territory)", "") %>%
    str_replace_all("ā", "a") %>%
    str_replace_all("ē", "e") %>%
    str_replace_all("ī", "i") %>%
    str_replace_all("ō", "o") %>%
    str_replace_all("ū", "u")
}


# Run this to update the TA/regc files
prep_areas <- function() {
  read_csv("sources/matched_ta.csv", show_col_types = FALSE) %>%
    mutate(
      ta_code = TA2022_V1_00,
      ta_name = TA2022_V1_00_NAME,
      ta_short_name =
        TA2022_V1_00_NAME %>%
        str_replace(" (District|City|Territory)", ""),,
      ta_match_name =
        TA2022_V1_00_NAME_ASCII %>%
        tolower() %>%
        str_replace(" (district|city|territory)", ""),
      regc_code = REGC2022_V1_00,
      regc_name = REGC2022_V1_00_NAME,
      regc_short_name =
        REGC2022_V1_00_NAME %>%
        str_replace(" Region", ""),,
      regc_match_name =
        REGC2022_V1_00_NAME_ASCII %>%
        tolower() %>%
        str_replace(" (region)", ""),
      regc_match_perc = MATCH_PERC,
      island = ISLAND,
      .keep = "none") %>%
    add_row(
      ta_code = 99,
      ta_name = "New Zealand",
      ta_short_name = "New Zealand",
      ta_match_name = "new zealand",
      regc_code = 99,
      regc_name = "New Zealand",
      regc_short_name = "New Zealand",
      regc_match_name = "new zealand",
      island = "New Zealand") %>%
    write_csv("inst/parsed/ta.csv") %>%
    select(regc_code, regc_name, regc_short_name, regc_match_name, island) %>%
    unique() %>%
    arrange(regc_code) %>%
    write_csv("inst/parsed/regc.csv")
}
