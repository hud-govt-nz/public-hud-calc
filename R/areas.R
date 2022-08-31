library(tidyverse)

IN_FN <- "R/source-data/matched_ta.csv"
ta_df <-
  read_csv(IN_FN, show_col_types = FALSE) %>%
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
    island = "New Zealand")

#' TA matcher
#'
#' Matches TA details
#' @name get_ta
#' @param targ_ta Column of target TAs
#' @param out_col Name of TA details to extract
#' @export
get_ta <- function(targ_ta, out_col = "ta_code", by = "ta_match_name") {
  targ_ta %>%
    tolower() %>%
    str_replace("tauranga district/tauranga city", "tauranga") %>%
    str_replace("total", "new zealand") %>%
    str_replace(" (district|city|territory)", "") %>%
    str_replace_all("ā", "a") %>%
    str_replace_all("ē", "e") %>%
    str_replace_all("ī", "i") %>%
    str_replace_all("ō", "o") %>%
    str_replace_all("ū", "u") %>%
    tibble(.) %>%
    setNames(by) %>%
    left_join(ta_df, by = by) %>%
    pull(out_col)
}
