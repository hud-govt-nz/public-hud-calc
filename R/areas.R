#' Get TAs
#' @name get_ta
#' @export
get_ta <- function() {
  find_path() %>%
    paste0("/parsed/ta.csv") %>%
    read_csv(show_col_types = FALSE)
}

#' TA matcher
#'
#' Matches TA details by ta_code or ta_match_name. Does a lot of cleaning and
#' simplification to resolve potential issues with name matches.
#' @name match_ta
#' @param src_ta Column of target TAs
#' @param out_col Name of column to extract
#' @param match_col Name of column to match against ("ta_match_name" or "ta_code")
#' @export
match_ta <- function(src_ta, out_col = "ta_code", match_col = "ta_match_name") {
  if (match_col == "ta_match_name") {
    joined_df <-
      tibble(src_ta) %>%
      mutate(ta_match_name = scrub(src_ta)) %>%
      left_join(get_ta(), by = match_col)
  }
  else if (match_col == "ta_code") {
    joined_df <-
      tibble(src_ta) %>%
      mutate(ta_code = src_ta) %>%
      left_join(get_ta(), by = match_col)
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


#' Get TA-local boards
#' @name get_talb
#' @export
get_talb <- function() {
  find_path() %>%
    paste0("/parsed/talb.csv") %>%
    read_csv(show_col_types = FALSE)
}

#' TA-local board matcher
#'
#' Matches TA/local board details by talb_code or talb_match_name. Does a lot of cleaning and
#' simplification to resolve potential issues with name matches.
#' @name match_talb
#' @param src_talb Column of target TA/local boards
#' @param out_col Name of column to extract
#' @param match_col Name of column to match against ("talb_match_name" or "talb_code")
#' @export
match_talb <- function(src_talb, out_col = "talb_code", match_col = "talb_match_name") {
  if (match_col == "talb_match_name") {
    joined_df <-
      tibble(src_talb) %>%
      mutate(talb_match_name = scrub(src_talb)) %>%
      left_join(get_talb(), by = match_col)
  }
  else if (match_col == "talb_code") {
    joined_df <-
      tibble(src_talb) %>%
      mutate(ta_code = src_talb) %>%
      left_join(get_talb(), by = match_col)
  }
  else {
    stop("I can only match by TA/LB code or TA/LB name!")
  }
  # Check unmatched
  unmatched <- filter(joined_df, is.na(talb_code)) %>% unique() %>% pull(src_talb)
  if (length(unmatched) > 0) {
    warning("Unmatched TA/LBs - ", paste(unmatched, collapse = ", "))
  }
  pull(joined_df, out_col)
}


#' Get regions
#' @name get_regc
#' @export
get_regc <- function() {
  find_path() %>%
    paste0("/parsed/regc.csv") %>%
    read_csv(show_col_types = FALSE)
}

#' Region matcher
#'
#' Matches region details by regc_code or regc_match_name. Does a lot of cleaning and
#' simplification to resolve potential issues with name matches.
#' @name match_regc
#' @param src_regc Column of target regions
#' @param out_col Name of column to extract
#' @param match_col Name of column to match against ("regc_match_name" or "regc_code")
#' @export
match_regc <- function(src_regc, out_col = "regc_code", match_col = "regc_match_name") {
  if (match_col == "regc_match_name") {
    joined_df <-
      tibble(src_regc) %>%
      mutate(regc_match_name = scrub(src_regc)) %>%
      left_join(get_regc(), by = match_col)
  }
  else if (match_col == "regc_code") {
    joined_df <-
      tibble(src_regc) %>%
      mutate(regc_code = src_regc) %>%
      left_join(get_regc(), by = match_col)
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


#' Get regions/TAs/LBs/islands/NZ
#' @name get_area
#' @export
get_area <- function() {
  data.frame() %>%
    rbind(get_ta() %>%
          transmute(
            area_type = "ta",
            area_code = ta_code,
            area_name = ta_name,
            area_short_name = ta_short_name,
            area_match_name = ta_match_name)) %>%
    rbind(get_talb() %>%
          transmute(
            area_type = "talb",
            area_code = talb_code,
            area_name = talb_name,
            area_short_name = talb_short_name,
            area_match_name = talb_match_name)) %>%
    rbind(get_regc() %>%
          transmute(
            area_type = "regc",
            area_code = regc_code,
            area_name = regc_name,
            area_short_name = regc_short_name,
            area_match_name = regc_match_name)) %>%
    add_row(
        area_type = "island",
        area_code = "91",
        area_name = "North Island",
        area_short_name = "North Island",
        area_match_name = "north island") %>%
    add_row(
        area_type = "island",
        area_code = "92",
        area_name = "South Island",
        area_short_name = "South Island",
        area_match_name = "south island") %>%
    add_row(
        area_type = "nz",
        area_code = "99",
        area_name = "New Zealand",
        area_short_name = "New Zealand",
        area_match_name = "new zealand")
}


#' Unknown/multi-level area matcher
#'
#' Matches region/TA/local board details by match_name,
#' without knowing what type of area it is.
#' @name match_area
#' @param src_area Column of target areas
#' @param out_col Name of column to extract
#' @export
match_area <- function(src_area, out_col = "area_name") {
  raw_df <-
    tibble(src_area) %>%
    mutate(
      src_area_cleaned =
        src_area %>%
          tolower() %>%
          str_replace("total", "new zealand") %>%
          str_trim(),
      area_type =
        case_when(
          str_detect(src_area_cleaned, "new zealand") ~ "nz",
          str_detect(src_area_cleaned, "north island|south island") ~ "island",
          str_detect(src_area_cleaned, ".* region") ~ "regc",
          str_detect(src_area_cleaned, "^auckland$|.* (city|district|territory)") ~ "ta",
          str_detect(src_area_cleaned, ".* (local board area)") ~ "talb"),
      area_match_name = scrub(src_area))

  # Look for local boards being present and Auckland being missing, treat this as TALB
  if ("ta" %in% raw_df$area_type &
      "talb" %in% raw_df$area_type &
      !any(str_detect(raw_df$src_area_cleaned, "^auckland$"))) {
    raw_df <-
      raw_df %>%
      mutate(area_level = str_replace(area_type, "^ta$", "talb"))
  }
  raw_df %>%
    left_join(get_area(), by=c("area_type", "area_match_name")) %>%
    pull(out_col)
}


# Scrubs names to make them easier to match
scrub <- function(raw) {
  raw %>%
    tolower() %>%
    str_replace("wanganui", "whanganui") %>%
    str_replace("tauranga district/tauranga city", "tauranga") %>%
    str_replace("^great barrier", "aotea/great barrier") %>%
    str_replace("north island.*", "north island") %>%
    str_replace("south island.*", "north island") %>%
    str_replace("total", "new zealand") %>%
    str_replace("'", "") %>%
    str_replace("-", " ") %>%
    str_replace(" (region|district|city|territory|local board area)", "") %>%
    str_replace_all("ā", "a") %>%
    str_replace_all("ē", "e") %>%
    str_replace_all("ī", "i") %>%
    str_replace_all("ō", "o") %>%
    str_replace_all("ū", "u") %>%
    str_trim()
}


# Run this to update the TA/regc files
prep_areas <- function() {
  # Hand-matched using: https://en.wikipedia.org/wiki/Territorial_authorities_of_New_Zealand#List_of_territorial_authorities
  match <-
    read_csv("sources/matched_ta.csv", show_col_types = FALSE) %>%
    mutate(
      ta_match_name = TA2022_V1_00_NAME_ASCII %>% scrub(),
      regc_code = REGC2022_V1_00,
      regc_name = REGC2022_V1_00_NAME,
      regc_short_name = REGC2022_V1_00_NAME %>% str_replace(" Region", ""),
      regc_match_name = REGC2022_V1_00_NAME_ASCII %>% scrub(),
      regc_match_perc = MATCH_PERC,
      island = ISLAND,
      .keep = "none")

  # https://datafinder.stats.govt.nz/layer/106669-territorial-authority-2022-clipped-generalised/
  read_csv("sources/territorial_authority_2022_clipped_csv.csv") %>%
    transmute(
      ta_code = TA2022_V1_00,
      ta_name = TA2022_V1_00_NAME,
      ta_short_name = TA2022_V1_00_NAME %>% str_replace(" (District|City|Territory|Local Board Area)", ""),
      ta_match_name = TA2022_V1_00_NAME_ASCII %>% scrub()
    ) %>%
    left_join(match, by = "ta_match_name") %>%
    add_row(
      ta_code = "099",
      ta_name = "New Zealand",
      ta_short_name = "New Zealand",
      ta_match_name = "new zealand",
      regc_code = 99,
      regc_name = "New Zealand",
      regc_short_name = "New Zealand",
      regc_match_name = "new zealand",
      island = "New Zealand") %>%
    write_csv("inst/parsed/ta.csv")

  # https://datafinder.stats.govt.nz/layer/106694-territorial-authority-local-board-2022-clipped-generalised/
  read_csv("sources/territorial_authority_local_board_2022_clipped_csv.csv") %>%
    transmute(
      talb_code = TALB2022_V1_00,
      talb_name = TALB2022_V1_00_NAME,
      talb_short_name = TALB2022_V1_00_NAME %>% str_replace(" (District|City|Territory|Local Board Area)", ""),
      talb_match_name = TALB2022_V1_00_NAME_ASCII %>% scrub()
    ) %>%
    left_join(match %>%
              rename(talb_match_name = ta_match_name),
              by = "talb_match_name") %>%
    add_row(
      talb_code = "09900",
      talb_name = "New Zealand",
      talb_short_name = "New Zealand",
      talb_match_name = "new zealand",
      regc_code = 99,
      regc_name = "New Zealand",
      regc_short_name = "New Zealand",
      regc_match_name = "new zealand",
      island = "New Zealand") %>%
    write_csv("inst/parsed/talb.csv")

  # https://datafinder.stats.govt.nz/layer/106667-regional-council-2022-clipped-generalised/
  read_csv("sources/regional_council_2022_clipped_csv.csv") %>%
    transmute(
      regc_code = REGC2022_V1_00,
      regc_name = REGC2022_V1_00_NAME,
      regc_short_name = REGC2022_V1_00_NAME %>% str_replace(" (District|City|Territory|Local Board Area)", ""),
      regc_match_name = REGC2022_V1_00_NAME_ASCII %>% scrub()
    ) %>%
    left_join(match %>%
              select(regc_match_name, island),
              by = "regc_match_name") %>%
    unique() %>%
    add_row(
      regc_code = "99",
      regc_name = "New Zealand",
      regc_short_name = "New Zealand",
      regc_match_name = "new zealand",
      island = "New Zealand") %>%
    write_csv("inst/parsed/regc.csv")
}
