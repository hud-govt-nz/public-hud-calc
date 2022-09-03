# This is a crummy way to find the font path, but here() and sys.frame(1)$ofile don't work
find_path <- function() {
  for (lp in .libPaths()) {
    p <- paste0(lp, "/hud.calc")
    if (dir.exists(p)) return(p)
  }
  stop("Cannot find library path!")
}

#' Sample data
#' @name calc_test_data
#' @export
calc_test_data <- function() {
  find_path() %>%
  paste0("/test/housing-register-by-ta.csv") %>%
  read_csv(show_col_types = FALSE) %>%
  mutate(period = parse_date(period, "%b-%y"))
}
