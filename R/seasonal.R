#' Sample wide data
#'
#' Jobs Online data in wide format
#' @name sample_wide
#' @export
sample_wide <- readr::read_csv("R/sample-data/jobs-online-wide.csv", show_col_types = FALSE)

#' Seasonal adjustment
#'
#' Makes seasonally adjusted column from [period, value] dataframe.
#' @name make_seasonal
#' @param val Value column
#' @param period Period column
#' @keywords hud clean seasonal seasonality
#' @importFrom zoo read.zoo as.yearmon
#' @importFrom seasonal seas
#' @export
#' @examples
#' df %>% mutate(value_sadj = make_seasonal(value, period))
make_seasonal <- function(val, period) {
  s_res <-
    data.frame(period, val) %>%
    read.zoo(FUN = as.yearmon) %>%
    as.ts() %>%
    seas()
  s_res$data[,"seasonaladj"]
}
