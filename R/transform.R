#' FUNCTION: transform_execution_times_for_threshold
#'
#' Transform the timing values so that they do not include any below a specified number of ms.
#' Include rounding down
#' @param d Data frame of mutants
#' @param t A threshold time (ms)
#' @return A data frame of a transforemd test generation timing
#' @importFrom magrittr %>%
#' @export
transform_execution_times_for_threshold <- function(d, t) {
  td <- d %>% dplyr::mutate(testgenerationtime=ifelse(testgenerationtime<t, 0, (floor(testgenerationtime/t) * t )))
  return(td)
}

#' FUNCTION: transform_execution_times_for_threshold_no_rounding
#'
#' Transform the timing values so that they do not include any below a specified number of ms.
#' @param d Data frame of mutants
#' @param t A threshold time (ms)
#' @return A data frame of a transforemd test generation timing (no rounding)
#' @importFrom magrittr %>%
#' @export
transform_execution_times_for_threshold_no_rounding <- function(d, t) {
  td <- d %>% dplyr::mutate(testgenerationtime=ifelse(testgenerationtime<t, 0, testgenerationtime))
  return(td)
}
