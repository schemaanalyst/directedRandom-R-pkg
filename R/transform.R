


#' FUNCTION: transform_execution_times_for_threshold

#' Transform the timing values so that they do not include any below a specified number of ms.
#' @importFrom magrittr %>%
#' @export

transform_execution_times_for_threshold <- function(d, t) {
  td <- d %>% dplyr::mutate(testgenerationtime=ifelse(testgenerationtime<t, 0, testgenerationtime))
  return(td)
}


