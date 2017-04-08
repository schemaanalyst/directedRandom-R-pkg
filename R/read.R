#' FUNCTION: read_analysis
#'
#' Read the data file that contains the "analysis" data.
#' @export

read_analysis <- function() {
  f <- system.file("extdata", "analysis.csv", package="directedRandomR")
  d <- readr::read_csv(f)
  return(dplyr::tbl_df(d))
}

#' FUNCTION: read_time_constrained_mutation_data_subset
#'
#' Read the data file that contains the "time-constrained mutation" data. This is the data file that allows us to
#' compare mutation scores from the virtual mutation technique with scores when the original method is given the same
#' amount of running time as virtual mutation. (Phil previously called this "mutant selection").  Also, the data is
#' further subset to the 9 schemas that contain a consistent mutation pipeline through all experiments.
#' @export

read_mutants <- function() {
  f <- system.file("extdata", "mutants.csv", package="directedRandomR")
  d <- readr::read_csv(f)
  return(dplyr::tbl_df(d))
}
