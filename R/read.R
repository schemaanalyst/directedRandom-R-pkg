#' FUNCTION: read_analysis
#'
#' Read the data file that contains the "analysis" data. This is the data containing all test generation times, coverages,
#' evaluations etc. It is refered in SchemaAnalyst github repo as 'newmutationanalysis.dat'. And it allow us to compare
#' test generation timing and coverages results.
#' @export
read_analysis <- function() {
  f <- system.file("extdata", "analysis.csv", package="directedRandomR")
  d <- readr::read_csv(f)
  return(dplyr::tbl_df(d))
}

#' FUNCTION: read_mutants
#'
#' Read the data file that contains the "time-constrained mutation" data. This is the data file containts all mutants,
#' killed or alive. It is refered in SchemaAnalyst github repo as 'mutanttiming.dat'.
#' This file is useful if you are interested in looking at individual mutants.
#' This file contains seven attributes: identifier, dbms, schema, operator, type, killed, time.
#' It allow us to compare mutation scores.
#' @export
read_mutants <- function() {
  f <- system.file("extdata", "mutants.csv", package="directedRandomR")
  d <- readr::read_csv(f)
  return(dplyr::tbl_df(d))
}
