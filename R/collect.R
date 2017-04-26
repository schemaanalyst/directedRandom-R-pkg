#' FUNCTION: collect_mutationanalysistime
#'
#' Preform binding for all split files for mutationanalysistime.
#' @return A Data Frame of analysis
#' @importFrom magrittr %>%
#' @export
collect_mutationanalysistime <- function() {
  f <- system.file("extdata", "analysis.csv", package="directedRandomR")
  d <- readr::read_csv(f)
  return(dplyr::tbl_df(d))
}

#' FUNCTION: collect_mutanttiming
#'
#' Preform binding for all split files for mutatnttimng.
#' @return A Data Frame of mutants
#' @importFrom magrittr %>%
#' @export
collect_mutanttiming <- function() {
  f <- system.file("extdata", "mutants.csv", package="directedRandomR")
  d <- readr::read_csv(f)
  return(dplyr::tbl_df(d))
}

#' FUNCTION: generate_15_schmea_mutation_analysis
#'
#' Gets a 15 schemas from the dataset of analysis file, or mutantionanalysistime.dat.
#' @param d A data frame of analysis
#' @return A data frame with 15 schema randomly picked
#' @importFrom magrittr %>%
#' @export
generate_15_schmea_analysis <- function(d) {
  set.seed(2)
  cases <- dplyr::distinct(d, casestudy)
  random_schema <- kimisc::sample.rows(cases %>% dplyr::filter(casestudy != "iTrust", casestudy != "Products"), 13)
  d_13 <- d %>% dplyr::filter(casestudy %in% random_schema)
  d_itrust <- d %>% dplyr::filter(casestudy == "iTrust")
  d_products <- d %>% dplyr::filter(casestudy == "Products")
  d <- rbind(d_13, d_products, d_itrust)
  return(d)
}

#' FUNCTION: generate_15_schmea_mutanttiming
#'
#' Gets a 15 schemas from the dataset of mutants file, or mutanttiming.dat.
#' @param d A data frame of mutants
#' @return A data frame with 15 schema randomly picked
#' @importFrom magrittr %>%
#' @export
generate_15_schmea_mutants <- function(d) {
  set.seed(2)
  cases <- dplyr::distinct(d, schema)
  random_schema <- kimisc::sample.rows(cases %>% filter(schema != "iTrust", schema != "Products"), 13)
  d_13 <- d %>% dplyr::filter(schema %in% random_schema)
  d_itrust <- d %>% dplyr::filter(schema == "iTrust")
  d_products <- d %>% dplyr::filter(schema == "Products")
  d <- rbind(d_13, d_products, d_itrust)
  return(d)
}
