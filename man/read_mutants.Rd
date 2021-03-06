% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read.R
\name{read_mutants}
\alias{read_mutants}
\title{FUNCTION: read_mutants}
\usage{
read_mutants()
}
\value{
A Data Frame of mutants
}
\description{
Read the data file that contains the "time-constrained mutation" data. This is the data file containts all mutants,
killed or alive. It is refered in SchemaAnalyst github repo as 'mutanttiming.dat'.
This file is useful if you are interested in looking at individual mutants.
This file contains seven attributes: identifier, dbms, schema, operator, type, killed, time.
It allow us to compare mutation scores.
}
