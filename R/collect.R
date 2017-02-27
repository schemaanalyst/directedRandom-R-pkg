library(dplyr)
library(ggplot2)
library(reshape2)

# Clear All
#rm(list = ls())
#mutationanalysistime <- NULL


#' FUNCTION: collect_mutationanalysistime
#'

#' Preform binding for all split files for mutationanalysistime.
#' @importFrom magrittr %>%
#' @export
collect_mutationanalysistime <- function() {
  `SQLite.directedRandom.minusitrust.mutationanalysis` <- read.csv("original-dr-files/30-SQLite-directedRandom-minusitrust-mutationanalysis.dat")
  `SQLite.directedRandom.itrust.mutationanalysis` <- read.csv("original-dr-files/30-SQLite-directedRandom-itrust-mutationanalysis.dat")
  `Postgres.directedRandom.minusitrust.mutationanalysis` <- read.csv("original-dr-files/30-Postgres-directedRandom-minusitrust-mutationanalysis.dat")
  `Postgres.directedRandom.itrust.mutationanalysis` <- read.csv("original-dr-files/30-Postgres-directedRandom-itrust-mutationanalysis.dat")
  `HyperSQL.directedRandom.minusitrust.mutationanalysis` <- read.csv("original-dr-files/30-HyperSQL-directedRandom-minusitrust-mutationanalysis.dat")
  `HyperSQL.directedRandom.itrust.mutationanalysis` <- read.csv("original-dr-files/30-HyperSQL-directedRandom-itrust-mutationanalysis.dat")

  `HyperSQL.avs.minusitrust.mutationanalysis` <- read.csv("original-avs-files/30-HyperSQL-avs-minusitrust-mutationanalysis.dat")
  `Postgres.avs.minusitrust.mutationanalysis` <- read.csv("original-avs-files/30-Postgres-avs-minusitrust-mutationanalysis.dat")
  `SQLite.avs.minusitrust.mutationanalysis` <- read.csv("original-avs-files/30-SQLite-avs-minusitrust-mutationanalysis.dat")


  `HyperSQL.avs.itrust.mutationanalysis` <- read.csv("original-avs-files/30-HyperSQL-avs-itrust-mutationanalysis.dat")
  `SQLite.avs.itrust.mutationanalysis` <- read.csv("original-avs-files/30-SQLite-avs-itrust-mutationanalysis.dat")

  `mutation.analysis` <- read.csv("transformed-files/mutation-analysis.dat")

  allFrames <- rbind(`Postgres.directedRandom.minusitrust.mutationanalysis`,
                     `Postgres.directedRandom.itrust.mutationanalysis`,
                     `SQLite.directedRandom.minusitrust.mutationanalysis`,
                     `SQLite.directedRandom.itrust.mutationanalysis`,
                     `HyperSQL.directedRandom.minusitrust.mutationanalysis`,
                     `HyperSQL.directedRandom.itrust.mutationanalysis`,
                     `HyperSQL.avs.minusitrust.mutationanalysis`,
                     `Postgres.avs.minusitrust.mutationanalysis`,
                     `SQLite.avs.minusitrust.mutationanalysis`,
                     `HyperSQL.avs.itrust.mutationanalysis`,
                     `SQLite.avs.itrust.mutationanalysis`)

  allFrames <- allFrames %>% dplyr::mutate(casestudy = as.character(gsub("parsedcasestudy.","",casestudy)))
  allFrames <- rbind(allFrames, `mutation.analysis`)
  #allFrames <- rbind(`mutation.analysis`, `Postgres.directedRandom.minusitrust.mutationanalysis`, `SQLite.directedRandom.minusitrust.mutationanalysis`)
  allFrames$casestudy <- gsub("IsoFlav_R2Repaired", "IsoFlav_R2", allFrames$casestudy)
  return(allFrames)
}

#' FUNCTION: collect_mutanttiming
#'

#' Preform binding for all split files for mutatnttimng.
#' @importFrom magrittr %>%
#' @export
collect_mutanttiming <- function() {
    # deep mutation analysis
    `30.SQLite.directedRandom.minusitrust.mutanttiming` <- read.csv("original-dr-files/30-SQLite-directedRandom-minusitrust-mutanttiming.dat")
    `30.SQLite.directedRandom.itrust.mutanttiming` <- read.csv("original-dr-files/30-SQLite-directedRandom-itrust-mutanttiming.dat")
    `30.Postgres.directedRandom.minusitrust.mutanttiming` <- read.csv("original-dr-files/30-Postgres-directedRandom-minusitrust-mutanttiming.dat")
    `30.Postgres.directedRandom.itrust.mutanttiming` <- read.csv("original-dr-files/30-Postgres-directedRandom-itrust-mutanttiming.dat")
    `30.HyperSQL.directedRandom.itrust.mutanttiming` <- read.csv("original-dr-files/30-HyperSQL-directedRandom-itrust-mutanttiming.dat")
    `30.HyperSQL.directedRandom.minusitrust.mutanttiming` <- read.csv("original-dr-files/30-HyperSQL-directedRandom-minusitrust-mutanttiming.dat")

    `HyperSQL.avs.minusitrust.mutanttiming` <- read.csv("original-avs-files/30-HyperSQL-avs-minusitrust-mutanttiming.dat")
    `Postgres.avs.minusitrust.mutanttiming` <- read.csv("original-avs-files/30-Postgres-avs-minusitrust-mutanttiming.dat")
    `SQLite.avs.minusitrust.mutanttiming` <- read.csv("original-avs-files/30-SQLite-avs-minusitrust-mutanttiming.dat")

    `HyperSQL.avs.itrust.mutanttiming` <- read.csv("original-avs-files/30-HyperSQL-avs-itrust-mutanttiming.dat")
    `SQLite.avs.itrust.mutanttiming` <- read.csv("original-avs-files/30-SQLite-avs-itrust-mutanttiming.dat")

    hypersql.avmdefaults <- read.csv("transformed-files/hypersql-avmdefaults.dat")
    sqlite.avmdefaults <- read.csv("transformed-files/sqlite-avmdefaults.dat")
    postgres.avmdefaults <- read.csv("transformed-files/postgres-avmdefaults.dat")

    hypersql.random <- read.csv("transformed-files/hypersql-random.dat")
    sqlite.random <- read.csv("transformed-files/sqlite-random.dat")
    postgres.random <- read.csv("transformed-files/postgres-random.dat")

    namevector <- c("generator")
    `30.SQLite.directedRandom.minusitrust.mutanttiming`[,namevector] <- "directedRandom"
    `30.SQLite.directedRandom.itrust.mutanttiming`[,namevector] <- "directedRandom"
    `30.Postgres.directedRandom.minusitrust.mutanttiming`[,namevector] <- "directedRandom"
    `30.HyperSQL.directedRandom.itrust.mutanttiming`[,namevector] <- "directedRandom"
    `30.HyperSQL.directedRandom.minusitrust.mutanttiming`[,namevector] <- "directedRandom"
    `30.Postgres.directedRandom.itrust.mutanttiming`[,namevector] <- "directedRandom"
    hypersql.avmdefaults[,namevector] <- "avsDefaults"
    sqlite.avmdefaults[,namevector] <- "avsDefaults"
    postgres.avmdefaults[,namevector] <- "avsDefaults"
    hypersql.random[,namevector] <- "random"
    sqlite.random[,namevector] <- "random"
    postgres.random[,namevector] <- "random"

    `HyperSQL.avs.minusitrust.mutanttiming`[,namevector] <- "avs"
    `Postgres.avs.minusitrust.mutanttiming`[,namevector] <- "avs"
    `SQLite.avs.minusitrust.mutanttiming`[,namevector] <- "avs"
    `HyperSQL.avs.itrust.mutanttiming`[,namevector] <- "avs"
    `SQLite.avs.itrust.mutanttiming`[,namevector] <- "avs"

    mutanttiming <- rbind(`30.SQLite.directedRandom.minusitrust.mutanttiming`,
                          `30.SQLite.directedRandom.itrust.mutanttiming`,
                          `30.Postgres.directedRandom.minusitrust.mutanttiming`,
                          `30.HyperSQL.directedRandom.itrust.mutanttiming`,
                          `30.HyperSQL.directedRandom.minusitrust.mutanttiming`,
                          `30.Postgres.directedRandom.itrust.mutanttiming`,
                          hypersql.avmdefaults,
                          sqlite.avmdefaults,
                          postgres.avmdefaults,
                          hypersql.random,
                          postgres.random,
                          sqlite.random,
                          `HyperSQL.avs.minusitrust.mutanttiming`,
                          `Postgres.avs.minusitrust.mutanttiming`,
                          `SQLite.avs.minusitrust.mutanttiming`,
                          `HyperSQL.avs.itrust.mutanttiming`,
                          `SQLite.avs.itrust.mutanttiming`
    )
    #mutanttiming$schema <- gsub("IsoFlav_R2Repaired", "IsoFlav_R2", schema$casestudy)

    return(mutanttiming)
}


#mutatnttiming <- collect_mutanttiming()
#mutationanalysistime <- collect_mutationanalysistime()



