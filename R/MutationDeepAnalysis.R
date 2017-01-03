library(dplyr)
library(ggplot2)
library(reshape2)

# Clear All
#rm(list = ls())


# deep mutation analysis 
`30.SQLite.directedRandom.minusitrust.mutanttiming` <- read.csv("~/phd_exper/mutantdata/mutant-data/original-dr-files/30-SQLite-directedRandom-minusitrust-mutanttiming.dat")
`30.SQLite.directedRandom.itrust.mutanttiming` <- read.csv("~/phd_exper/mutantdata/mutant-data/original-dr-files/30-SQLite-directedRandom-itrust-mutanttiming.dat")
`30.Postgres.directedRandom.minusitrust.mutanttiming` <- read.csv("~/phd_exper/mutantdata/mutant-data/original-dr-files/30-Postgres-directedRandom-minusitrust-mutanttiming.dat")
`30.HyperSQL.directedRandom.itrust.mutanttiming` <- read.csv("~/phd_exper/mutantdata/mutant-data/original-dr-files/30-HyperSQL-directedRandom-itrust-mutanttiming.dat")
`30.HyperSQL.directedRandom.minusitrust.mutanttiming` <- read.csv("~/phd_exper/mutantdata/mutant-data/original-dr-files/30-HyperSQL-directedRandom-minusitrust-mutanttiming.dat")
hypersql.avmdefaults <- read.csv("~/phd_exper/mutantdata/mutant-data/transformed-files/hypersql-avmdefaults.dat")
sqlite.avmdefaults <- read.csv("~/phd_exper/mutantdata/mutant-data/transformed-files/sqlite-avmdefaults.dat")
postgres.avmdefaults <- read.csv("~/phd_exper/mutantdata/mutant-data/transformed-files/postgres-avmdefaults.dat")

mutationanalysistime <- rbind(`30.SQLite.directedRandom.minusitrust.mutanttiming`,
                   `30.SQLite.directedRandom.itrust.mutanttiming`,
                   `30.Postgres.directedRandom.minusitrust.mutanttiming`,
                   `30.HyperSQL.directedRandom.itrust.mutanttiming`,
                   `30.HyperSQL.directedRandom.minusitrust.mutanttiming`,
                   hypersql.avmdefaults,
                   sqlite.avmdefaults,
                   postgres.avmdefaults
)

rm(`30.SQLite.directedRandom.minusitrust.mutanttiming`)
rm(`30.SQLite.directedRandom.itrust.mutanttiming`)
rm(`30.Postgres.directedRandom.minusitrust.mutanttiming`)
rm(`30.HyperSQL.directedRandom.itrust.mutanttiming`)
rm(`30.HyperSQL.directedRandom.minusitrust.mutanttiming`)
rm(hypersql.avmdefaults)
rm(sqlite.avmdefaults)
rm(postgres.avmdefaults)


