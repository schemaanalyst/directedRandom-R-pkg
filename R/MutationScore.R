# # Clear All
# rm(list = ls())
#
# SQLite.directedRandom.minusitrust.mutationanalysis <- read.csv("data/original-dr-files/30-SQLite-directedRandom-minusitrust-mutationanalysis.dat")
# Postgres.directedRandom.minusitrust.mutationanalysis <- read.csv("data/original-dr-files/30-Postgres-directedRandom-minusitrust-mutationanalysis.dat")
# HyperSQL.directedRandom.minusitrust.mutationanalysis <- read.csv("data/original-dr-files/30-HyperSQL-directedRandom-minusitrust-mutationanalysis.dat")
# HyperSQL.directedRandom.itrust.mutationanalysis <- read.csv("data/original-dr-files/30-HyperSQL-directedRandom-itrust-mutationanalysis.dat")
# SQLite.directedRandom.itrust.mutationanalysis <- read.csv("data/original-dr-files/30-SQLite-directedRandom-itrust-mutationanalysis.dat")
# mutation.analysis <- read.csv("data/transformed-files/mutation-analysis.dat")
#
# library(dplyr)
# library(ggplot2)
# library(reshape2)
# #library(png)
#
# mutationanalysistime <- rbind(`mutation.analysis`, `Postgres.directedRandom.minusitrust.mutationanalysis`, `SQLite.directedRandom.minusitrust.mutationanalysis`, `HyperSQL.directedRandom.minusitrust.mutationanalysis`, `SQLite.directedRandom.itrust.mutationanalysis`, `HyperSQL.directedRandom.itrust.mutationanalysis`)
# mutationanalysistime$realcasestudy <- gsub("IsoFlav_R2Repaired", "IsoFlav_R2", allFrames$casestudy)
#
# rm(mutation.analysis,SQLite.directedRandom.itrust.mutationanalysis, SQLite.directedRandom.minusitrust.mutationanalysis, Postgres.directedRandom.minusitrust.mutationanalysis, HyperSQL.directedRandom.itrust.mutationanalysis, HyperSQL.directedRandom.minusitrust.mutationanalysis)
# mutationscore <- mutationanalysistime %>% mutate(realcasestudy = as.character(gsub("parsedcasestudy.","",realcasestudy))) %>% group_by(dbms, datagenerator, realcasestudy) %>% summarise(mutationscore = mean((scorenumerator/scoredenominator) * 100))
#
# drMScore <- mutationscore %>% filter(datagenerator == "directedRandom")
# avsMScore <- mutationscore %>% filter(datagenerator == "avsDefaults")
#
# print(wilcox.test(avsMScore$mutationscore,drMScore$mutationscore))
#
# getVarName <- function(v1) {
#   deparse(substitute(v1))
# }
#
# # A-Hat Function
#
# AMeasure <- function(a,b){
#
#   # Compute the rank sum (Eqn 13)
#   r = rank(c(a,b))
#   r1 = sum(r[seq_along(a)])
#
#   # Compute the measure (Eqn 14)
#   m = length(a)
#   n = length(b)
#   A = (2* r1 - m*(m+1))/(2*m*n)
#
#   if (A == 0) {
#     print("the two techniques achieve equal performance")
#   }  else if (A < 0.5) {
#     print("the first technique is worse")
#   } else
#     print("the second technique is worse")
#   A
# }
#
# # The A measure is a value between 0 and 1
# # when the A measure is exactly 0.5, then the two techniques achieve equal performance;
# # when A is less than 0.5, the first technique is worse;
# # and when A is more than 0.5, the second technique is worse.
# # The closer to 0.5, the smaller the difference between the techniques; the farther from 0.5, the larger the difference.
#
# a <- AMeasure(avsMScore$mutationscore,drMScore$mutationscore)
# print(a)
