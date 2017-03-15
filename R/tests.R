library(testthat)
library(directedRandomR)
context("mutant-operators-hsql-CRelOpE")

test_that("testing_operator_mutation_CRelOpE_hsql", {
  library(dplyr)
  # setting to the data
  #setwd('~/phd_exper/mutantdata/bitbucket/directedrandomr/')
  print(getwd())
  # Collecting mutant operators from mutant timing
  mutant_operator <- directedRandomR::collect_mutanttiming()
  #setwd('~/phd_exper/mutantdata/bitbucket/directedrandomr/R/')
  selected_operator <- "CCRelationalExpressionOperatorE"
  db <- "HyperSQL"

  # Filtering data from mutanttiming without iTrust
  filtered_data <- mutant_operator %>% filter(operator == selected_operator, schema != "iTrust", dbms == db) %>% group_by(identifier, dbms)
  # Selecting first schema to get the runs
  first_schema <- filtered_data[1,3]
  # empty var
  test <- NULL
  # Adding a number column represnting runs
  ids <- filtered_data %>% filter(schema== first_schema[[1,1]], generator == "directedRandom") %>% select(identifier,dbms,schema,operator,type) %>% unique
  ids$number=1:nrow(ids)
  filtered_data %>% left_join(ids)  %>% mutate(number=as.numeric(ifelse(is.na(number),1,number))) %>% ungroup %>% mutate(number = cummax(number)) -> test

  # geting DR runs Non-iTrust
  dr_minsitrust <- test %>% filter(generator == "directedRandom") %>% group_by(identifier, dbms, generator, number) %>% summarise(killed_mutants = sum(killed == "true"), total_mutants = (sum(killed == "true") + sum(killed == "false"))) %>% mutate(mutationScore = round((killed_mutants/total_mutants) * 100, 2))
  # geting DR runs iTrust
  dr_itrust <- mutant_operator %>% filter(schema == "iTrust", generator == "directedRandom", type == "NORMAL", operator == selected_operator, dbms == db) %>% group_by(identifier, dbms, generator) %>% summarise(killed_mutants = sum(killed == "true"), total_mutants = (sum(killed == "true") + sum(killed == "false"))) %>% mutate(mutationScore = round((killed_mutants/total_mutants) * 100, 2))
  if (nrow(dr_itrust) > 0) {
    dr_itrust$number=1:nrow(dr_itrust)
  }
  # iTrust has 0 row for this operator
  expect_equal(nrow(dr_itrust), 0)
  dr <- rbind(dr_minsitrust, dr_itrust)
  dr <- dr %>% group_by(number, generator, dbms) %>% summarise(killed_mutants = sum(killed_mutants), total_mutants = sum(total_mutants)) %>% mutate(mutationScore = round((killed_mutants/total_mutants) * 100, 2))
  # Check that dr is 30 rows meaning the number of runs
  expect_equal(nrow(dr), 30)

  # Adding a number column represnting runs
  ids <- filtered_data %>% filter(schema== first_schema[[1,1]], generator == "avs") %>% select(identifier,dbms,schema,operator,type) %>% unique
  ids$number=1:nrow(ids)
  filtered_data %>% left_join(ids)  %>% mutate(number=as.numeric(ifelse(is.na(number),1,number))) %>% ungroup %>% mutate(number = cummax(number)) -> test


  # geting AVM-R runs Non-iTrust
  avmr_minsitrust <- test %>% filter(generator == "avs") %>% group_by(identifier, dbms, generator, number) %>% summarise(killed_mutants = sum(killed == "true"), total_mutants = (sum(killed == "true") + sum(killed == "false"))) %>% mutate(mutationScore = round((killed_mutants/total_mutants) * 100, 2))
  # geting AVM-R runs iTrust
  avmr_itrust <- mutant_operator %>% filter(schema == "iTrust", generator == "avs", type == "NORMAL", operator == selected_operator, dbms == db) %>% group_by(identifier, dbms, generator) %>% summarise(killed_mutants = sum(killed == "true"), total_mutants = (sum(killed == "true") + sum(killed == "false"))) %>% mutate(mutationScore = round((killed_mutants/total_mutants) * 100, 2))
  if (nrow(avmr_itrust) > 0) {
    avmr_itrust$number=1:nrow(avmr_itrust)
  }
  # iTrust has 0 row for this operator
  expect_equal(nrow(avmr_itrust), 0)
  avmr <- rbind(avmr_minsitrust, avmr_minsitrust)
  avmr <- avmr %>% group_by(number, generator, dbms) %>% summarise(killed_mutants = sum(killed_mutants), total_mutants = sum(total_mutants)) %>% mutate(mutationScore = round((killed_mutants/total_mutants) * 100, 2))
  # Check that AVM-R is 30 rows meaning the number of runs
  expect_equal(nrow(avmr), 30)

  # Adding a number column represnting runs
  ids <- filtered_data %>% filter(schema== first_schema[[1,1]], generator == "avsDefaults") %>% select(identifier,dbms,schema,operator,type) %>% unique
  ids$number=1:nrow(ids)
  filtered_data %>% left_join(ids)  %>% mutate(number=as.numeric(ifelse(is.na(number),1,number))) %>% ungroup %>% mutate(number = cummax(number)) -> test


  # geting AVM-D runs Non-iTrust
  avmd_minsitrust <- test %>% filter(generator == "avsDefaults") %>% group_by(identifier, dbms, generator, number) %>% summarise(killed_mutants = sum(killed == "true"), total_mutants = (sum(killed == "true") + sum(killed == "false"))) %>% mutate(mutationScore = round((killed_mutants/total_mutants) * 100, 2))
  # geting AVM-D runs iTrust
  avmd_itrust <- mutant_operator %>% filter(schema == "iTrust", generator == "avsDefaults", type == "NORMAL", operator == selected_operator, dbms == db) %>% group_by(identifier, dbms, generator) %>% summarise(killed_mutants = sum(killed == "true"), total_mutants = (sum(killed == "true") + sum(killed == "false"))) %>% mutate(mutationScore = round((killed_mutants/total_mutants) * 100, 2))
  if (nrow(avmd_itrust) > 0) {
    avmd_itrust$number=1:nrow(avmd_itrust)
  }
  # iTrust has 0 row for this operator
  expect_equal(nrow(avmd_itrust), 0)
  avmd <- rbind(avmd_minsitrust, avmd_itrust)
  avmd <- avmd %>% group_by(number, generator, dbms) %>% summarise(killed_mutants = sum(killed_mutants), total_mutants = sum(total_mutants)) %>% mutate(mutationScore = round((killed_mutants/total_mutants) * 100, 2))
  # Check that dr is 30 rows meaning the number of runs
  expect_equal(nrow(avmd), 30)

  # Adding a number column represnting runs
  ids <- filtered_data %>% filter(schema== first_schema[[1,1]], generator == "random") %>% select(identifier,dbms,schema,operator,type) %>% unique
  ids$number=1:nrow(ids)
  filtered_data %>% left_join(ids)  %>% mutate(number=as.numeric(ifelse(is.na(number),1,number))) %>% ungroup %>% mutate(number = cummax(number)) -> test


  # geting Random runs Non-iTrust
  rand_minsitrust <- test %>% filter(generator == "random") %>% group_by(identifier, dbms, generator, number) %>% summarise(killed_mutants = sum(killed == "true"), total_mutants = (sum(killed == "true") + sum(killed == "false"))) %>% mutate(mutationScore = round((killed_mutants/total_mutants) * 100, 2))
  # geting Random runs iTrust
  rand_itrust <- mutant_operator %>% filter(schema == "iTrust", generator == "random", type == "NORMAL", operator == selected_operator, dbms == db) %>% group_by(identifier, dbms, generator) %>% summarise(killed_mutants = sum(killed == "true"), total_mutants = (sum(killed == "true") + sum(killed == "false"))) %>% mutate(mutationScore = round((killed_mutants/total_mutants) * 100, 2))
  if (nrow(rand_itrust) > 0) {
    rand_itrust$number=1:nrow(rand_itrust)
  }
  # iTrust has 0 row for this operator
  expect_equal(nrow(rand_itrust), 0)
  rand <- rbind(rand_minsitrust, rand_itrust)
  rand <- rand %>% group_by(number, generator, dbms) %>% summarise(killed_mutants = sum(killed_mutants), total_mutants = sum(total_mutants)) %>% mutate(mutationScore = round((killed_mutants/total_mutants) * 100, 2))
  # Check that dr is 30 rows meaning the number of runs
  expect_equal(nrow(rand), 30)

  dr <- arrange(dr, number)
  avmr <- arrange(avmr, number)
  avmd <- arrange(avmd, number)
  rand <- arrange(rand, number)

  avmr_effectsize <- directedRandomR::effectsize_accurate(dr$mutationScore, avmr$mutationScore)$size
  avmd_effectsize <- directedRandomR::effectsize_accurate(dr$mutationScore, avmd$mutationScore)$size
  rand_effectsize <- directedRandomR::effectsize_accurate(dr$mutationScore, rand$mutationScore)$size

  expect_equal(avmr_effectsize, "small")

  expect_equal(avmd_effectsize, "large")

  expect_equal(rand_effectsize, "large")

})
