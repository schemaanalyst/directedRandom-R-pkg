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

  # Testing if the mean is correct to the table
  average <- round(sum(dr$killed_mutants) / sum(dr$total_mutants) * 100, 2)
  expect_equal(average, 88.82)

  average <- round(sum(avmr$killed_mutants) / sum(avmr$total_mutants) * 100, 2)
  expect_equal(average, 88.52)

  average <- round(sum(avmd$killed_mutants) / sum(avmd$total_mutants) * 100, 2)
  expect_equal(average, 84.55)

  average <- round(sum(rand$killed_mutants) / sum(rand$total_mutants) * 100, 2)
  expect_equal(average, 87.09)

  # Testing Siginificant results to the table
  p <- wilcox.test(dr$mutationScore, avmr$mutationScore)$p.value <= 0.05
  expect_false(p)

  p <- wilcox.test(dr$mutationScore, avmd$mutationScore)$p.value <= 0.05
  expect_true(p)

  p <- wilcox.test(dr$mutationScore, rand$mutationScore)$p.value <= 0.05
  expect_true(p)


})


test_that("testing_mutatin_score_for_StudentResidence_SQLite", {
  # Gettting Mutants
  mutants <- directedRandomR::collect_mutanttiming()
  # Filtering mutants and getting precentage kills SQLite
  mutants <- mutants %>% filter(type == "NORMAL", dbms == "SQLite") %>% select(identifier, dbms, schema, generator, killed) %>% group_by(identifier, dbms, schema, generator) %>% summarise(killed_mutants = sum(killed == "true"), total_mutants = (sum(killed == "true") + sum(killed == "false"))) %>% mutate(mutationScore = round((killed_mutants/total_mutants) * 100, 2))
  # getting mutants per generator

  dr <- mutants %>% filter(generator == "directedRandom")
  avmr <- mutants %>% filter(generator == "avs")
  avmd <- mutants %>% filter(generator == "avsDefaults")
  rand <- mutants %>% filter(generator == "random")


  # Checking StudentResidence schema
  dr_StudentResidence <- dr %>% filter(schema == "StudentResidence")
  avmr_StudentResidence <- avmr %>% filter(schema == "StudentResidence")
  avmd_StudentResidence <- avmd %>% filter(schema == "StudentResidence")
  rand_StudentResidence <- rand %>% filter(schema == "StudentResidence")

  # Check 30 runs
  expect_equal(nrow(dr_StudentResidence), 30)
  expect_equal(nrow(avmr_StudentResidence), 30)
  expect_equal(nrow(avmd_StudentResidence), 30)
  expect_equal(nrow(rand_StudentResidence), 30)

  # check averages

  averages <- round(sum(dr_StudentResidence$killed_mutants) / sum(dr_StudentResidence$total_mutants) * 100, 2)
  expect_equal(averages, 95.73)

  averages <- round(sum(avmr_StudentResidence$killed_mutants) / sum(avmr_StudentResidence$total_mutants) * 100, 2)
  expect_equal(averages, 96.58)

  averages <- round(sum(avmd_StudentResidence$killed_mutants) / sum(avmd_StudentResidence$total_mutants) * 100, 2)
  expect_equal(averages, 87.18)

  averages <- round(sum(rand_StudentResidence$killed_mutants) / sum(rand_StudentResidence$total_mutants) * 100, 2)
  expect_equal(averages, 77.69)


  # Testing Siginificant results to the table
  p <- wilcox.test(dr_StudentResidence$mutationScore, avmr_StudentResidence$mutationScore)$p.value <= 0.05
  expect_true(p)

  p <- wilcox.test(dr_StudentResidence$mutationScore, avmd_StudentResidence$mutationScore)$p.value <= 0.05
  expect_true(p)

  p <- wilcox.test(dr_StudentResidence$mutationScore, rand_StudentResidence$mutationScore)$p.value <= 0.05
  expect_true(p)

  # Testing Effect size

  avmr_effectsize <- directedRandomR::effectsize_accurate(dr_StudentResidence$mutationScore, avmr_StudentResidence$mutationScore)$size
  avmd_effectsize <- directedRandomR::effectsize_accurate(dr_StudentResidence$mutationScore, avmd_StudentResidence$mutationScore)$size
  rand_effectsize <- directedRandomR::effectsize_accurate(dr_StudentResidence$mutationScore, rand_StudentResidence$mutationScore)$size

  expect_equal(avmr_effectsize, "small")

  expect_equal(avmd_effectsize, "large")

  expect_equal(rand_effectsize, "large")


})


test_that("testing_mutatin_score_for_NistWeather_HyperSQL", {
  # Gettting Mutants
  mutants <- directedRandomR::collect_mutanttiming()
  # Filtering mutants and getting precentage kills HyperSQL
  mutants <- mutants %>% filter(type == "NORMAL", dbms == "HyperSQL") %>% select(identifier, dbms, schema, generator, killed) %>% group_by(identifier, dbms, schema, generator) %>% summarise(killed_mutants = sum(killed == "true"), total_mutants = (sum(killed == "true") + sum(killed == "false"))) %>% mutate(mutationScore = round((killed_mutants/total_mutants) * 100, 2))
  # getting mutants per generator

  dr <- mutants %>% filter(generator == "directedRandom")
  avmr <- mutants %>% filter(generator == "avs")
  avmd <- mutants %>% filter(generator == "avsDefaults")
  rand <- mutants %>% filter(generator == "random")


  # Checking StudentResidence schema
  dr_NistWeather <- dr %>% filter(schema == "NistWeather")
  avmr_NistWeather <- avmr %>% filter(schema == "NistWeather")
  avmd_NistWeather <- avmd %>% filter(schema == "NistWeather")
  rand_NistWeather <- rand %>% filter(schema == "NistWeather")

  # Check 30 runs
  expect_equal(nrow(dr_NistWeather), 30)
  expect_equal(nrow(avmr_NistWeather), 30)
  expect_equal(nrow(avmd_NistWeather), 30)
  expect_equal(nrow(rand_NistWeather), 30)

  # check averages

  averages <- round(sum(dr_NistWeather$killed_mutants) / sum(dr_NistWeather$total_mutants) * 100, 2)
  expect_equal(averages, 98.22)

  averages <- round(sum(avmr_NistWeather$killed_mutants) / sum(avmr_NistWeather$total_mutants) * 100, 2)
  expect_equal(averages, 100.00)

  averages <- round(sum(avmd_NistWeather$killed_mutants) / sum(avmd_NistWeather$total_mutants) * 100, 2)
  expect_equal(averages, 93.33)

  averages <- round(sum(rand_NistWeather$killed_mutants) / sum(rand_NistWeather$total_mutants) * 100, 2)
  expect_equal(averages, 50.67)


  # Testing Siginificant results to the table
  p <- wilcox.test(dr_NistWeather$mutationScore, avmr_NistWeather$mutationScore)$p.value <= 0.05
  expect_true(p)

  p <- wilcox.test(dr_NistWeather$mutationScore, avmd_NistWeather$mutationScore)$p.value <= 0.05
  expect_true(p)

  p <- wilcox.test(dr_NistWeather$mutationScore, rand_NistWeather$mutationScore)$p.value <= 0.05
  expect_true(p)

  # Testing Effect Size

  avmr_effectsize <- directedRandomR::effectsize_accurate(dr_NistWeather$mutationScore, avmr_NistWeather$mutationScore)$size
  avmd_effectsize <- directedRandomR::effectsize_accurate(dr_NistWeather$mutationScore, avmd_NistWeather$mutationScore)$size
  rand_effectsize <- directedRandomR::effectsize_accurate(dr_NistWeather$mutationScore, rand_NistWeather$mutationScore)$size

  expect_equal(avmr_effectsize, "medium")

  expect_equal(avmd_effectsize, "large")

  expect_equal(rand_effectsize, "large")

})
