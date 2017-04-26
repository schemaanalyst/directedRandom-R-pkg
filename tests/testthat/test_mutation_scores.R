library(testthat)
library(directedRandomR)
context("mutation-scores")

test_that("testing_mutatin_score_for_StudentResidence_SQLite", {
  setwd('../../')
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
  p <- wilcox.test(dr_StudentResidence$mutationScore, avmr_StudentResidence$mutationScore, exact = FALSE)$p.value <= 0.05
  expect_true(p)

  p <- wilcox.test(dr_StudentResidence$mutationScore, avmd_StudentResidence$mutationScore, exact = FALSE)$p.value <= 0.05
  expect_true(p)

  p <- wilcox.test(dr_StudentResidence$mutationScore, rand_StudentResidence$mutationScore, exact = FALSE)$p.value <= 0.05
  expect_true(p)

  # Testing Effect size

  avmr_effectsize <- directedRandomR::effectsize_accurate(dr_StudentResidence$mutationScore, avmr_StudentResidence$mutationScore)$size
  avmd_effectsize <- directedRandomR::effectsize_accurate(dr_StudentResidence$mutationScore, avmd_StudentResidence$mutationScore)$size
  rand_effectsize <- directedRandomR::effectsize_accurate(dr_StudentResidence$mutationScore, rand_StudentResidence$mutationScore)$size

  expect_equal(avmr_effectsize, "small")

  expect_equal(avmd_effectsize, "large")

  expect_equal(rand_effectsize, "large")


})


# test_that("testing_mutatin_score_for_NistWeather_HyperSQL", {
#   #setwd('../../')
#   # Gettting Mutants
#   mutants <- directedRandomR::collect_mutanttiming()
#   # Filtering mutants and getting precentage kills HyperSQL
#   mutants <- mutants %>% filter(type == "NORMAL", dbms == "HyperSQL") %>% select(identifier, dbms, schema, generator, killed) %>% group_by(identifier, dbms, schema, generator) %>% summarise(killed_mutants = sum(killed == "true"), total_mutants = (sum(killed == "true") + sum(killed == "false"))) %>% mutate(mutationScore = round((killed_mutants/total_mutants) * 100, 2))
#   # getting mutants per generator
#
#   dr <- mutants %>% filter(generator == "directedRandom")
#   avmr <- mutants %>% filter(generator == "avs")
#   avmd <- mutants %>% filter(generator == "avsDefaults")
#   rand <- mutants %>% filter(generator == "random")
#
#
#   # Checking StudentResidence schema
#   dr_NistWeather <- dr %>% filter(schema == "NistWeather")
#   avmr_NistWeather <- avmr %>% filter(schema == "NistWeather")
#   avmd_NistWeather <- avmd %>% filter(schema == "NistWeather")
#   rand_NistWeather <- rand %>% filter(schema == "NistWeather")
#
#   # Check 30 runs
#   expect_equal(nrow(dr_NistWeather), 30)
#   expect_equal(nrow(avmr_NistWeather), 30)
#   expect_equal(nrow(avmd_NistWeather), 30)
#   expect_equal(nrow(rand_NistWeather), 30)
#
#   # check averages
#
#   averages <- round(sum(dr_NistWeather$killed_mutants) / sum(dr_NistWeather$total_mutants) * 100, 2)
#   expect_equal(averages, 98.22)
#
#   averages <- round(sum(avmr_NistWeather$killed_mutants) / sum(avmr_NistWeather$total_mutants) * 100, 2)
#   expect_equal(averages, 100.00)
#
#   averages <- round(sum(avmd_NistWeather$killed_mutants) / sum(avmd_NistWeather$total_mutants) * 100, 2)
#   expect_equal(averages, 93.33)
#
#   averages <- round(sum(rand_NistWeather$killed_mutants) / sum(rand_NistWeather$total_mutants) * 100, 2)
#   expect_equal(averages, 50.67)
#
#
#   # Testing Siginificant results to the table
#   p <- wilcox.test(dr_NistWeather$mutationScore, avmr_NistWeather$mutationScore)$p.value <= 0.05
#   expect_true(p)
#
#   p <- wilcox.test(dr_NistWeather$mutationScore, avmd_NistWeather$mutationScore)$p.value <= 0.05
#   expect_true(p)
#
#   p <- wilcox.test(dr_NistWeather$mutationScore, rand_NistWeather$mutationScore)$p.value <= 0.05
#   expect_true(p)
#
#   # Testing Effect Size
#
#   avmr_effectsize <- directedRandomR::effectsize_accurate(dr_NistWeather$mutationScore, avmr_NistWeather$mutationScore)$size
#   avmd_effectsize <- directedRandomR::effectsize_accurate(dr_NistWeather$mutationScore, avmd_NistWeather$mutationScore)$size
#   rand_effectsize <- directedRandomR::effectsize_accurate(dr_NistWeather$mutationScore, rand_NistWeather$mutationScore)$size
#
#   expect_equal(avmr_effectsize, "medium")
#
#   expect_equal(avmd_effectsize, "large")
#
#   expect_equal(rand_effectsize, "large")
#
# })
