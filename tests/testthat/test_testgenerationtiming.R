library(testthat)
library(directedRandomR)
context("testgenerationtime")

test_that("testing_test_generation_timing_table", {
  setwd('../../')
  # Gettting Mutants
  analysis <- directedRandomR::collect_mutationanalysistime()
  # getting mutants per generator
  analysis <- analysis %>% filter(dbms == "HyperSQL")
  dr <- analysis %>% filter(datagenerator == "directedRandom")
  avmr <- analysis %>% filter(datagenerator == "avs")
  avmd <- analysis %>% filter(datagenerator == "avsDefaults")
  rand <- analysis %>% filter(datagenerator == "random")


  # Checking StudentResidence schema
  dr_StudentResidence <- dr %>% filter(casestudy == "StudentResidence")
  avmr_StudentResidence <- avmr %>% filter(casestudy == "StudentResidence")
  avmd_StudentResidence <- avmd %>% filter(casestudy == "StudentResidence")
  rand_StudentResidence <- rand %>% filter(casestudy == "StudentResidence")

  # Check 30 runs
  expect_equal(nrow(dr_StudentResidence), 30)
  expect_equal(nrow(avmr_StudentResidence), 30)
  expect_equal(nrow(avmd_StudentResidence), 30)
  expect_equal(nrow(rand_StudentResidence), 30)

  # check testgenerationtime median

  expect_equal(round(median(dr_StudentResidence$testgenerationtime) / 1000, 2), 0.57)

  expect_equal(round(median(avmr_StudentResidence$testgenerationtime) / 1000, 2), 0.96)

  expect_equal(round(median(avmd_StudentResidence$testgenerationtime) / 1000, 2), 0.78)

  expect_equal(round(median(rand_StudentResidence$testgenerationtime) / 1000, 2), 21.01)


  # Transformation of test generation timing

  dr_StudentResidence <- directedRandomR::transform_execution_times_for_threshold(dr_StudentResidence, 1000)
  avmr_StudentResidence <- directedRandomR::transform_execution_times_for_threshold(avmr_StudentResidence, 1000)
  avmd_StudentResidence <- directedRandomR::transform_execution_times_for_threshold(avmd_StudentResidence, 1000)
  rand_StudentResidence <- directedRandomR::transform_execution_times_for_threshold(rand_StudentResidence, 1000)

  # Testing Siginificant results to the table
  p <- wilcox.test(dr_StudentResidence$testgenerationtime, avmr_StudentResidence$testgenerationtime)$p.value <= 0.05
  # Because of ties
  expect_true(p)

  p <- wilcox.test(dr_StudentResidence$testgenerationtime, avmd_StudentResidence$testgenerationtime)$p.value <= 0.05
  # Because of ties
  expect_false(isTRUE(p))

  p <- wilcox.test(dr_StudentResidence$testgenerationtime, rand_StudentResidence$testgenerationtime)$p.value <= 0.05
  expect_true(p)

  # Testing Effect size

  avmr_effectsize <- directedRandomR::effectsize_accurate(dr_StudentResidence$testgenerationtime, avmr_StudentResidence$testgenerationtime)$size
  avmd_effectsize <- directedRandomR::effectsize_accurate(dr_StudentResidence$testgenerationtime, avmd_StudentResidence$testgenerationtime)$size
  rand_effectsize <- directedRandomR::effectsize_accurate(dr_StudentResidence$testgenerationtime, rand_StudentResidence$testgenerationtime)$size

  expect_equal(avmr_effectsize, "medium")

  expect_equal(avmd_effectsize, "none")

  expect_equal(rand_effectsize, "large")


})
