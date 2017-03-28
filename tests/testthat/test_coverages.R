library(testthat)
library(directedRandomR)
context("coverages")

test_that("testing_coverage_table", {
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

  # check coverage median

  expect_equal(round(median(dr_StudentResidence$coverage), 2), 100.00)

  expect_equal(round(median(avmr_StudentResidence$coverage), 2), 100.00)

  expect_equal(round(median(avmd_StudentResidence$coverage), 2), 100.00)

  expect_equal(round(median(rand_StudentResidence$coverage), 2), 60.00)


  # Testing Siginificant results to the table
  p <- wilcox.test(dr_StudentResidence$coverage, avmr_StudentResidence$coverage)$p.value <= 0.05
  expect_false(isTRUE(p))

  p <- wilcox.test(dr_StudentResidence$coverage, avmd_StudentResidence$coverage)$p.value <= 0.05
  expect_false(isTRUE(p))

  p <- wilcox.test(dr_StudentResidence$coverage, rand_StudentResidence$coverage)$p.value <= 0.05
  expect_true(p)

  # Testing Effect size

  avmr_effectsize <- directedRandomR::effectsize_accurate(dr_StudentResidence$coverage, avmr_StudentResidence$coverage)$size
  avmd_effectsize <- directedRandomR::effectsize_accurate(dr_StudentResidence$coverage, avmd_StudentResidence$coverage)$size
  rand_effectsize <- directedRandomR::effectsize_accurate(dr_StudentResidence$coverage, rand_StudentResidence$coverage)$size

  expect_equal(avmr_effectsize, "none")

  expect_equal(avmd_effectsize, "none")

  expect_equal(rand_effectsize, "large")


})
