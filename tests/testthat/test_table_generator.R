### Context: test cases for tables generators
context("table-generator")

test_that("Coverage Table checker", {
  # Get data
  analysis <- directedRandomR::collect_mutationanalysistime()

  dt <- directedRandomR::table_generator_coverage(analysis, rtrn = "data", m = "mean")

  # Check number of rows
  expect_equal(nrow(dt), 15)

  # Check values
  expect_equal(dt[2,2], "99.3")
  expect_equal(dt[12,2], "97.9")
  expect_equal(dt[6,5], "$^{\\ast\\ast\\ast}$\\textbf{97.1}")
})

test_that("Timing Table checker", {
  # Get data
  analysis <- directedRandomR::collect_mutationanalysistime()

  dt <- directedRandomR::table_generator_timing(analysis, rtrn = "data", m = "mean")

  # Check number of rows
  expect_equal(nrow(dt), 15)

  # Check values
  expect_equal(dt[2,2], "8.78")
  expect_equal(dt[12,2], "3.14")
  expect_equal(dt[6,5], "$^{\\ast\\ast\\ast}$\\textbf{24.84}")
})

test_that("Mutation Score Table checker", {
  # Get data
  mutants <- directedRandomR::collect_mutanttiming()

  dt <- directedRandomR::table_generator_mutation_score(mutants, rtrn = "data", m = "mean")

  # Check number of rows
  expect_equal(nrow(dt), 15)

  # Check values
  expect_equal(dt[2,2], "99.5")
  expect_equal(dt[12,2], "87.2")
  expect_equal(dt[6,5], "$^{\\ast\\ast\\ast}$\\textbf{97.9}")
  expect_equal(dt[11,9], "$^{\\ast\\ast\\ast}$\\textit{96.0}")
})
