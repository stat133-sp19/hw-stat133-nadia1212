context("Checker functions")

test_that("check_prob works as expected", {
  expect_true(check_prob(.5))
  expect_error(check_prob(5))
  expect_error(check_prob("Asdf"))
})

test_that("check_trials works as expected", {
  expect_true(check_trials(7))
  expect_error(check_trials(.4))
  expect_error(check_trials(-8))
})

test_that("check_success works as expected", {
  expect_true(check_success(c(4), 8))
  expect_true(check_success(c(4, 2), 8))
  expect_error(check_success(c(8), 4))
  expect_error(check_success(-8))
})

context("Auxiliary functions")

test_that("aux_mean works as expected", {
  expect_equal(aux_mean(10, .3), 3)

})

test_that("aux_variance works as expected", {
  expect_equal(aux_variance(10, .3), 2.1)

})

test_that("aux_mode works as expected", {
  expect_equal(aux_mode(10, .3), 3)
  expect_equal(aux_mode(10, .5), 5)
  expect_equal(aux_mode(5, .5), c(3, 2))
})


test_that("aux_skewness works as expected", {
  expect_equal(aux_skewness(10, .3), .2760262, tolerance = .01)

})


test_that("aux_kurtosis works as expected", {
  expect_equal(aux_kurtosis(10, .3), -0.1238095, tolerance= .01)

})

context("main functions")

test_that("bin_choose works as expected", {
  expect_equal(bin_choose(n = 5, k = 2), 10)
  expect_equal(bin_choose(5, 0), 1)
  expect_equal(bin_choose(5, 1:3), c(5, 10, 10))
})

test_that("bin_probability works as expected", {
  expect_error(bin_probability(2, 5, 5))
  expect_equal(bin_probability(2, 5, .5), .3125)
  expect_equal(bin_probability(success = 0:2, trials = 5, prob = 0.5), c(0.03125, 0.15625, 0.31250))

})

