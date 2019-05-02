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

context("Summary Measures")

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

context("Binomial functions")

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

test_that("bin_distrubution works as expected", {
  res <- data.frame(c(0, 1, 2, 3, 4, 5), c(0.03125, 0.15625, 0.31250, 0.31250, 0.15625, 0.03125))
  colnames(res) <- c("success", "probability")
  class(res) <- c("bindis", "data.frame")
  expect_error(bin_distrubtion(2, 5, 5))
  expect_equal(bin_distribution(5, .5), res)
  #expect_equal(bin_distribution(success = 0:2, trials = 5, prob = 0.5), c(0.03125, 0.15625, 0.31250))

})

test_that("bin_cumulative works as expected", {
  res <- data.frame(c(0, 1, 2, 3, 4, 5), c(0.03125, 0.15625, 0.31250, 0.31250, 0.15625, 0.03125),
                    c(0.03125, 0.18750, 0.50000, 0.81250, 0.96875, 1.00000))
  colnames(res) <- c("success", "probability", "cumulative")
  class(res) <- c("bincum", "data.frame")
  expect_error(bin_cumulative(2, 5, 5))
  expect_equal(bin_cumulative(5, .5), res)
  #expect_equal(bin_distribution(success = 0:2, trials = 5, prob = 0.5), c(0.03125, 0.15625, 0.31250))

})

