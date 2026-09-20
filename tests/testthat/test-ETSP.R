library(TSP)
library(testthat)

test_that("ETSP objects expose cities and labels", {
  x <- data.frame(x = runif(20), y = runif(20), row.names = LETTERS[1:20])
  etsp <- ETSP(x)

  expect_equal(n_of_cities(etsp), 20L)
  expect_equal(labels(etsp), LETTERS[1:20])
})

test_that("ETSP tour lengths agree with converted TSP objects", {
  x <- data.frame(x = runif(20), y = runif(20), row.names = LETTERS[1:20])
  etsp <- ETSP(x)
  tour <- solve_TSP(etsp)

  expect_equal(tour_length(etsp), tour_length(as.TSP(etsp)))
  expect_equal(tour_length(tour, etsp), tour_length(tour, as.TSP(etsp)))
})
