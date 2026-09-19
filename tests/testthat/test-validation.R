library(TSP)
library(testthat)

context("input validation")

test_that("TSP and ATSP constructors reject missing distances", {
  x <- matrix(c(0, NA, NA, 0), nrow = 2)

  expect_error(TSP(x), "NAs")
  expect_error(TSP(as.dist(x)), "NAs")
  expect_error(ATSP(x), "NAs")
})

test_that("insertion and nearest-neighbor starts are valid city indices", {
  tsp <- TSP(dist(matrix(seq_len(8), ncol = 2)))
  methods <- c("nearest_insertion", "nn")
  invalid <- list(0, 5, 1.5, NA_real_, numeric(), c(1, 2), "1")

  for (method in methods)
    for (start in invalid)
      expect_error(
        solve_TSP(tsp, method = method, start = start),
        "start must be a single integer between 1 and 4"
      )

  for (method in methods)
    expect_s3_class(
      solve_TSP(tsp, method = method, start = 1),
      "TOUR"
    )
})

test_that("tour lengths reject malformed permutations", {
  coords <- matrix(c(0, 0, 1, 0, 1, 1, 0, 1), ncol = 2, byrow = TRUE)
  problems <- list(TSP(dist(coords)), ATSP(as.matrix(dist(coords))), ETSP(coords))
  invalid <- list(1:3, c(1, 2, 3, 3), c(0, 1, 2, 3),
    c(1, 2, 3, 4.5), c(1, 2, 3, NA_real_), "1:4")

  for (problem in problems) {
    expect_equal(tour_length(problem, c(1, 2, 3, 4)), 4)
    for (order in invalid)
      expect_error(tour_length(problem, order))
  }
})

test_that("one-city problems have zero tour length", {
  coords <- matrix(c(1, 2), nrow = 1)

  expect_equal(tour_length(TSP(dist(coords))), 0)
  expect_equal(tour_length(ATSP(matrix(0, nrow = 1))), 0)
  expect_equal(tour_length(ETSP(coords)), 0)
  expect_error(tour_length(ETSP(coords), integer()))
})
