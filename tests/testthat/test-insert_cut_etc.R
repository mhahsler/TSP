library(TSP)
library(testthat)

test_that("dummy cities are inserted with the requested labels", {
  data("USCA50")

  tsp <- insert_dummy(USCA50, label = "cut")
  expect_equal(n_of_cities(tsp), n_of_cities(USCA50) + 1L)
  expect_equal(labels(tsp), c(labels(USCA50), "cut"))

  tsp5 <- insert_dummy(USCA50, n = 5, label = "cut")
  expect_equal(n_of_cities(tsp5), n_of_cities(USCA50) + 5L)
  expect_equal(labels(tsp5), c(labels(USCA50), rep("cut", 5)))
})

test_that("cut_tour handles ordinary and boundary cuts", {
  data("USCA50")
  tsp <- insert_dummy(USCA50, n = 5, label = "cut")
  tour <- solve_TSP(tsp)
  path <- cut_tour(tour, "cut")
  expect_equal(sum(lengths(path)), n_of_cities(USCA50))

  tour <- TOUR(1:10)
  expect_equal(cut_tour(tour, "1"), 2:10)
  expect_equal(cut_tour(tour, 1), 2:10)
  expect_equal(cut_tour(tour, "10"), 1:9)
  expect_equal(cut_tour(tour, 10), 1:9)

  path <- cut_tour(tour, 1:3)
  expect_length(path, 3L)
  expect_equal(sum(lengths(path)), length(tour) - 3L)

  path <- cut_tour(tour, 1:3, exclude_cut = FALSE)
  expect_length(path, 3L)
  expect_equal(sum(lengths(path)), length(tour))

  path <- cut_tour(tour, 8:10)
  expect_length(path, 3L)
  expect_equal(sum(lengths(path)), length(tour) - 3L)

  path <- cut_tour(tour, 8:10, exclude_cut = FALSE)
  expect_length(path, 3L)
  expect_equal(sum(lengths(path)), length(tour))

  one_city <- TOUR(1)
  expect_length(cut_tour(one_city, 1), 0L)
  expect_length(cut_tour(one_city, 1, exclude_cut = FALSE), 1L)
})

test_that("ATSP reformulation preserves tour lengths", {
  data("USCA50")
  atsp <- as.ATSP(USCA50)
  expect_equal(tour_length(USCA50), tour_length(atsp))

  tsp <- reformulate_ATSP_as_TSP(atsp, cheap = 0)
  expect_equal(n_of_cities(tsp), n_of_cities(atsp) * 2)
})

test_that("Concorde solves reformulated ATSP instances", {
  skip_if(Sys.which("concorde") == "", "Concorde is not installed")
  data("USCA50")
  atsp <- as.ATSP(USCA50)
  tsp <- reformulate_ATSP_as_TSP(atsp, cheap = 0)

  tour_tsp <- solve_TSP(tsp, method = "concorde", verbose = FALSE)
  tour_atsp <- filter_ATSP_as_TSP_dummies(tour_tsp, atsp)

  expect_length(tour_atsp, n_of_cities(USCA50))
  expect_equal(tour_length(tour_tsp), tour_length(tour_atsp))
})
