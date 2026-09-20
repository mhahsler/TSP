library(TSP)
library(testthat)

test_that("simulated annealing moves preserve tour permutations", {
  tour <- 1:8
  tsp <- TSP(dist(matrix(seq_len(16), ncol = 2)))

  set.seed(1)
  expect_setequal(TSP:::tsp_SA_reversal(tour, tsp), tour)
  set.seed(1)
  expect_setequal(TSP:::tsp_SA_swap(tour, tsp), tour)

  set.seed(1)
  expect_setequal(TSP:::tsp_SA_mixed(tour, tsp), tour)
  set.seed(4)
  expect_setequal(TSP:::tsp_SA_mixed(tour, tsp), tour)
})

test_that("simulated annealing is reproducible and accepts an initial tour", {
  tsp <- TSP(dist(matrix(seq_len(16), ncol = 2)))
  initial <- TOUR(seq_len(n_of_cities(tsp)))

  first <- solve_TSP(tsp, method = "sa", tour = initial,
    local_move = TSP:::tsp_SA_swap, temp = 1, tmax = 5, maxit = 50,
    seed = 123)
  second <- solve_TSP(tsp, method = "sa", tour = initial,
    local_move = TSP:::tsp_SA_swap, temp = 1, tmax = 5, maxit = 50,
    seed = 123)

  expect_identical(second, first)
  expect_s3_class(first, "TOUR")
  expect_setequal(as.integer(first), seq_len(n_of_cities(tsp)))
  expect_equal(attr(first, "method"), "sa")
})

test_that("simulated annealing reports progress and handles one city", {
  tsp <- TSP(dist(matrix(seq_len(16), ncol = 2)))
  expect_output(
    solve_TSP(tsp, method = "sa", verbose = TRUE, maxit = 2, seed = 1),
    regexp = "Initial temperature set to"
  )

  one_city <- TSP(dist(1))
  tour <- solve_TSP(one_city, method = "sa", seed = 1)
  expect_identical(as.integer(tour), 1L)
  expect_equal(tour_length(tour), 0)
})
