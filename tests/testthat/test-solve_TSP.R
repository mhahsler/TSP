library(TSP)
library(testthat)

solver_methods <- function() {
  c(
    "nearest_insertion", "cheapest_insertion", "farthest_insertion",
    "arbitrary_insertion", "nn", "repetitive_nn", "two_opt", "random",
    "identity", "sa"
  )
}

solve_test_method <- function(x, method) {
  if (method == "sa")
    solve_TSP(x, method = method, maxit = 25, tmax = 5, temp = 1, seed = 42)
  else
    solve_TSP(x, method = method)
}

infinite_tsp <- function() {
  m <- rbind(
    c(0, 1, 0, 1),
    c(1, 0, 1, Inf),
    c(0, 1, 0, 1),
    c(1, Inf, 1, 0)
  )
  TSP(as.dist(m))
}

test_that("TSP solvers handle matrix conversion, missing, and infinite values", {
  tsp <- infinite_tsp()
  expect_equal(as.numeric(tsp), as.numeric(TSP(as.matrix(tsp))))

  tsp_na <- tsp
  tsp_na[4] <- NA
  expect_error(solve_TSP(tsp_na), "NAs not allowed", fixed = TRUE)

  tours <- lapply(solver_methods(), function(method) {
    solve_test_method(tsp, method)
  })
  lengths <- vapply(tours, attr, numeric(1), "tour_length")
  expect_true(all(lengths == 4 | lengths == Inf))
})

test_that("repetitions and two-opt controls produce valid tours", {
  tsp <- infinite_tsp()

  repeated <- solve_TSP(tsp, rep = 10)
  expect_true(attr(repeated, "tour_length") %in% c(4, Inf))

  unrefined <- solve_TSP(tsp, two_opt = FALSE)
  expect_true(attr(unrefined, "tour_length") %in% c(4, Inf))
})

test_that("seeded repetitions are reproducible sequentially", {
  set.seed(123)
  random_tsp <- TSP(dist(matrix(runif(40), ncol = 2)))
  foreach::registerDoSEQ()

  first <- solve_TSP(random_tsp, method = "random", rep = 20, seed = 42)
  second <- solve_TSP(random_tsp, method = "random", rep = 20, seed = 42)
  expect_identical(second, first)
})

test_that("seeded repetitions are reproducible across foreach backends", {
  skip_if_not_installed("doParallel")
  set.seed(123)
  random_tsp <- TSP(dist(matrix(runif(40), ncol = 2)))
  tied <- matrix(1, nrow = 8, ncol = 8)
  diag(tied) <- 0
  tied_tsp <- TSP(as.dist(tied))

  foreach::registerDoSEQ()
  sequential <- list(
    random = solve_TSP(random_tsp, method = "random", rep = 20, seed = 42),
    repetitive_nn = solve_TSP(tied_tsp, method = "repetitive_nn", seed = 42),
    sa = solve_TSP(random_tsp, method = "sa", rep = 4, maxit = 20,
      tmax = 5, temp = 1, seed = 42)
  )

  cluster <- parallel::makePSOCKcluster(2)
  doParallel::registerDoParallel(cluster)
  parallel_results <- tryCatch(
    list(
      random = solve_TSP(random_tsp, method = "random", rep = 20, seed = 42),
      repetitive_nn = solve_TSP(tied_tsp, method = "repetitive_nn", seed = 42),
      sa = solve_TSP(random_tsp, method = "sa", rep = 4, maxit = 20,
        tmax = 5, temp = 1, seed = 42)
    ),
    finally = {
      parallel::stopCluster(cluster)
      foreach::registerDoSEQ()
    }
  )

  expect_identical(parallel_results, sequential)
})

test_that("solvers handle one- and two-city problems", {
  methods <- solver_methods()

  distance <- dist(rbind(c(0, 0), c(1, 1)))
  tsp2 <- TSP(distance)
  tours2 <- lapply(methods, function(method) {
    solve_test_method(tsp2, method)
  })
  expect_true(all(vapply(tours2, attr, numeric(1), "tour_length") ==
    as.numeric(distance) * 2))

  tsp1 <- TSP(dist(1))
  tours1 <- lapply(methods, function(method) {
    solve_test_method(tsp1, method)
  })
  expect_true(all(vapply(tours1, attr, numeric(1), "tour_length") == 0))
})

test_that("all internal solvers handle ATSP objects", {
  distances <- structure(
    c(
      0.13930352916941, 0.897691324818879, 0.509101516567171,
      0.430898967897519, 0.141799068776891, 0.0334562903735787,
      0.902805947931483, 0.203576791565865, 0.435874363640323,
      0.0641707226168364, 0.101683554705232, 0.631239329231903,
      0.555331876967102, 0.0829615572001785, 0.272443652851507,
      0.215095571940765, 0.532841097796336, 0.795302660670131,
      0.43256876245141, 0.582661165855825, 0.250269076088443,
      0.164849652675912, 0.638499777996913, 0.857200765516609,
      0.0134391817264259
    ),
    dim = c(5L, 5L),
    dimnames = list(as.character(1:5), as.character(1:5))
  )
  atsp <- ATSP(distances)

  for (method in solver_methods())
    expect_s3_class(solve_test_method(atsp, method), "TOUR")
})

test_that("solvers handle negative and infinite distances", {
  distances <- rbind(
    c(0, -Inf, 2, 4),
    c(-Inf, 0, -1, 3),
    c(2, -1, 0, Inf),
    c(4, 3, Inf, 0)
  )
  tsp <- TSP(distances)
  replaced <- TSP:::.replaceInf(tsp)

  expect_true(all(is.finite(replaced)))
  expect_lt(replaced[1], min(tsp[is.finite(tsp)]))
  expect_gt(replaced[6], max(tsp[is.finite(tsp)]))

  for (method in solver_methods()) {
    tour <- solve_test_method(tsp, method)
    expect_s3_class(tour, "TOUR")
    expect_setequal(as.integer(tour), seq_len(n_of_cities(tsp)))
  }
})
