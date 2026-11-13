library(TSP)
library(testthat)

is_perm <- function(tour, n) setequal(as.integer(tour), seq_len(n)) && length(tour) == n

test_that("or_opt and three_opt return valid tours that never get longer", {
  set.seed(1234)
  tsp <- TSP(dist(matrix(runif(60), ncol = 2)))
  start <- solve_TSP(tsp, method = "random")

  for (method in c("or_opt", "three_opt", "two_opt")) {
    tour <- solve_TSP(tsp, method = method, tour = start)
    expect_true(is_perm(tour, 30))
    expect_lte(tour_length(tour), tour_length(start) + 1e-8)
    expect_equal(attr(tour, "tour_length"), tour_length(tsp, as.integer(tour)))
  }
})

test_that("three_opt is at least as good as its 2-opt local optimum", {
  set.seed(42)
  tsp <- TSP(dist(matrix(runif(50), ncol = 2)))
  t2 <- solve_TSP(tsp, method = "two_opt")
  t3 <- solve_TSP(tsp, method = "three_opt", tour = t2)
  expect_lte(tour_length(t3), tour_length(t2) + 1e-8)
})

test_that("or_opt moves a misplaced city", {
  ## cities on a line; city 3 is far out of place
  x <- cbind(1:8, 0)
  tsp <- TSP(dist(x))
  tour <- solve_TSP(tsp, method = "or_opt", tour = c(1, 2, 8, 3, 4, 5, 6, 7))
  expect_equal(tour_length(tour), 14)
  tour <- solve_TSP(tsp, method = "or_opt", tour = c(1, 2, 7, 8, 3, 4, 5, 6),
    max_segment = 2)
  expect_equal(tour_length(tour), 14)
})

test_that("or_opt and three_opt handle asymmetric problems", {
  set.seed(7)
  atsp <- ATSP(matrix(runif(100), 10, 10))
  start <- solve_TSP(atsp, method = "random")
  for (method in c("or_opt", "three_opt")) {
    tour <- solve_TSP(atsp, method = method, tour = start)
    expect_true(is_perm(tour, 10))
    expect_lte(tour_length(tour), tour_length(start) + 1e-8)
    expect_equal(attr(tour, "tour_length"),
      tour_length(atsp, as.integer(tour)))
  }
})

test_that("or_opt and three_opt work on ETSP and tiny problems", {
  set.seed(1)
  etsp <- ETSP(data.frame(x = runif(15), y = runif(15)))
  for (method in c("or_opt", "three_opt"))
    expect_true(is_perm(solve_TSP(etsp, method = method), 15))

  for (n in 1:4) {
    tsp <- TSP(dist(matrix(runif(2 * n), ncol = 2)))
    for (method in c("or_opt", "three_opt"))
      expect_true(is_perm(solve_TSP(tsp, method = method), n))
  }
})

test_that("or_opt and three_opt validate tours and parameters", {
  tsp <- TSP(dist(matrix(runif(20), ncol = 2)))
  expect_error(solve_TSP(tsp, method = "or_opt", control = list(max_segment = 0)))
  expect_error(solve_TSP(tsp, method = "or_opt", control = list(max_segment = 1.5)))
  expect_error(solve_TSP(tsp, method = "three_opt", control = list(tour = c(1, 2, 3))))
  expect_error(solve_TSP(tsp, method = "or_opt", control = list(tour = rep(11L, 10))))
})

test_that("opt refines constructed tours and two_opt is deprecated", {
  set.seed(10)
  tsp <- TSP(dist(matrix(runif(80), ncol = 2)))

  base <- solve_TSP(tsp, method = "nn")
  lens <- c(none = tour_length(base))
  for (opt in c("two", "or", "three", "two+or")) {
    tour <- solve_TSP(tsp, method = "nn", opt = opt)
    expect_true(is_perm(tour, 40))
    expect_match(attr(tour, "method"),
      c(two = "nn+two_opt", or = "nn+or_opt", three = "nn+three_opt",
        "two+or" = "nn+two_opt+or_opt")[[opt]], fixed = TRUE)
    expect_lte(tour_length(tour), lens[["none"]] + 1e-8)
  }

  ## default is arbitrary insertion + 2-opt
  expect_match(attr(solve_TSP(tsp), "method"), "arbitrary_insertion+two_opt",
    fixed = TRUE)

  ## deprecated two_opt gives a message and sets opt = "two"
  set.seed(1)
  expect_message(old <- solve_TSP(tsp, method = "nn", two_opt = TRUE),
    "deprecated")
  expect_match(attr(old, "method"), "nn+two_opt", fixed = TRUE)
  set.seed(1)
  new <- solve_TSP(tsp, method = "nn", opt = "two")
  expect_equal(as.integer(old), as.integer(new))
  expect_message(plain <- solve_TSP(tsp, method = "nn", two_opt = FALSE),
    "deprecated")
  expect_equal(attr(plain, "method"), "nn")

  expect_error(solve_TSP(tsp, method = "nn", opt = "tree"), "opt must be one of")
})

test_that("opt does not warn about construction parameters", {
  tsp <- TSP(dist(matrix(runif(40), ncol = 2)))
  expect_no_warning(solve_TSP(tsp, method = "nn", start = 1, opt = "two+or"))
  expect_no_warning(solve_TSP(tsp, method = "nn", opt = "or", max_segment = 2))
})
