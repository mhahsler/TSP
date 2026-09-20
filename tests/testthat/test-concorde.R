library(TSP)
library(testthat)

test_that("Concorde and Linkern solve symmetric and asymmetric problems", {
  skip_if(any(Sys.which(c("concorde", "linkern")) == ""),
    "Concorde and linkern are not installed")

  m <- rbind(
    c(0, 1, 0, 1),
    c(1, 0, 1, Inf),
    c(0, 1, 0, 1),
    c(1, Inf, 1, 0)
  )
  tsp <- TSP(as.dist(m))
  verbose <- FALSE

  concorde_tour <- solve_TSP(tsp, method = "concorde", verbose = verbose)
  expect_equal(tour_length(tsp, concorde_tour), 4)

  expect_warning(
    scaled_tour <- solve_TSP(tsp * 2^15, method = "concorde",
      verbose = verbose),
    regexp = "Converting the provided distances to integers"
  )
  expect_equal(scaled_tour, concorde_tour, ignore_attr = TRUE)

  expect_warning(
    scaled_tour <- solve_TSP(tsp * 10^10, method = "concorde",
      verbose = verbose),
    regexp = "Converting the provided distances to integers"
  )
  expect_equal(scaled_tour, concorde_tour, ignore_attr = TRUE)

  expect_warning(
    scaled_tour <- solve_TSP(tsp * 2^15 + 0.1, method = "concorde",
      verbose = verbose),
    regexp = "Converting the provided distances to integers"
  )
  expect_equal(scaled_tour, concorde_tour, ignore_attr = TRUE)

  expect_warning(
    rounded_tour <- solve_TSP(tsp / 0.3, method = "concorde",
      verbose = verbose),
    regexp = "Converting the provided distances to integers"
  )
  expect_equal(rounded_tour, concorde_tour, ignore_attr = TRUE)

  linkern_tour <- solve_TSP(tsp, method = "linkern", verbose = verbose)
  expect_equal(tour_length(tsp, linkern_tour), 4)

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

  expect_warning(
    concorde_atsp <- solve_TSP(atsp, method = "concorde",
      verbose = verbose),
    regexp = "Solver cannot solve the ATSP directly"
  )
  concorde_reformulated <- solve_TSP(atsp, method = "concorde",
    as_TSP = TRUE, verbose = verbose)
  expect_length(concorde_atsp, 5L)
  expect_length(concorde_reformulated, 5L)
  expect_equal(round(tour_length(concorde_atsp), 7), 0.8082826)
  expect_equal(round(tour_length(concorde_reformulated), 7), 0.8082826)

  expect_warning(
    linkern_atsp <- solve_TSP(atsp, method = "linkern", verbose = verbose),
    regexp = "Solver cannot solve the ATSP directly"
  )
  linkern_reformulated <- solve_TSP(atsp, method = "linkern",
    as_TSP = TRUE, verbose = verbose)
  expect_length(linkern_atsp, 5L)
  expect_length(linkern_reformulated, 5L)
  expect_equal(round(tour_length(linkern_atsp), 7), 0.8082826)
  expect_equal(round(tour_length(linkern_reformulated), 7), 0.8082826)
})
