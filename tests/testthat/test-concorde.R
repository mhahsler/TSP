library(TSP)
library(testthat)


context("solve_TSP (Concorde and Linkern)")

m <- rbind(
  c(0, 1, 0, 1),
  c(1, 0, 1, Inf),
  c(0, 1, 0, 1),
  c(1, Inf, 1, 0)
)

d <- as.dist(m)
tsp <- TSP(d)
tsp

skip_if_not(
  Sys.which("concorde") != "" &&
    Sys.which("linkern") != "",
  message = "skipped test for concorde/linkern. Not installed.")

v <- FALSE
o <- solve_TSP(tsp, method="concorde", verbose = v)
expect_equivalent(tour_length(tsp, o), 4)

# large numbers should be scaled right.
expect_warning(o_large <- solve_TSP(tsp*2^15, method="concorde", verbose = v), regex = "Converting the provided distances to integers")
expect_equivalent(o, o_large)

expect_warning(o_large <- solve_TSP(tsp*10^10, method="concorde", verbose = v), regex = "Converting the provided distances to integers")
expect_equivalent(o, o_large)

# expect warning for rounding
expect_warning(o_large <- solve_TSP(tsp*2^15+0.1, method="concorde", verbose = v), regex = "Converting the provided distances to integers")
expect_equivalent(o, o_large)

# expect a warning for rounding
expect_warning(o_round <- solve_TSP(tsp/0.3, method="concorde", verbose = v), regex = "Converting the provided distances to integers")
expect_equivalent(o, o_round)

o <- solve_TSP(tsp, method="linkern", verbose = v)
expect_equivalent(tour_length(tsp, o), 4)

# test ATSP
#data <- matrix(runif(5^2), ncol = 5, dimnames = list(1:5, 1:5))
data <- structure(c(0.13930352916941, 0.897691324818879, 0.509101516567171,
  0.430898967897519, 0.141799068776891, 0.0334562903735787, 0.902805947931483,
  0.203576791565865, 0.435874363640323, 0.0641707226168364, 0.101683554705232,
  0.631239329231903, 0.555331876967102, 0.0829615572001785, 0.272443652851507,
  0.215095571940765, 0.532841097796336, 0.795302660670131, 0.43256876245141,
  0.582661165855825, 0.250269076088443, 0.164849652675912, 0.638499777996913,
  0.857200765516609, 0.0134391817264259), .Dim = c(5L, 5L), .Dimnames = list(
    c("1", "2", "3", "4", "5"), c("1", "2", "3", "4", "5")))

atsp <- ATSP(data)

## Concorde (gives conversation warning for reformulation of ATSP to TSP)
expect_warning(o1 <- solve_TSP(atsp, method = "concorde", verbose = v), regex = "Solver cannot solve the ATSP directly")
o2 <- solve_TSP(atsp, method = "concorde", as_TSP = TRUE, verbose = v)
expect_equal(length(o1), 5L)
expect_equal(length(o2), 5L)

# Concorde should find the optimal solution of 0.8082826
expect_equal(round(tour_length(o1), 7), 0.8082826)
expect_equal(round(tour_length(o2), 7), 0.8082826)

## Linkern
## warning for reformulate as TSP automatically
expect_warning(o1 <- solve_TSP(atsp, method = "linkern", verbose = v), regex = "Solver cannot solve the ATSP directly")
o2 <- solve_TSP(atsp, method = "linkern", as_TSP = TRUE, verbose = v)
expect_equal(length(o1), 5L)
expect_equal(length(o2), 5L)

# Linkern should find the optimal solution of 0.8082826
expect_equal(round(tour_length(o1), 7), 0.8082826)
expect_equal(round(tour_length(o2), 7), 0.8082826)

