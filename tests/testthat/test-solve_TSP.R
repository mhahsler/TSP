library(TSP)
library(testthat)

context("solve_TSP")
m <- rbind(
  c(0, 1, 0, 1),
  c(1, 0, 1, Inf),
  c(0, 1, 0, 1),
  c(1, Inf, 1, 0)
)

d <- as.dist(m)
tsp <- TSP(d)
tsp

## from matrix should give the same result
expect_equal(as.numeric(tsp), as.numeric(TSP(as.matrix(d))))


## test error on NA
tsp_na <- tsp
tsp_na[4] <- NA
expect_error(o <- solve_TSP(tsp_na))

## test Inf
methods <- c("nearest_insertion", "cheapest_insertion", "farthest_insertion",
  "arbitrary_insertion", "nn", "repetitive_nn", "two_opt", "random", "identity")

tours <- lapply(methods, FUN = function(m) solve_TSP(tsp, method = m))
names(tours) <- methods
#tours

tl <- sapply(tours, attr, "tour_length")
expect_true(all(tl == 4 | tl == Inf))

## test rep
res <- solve_TSP(tsp, rep=10)
tl <- attr("res", "tour_length")
expect_true(all(tl == 4 | tl == Inf))

## no two_opt
res <- solve_TSP(tsp, two_opt=FALSE)
tl <- attr("res", "tour_length")
expect_true(all(tl == 4 | tl == Inf))

## test special case: two cities
d <- dist(rbind(c(0,0), c(1,1)))
tsp2 <- TSP(d)
tours2 <- lapply(methods, FUN = function(m) solve_TSP(tsp2, method = m))
expect_true(all(sapply(tours2, attr, "tour_length") == as.numeric(d)*2))

## test special case: one city
tsp1 <- TSP(dist(1))
tours1 <- lapply(methods, FUN = function(m) solve_TSP(tsp1, method = m))
expect_true(all(sapply(tours1, attr, "tour_length") == 0))

## test ATSP (just for errors)

#data <- matrix(runif(5^2), ncol = 5, dimnames = list(1:5, 1:5))
data <- structure(c(0.13930352916941, 0.897691324818879, 0.509101516567171,
  0.430898967897519, 0.141799068776891, 0.0334562903735787, 0.902805947931483,
  0.203576791565865, 0.435874363640323, 0.0641707226168364, 0.101683554705232,
  0.631239329231903, 0.555331876967102, 0.0829615572001785, 0.272443652851507,
  0.215095571940765, 0.532841097796336, 0.795302660670131, 0.43256876245141,
  0.582661165855825, 0.250269076088443, 0.164849652675912, 0.638499777996913,
  0.857200765516609, 0.0134391817264259), .Dim = c(5L, 5L), .Dimnames = list(
    c("1", "2", "3", "4", "5"), c("1", "2", "3", "4", "5")))
# best solution is 0.8082826

atsp <- ATSP(data)

methods <- c("nearest_insertion", "cheapest_insertion", "farthest_insertion",
  "arbitrary_insertion", "nn", "repetitive_nn", "two_opt", "random", "identity")

tours <- lapply(methods, FUN = function(m) solve_TSP(atsp, method = m))
names(tours) <- methods
