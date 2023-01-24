library("testthat")
library("TSP")

data("USCA50")

context("insert_dummy")

tsp <- insert_dummy(USCA50, label = "cut")
#labels(tsp)
expect_equal(n_of_cities(tsp), n_of_cities(USCA50) + 1L)
expect_equal(labels(tsp), c(labels(USCA50), "cut"))

tsp5 <- insert_dummy(USCA50, n = 5, label = "cut")

#labels(tsp5)
expect_equal(n_of_cities(tsp5), n_of_cities(USCA50) + 5L)
expect_equal(labels(tsp5), c(labels(USCA50), rep("cut", 5)))

context("cut_tour")

tour <- solve_TSP(tsp5)
path <- cut_tour(tour, "cut")
expect_equal(sum(sapply(path, length)), n_of_cities(USCA50))

#labels(tour)
#path

## border cases
tour <- TOUR(1:10)
expect_equal(cut_tour(tour, "1"), 2:10)
expect_equal(cut_tour(tour, 1), 2:10)
expect_equal(cut_tour(tour, "10"), 1:9)
expect_equal(cut_tour(tour, 10), 1:9)

path <- cut_tour(tour, 1:3)
expect_equal(length(path), 3L)
expect_equal(sum(sapply(path, length)), length(tour) - 3L)
#path

path <- cut_tour(tour, 1:3, exclude_cut = FALSE)
expect_equal(length(path), 3L)
expect_equal(sum(sapply(path, length)), length(tour))
#path

path <- cut_tour(tour, 8:10)
expect_equal(length(path), 3L)
expect_equal(sum(sapply(path, length)), length(tour) - 3L)
#path

path <- cut_tour(tour, 8:10, exclude_cut = FALSE)
expect_equal(length(path), 3L)
expect_equal(sum(sapply(path, length)), length(tour))
#path

tour <- TOUR(1)
path <- cut_tour(tour, 1)
expect_equal(length(path), 0L)
#path

path <- cut_tour(tour, 1, exclude_cut = FALSE)
expect_equal(length(path), 1L)
#path

context("reformulate")
## reformulate
atsp <- as.ATSP(USCA50)
expect_equal(tour_length(USCA50), tour_length(atsp))

tsp <- reformulate_ATSP_as_TSP(atsp, cheap = 0)
expect_equal(n_of_cities(atsp)*2, n_of_cities(tsp))

## only Concorde guarantees to find the optimal solution
skip_if_not(
  Sys.which("concorde") != "" &&
    Sys.which("linkern") != "",
  message = "skipped test for concorde/linkern. Not installed.")

v <- FALSE

tour_tsp <- solve_TSP(tsp, method = "concorde", verbose = v)
tour_atsp <- filter_ATSP_as_TSP_dummies(tour_tsp, atsp)

expect_equal(length(tour_atsp), n_of_cities(USCA50))
expect_equal(tour_length(tour_tsp), tour_length(tour_atsp))
