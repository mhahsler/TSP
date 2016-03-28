library("testthat")
library("TSP")

data("USCA50")

context("insert_dummy")

tsp <- insert_dummy(USCA50, label = "cut")
expect_equal(n_of_cities(tsp), n_of_cities(USCA50) + 1L)
expect_equal(labels(tsp), c(labels(USCA50), "cut"))

context("cut_tour")

tour <- solve_TSP(tsp)

## cut tour into path at the dummy city
path <- cut_tour(tour, "cut")
expect_equal(length(path), n_of_cities(USCA50))

context("reformulate")
## reformualte
atsp <- as.ATSP(USCA50)
expect_equal(tour_length(USCA50), tour_length(atsp))

tsp <- reformulate_ATSP_as_TSP(atsp)
expect_equal(n_of_cities(atsp)*2, n_of_cities(tsp))
