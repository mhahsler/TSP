library(TSP)
library(testthat)

context("ETSP")

x <- data.frame(x = runif(20), y = runif(20), row.names = LETTERS[1:20])

## create a TSP
etsp <- ETSP(x)
etsp

## use some methods
expect_equal(n_of_cities(etsp), 20L)
expect_equal(labels(etsp), LETTERS[1:20])

## solve
tour <- solve_TSP(etsp)
tour

## compare tour_length
expect_equal(tour_length(etsp), tour_length(as.TSP(etsp)))
expect_equal(tour_length(tour, etsp), tour_length(tour, as.TSP(etsp)))
