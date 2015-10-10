library(TSP)
library(testthat)

context("TSPLIB")

set.seed(1234)
x <- data.frame(x=runif(5), y=runif(5))

## create TSP, ATSP and ETSP (2D)
d <- round(dist(x), 3)

## TSP
tsp <- TSP(d)
write_TSPLIB(tsp, file="example.tsp", precision = 6)
#file.show("example.tsp")
r <- read_TSPLIB("example.tsp", precision = 6)  
expect_equivalent(tsp, r)

## ATSP
atsp <- ATSP(d)
write_TSPLIB(atsp, file="example.tsp", precision = 6)
#file.show("example.tsp")
r <- read_TSPLIB("example.tsp", precision = 6)  
expect_equivalent(atsp, r)

## ETSP (2D)
etsp <- ETSP(round(x[,1:2], 3))  
write_TSPLIB(etsp, file="example.tsp", precision = 6)
#file.show("example.tsp")
r <- read_TSPLIB("example.tsp", precision = 6)  
expect_equivalent(etsp, r)

## Infinity
d[2] <- Inf
tsp <- TSP(d)
write_TSPLIB(tsp, file="example.tsp", precision = 6)
r <- read_TSPLIB("example.tsp", precision = 6)  
expect_equivalent(tsp[-2], r[-2])
expect_more_than(r[2], range(tsp, finite = TRUE)[2])

## clean up
unlink("example.tsp")