library(TSP)
library(testthat)

context("TSPLIB")

set.seed(1234)
x <- data.frame(x=runif(5), y=runif(5))

## create TSP, ATSP and ETSP (2D)
d <- round(dist(x), 3)

## TSP
tsp <- TSP(d)
tsp

write_TSPLIB(tsp, file="example.tsp", precision = 6)
#file.show("example.tsp")
r <- read_TSPLIB("example.tsp", precision = 6)
expect_equivalent(tsp, r)

## ATSP
atsp <- ATSP(d)
atsp

write_TSPLIB(atsp, file="example.tsp", precision = 6)
#file.show("example.tsp")
r <- read_TSPLIB("example.tsp", precision = 6)
expect_equivalent(atsp, r)

## ETSP (2D)
etsp <- ETSP(round(x[,1:2], 3))
etsp

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
expect_gt(r[2], range(tsp, finite = TRUE)[2])

## ATT
writeLines(c(
  "NAME: ATT_EXAMPLE",
  "TYPE: TSP",
  "DIMENSION: 4",
  "EDGE_WEIGHT_TYPE: ATT",
  "NODE_COORD_SECTION",
  "1 0 0",
  "2 3 4",
  "3 6 8",
  "4 9 12",
  "EOF"
), con = "att-example.tsp")

r <- read_TSPLIB("att-example.tsp")
expected <- matrix(c(
  0, 2, 4, 5,
  2, 0, 2, 4,
  4, 2, 0, 2,
  5, 4, 2, 0
), nrow = 4, byrow = TRUE)
expected <- TSP(expected, labels = as.character(1:4), method = "ATT")
expect_equivalent(r, expected)

## GEO
writeLines(c(
  "NAME: GEO_EXAMPLE",
  "TYPE: TSP",
  "DIMENSION: 3",
  "EDGE_WEIGHT_TYPE: GEO",
  "NODE_COORD_SECTION",
  "1 48.12 16.22",
  "2 46.38 14.18",
  "3 48.18 14.17",
  "EOF"
), con = "geo-example.tsp")

r <- read_TSPLIB("geo-example.tsp")
expected <- matrix(c(
  0, 234, 155,
  234, 0, 186,
  155, 186, 0
), nrow = 3, byrow = TRUE)
expected <- TSP(expected, labels = as.character(1:3), method = "GEO")
expect_equivalent(r, expected)

## GEO with indented coordinates
writeLines(c(
  "NAME: GEO_INDENTED",
  "TYPE: TSP",
  "DIMENSION: 3",
  "EDGE_WEIGHT_TYPE: GEO",
  "NODE_COORD_SECTION",
  " 1 48.12 16.22",
  " 2 46.38 14.18",
  " 3 48.18 14.17",
  " EOF"
), con = "geo-example.tsp")

r <- read_TSPLIB("geo-example.tsp")
expected <- TSP(expected, labels = as.character(1:3), method = "GEO")
expect_equivalent(r, expected)

## clean up
unlink("example.tsp")
unlink("att-example.tsp")
unlink("geo-example.tsp")
