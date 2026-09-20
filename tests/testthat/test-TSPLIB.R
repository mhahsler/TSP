library(TSP)
library(testthat)

test_that("TSPLIB round trips TSP, ATSP, and ETSP objects", {
  set.seed(1234)
  x <- data.frame(x = runif(5), y = runif(5))
  d <- round(dist(x), 3)
  path <- tempfile(fileext = ".tsp")
  on.exit(unlink(path), add = TRUE)

  objects <- list(
    TSP = TSP(d),
    ATSP = ATSP(d),
    ETSP = ETSP(round(x[, 1:2], 3))
  )

  for (object in objects) {
    write_TSPLIB(object, file = path, precision = 6)
    expect_equal(read_TSPLIB(path, precision = 6), object,
      ignore_attr = TRUE)
  }
})

test_that("TSPLIB replaces infinite distances", {
  set.seed(1234)
  d <- round(dist(data.frame(x = runif(5), y = runif(5))), 3)
  d[2] <- Inf
  tsp <- TSP(d)
  path <- tempfile(fileext = ".tsp")
  on.exit(unlink(path), add = TRUE)

  write_TSPLIB(tsp, file = path, precision = 6)
  result <- read_TSPLIB(path, precision = 6)

  expect_equal(result[-2], tsp[-2], ignore_attr = TRUE)
  expect_gt(result[2], range(tsp, finite = TRUE)[2])
})

test_that("TSPLIB reads ATT coordinates", {
  path <- tempfile(fileext = ".tsp")
  on.exit(unlink(path), add = TRUE)
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
  ), con = path)

  expected <- matrix(c(
    0, 2, 4, 5,
    2, 0, 2, 4,
    4, 2, 0, 2,
    5, 4, 2, 0
  ), nrow = 4, byrow = TRUE)
  expected <- TSP(expected, labels = as.character(1:4), method = "ATT")

  expect_equal(read_TSPLIB(path), expected, ignore_attr = TRUE)
})

test_that("TSPLIB reads GEO coordinates with optional indentation", {
  path <- tempfile(fileext = ".tsp")
  on.exit(unlink(path), add = TRUE)

  expected <- matrix(c(
    0, 234, 155,
    234, 0, 186,
    155, 186, 0
  ), nrow = 3, byrow = TRUE)
  expected <- TSP(expected, labels = as.character(1:3), method = "GEO")

  for (indent in c("", " ")) {
    writeLines(c(
      "NAME: GEO_EXAMPLE",
      "TYPE: TSP",
      "DIMENSION: 3",
      "EDGE_WEIGHT_TYPE: GEO",
      "NODE_COORD_SECTION",
      paste0(indent, "1 48.12 16.22"),
      paste0(indent, "2 46.38 14.18"),
      paste0(indent, "3 48.18 14.17"),
      paste0(indent, "EOF")
    ), con = path)

    expect_equal(read_TSPLIB(path), expected, ignore_attr = TRUE)
  }
})
