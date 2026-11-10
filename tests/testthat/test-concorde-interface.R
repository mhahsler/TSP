library(TSP)
library(testthat)

make_fake_solver <- function(output, status = 0L) {
  script <- tempfile("fake-solver-")
  output_command <- if (is.null(output))
    ":"
  else
    "printf '%b\\n' \"$FAKE_SOLVER_OUTPUT\" > \"$out\""
  writeLines(c(
    "#!/bin/sh",
    "out=",
    "while [ \"$#\" -gt 0 ]; do",
    "  if [ \"$1\" = '-o' ]; then",
    "    shift",
    "    out=$1",
    "  fi",
    "  shift",
    "done",
    output_command,
    "printf 'auxiliary file\\n' > auxiliary.tmp",
    paste("exit", status)
  ), script)
  Sys.chmod(script, mode = "0755")
  if (is.null(output))
    Sys.unsetenv("FAKE_SOLVER_OUTPUT")
  else
    Sys.setenv(FAKE_SOLVER_OUTPUT = output)
  script
}

solver_tempdirs <- function(solver) {
  Sys.glob(file.path(tempdir(), paste0("TSP-", solver, "-*")))
}

test_that("Concorde checks exit status and always cleans temporary files", {
  skip_on_os("windows")
  on.exit(Sys.unsetenv("FAKE_SOLVER_OUTPUT"), add = TRUE)
  tsp <- TSP(dist(matrix(seq_len(8), ncol = 2)))
  before <- solver_tempdirs("concorde")

  executable <- make_fake_solver("4 0 1 2 3", status = 23L)
  on.exit(unlink(executable), add = TRUE)
  expect_error(
    suppressWarnings(TSP:::tsp_concorde(tsp,
      control = list(exe = executable, verbose = FALSE))),
    "Concorde exited with status 23",
    fixed = TRUE
  )
  expect_setequal(solver_tempdirs("concorde"), before)

  executable <- make_fake_solver(NULL)
  on.exit(unlink(executable), add = TRUE)
  expect_error(
    suppressWarnings(TSP:::tsp_concorde(tsp,
      control = list(exe = executable, verbose = FALSE))),
    "Concorde has not produced a result file",
    fixed = TRUE
  )
  expect_setequal(solver_tempdirs("concorde"), before)

  ## Some Concorde builds use exit(-1) even after a successful solve.
  executable <- make_fake_solver("4 0 1 2 3", status = 255L)
  on.exit(unlink(executable), add = TRUE)
  expect_equal(
    suppressWarnings(TSP:::tsp_concorde(tsp,
      control = list(exe = executable, verbose = FALSE))),
    1:4
  )
  expect_s3_class(
    suppressWarnings(solve_TSP(tsp, method = "concorde",
      control = list(exe = executable, verbose = FALSE))),
    "TOUR"
  )
  etsp <- ETSP(matrix(seq_len(8), ncol = 2))
  expect_equal(
    suppressWarnings(TSP:::tsp_concorde(etsp,
      control = list(exe = executable, verbose = FALSE))),
    1:4
  )
  expect_setequal(solver_tempdirs("concorde"), before)
})

test_that("Linkern checks exit status and always cleans temporary files", {
  skip_on_os("windows")
  on.exit(Sys.unsetenv("FAKE_SOLVER_OUTPUT"), add = TRUE)
  tsp <- TSP(dist(matrix(seq_len(8), ncol = 2)))
  before <- solver_tempdirs("linkern")

  executable <- make_fake_solver("0\\n1\\n2\\n3", status = 24L)
  on.exit(unlink(executable), add = TRUE)
  expect_error(
    suppressWarnings(TSP:::tsp_linkern(tsp,
      control = list(exe = executable, verbose = FALSE))),
    "Linkern exited with status 24",
    fixed = TRUE
  )
  expect_setequal(solver_tempdirs("linkern"), before)

  executable <- make_fake_solver(NULL)
  on.exit(unlink(executable), add = TRUE)
  expect_error(
    suppressWarnings(TSP:::tsp_linkern(tsp,
      control = list(exe = executable, verbose = FALSE))),
    "Linkern has not produced a result file",
    fixed = TRUE
  )
  expect_setequal(solver_tempdirs("linkern"), before)

  executable <- make_fake_solver("0\\n1\\n2\\n3")
  on.exit(unlink(executable), add = TRUE)
  expect_equal(
    suppressWarnings(TSP:::tsp_linkern(tsp,
      control = list(exe = executable, verbose = FALSE))),
    1:4
  )
  expect_s3_class(
    suppressWarnings(solve_TSP(tsp, method = "linkern",
      control = list(exe = executable, verbose = FALSE))),
    "TOUR"
  )
  etsp <- ETSP(matrix(seq_len(8), ncol = 2))
  expect_equal(
    suppressWarnings(TSP:::tsp_linkern(etsp,
      control = list(exe = executable, verbose = FALSE))),
    1:4
  )
  expect_setequal(solver_tempdirs("linkern"), before)
})

test_that("Concorde distance preparation handles negative and infinite values", {
  distances <- rbind(
    c(0, -Inf, 2),
    c(-Inf, 0, Inf),
    c(2, Inf, 0)
  )
  tsp <- TSP(distances)

  expect_warning(
    prepared <- TSP:::.prepare_dist_concorde(tsp, MAX = 2^15 - 1,
      precision = 2, verbose = TRUE),
    regexp = "negative distances"
  )
  expect_type(prepared, "integer")
  expect_true(all(is.finite(prepared)))
  expect_gte(min(prepared), 0)
})

test_that("seed is passed on to Concorde and linkern as option -s", {
  skip_on_os("windows")
  on.exit(Sys.unsetenv("FAKE_SOLVER_OUTPUT"), add = TRUE)
  tsp <- TSP(dist(matrix(seq_len(8), ncol = 2)))

  set.seed(1)
  expected <- paste("-s", sample.int(.Machine$integer.max, 1L))
  set.seed(1)
  expect_equal(TSP:::.seed_clo(NULL), expected)
  expect_error(TSP:::.seed_clo(-1))
  expect_error(TSP:::.seed_clo(1.5))
  expect_error(TSP:::.seed_clo(2^31))
  expect_error(TSP:::.seed_clo(NA_real_))
  expect_error(TSP:::.seed_clo(c(1, 2)))
  expect_equal(TSP:::.seed_clo(1234), "-s 1234")

  ## fake solver that records its arguments
  args_file <- tempfile("args-")
  on.exit(unlink(args_file), add = TRUE)
  record <- function(solver_output) {
    executable <- make_fake_solver(solver_output)
    lines <- readLines(executable)
    lines <- append(lines, paste0("echo \"$@\" > '", args_file, "'"), after = 1)
    writeLines(lines, executable)
    executable
  }

  executable <- record("4 0 1 2 3")
  on.exit(unlink(executable), add = TRUE)
  suppressWarnings(solve_TSP(tsp, method = "concorde",
    control = list(exe = executable, verbose = FALSE, seed = 42)))
  expect_match(readLines(args_file), "-s 42", fixed = TRUE)

  suppressWarnings(solve_TSP(tsp, method = "concorde",
    control = list(exe = executable, verbose = FALSE)))
  expect_match(readLines(args_file), "-s [0-9]+")

  executable <- record("0\\n1\\n2\\n3")
  on.exit(unlink(executable), add = TRUE)
  suppressWarnings(solve_TSP(tsp, method = "linkern",
    control = list(exe = executable, verbose = FALSE, seed = 7)))
  expect_match(readLines(args_file), "-s 7", fixed = TRUE)
})

test_that("linkern with the same seed is reproducible", {
  skip_if_no_linkern()
  set.seed(1)
  etsp <- ETSP(data.frame(x = runif(60), y = runif(60)))
  control <- list(seed = 1, verbose = FALSE, clo = "-R 10")
  t1 <- solve_TSP(etsp, method = "linkern", control = control)
  t2 <- solve_TSP(etsp, method = "linkern", control = control)
  expect_equal(as.integer(t1), as.integer(t2))
})

test_that("ETSP coordinates are scaled to integers for Concorde and linkern", {
  etsp <- ETSP(data.frame(x = c(0, 0.5, 1), y = c(0.25, 0, 0.75)))
  prepared <- TSP:::.prepare_coords_concorde(etsp, 1e7, precision = 6)
  expect_s3_class(prepared, "ETSP")
  expect_equal(unclass(prepared)[, 1], c(0, 5e5, 1e6), ignore_attr = TRUE)
  expect_true(all(prepared == round(prepared)))

  ## large coordinates are scaled down to avoid overflow
  big <- ETSP(data.frame(x = c(0, 1e6), y = c(0, 1e6)))
  expect_warning(
    prepared <- TSP:::.prepare_coords_concorde(big, 1e7, precision = 6),
    "integer coordinates")
  expect_lte(max(prepared), 1e7)
})

test_that("Concorde and linkern find good tours for ETSP in the unit square", {
  skip_if_no_concorde()
  set.seed(1)
  etsp <- ETSP(data.frame(x = runif(100), y = runif(100)))
  t_concorde <- solve_TSP(etsp, method = "concorde", verbose = FALSE)
  t_nn <- solve_TSP(etsp, method = "two_opt")
  expect_lt(tour_length(t_concorde), tour_length(t_nn))
  if (has_exe("linkern"))
    expect_lt(tour_length(solve_TSP(etsp, method = "linkern", verbose = FALSE)),
      tour_length(t_nn))
})

test_that("ETSP coordinate preparation handles negative, tiny, and huge ranges", {
  MAX <- 1e7
  set.seed(1)
  pts <- cbind(x = runif(20), y = runif(20))

  ## negative and mixed-sign coordinates are shifted to start at zero and keep
  ## their relative distances (up to the scaling factor 10^precision)
  shifted <- ETSP(pts - 0.7)
  prepared <- TSP:::.prepare_coords_concorde(shifted, MAX, precision = 6)
  expect_true(all(prepared >= 0))
  expect_true(all(prepared == round(prepared)))
  expect_equal(as.vector(dist(unclass(prepared))), as.vector(dist(pts)) * 1e6,
    tolerance = 1e-5)

  ## three dimensions
  prepared3 <- TSP:::.prepare_coords_concorde(
    ETSP(cbind(pts, z = -pts[, 1])), MAX, precision = 6)
  expect_equal(ncol(prepared3), 3L)
  expect_true(all(prepared3 >= 0))

  ## tiny extent: precision controls the resolution
  tiny <- ETSP(pts * 1e-6)
  low <- TSP:::.prepare_coords_concorde(tiny, MAX, precision = 6)
  ## ... and write_TSPLIB() warns about cities that became identical
  expect_warning(
    write_TSPLIB(low, tempfile(), precision = 15),
    "same coordinates")
  high <- TSP:::.prepare_coords_concorde(tiny, MAX, precision = 12)
  expect_lt(length(unique(as.vector(low))), length(unique(as.vector(high))))
  expect_lte(max(high), MAX)

  ## huge extent never overflows
  expect_warning(
    huge <- TSP:::.prepare_coords_concorde(ETSP(pts * 1e9), MAX, precision = 6),
    "integer coordinates")
  expect_lte(max(huge), MAX)

  ## all points identical (zero extent)
  same <- TSP:::.prepare_coords_concorde(ETSP(matrix(5, 4, 2)), MAX, 6)
  expect_true(all(same == 0))
})

test_that("Concorde and linkern give the same tours for shifted and rescaled ETSP", {
  skip_if_no_concorde_and_linkern()

  set.seed(2)
  pts <- cbind(x = runif(25), y = runif(25))
  etsp <- ETSP(pts)
  best <- tour_length(solve_TSP(etsp, method = "concorde", verbose = FALSE))

  variants <- list(
    negative = pts - 0.5,
    scaled_up = pts * 1000,
    scaled_down = pts * 0.001,
    shifted_scaled = pts * 50 - 1000
  )

  for (name in names(variants)) {
    for (method in c("concorde", "linkern")) {
      solve <- function()
        solve_TSP(ETSP(variants[[name]]), method = method, verbose = FALSE)
      ## larger ranges need a lower precision to stay within the integer range
      if (name %in% c("scaled_up", "shifted_scaled"))
        expect_warning(tour <- solve(), "integer coordinates")
      else
        tour <- solve()
      ## evaluate the tour on the original coordinates
      len <- tour_length(etsp, as.integer(tour))
      expect_equal(len, best, tolerance = 1e-3,
        label = paste(method, name, "tour length"))
    }
  }
})

test_that("Concorde solves a TSP with negative distances", {
  skip_if_no_concorde()

  set.seed(3)
  tsp <- TSP(dist(matrix(runif(24), ncol = 2)))
  best <- tour_length(solve_TSP(tsp, method = "concorde", verbose = FALSE))

  ## subtracting a constant from all distances does not change the optimal tour
  tsp_neg <- tsp - 2 * max(tsp)
  expect_lt(min(tsp_neg), 0)
  tour <- solve_TSP(tsp_neg, method = "concorde", verbose = FALSE)
  expect_equal(tour_length(tsp, tour), best, tolerance = 1e-4)
})

test_that("Concorde distance preparation rounds instead of truncating", {
  d <- as.dist(rbind(c(0, 0.29, 0.6), c(0.29, 0, 0.7), c(0.6, 0.7, 0)))
  prepared <- TSP:::.prepare_dist_concorde(TSP(d), 2^28 - 1, precision = 2)
  expect_equal(as.vector(prepared), c(29L, 60L, 70L))
})

test_that("unseeded Concorde/linkern calls use R's random number generator", {
  skip_on_os("windows")
  on.exit(Sys.unsetenv("FAKE_SOLVER_OUTPUT"), add = TRUE)
  tsp <- TSP(dist(matrix(seq_len(8), ncol = 2)))

  args_file <- tempfile("args-")
  on.exit(unlink(args_file), add = TRUE)
  executable <- make_fake_solver("4 0 1 2 3")
  on.exit(unlink(executable), add = TRUE)
  lines <- readLines(executable)
  writeLines(append(lines, paste0("echo \"$@\" > '", args_file, "'"), after = 1),
    executable)

  seed_used <- function(rng_seed) {
    set.seed(rng_seed)
    suppressWarnings(solve_TSP(tsp, method = "concorde",
      control = list(exe = executable, verbose = FALSE)))
    readLines(args_file)
  }

  expect_equal(seed_used(1), seed_used(1))
  expect_false(identical(seed_used(1), seed_used(2)))
})
