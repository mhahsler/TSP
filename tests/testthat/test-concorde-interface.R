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
  suppressWarnings(solve_TSP(tsp, method = "concorde", seed = 42,
    control = list(exe = executable, verbose = FALSE)))
  expect_match(readLines(args_file), "-s 42", fixed = TRUE)

  suppressWarnings(solve_TSP(tsp, method = "concorde",
    control = list(exe = executable, verbose = FALSE)))
  expect_match(readLines(args_file), "-s [0-9]+")

  executable <- record("0\\n1\\n2\\n3")
  on.exit(unlink(executable), add = TRUE)
  suppressWarnings(solve_TSP(tsp, method = "linkern", seed = 7,
    control = list(exe = executable, verbose = FALSE)))
  expect_match(readLines(args_file), "-s 7", fixed = TRUE)
})

test_that("linkern with the same seed is reproducible", {
  skip_if_not(Sys.which("linkern") != "")
  set.seed(1)
  etsp <- ETSP(data.frame(x = runif(60), y = runif(60)))
  t1 <- solve_TSP(etsp, method = "linkern", seed = 1, verbose = FALSE,
    clo = "-R 10")
  t2 <- solve_TSP(etsp, method = "linkern", seed = 1, verbose = FALSE,
    clo = "-R 10")
  expect_equal(as.integer(t1), as.integer(t2))
})

test_that("ETSP coordinates are scaled to integers for Concorde and linkern", {
  etsp <- ETSP(data.frame(x = c(0, 0.5, 1), y = c(0.25, 0, 0.75)))
  prepared <- TSP:::.prepare_coords_concorde(etsp, 2^28 - 1, precision = 6)
  expect_s3_class(prepared, "ETSP")
  expect_equal(unclass(prepared)[, 1], c(0, 5e5, 1e6), ignore_attr = TRUE)
  expect_true(all(prepared == round(prepared)))

  ## large coordinates are scaled down to avoid overflow
  big <- ETSP(data.frame(x = c(0, 1e6), y = c(0, 1e6)))
  expect_warning(
    prepared <- TSP:::.prepare_coords_concorde(big, 2^28 - 1, precision = 6),
    "integer coordinates")
  expect_lte(max(prepared), 2^28 - 1)
})

test_that("Concorde and linkern find good tours for ETSP in the unit square", {
  skip_if_not(Sys.which("concorde") != "" || !inherits(try(concorde_path(), silent = TRUE), "try-error"))
  skip_if_not(file.exists(file.path(concorde_path(), "concorde")))
  set.seed(1)
  etsp <- ETSP(data.frame(x = runif(100), y = runif(100)))
  t_concorde <- solve_TSP(etsp, method = "concorde", verbose = FALSE)
  t_nn <- solve_TSP(etsp, method = "two_opt")
  expect_lt(tour_length(t_concorde), tour_length(t_nn))
  if (file.exists(file.path(concorde_path(), "linkern")))
    expect_lt(tour_length(solve_TSP(etsp, method = "linkern", verbose = FALSE)),
      tour_length(t_nn))
})
