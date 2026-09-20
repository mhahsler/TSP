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
