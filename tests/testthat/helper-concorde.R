## Concorde and linkern are external programs that are not part of the
## package. Tests that call them need to be guarded with these helpers.
has_exe <- function(prog) {
  p <- tryCatch(concorde_path(), error = function(e) NULL)
  !is.null(p) && Sys.which(file.path(p, prog)) != ""
}

skip_if_no_concorde <- function() {
  skip_if_not(has_exe("concorde"), "concorde is not installed")
}

skip_if_no_linkern <- function() {
  skip_if_not(has_exe("linkern"), "linkern is not installed")
}

skip_if_no_concorde_and_linkern <- function() {
  skip_if_not(has_exe("concorde") && has_exe("linkern"),
    "concorde/linkern are not installed")
}
