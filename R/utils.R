# Internal helpers shared across the package.

`%||%` <- function(x, y) if (is.null(x)) y else x

# Fall back to `y` when `x` is NULL or a length-one NA.
`%|NA%` <- function(x, y) {
  if (is.null(x) || (length(x) == 1 && is.na(x))) y else x
}

# Drop NULL elements of a list (used to omit optional request fields).
compact <- function(x) x[!vapply(x, is.null, logical(1))]

nzchar_or_null <- function(x) if (nzchar(x)) x else NULL

set_names <- function(x, nm) {
  names(x) <- rep_len(nm, length(x))
  x
}

# Vendored from rlang: register an S3 method for a generic in a possibly
# not-yet-loaded package, so Suggested backends (dbplyr) are supported without
# a hard dependency.
s3_register <- function(generic, class, method = NULL) {
  stopifnot(is.character(generic), length(generic) == 1)
  pieces <- strsplit(generic, "::")[[1]]
  stopifnot(length(pieces) == 2)
  package <- pieces[[1]]
  generic <- pieces[[2]]

  caller <- parent.frame()
  get_method <- function(method) {
    if (is.null(method)) {
      get(paste0(generic, ".", class), envir = caller)
    } else {
      method
    }
  }

  register <- function(...) {
    envir <- asNamespace(package)
    method_fn <- get_method(method)
    registerS3method(generic, class, method_fn, envir = envir)
  }

  if (isNamespaceLoaded(package)) {
    register()
  }
  setHook(packageEvent(package, "onLoad"), function(...) register())
  invisible()
}
