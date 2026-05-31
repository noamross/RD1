#' D1 DBI result
#'
#' Represents the result of a statement run against a [D1Connection-class].
#' Because D1 returns the full result of a query in one HTTP response, the rows
#' are materialised eagerly; [DBI::dbFetch()] then pages through them locally.
#'
#' @slot conn The originating connection.
#' @slot statement The SQL statement.
#' @slot state An environment holding the materialised rows and cursor.
#' @name D1Result-class
#' @export
setClass(
  "D1Result",
  contains = "DBIResult",
  slots = c(
    conn = "D1Connection",
    statement = "character",
    state = "environment"
  )
)

# Create a result and execute it (immediately, unless deferred for binding).
result_new <- function(conn, statement, params = NULL) {
  res <- new(
    "D1Result",
    conn = conn,
    statement = statement,
    state = new.env(parent = emptyenv())
  )
  res@state$valid <- TRUE
  result_execute(res, params %||% list())
  res
}

result_execute <- function(res, params) {
  out <- d1_run(res@conn, res@statement, params)
  res@state$data <- out$data
  res@state$meta <- out$meta
  res@state$pos <- 0L
  # A write changes size/tables/bookmark, so drop the cached connection
  # metadata; it is re-fetched the next time the connection is shown.
  if (isTRUE(out$meta$changed_db)) {
    res@conn@state$meta <- NULL
  }
  invisible(res)
}

setMethod("dbBind", "D1Result", function(res, params, ...) {
  result_execute(res, params)
  invisible(res)
})

setMethod("dbFetch", "D1Result", function(res, n = -1, ...) {
  data <- res@state$data
  pos <- res@state$pos
  end <- if (n < 0) nrow(data) else min(pos + n, nrow(data))
  res@state$pos <- end
  out <- data[seq_len(end - pos) + pos, , drop = FALSE]
  rownames(out) <- NULL
  out
})

setMethod("dbHasCompleted", "D1Result", function(res, ...) {
  res@state$pos >= nrow(res@state$data)
})

setMethod("dbGetRowCount", "D1Result", function(res, ...) res@state$pos)

setMethod("dbGetRowsAffected", "D1Result", function(res, ...) {
  res@state$meta$changes %||% 0L
})

setMethod("dbGetStatement", "D1Result", function(res, ...) res@statement)

setMethod("dbColumnInfo", "D1Result", function(res, ...) {
  data <- res@state$data
  data.frame(
    name = names(data),
    type = vapply(data, function(x) class(x)[1], character(1)),
    row.names = NULL,
    stringsAsFactors = FALSE
  )
})

setMethod("dbIsValid", "D1Result", function(dbObj, ...) {
  isTRUE(dbObj@state$valid)
})

setMethod("dbClearResult", "D1Result", function(res, ...) {
  res@state$valid <- FALSE
  invisible(TRUE)
})

setMethod("show", "D1Result", function(object) {
  cat("<D1Result>\n  ", object@statement, "\n", sep = "")
})
