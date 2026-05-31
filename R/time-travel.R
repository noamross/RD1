#' Time travel: bookmarks and restoration
#'
#' D1 retains write history so a database can be rewound to any point within
#' its retention window. A *bookmark* is an opaque marker for a moment in that
#' history. `d1_bookmark()` looks one up; `d1_restore()` rewinds the database.
#'
#' @param database_id A database UUID, name, `d1_database` object, or open
#'   `D1Connection` (resolved with [d1_database_id()]).
#' @param timestamp An ISO 8601 timestamp (or `Date`/`POSIXct`). For
#'   `d1_bookmark()`, returns the bookmark at or just before this time
#'   (defaults to now). For `d1_restore()`, an alternative to `bookmark`.
#' @param bookmark A bookmark string from `d1_bookmark()`.
#' @param account_id,token Cloudflare credentials. See [d1_token()].
#' @return `d1_bookmark()` returns a bookmark string. `d1_restore()` returns a
#'   list with the new and previous bookmarks.
#' @export
d1_bookmark <- function(
  database_id,
  timestamp = NULL,
  account_id = d1_account(),
  token = d1_token()
) {
  resolve_db(database_id, account_id, token)
  res <- d1_call(
    account_id,
    database_id,
    "time_travel",
    "bookmark",
    token = token,
    query = list(timestamp = iso8601(timestamp))
  )
  res$bookmark
}

#' @rdname d1_bookmark
#' @export
d1_restore <- function(
  database_id,
  bookmark = NULL,
  timestamp = NULL,
  account_id = d1_account(),
  token = d1_token()
) {
  if (is.null(bookmark) && is.null(timestamp)) {
    cli::cli_abort("Supply either {.arg bookmark} or {.arg timestamp}.")
  }
  resolve_db(database_id, account_id, token)
  d1_call(
    account_id,
    database_id,
    "time_travel",
    "restore",
    token = token,
    method = "POST",
    query = list(bookmark = bookmark, timestamp = iso8601(timestamp))
  )
}

# Format a Date/POSIXct/string as ISO 8601 UTC, passing NULL through.
iso8601 <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  if (inherits(x, c("POSIXt", "Date"))) {
    return(format(as.POSIXct(x, tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
  }
  x
}
