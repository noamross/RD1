#' Manage D1 databases
#'
#' Wrappers for the D1 database management endpoints.
#'
#' @param name Database name. For `d1_list_databases()`, an optional filter.
#' @param database_id Database UUID.
#' @param location_hint Optional primary location hint, e.g. `"wnam"`.
#' @param mode Read-replication mode, `"auto"` or `"disabled"`.
#' @param account_id,token Cloudflare credentials. See [d1_token()].
#' @return `d1_list_databases()` returns a data frame; `d1_create_database()`
#'   and `d1_get_database()` return a database record (a list);
#'   `d1_delete_database()` returns `NULL` invisibly.
#' @export
d1_list_databases <- function(
  name = NULL,
  account_id = d1_account(),
  token = d1_token()
) {
  records_to_df(d1_call(
    account_id,
    token = token,
    query = list(name = name, per_page = 1000)
  ))
}

#' @rdname d1_list_databases
#' @export
d1_create_database <- function(
  name,
  location_hint = NULL,
  account_id = d1_account(),
  token = d1_token()
) {
  rec <- d1_call(
    account_id,
    token = token,
    method = "POST",
    body = list(name = name, primary_location_hint = location_hint)
  )
  new_database(rec, account_id, token)
}

#' @rdname d1_list_databases
#' @export
d1_get_database <- function(
  database_id,
  account_id = d1_account(),
  token = d1_token()
) {
  new_database(
    d1_call(account_id, database_id, token = token),
    account_id,
    token
  )
}

# Wrap a database record, attaching the current bookmark (and the time it was
# read) so it can be printed without further API calls. Credentials are
# optional so the constructor can also be used offline (e.g. in tests).
new_database <- function(x, account_id = NULL, token = NULL) {
  if (!is.null(account_id) && !is.null(token)) {
    x$bookmark <- tryCatch(
      d1_bookmark(x$uuid, account_id = account_id, token = token),
      error = function(e) NA_character_
    )
    x$bookmark_time <- Sys.time()
  }
  structure(x, class = "d1_database")
}

#' @export
print.d1_database <- function(x, ...) {
  cli::cli_text("{.strong <D1 database>} {.val {x$name}}")
  bullets <- c(
    "*" = "uuid: {.field {x$uuid}}",
    "*" = "tables: {x$num_tables %||% NA}",
    "*" = "size: {prettyNum(x$file_size %||% NA, big.mark = ',')} bytes",
    "*" = "created: {x$created_at %||% NA}"
  )
  if (!is.null(x$bookmark_time)) {
    bullets <- c(
      bullets,
      "*" = "bookmark as of: {format(x$bookmark_time, '%Y-%m-%d %H:%M:%S')}"
    )
  }
  cli::cli_bullets(bullets)
  invisible(x)
}

#' @rdname d1_list_databases
#' @export
d1_delete_database <- function(
  database_id,
  account_id = d1_account(),
  token = d1_token()
) {
  d1_call(account_id, database_id, token = token, method = "DELETE")
  invisible(NULL)
}

#' Resolve a database name to its UUID
#'
#' Passes a UUID through unchanged; otherwise looks up the database by exact
#' name. Useful for calling any `d1_*()` function, or [DBI::dbConnect()], with
#' a human-readable name instead of a UUID.
#'
#' @param database A database UUID, name, or object from
#'   [d1_create_database()] / [d1_get_database()].
#' @param account_id,token Cloudflare credentials. See [d1_token()].
#' @return A database UUID string.
#' @export
d1_database_id <- function(
  database,
  account_id = d1_account(),
  token = d1_token()
) {
  if (inherits(database, "d1_database")) {
    return(database$uuid)
  }
  uuid <- "^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$"
  if (grepl(uuid, database, ignore.case = TRUE)) {
    return(database)
  }
  dbs <- d1_list_databases(
    name = database,
    account_id = account_id,
    token = token
  )
  hit <- dbs$uuid[dbs$name == database]
  if (length(hit) != 1L) {
    cli::cli_abort(
      "Found {length(hit)} database{?s} named {.val {database}}; expected 1."
    )
  }
  hit
}

#' @rdname d1_list_databases
#' @export
d1_set_replication <- function(
  database_id,
  mode = c("auto", "disabled"),
  account_id = d1_account(),
  token = d1_token()
) {
  mode <- match.arg(mode)
  d1_call(
    account_id,
    database_id,
    token = token,
    method = "PATCH",
    body = list(read_replication = list(mode = mode))
  )
}

# Convert a list of flat JSON records into a data frame, filling missing
# fields with NA and dropping nested (non-scalar) fields to NA.
records_to_df <- function(x) {
  if (!length(x)) {
    return(data.frame())
  }
  cols <- unique(unlist(lapply(x, names)))
  data <- lapply(cols, function(col) {
    simplify_col(lapply(x, function(r) r[[col]]))
  })
  new_df(data, cols)
}
