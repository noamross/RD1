#' RD1 DBI driver
#'
#' Returns a driver object used to connect to a Cloudflare D1 database with
#' [DBI::dbConnect()]. D1 is serverless SQLite accessed over HTTP, so a
#' "connection" simply bundles the database ID and credentials; there is no
#' persistent socket. `d1()`, `D1()`, `Rd1()`, and `RD1()` are interchangeable
#' aliases.
#'
#' @param drv A `D1Driver`, from `d1()`.
#' @param database_id Database UUID or name to connect to (names are resolved
#'   with [d1_database_id()]).
#' @param account_id,token Cloudflare credentials. See [d1_token()].
#' @param ... Unused.
#' @return `d1()` returns a `D1Driver`. `dbConnect()` returns a `D1Connection`.
#' @aliases D1 Rd1 RD1
#'
#' @section Differences between D1 and SQLite:
#' D1 *is* SQLite, but reached over Cloudflare's HTTP API rather than a local
#' file. This shapes how RD1 behaves compared with a file-based driver such as
#' RSQLite:
#'
#' * **No persistent connection.** A `D1Connection` only bundles the database
#'   ID and credentials. [DBI::dbDisconnect()] is a no-op that marks the
#'   connection invalid; there is no socket to close.
#' * **No transactions.** Each statement is sent and committed on its own.
#'   `dbBegin()`/`dbCommit()`/`dbRollback()` are not supported. Atomic restores
#'   are instead available through time travel (see below).
#' * **Eager results.** D1 returns a query's full result in one response, so
#'   results are materialised in memory and [DBI::dbFetch()] simply pages
#'   through them locally.
#' * **Storage classes versus JSON.** Results arrive as JSON, which cannot
#'   distinguish SQLite storage classes for whole numbers (a `REAL` `6.0` and an
#'   `INTEGER` `6` both serialise as `6`). For ad-hoc queries RD1 infers types
#'   from the values (whole numbers become integer, otherwise double). For
#'   [DBI::dbReadTable()] it reads the declared column types with
#'   `PRAGMA table_info()` and coerces columns to exactly what RSQLite would
#'   return, so whole tables round-trip faithfully.
#' * **No 64-bit integer tuning.** There is no `bigint` connection argument;
#'   integers beyond double precision may lose precision.
#' * **Booleans, dates, and times.** As in SQLite, these have no native type.
#'   Like RSQLite, logical maps to `INTEGER` and `Date`/`POSIXct`/`difftime`
#'   map to `REAL` on write, and are read back as integer/numeric rather than
#'   reconstructed.
#' * **Bound-parameter limit.** D1 caps bound parameters per request, so
#'   [DBI::dbWriteTable()] inserts rows in chunks.
#' * **Hidden internal tables.** [DBI::dbListTables()] omits D1/SQLite internal
#'   tables (`sqlite_*`, `_cf_*`).
#' * **Capabilities SQLite lacks.** Time travel and bookmarks
#'   ([d1_bookmark()], [d1_restore()]), HTTP export/import
#'   ([d1_export()], [d1_import()]), database management, and read replication.
#' @export
#' @examples
#' \dontrun{
#' con <- DBI::dbConnect(d1(), database_id = "....")
#' DBI::dbListTables(con)
#' }
d1 <- function() {
  new("D1Driver")
}

#' @export
D1 <- d1

#' @export
Rd1 <- d1

#' @export
RD1 <- d1

#' @rdname d1
#' @export
setClass("D1Driver", contains = "DBIDriver")

#' @rdname d1
#' @export
setMethod(
  "dbConnect",
  "D1Driver",
  function(
    drv,
    database_id,
    account_id = d1_account(),
    token = d1_token(),
    ...
  ) {
    database_id <- d1_database_id(database_id, account_id, token)
    state <- new.env(parent = emptyenv())
    state$valid <- TRUE
    con <- new(
      "D1Connection",
      database_id = database_id,
      account_id = account_id,
      token = token,
      state = state
    )
    on_connection_opened(con)
    con
  }
)

setMethod("dbDataType", "D1Driver", function(dbObj, obj, ...) {
  d1_data_type(obj)
})

setMethod("dbIsValid", "D1Driver", function(dbObj, ...) TRUE)

setMethod("dbGetInfo", "D1Driver", function(dbObj, ...) {
  v <- unname(getNamespaceVersion("RD1"))
  list(driver.version = v, client.version = v, max.connections = NA_integer_)
})

setMethod("show", "D1Driver", function(object) {
  cat("<D1Driver>\n")
})

# Map an R object to the SQLite storage class used by D1, mirroring RSQLite so
# that types round-trip between a D1 connection and a local SQLite file.
d1_data_type <- function(x) {
  if (is.data.frame(x)) {
    return(vapply(x, d1_data_type, character(1)))
  }
  if (inherits(x, "AsIs")) {
    x <- unclass(x)
  }
  switch(
    class(x)[1],
    logical = ,
    integer = "INTEGER",
    numeric = ,
    double = ,
    Date = ,
    POSIXct = ,
    difftime = "REAL",
    character = ,
    factor = ,
    ordered = ,
    raw = "TEXT",
    blob = "BLOB",
    list = if (all(vapply(x, is.raw, logical(1)))) "BLOB" else "TEXT",
    cli::cli_abort("Cannot map class {.cls {class(x)[1]}} to a D1 column type.")
  )
}
