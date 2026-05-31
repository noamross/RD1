#' Import SQL into a D1 database
#'
#' `d1_import()` uploads a local SQL file and ingests it, following
#' Cloudflare's init/upload/ingest/poll flow. `d1_upload_sqlite()` is a
#' convenience wrapper that dumps a local SQLite database to SQL and imports
#' it, the inverse of [d1_download_sqlite()].
#'
#' @param database_id Database UUID.
#' @param file Path to a `.sql` file (`d1_import()`) or `.sqlite` file
#'   (`d1_upload_sqlite()`).
#' @param poll_interval Seconds between polls while the import is applied.
#' @param account_id,token Cloudflare credentials. See [d1_token()].
#' @return A list describing the completed import (number of queries and final
#'   bookmark).
#' @export
d1_import <- function(
  database_id,
  file,
  poll_interval = 1,
  account_id = d1_account(),
  token = d1_token()
) {
  etag <- unname(tools::md5sum(file))
  import <- function(body) {
    d1_call(
      account_id,
      database_id,
      "import",
      token = token,
      method = "POST",
      body = body
    )
  }

  init <- import(list(action = "init", etag = etag))
  httr2::request(init$upload_url) |>
    httr2::req_method("PUT") |>
    httr2::req_body_file(file) |>
    httr2::req_perform()

  res <- import(list(action = "ingest", etag = etag, filename = init$filename))
  repeat {
    if (identical(res$status, "complete")) {
      return(res$result)
    }
    if (identical(res$status, "error")) {
      cli::cli_abort(c(
        "D1 import failed.",
        set_names(unlist(res$messages), "x")
      ))
    }
    Sys.sleep(poll_interval)
    res <- import(list(action = "poll", current_bookmark = res$at_bookmark))
  }
}

#' @rdname d1_import
#' @export
d1_upload_sqlite <- function(
  database_id,
  file,
  account_id = d1_account(),
  token = d1_token()
) {
  rlang::check_installed("RSQLite", "to read a local SQLite database.")
  con <- RSQLite::dbConnect(RSQLite::SQLite(), file)
  on.exit(DBI::dbDisconnect(con))
  sql <- tempfile(fileext = ".sql")
  writeLines(sqlite_dump(con), sql)
  d1_import(database_id, sql, account_id = account_id, token = token)
}

# Produce a SQL dump (schema + INSERTs) for every user table in a SQLite
# connection, using the live D1 connection's own quoting/value formatting.
sqlite_dump <- function(con) {
  schema <- DBI::dbGetQuery(
    con,
    "SELECT sql FROM sqlite_master WHERE type = 'table' AND name NOT LIKE 'sqlite_%'"
  )$sql
  tables <- DBI::dbListTables(con)
  inserts <- unlist(lapply(tables, function(t) insert_statements(con, t)))
  c(paste0(schema, ";"), inserts)
}

# Build parameterless INSERT statements for one table by formatting each row
# as SQL literals.
insert_statements <- function(con, table) {
  df <- DBI::dbReadTable(con, table)
  if (!nrow(df)) {
    return(character())
  }
  id <- DBI::dbQuoteIdentifier(con, table)
  cols <- paste(DBI::dbQuoteIdentifier(con, names(df)), collapse = ", ")
  values <- apply(df, 1, function(row) {
    paste(DBI::dbQuoteLiteral(con, unname(row)), collapse = ", ")
  })
  sprintf("INSERT INTO %s (%s) VALUES (%s);", id, cols, values)
}
