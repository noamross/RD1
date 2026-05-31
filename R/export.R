#' Export a D1 database
#'
#' `d1_export()` triggers a SQL dump and polls until Cloudflare has staged the
#' file, returning the result containing a signed download URL.
#' `d1_download()` is a convenience wrapper that exports and downloads the SQL
#' dump to a local file. `d1_download_sqlite()` goes one step further,
#' materialising the dump into a local SQLite database that can be opened with
#' RSQLite, mirroring the types of the live database.
#'
#' @param database_id Database UUID.
#' @param tables Optional character vector restricting the export to these
#'   tables.
#' @param no_data If `TRUE`, export only the schema.
#' @param no_schema If `TRUE`, export only the data.
#' @param path Destination file path.
#' @param poll_interval Seconds between polls while the export is prepared.
#' @param account_id,token Cloudflare credentials. See [d1_token()].
#' @return `d1_export()` returns a list with `filename` and `signed_url`.
#'   `d1_download()` and `d1_download_sqlite()` return `path` invisibly.
#' @export
d1_export <- function(
  database_id,
  tables = NULL,
  no_data = FALSE,
  no_schema = FALSE,
  poll_interval = 1,
  account_id = d1_account(),
  token = d1_token()
) {
  dump_options <- compact(list(
    no_data = no_data,
    no_schema = no_schema,
    tables = if (length(tables)) as.list(tables)
  ))
  bookmark <- NULL
  repeat {
    res <- d1_call(
      account_id,
      database_id,
      "export",
      token = token,
      method = "POST",
      body = list(
        output_format = "polling",
        current_bookmark = bookmark,
        dump_options = dump_options
      )
    )
    if (identical(res$status, "complete")) {
      return(res$result)
    }
    if (identical(res$status, "error")) {
      cli::cli_abort(c(
        "D1 export failed.",
        set_names(unlist(res$messages), "x")
      ))
    }
    bookmark <- res$at_bookmark
    Sys.sleep(poll_interval)
  }
}

#' @rdname d1_export
#' @export
d1_download <- function(
  database_id,
  path,
  tables = NULL,
  account_id = d1_account(),
  token = d1_token()
) {
  res <- d1_export(
    database_id,
    tables = tables,
    account_id = account_id,
    token = token
  )
  httr2::request(res$signed_url) |> httr2::req_perform(path = path)
  invisible(path)
}

#' @rdname d1_export
#' @export
d1_download_sqlite <- function(
  database_id,
  path,
  tables = NULL,
  account_id = d1_account(),
  token = d1_token()
) {
  rlang::check_installed("RSQLite", "to build a local SQLite database.")
  sql <- d1_download(
    database_id,
    tempfile(fileext = ".sql"),
    tables = tables,
    account_id = account_id,
    token = token
  )
  con <- RSQLite::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con))
  exec_sql_script(con, sql)
  invisible(path)
}

# Execute a SQL dump (statements separated by ;) against a DBI connection.
exec_sql_script <- function(con, file) {
  script <- paste(readLines(file, warn = FALSE), collapse = "\n")
  for (stmt in split_statements(script)) {
    DBI::dbExecute(con, stmt)
  }
}

# Split a SQL script into individual statements on semicolons, ignoring those
# inside single-quoted string literals.
split_statements <- function(sql) {
  parts <- strsplit(sql, "(?<=;)", perl = TRUE)[[1]]
  stmts <- character()
  buf <- ""
  for (p in parts) {
    buf <- paste0(buf, p)
    if (count_char(buf, "'") %% 2 == 0) {
      if (nzchar(trimws(buf))) {
        stmts <- c(stmts, trimws(buf))
      }
      buf <- ""
    }
  }
  if (nzchar(trimws(buf))) {
    stmts <- c(stmts, trimws(buf))
  }
  stmts
}

count_char <- function(x, ch) {
  lengths(regmatches(x, gregexpr(ch, x, fixed = TRUE)))
}
