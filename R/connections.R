# RStudio Connections Pane integration via the `connectionObserver` option.
# This is a soft contract: when RStudio is not present the observer is NULL and
# every hook is a no-op, so there is no hard dependency.

connection_observer <- function() {
  getOption("connectionObserver")
}

# Path to a bundled toolbar icon (RStudio needs an icon to render an action as
# a button). Returns NULL if the icon is missing.
icon_path <- function(name) {
  path <- system.file("icons", paste0(name, ".png"), package = "RD1")
  if (nzchar(path)) path else NULL
}

# Database metadata (name, size, table count, current bookmark and the time it
# was read), cached in the connection's state. Fetched lazily on first use and
# whenever `refresh = TRUE`; invalidated automatically by writes (see
# result_execute()). Network failures degrade to NA rather than erroring.
connection_meta <- function(con, refresh = FALSE) {
  if (refresh || is.null(con@state$meta)) {
    con@state$meta <- fetch_connection_meta(con)
  }
  con@state$meta
}

fetch_connection_meta <- function(con) {
  # d1_get_database() already attaches the current bookmark and its timestamp.
  info <- tryCatch(
    unclass(d1_get_database(
      con@database_id,
      account_id = con@account_id,
      token = con@token
    )),
    error = function(e) list()
  )
  list(
    name = info$name %||% NA_character_,
    uuid = con@database_id,
    num_tables = info$num_tables %||% NA,
    file_size = info$file_size %||% NA,
    bookmark = info$bookmark %||% NA_character_,
    bookmark_time = info$bookmark_time %||% Sys.time()
  )
}

#' Show a D1 connection in the Connections pane
#'
#' Registers an open connection with the RStudio / Positron Connections pane,
#' listing its tables and adding bookmark, restore, and download actions.
#' [DBI::dbConnect()] does this automatically; call `d1_pane()` to re-open the
#' pane for an existing connection (for example after closing it).
#'
#' @param conn A connection from `DBI::dbConnect(d1(), ...)`.
#' @return `conn`, invisibly.
#' @export
d1_pane <- function(conn) {
  on_connection_opened(conn)
  invisible(conn)
}

# Notify RStudio that a connection has opened, exposing tables for browsing and
# time-travel/export actions as toolbar buttons.
on_connection_opened <- function(con) {
  observer <- connection_observer()
  if (is.null(observer)) {
    return(invisible())
  }
  code <- paste0(
    'library(RD1)\n',
    'con <- DBI::dbConnect(RD1::d1(), database_id = "',
    con@database_id,
    '")'
  )
  meta <- connection_meta(con)
  display <- paste0(
    meta$name %|NA% con@database_id,
    " (as of ",
    format(meta$bookmark_time, "%Y-%m-%d %H:%M"),
    ")"
  )
  try(
    observer$connectionOpened(
      type = "D1",
      displayName = display,
      host = con@database_id,
      icon = icon_path("d1"),
      connectCode = code,
      disconnect = function() DBI::dbDisconnect(con),
      listObjectTypes = function() {
        list(table = list(contains = "data"))
      },
      listObjects = function(type = "table") {
        tables <- DBI::dbListTables(con)
        data.frame(
          name = tables,
          type = rep("table", length(tables)),
          stringsAsFactors = FALSE
        )
      },
      listColumns = function(table) {
        info <- d1_run(
          con,
          paste0("PRAGMA table_info(", DBI::dbQuoteIdentifier(con, table), ")")
        )$data
        data.frame(
          name = as.character(info$name),
          type = as.character(info$type),
          stringsAsFactors = FALSE
        )
      },
      previewObject = function(rowLimit, table) {
        DBI::dbGetQuery(
          con,
          paste0(
            "SELECT * FROM ",
            DBI::dbQuoteIdentifier(con, table),
            " LIMIT ",
            rowLimit
          )
        )
      },
      actions = connection_actions(con),
      connectionObject = con
    ),
    silent = TRUE
  )
  invisible()
}

on_connection_closed <- function(con) {
  observer <- connection_observer()
  if (!is.null(observer)) {
    try(
      observer$connectionClosed(type = "D1", host = con@database_id),
      silent = TRUE
    )
  }
  invisible()
}

on_connection_updated <- function(con) {
  observer <- connection_observer()
  if (!is.null(observer)) {
    try(
      observer$connectionUpdated(type = "D1", host = con@database_id),
      silent = TRUE
    )
  }
  invisible()
}

# Toolbar buttons surfacing the headline time-travel and export workflows.
connection_actions <- function(con) {
  list(
    Bookmark = list(icon = icon_path("bookmark"), callback = function() {
      bm <- d1_bookmark(
        con@database_id,
        account_id = con@account_id,
        token = con@token
      )
      cli::cli_inform("D1 bookmark: {bm}")
      if (has_rstudioapi()) {
        rstudioapi::showDialog("D1 bookmark", paste("Current bookmark:", bm))
      }
    }),
    Restore = list(icon = icon_path("restore"), callback = function() {
      if (!has_rstudioapi()) {
        cli::cli_inform(
          "Use {.fn d1_restore} to restore by bookmark or timestamp."
        )
        return(invisible())
      }
      at <- rstudioapi::showPrompt(
        "D1 restore",
        "Bookmark or ISO 8601 timestamp"
      )
      if (!is.null(at) && nzchar(at)) {
        d1_restore(
          con@database_id,
          bookmark = at,
          account_id = con@account_id,
          token = con@token
        )
        connection_meta(con, refresh = TRUE)
        on_connection_updated(con)
      }
    }),
    `Download SQLite` = list(
      icon = icon_path("download"),
      callback = function() {
        path <- if (has_rstudioapi()) {
          rstudioapi::selectFile(
            "Save SQLite as",
            filter = "SQLite (*.sqlite)",
            existing = FALSE
          )
        } else {
          file.path(getwd(), paste0(con@database_id, ".sqlite"))
        }
        if (!is.null(path) && nzchar(path)) {
          d1_download_sqlite(
            con@database_id,
            path,
            account_id = con@account_id,
            token = con@token
          )
          cli::cli_inform("Downloaded to {.file {path}}")
        }
      }
    ),
    Refresh = list(icon = icon_path("refresh"), callback = function() {
      connection_meta(con, refresh = TRUE)
      on_connection_updated(con)
    })
  )
}

has_rstudioapi <- function() {
  rlang::is_installed("rstudioapi") && rstudioapi::isAvailable()
}
