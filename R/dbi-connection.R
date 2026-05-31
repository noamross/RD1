#' D1 DBI connection
#'
#' A connection to a single D1 database. Created by [DBI::dbConnect()] with a
#' [d1()] driver. Supports the usual DBI verbs: querying, listing and
#' reading/writing tables. As D1 runs over stateless HTTP there are no
#' transactions and disconnection is a no-op.
#'
#' @slot database_id,account_id,token Connection details.
#' @slot state Environment tracking whether the connection is open.
#' @name D1Connection-class
#' @export
setClass(
  "D1Connection",
  contains = "DBIConnection",
  slots = c(
    database_id = "character",
    account_id = "character",
    token = "character",
    state = "environment"
  )
)

setMethod("dbIsValid", "D1Connection", function(dbObj, ...) {
  isTRUE(dbObj@state$valid)
})

setMethod("dbDisconnect", "D1Connection", function(conn, ...) {
  if (!isTRUE(conn@state$valid)) {
    cli::cli_warn("Connection already closed.")
  }
  conn@state$valid <- FALSE
  on_connection_closed(conn)
  invisible(TRUE)
})

setMethod("dbDataType", "D1Connection", function(dbObj, obj, ...) {
  d1_data_type(obj)
})

setMethod("show", "D1Connection", function(object) {
  if (!isTRUE(object@state$valid)) {
    cli::cli_text("{.strong <D1Connection>} DISCONNECTED")
    return(invisible(object))
  }
  m <- connection_meta(object)
  cli::cli_text("{.strong <D1Connection>} {.val {m$name %|NA% m$uuid}}")
  cli::cli_bullets(c(
    "*" = "uuid: {.field {m$uuid}}",
    "*" = "tables: {m$num_tables}",
    "*" = "size: {prettyNum(m$file_size, big.mark = ',')} bytes",
    "*" = "bookmark as of: {format(m$bookmark_time, '%Y-%m-%d %H:%M:%S')}"
  ))
  invisible(object)
})

setMethod("dbGetInfo", "D1Connection", function(dbObj, ...) {
  list(
    dbname = dbObj@database_id,
    db.version = NA_character_,
    username = NA_character_,
    host = "api.cloudflare.com",
    port = 443L
  )
})

setMethod(
  "dbSendQuery",
  signature("D1Connection", "character"),
  function(conn, statement, params = NULL, ...) {
    result_new(conn, statement, params)
  }
)

setMethod(
  "dbSendStatement",
  signature("D1Connection", "character"),
  function(conn, statement, params = NULL, ...) {
    result_new(conn, statement, params)
  }
)

setMethod("dbListTables", "D1Connection", function(conn, ...) {
  df <- dbGetQuery(
    conn,
    "SELECT name FROM sqlite_master
     WHERE type = 'table' AND name NOT LIKE 'sqlite_%' AND name NOT LIKE '_cf_%'"
  )
  as.character(df$name)
})

setMethod(
  "dbExistsTable",
  signature("D1Connection", "character"),
  function(conn, name, ...) {
    tolower(name) %in% tolower(dbListTables(conn))
  }
)

setMethod(
  "dbListFields",
  signature("D1Connection", "character"),
  function(conn, name, ...) {
    out <- d1_run(
      conn,
      paste("SELECT * FROM", dbQuoteIdentifier(conn, name), "LIMIT 0")
    )
    names(out$data)
  }
)

setMethod(
  "dbReadTable",
  signature("D1Connection", "character"),
  function(conn, name, ...) {
    df <- dbGetQuery(
      conn,
      paste("SELECT * FROM", dbQuoteIdentifier(conn, name))
    )
    coerce_decltypes(conn, name, df)
  }
)

setMethod(
  "dbRemoveTable",
  signature("D1Connection", "character"),
  function(conn, name, ...) {
    dbExecute(conn, paste("DROP TABLE", dbQuoteIdentifier(conn, name)))
    invisible(TRUE)
  }
)

setMethod(
  "dbWriteTable",
  signature("D1Connection", "character", "data.frame"),
  function(
    conn,
    name,
    value,
    overwrite = FALSE,
    append = FALSE,
    field.types = NULL,
    ...
  ) {
    if (overwrite && append) {
      cli::cli_abort(
        "Only one of {.arg overwrite} and {.arg append} may be TRUE."
      )
    }
    exists <- dbExistsTable(conn, name)
    if (exists && overwrite) {
      dbRemoveTable(conn, name)
      exists <- FALSE
    }
    if (exists && !append) {
      cli::cli_abort("Table {.val {name}} already exists.")
    }
    if (!exists) {
      dbExecute(conn, sql_create_table(conn, name, value, field.types))
    }
    if (nrow(value)) {
      insert_rows(conn, name, value)
    }
    invisible(TRUE)
  }
)

# --- internal SQL execution -------------------------------------------------

# Run a single statement via the /raw endpoint, returning typed data and meta.
d1_run <- function(conn, statement, params = list()) {
  res <- d1_sql(
    conn@account_id,
    conn@database_id,
    "raw",
    statement,
    params,
    conn@token
  )[[1]]
  list(data = raw_to_df(res$results), meta = res$meta)
}

# Coerce a table's columns to the R types RSQLite would produce, using the
# declared column types (which D1's JSON values cannot convey on their own).
coerce_decltypes <- function(conn, name, df) {
  if (!ncol(df)) {
    return(df)
  }
  info <- d1_run(
    conn,
    paste0("PRAGMA table_info(", dbQuoteIdentifier(conn, name), ")")
  )$data
  types <- as.character(info$type)
  names(types) <- info$name
  for (col in names(df)) {
    df[[col]] <- coerce_affinity(df[[col]], sqlite_affinity(types[[col]]))
  }
  df
}

# SQLite type-affinity rules (https://www.sqlite.org/datatype3.html).
sqlite_affinity <- function(decltype) {
  t <- toupper(decltype %||% "")
  if (grepl("INT", t)) {
    "integer"
  } else if (grepl("CHAR|CLOB|TEXT", t)) {
    "character"
  } else if (t == "" || grepl("BLOB", t)) {
    "blob"
  } else if (grepl("REAL|FLOA|DOUB", t)) {
    "double"
  } else {
    "numeric"
  }
}

coerce_affinity <- function(x, affinity) {
  switch(
    affinity,
    integer = as.integer(x),
    double = as.double(x),
    character = as.character(x),
    x
  )
}

sql_create_table <- function(conn, name, value, field.types = NULL) {
  types <- d1_data_type(value)
  if (!is.null(field.types)) {
    types[names(field.types)] <- unlist(field.types)
  }
  fields <- paste(dbQuoteIdentifier(conn, names(types)), unname(types))
  paste0(
    "CREATE TABLE ",
    dbQuoteIdentifier(conn, name),
    " (",
    paste(fields, collapse = ", "),
    ")"
  )
}

# Insert a data frame, chunking rows so each request stays within D1's bound
# parameter limit, and binding values as `?` placeholders.
insert_rows <- function(conn, name, value) {
  ncol <- ncol(value)
  chunk <- max(1L, 100L %/% ncol)
  id <- dbQuoteIdentifier(conn, name)
  cols <- paste(dbQuoteIdentifier(conn, names(value)), collapse = ", ")
  tuple <- paste0("(", paste(rep("?", ncol), collapse = ", "), ")")
  groups <- split(seq_len(nrow(value)), (seq_len(nrow(value)) - 1L) %/% chunk)
  for (rows in groups) {
    sub <- value[rows, , drop = FALSE]
    sql <- paste0(
      "INSERT INTO ",
      id,
      " (",
      cols,
      ") VALUES ",
      paste(rep(tuple, length(rows)), collapse = ", ")
    )
    params <- unlist(
      lapply(rows, function(i) unname(as.list(sub[match(i, rows), ]))),
      recursive = FALSE
    )
    dbExecute(conn, sql, params = params)
  }
}
