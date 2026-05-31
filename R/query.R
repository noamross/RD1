#' Run SQL against a D1 database
#'
#' `d1_query()` calls the D1 `/query` endpoint and rectangles the response
#' into a data frame. `d1_raw()` calls the `/raw` endpoint, which returns
#' columns and rows as arrays; it is used internally by the DBI layer.
#'
#' A single SQL string may contain several statements separated by `;`. When
#' it does, a list with one element per statement is returned; otherwise a
#' single object is returned directly.
#'
#' @param database_id Database UUID.
#' @param sql SQL string. May contain `?` placeholders bound from `params`.
#' @param params A list (or vector) of values bound to `?` placeholders.
#' @param account_id,token Cloudflare credentials. See [d1_token()].
#' @return For `d1_query()`, a data frame (or list of data frames). For
#'   `d1_raw()`, the parsed `result` list.
#' @export
d1_query <- function(
  database_id,
  sql,
  params = list(),
  account_id = d1_account(),
  token = d1_token()
) {
  res <- d1_sql(account_id, database_id, "query", sql, params, token)
  unwrap1(lapply(res, function(r) obj_to_df(r$results)))
}

#' @rdname d1_query
#' @export
d1_raw <- function(
  database_id,
  sql,
  params = list(),
  account_id = d1_account(),
  token = d1_token()
) {
  unwrap1(d1_sql(account_id, database_id, "raw", sql, params, token))
}

# POST a SQL statement to /query or /raw and return the per-statement result
# list (always a list, one element per statement).
d1_sql <- function(account_id, database_id, endpoint, sql, params, token) {
  d1_call(
    account_id,
    database_id,
    endpoint,
    token = token,
    method = "POST",
    body = list(
      sql = sql,
      params = if (length(params)) as.list(params) else list()
    )
  )
}

# --- rectangling ------------------------------------------------------------

# Convert /raw `results` (columns + row arrays) to a typed data frame. Works
# even with zero rows, preserving column names and order.
raw_to_df <- function(results) {
  cols <- unlist(results$columns) %||% character()
  rows <- results$rows
  data <- lapply(seq_along(cols), function(j) {
    simplify_col(lapply(rows, function(r) r[[j]]))
  })
  new_df(data, cols)
}

# Convert /query `results` (a list of named row objects) to a typed data frame.
obj_to_df <- function(rows) {
  if (!length(rows)) {
    return(new_df(list(), character()))
  }
  cols <- names(rows[[1]])
  data <- lapply(cols, function(nm) {
    simplify_col(lapply(rows, function(r) r[[nm]]))
  })
  new_df(data, cols)
}

# Coerce a list of JSON-parsed cell values into a typed column, mirroring
# RSQLite: whole numbers become integer, other numbers double, JSON null NA.
simplify_col <- function(x) {
  if (!length(x)) {
    return(logical())
  }
  x[vapply(x, is.null, logical(1))] <- list(NA)
  if (any(vapply(x, \(v) is.list(v) || length(v) != 1, logical(1)))) {
    return(x)
  }
  v <- unlist(x, use.names = FALSE)
  if (is.numeric(v) && all(is.na(v) | (v == trunc(v) & abs(v) < 2^31))) {
    return(as.integer(v))
  }
  v
}

new_df <- function(cols, names) {
  names(cols) <- names
  as.data.frame(cols, check.names = FALSE, stringsAsFactors = FALSE)
}

# Return the single element of a length-one list, else the list itself.
unwrap1 <- function(x) if (length(x) == 1) x[[1]] else x
