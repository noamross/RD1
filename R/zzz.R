.onLoad <- function(libname, pkgname) {
  register_dbplyr_methods()
  register_ark_methods()
}

# Register dbplyr methods so D1 reuses dbplyr's SQLite backend (D1 is SQLite).
# Methods are registered lazily and only bite once dbplyr is loaded.
register_dbplyr_methods <- function() {
  s3_register("dbplyr::dbplyr_edition", "D1Connection", function(con) 2L)
  s3_register(
    "dbplyr::db_connection_describe",
    "D1Connection",
    function(con, ...) paste0("D1 [", con@database_id, "]")
  )
  s3_register("dbplyr::sql_translation", "D1Connection", function(con) {
    dbplyr::sql_translation(dbplyr::simulate_sqlite())
  })
}

# Register the connection as viewable in Positron's Variables pane via ark.
# A no-op outside Positron (where `.ark.register_method` is undefined).
register_ark_methods <- function() {
  tryCatch(
    {
      register <- get(".ark.register_method", envir = globalenv())
      register("ark_positron_variable_has_viewer", "D1Connection", function(x) {
        TRUE
      })
      register("ark_positron_variable_kind", "D1Connection", function(x) {
        "connection"
      })
      register("ark_positron_variable_view", "D1Connection", function(x) {
        d1_pane(x)
        TRUE
      })
    },
    error = function(e) NULL
  )
}
