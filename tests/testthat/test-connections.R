test_that("d1_database has a print method", {
  db <- new_database(list(
    uuid = "abc",
    name = "mydb",
    num_tables = 2,
    file_size = 4096,
    created_at = "2024-01-01T00:00:00Z"
  ))
  expect_s3_class(db, "d1_database")
  out <- paste(cli::cli_fmt(print(db)), collapse = "\n")
  expect_match(out, "mydb")
  expect_match(out, "abc")

  db$bookmark <- "00000000-0000000a-..."
  db$bookmark_time <- as.POSIXct("2024-01-02 03:04:05", tz = "UTC")
  out <- paste(cli::cli_fmt(print(db)), collapse = "\n")
  expect_match(out, "bookmark as of: 2024-01-02 03:04:05")
})

test_that("d1_database_id accepts a database object", {
  db <- new_database(list(uuid = "the-uuid", name = "mydb"))
  expect_equal(d1_database_id(db), "the-uuid")
})

test_that("connection action icons resolve to bundled PNG files", {
  for (name in c("d1", "bookmark", "restore", "download", "refresh")) {
    expect_true(file.exists(icon_path(name)))
  }
})

test_that("the connection show method displays live metadata", {
  skip_if_no_d1()
  con <- DBI::dbConnect(d1(), database_id = local_test_db())
  out <- paste(cli::cli_fmt(print(con)), collapse = "\n")
  expect_match(out, "uuid")
  expect_match(out, "bookmark")
})

test_that("connecting registers with the RStudio connection observer", {
  skip_if_no_d1()
  calls <- new.env()
  withr::local_options(
    connectionObserver = list(
      connectionOpened = function(...) calls$opened <- list(...),
      connectionClosed = function(...) calls$closed <- list(...),
      connectionUpdated = function(...) NULL
    )
  )

  id <- local_test_db()
  name <- d1_get_database(id)$name
  con <- DBI::dbConnect(d1(), database_id = id)
  expect_equal(calls$opened$type, "D1")
  expect_match(calls$opened$displayName, name, fixed = TRUE)

  # The exposed callbacks should work against the live connection.
  DBI::dbWriteTable(con, "t", data.frame(x = 1:2L))
  objs <- calls$opened$listObjects()
  expect_true("t" %in% objs$name)
  expect_named(
    calls$opened$actions,
    c("Bookmark", "Restore", "Download SQLite", "Refresh")
  )

  DBI::dbDisconnect(con)
  expect_equal(calls$closed$host, con@database_id)
})
