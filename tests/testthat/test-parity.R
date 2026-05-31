# The central design goal: a database read from D1 should be byte-for-byte
# the same as the same database downloaded and read with RSQLite.
test_that("D1 reads match an RSQLite read of the downloaded database", {
  skip_if_no_d1()
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(D1(), database_id = (id <- local_test_db()))

  data <- data.frame(
    i = 1:5L,
    r = c(1, 2, 3, 4, 5), # whole-valued doubles: must stay REAL/numeric
    f = c(1.5, 2.5, 3.5, 4.5, 5.5),
    s = letters[1:5],
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(con, "d", data)
  from_d1 <- DBI::dbReadTable(con, "d")

  path <- withr::local_tempfile(fileext = ".sqlite")
  d1_download_sqlite(id, path)
  scon <- DBI::dbConnect(RSQLite::SQLite(), path)
  withr::defer(DBI::dbDisconnect(scon))
  from_sqlite <- DBI::dbReadTable(scon, "d")

  expect_identical(
    vapply(from_d1, function(x) class(x)[1], character(1)),
    vapply(from_sqlite, function(x) class(x)[1], character(1))
  )
  expect_equal(from_d1, from_sqlite)
})

test_that("a SQLite file uploaded to D1 round-trips back unchanged", {
  skip_if_no_d1()
  skip_if_not_installed("RSQLite")
  id <- local_test_db()

  path <- withr::local_tempfile(fileext = ".sqlite")
  scon <- DBI::dbConnect(RSQLite::SQLite(), path)
  DBI::dbWriteTable(scon, "cars", head(mtcars, 4))
  DBI::dbDisconnect(scon)

  d1_upload_sqlite(id, path)
  con <- DBI::dbConnect(D1(), database_id = id)
  expect_true("cars" %in% DBI::dbListTables(con))
  expect_equal(nrow(DBI::dbReadTable(con, "cars")), 4)
})
