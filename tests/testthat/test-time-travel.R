test_that("iso8601 formats times and passes strings through", {
  expect_equal(iso8601(NULL), NULL)
  expect_equal(iso8601("2024-01-01T00:00:00Z"), "2024-01-01T00:00:00Z")
  expect_equal(
    iso8601(as.POSIXct("2024-01-02 03:04:05", tz = "UTC")),
    "2024-01-02T03:04:05Z"
  )
})

test_that("d1_restore requires a bookmark or timestamp", {
  expect_snapshot(d1_restore("id"), error = TRUE)
})

test_that("a database can be rewound to a bookmark", {
  skip_if_no_d1()
  con <- DBI::dbConnect(D1(), database_id = (id <- local_test_db()))
  DBI::dbWriteTable(con, "t", data.frame(x = 1:3L))

  bookmark <- d1_bookmark(id)
  expect_type(bookmark, "character")

  DBI::dbExecute(con, "DELETE FROM t")
  expect_equal(nrow(DBI::dbReadTable(con, "t")), 0)

  d1_restore(id, bookmark = bookmark)
  expect_equal(nrow(DBI::dbReadTable(con, "t")), 3)
})
