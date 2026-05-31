test_that("d1_data_type maps R types like RSQLite", {
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  withr::defer(DBI::dbDisconnect(con))
  for (x in list(1L, 1.5, "a", TRUE, as.raw(1))) {
    expect_equal(d1_data_type(x), DBI::dbDataType(con, x))
  }
})

test_that("a full DBI table lifecycle works", {
  skip_if_no_d1()
  con <- DBI::dbConnect(D1(), database_id = local_test_db())

  DBI::dbWriteTable(con, "people", data.frame(id = 1:2L, name = c("a", "b")))
  expect_true(DBI::dbExistsTable(con, "people"))
  expect_equal(DBI::dbListTables(con), "people")
  expect_equal(DBI::dbListFields(con, "people"), c("id", "name"))

  out <- DBI::dbReadTable(con, "people")
  expect_equal(out, data.frame(id = 1:2L, name = c("a", "b")))

  DBI::dbRemoveTable(con, "people")
  expect_false(DBI::dbExistsTable(con, "people"))
})

test_that("parameterised queries and dbExecute report rows affected", {
  skip_if_no_d1()
  con <- DBI::dbConnect(D1(), database_id = local_test_db())
  DBI::dbWriteTable(
    con,
    "t",
    data.frame(g = c(1L, 1L, 2L), v = c("x", "y", "z"))
  )

  got <- DBI::dbGetQuery(con, "SELECT v FROM t WHERE g = ?", params = list(1L))
  expect_equal(got$v, c("x", "y"))

  n <- DBI::dbExecute(con, "DELETE FROM t WHERE g = ?", params = list(1L))
  expect_equal(n, 2L)
})

test_that("dbWriteTable respects overwrite and append", {
  skip_if_no_d1()
  con <- DBI::dbConnect(D1(), database_id = local_test_db())
  d <- data.frame(x = 1:2L)
  DBI::dbWriteTable(con, "t", d)
  DBI::dbWriteTable(con, "t", d, append = TRUE)
  expect_equal(nrow(DBI::dbReadTable(con, "t")), 4)
  DBI::dbWriteTable(con, "t", d, overwrite = TRUE)
  expect_equal(nrow(DBI::dbReadTable(con, "t")), 2)
})
