test_that("split_statements respects semicolons inside string literals", {
  sql <- "INSERT INTO t VALUES ('a;b'); DROP TABLE t;"
  expect_equal(
    split_statements(sql),
    c("INSERT INTO t VALUES ('a;b');", "DROP TABLE t;")
  )
})

test_that("d1_export returns a signed download URL", {
  skip_if_no_d1()
  con <- DBI::dbConnect(D1(), database_id = (id <- local_test_db()))
  DBI::dbWriteTable(con, "t", data.frame(x = 1:3L))

  res <- d1_export(id)
  expect_match(res$signed_url, "^https://")
})

test_that("d1_download writes a SQL dump to disk", {
  skip_if_no_d1()
  con <- DBI::dbConnect(D1(), database_id = (id <- local_test_db()))
  DBI::dbWriteTable(con, "t", data.frame(x = 1:3L))

  path <- withr::local_tempfile(fileext = ".sql")
  d1_download(id, path)
  expect_match(paste(readLines(path), collapse = "\n"), "CREATE TABLE")
})
