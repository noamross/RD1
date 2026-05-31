test_that("databases can be created, listed, fetched, and deleted", {
  skip_if_no_d1()
  id <- local_test_db()

  dbs <- d1_list_databases()
  expect_s3_class(dbs, "data.frame")
  expect_true(id %in% dbs$uuid)

  info <- d1_get_database(id)
  expect_equal(info$uuid, id)
})

test_that("d1_database_id passes UUIDs through and resolves names", {
  skip_if_no_d1()
  id <- local_test_db()
  expect_equal(d1_database_id(id), id)
  expect_equal(d1_database_id(d1_get_database(id)$name), id)
})

test_that("dbConnect accepts a database name", {
  skip_if_no_d1()
  id <- local_test_db()
  name <- d1_get_database(id)$name
  con <- DBI::dbConnect(d1(), database_id = name)
  expect_equal(con@database_id, id)
})
