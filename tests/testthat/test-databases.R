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

test_that("d1_database_id accepts a connection object", {
  con <- DBI::dbConnect(
    d1(),
    database_id = "00000000-0000-0000-0000-000000000000",
    account_id = "a",
    token = "t"
  )
  expect_equal(d1_database_id(con), "00000000-0000-0000-0000-000000000000")
})

test_that("resolve_db inherits credentials from a connection without env vars", {
  withr::local_envvar(CLOUDFLARE_API_TOKEN = "", CLOUDFLARE_ACCOUNT_ID = "")
  withr::local_options(RD1.token = NULL, RD1.account_id = NULL)
  con <- DBI::dbConnect(
    d1(),
    database_id = "00000000-0000-0000-0000-000000000000",
    account_id = "acc",
    token = "tok"
  )
  f <- function(database_id, account_id = d1_account(), token = d1_token()) {
    resolve_db(database_id, account_id, token)
    c(database_id, account_id, token)
  }
  expect_equal(f(con), c("00000000-0000-0000-0000-000000000000", "acc", "tok"))
})

test_that("d1_* functions accept a database name and connection", {
  skip_if_no_d1()
  con <- DBI::dbConnect(d1(), database_id = local_test_db())
  DBI::dbWriteTable(con, "t", data.frame(x = 1:3L))
  name <- d1_get_database(con)$name

  expect_equal(nrow(d1_query(con, "SELECT * FROM t")), 3)
  expect_equal(nrow(d1_query(name, "SELECT * FROM t")), 3)
  expect_type(d1_bookmark(con), "character")
})
