test_that("D1 reuses dbplyr's SQLite translation (offline)", {
  skip_if_not_installed("dbplyr")
  # A UUID-form id passes through without a network call, and with no
  # connection observer present dbConnect makes no requests, so dummy
  # credentials keep this test fully offline (and CI-safe).
  con <- DBI::dbConnect(
    d1(),
    database_id = "00000000-0000-0000-0000-000000000000",
    account_id = "test",
    token = "test"
  )

  expect_equal(dbplyr::dbplyr_edition(con), 2L)
  # Identifier quote style (" vs `) is cosmetic and both work on D1; normalise
  # it to confirm the translation otherwise matches SQLite's.
  norm <- function(x) gsub("`", '"', as.character(x))
  expect_equal(
    norm(dbplyr::translate_sql(as.numeric(x), con = con)),
    norm(dbplyr::translate_sql(as.numeric(x), con = dbplyr::simulate_sqlite()))
  )
})

test_that("dplyr verbs build SQL and collect from D1", {
  skip_if_no_d1()
  skip_if_not_installed("dbplyr")
  con <- DBI::dbConnect(d1(), database_id = local_test_db())
  DBI::dbWriteTable(con, "mt", head(mtcars, 10))

  lazy <- dplyr::tbl(con, "mt")
  expect_match(dbplyr::remote_name(lazy) |> as.character(), "mt")

  out <- lazy |>
    dplyr::filter(cyl == 6) |>
    dplyr::select(mpg, cyl) |>
    dplyr::arrange(dplyr::desc(mpg)) |>
    dplyr::collect()

  expect_s3_class(out, "tbl_df")
  expect_named(out, c("mpg", "cyl"))
  expect_true(all(out$cyl == 6))
  expect_equal(out$mpg, sort(out$mpg, decreasing = TRUE))
})

test_that("dplyr aggregation translates to a GROUP BY query", {
  skip_if_no_d1()
  skip_if_not_installed("dbplyr")
  con <- DBI::dbConnect(d1(), database_id = local_test_db())
  DBI::dbWriteTable(con, "mt", head(mtcars, 12))

  out <- dplyr::tbl(con, "mt") |>
    dplyr::group_by(cyl) |>
    dplyr::summarise(n = dplyr::n(), mean_mpg = mean(mpg, na.rm = TRUE)) |>
    dplyr::collect()

  expect_setequal(names(out), c("cyl", "n", "mean_mpg"))
  expect_equal(sum(out$n), 12)
})
