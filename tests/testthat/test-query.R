test_that("simplify_col types cells like SQLite", {
  expect_identical(simplify_col(list(1, 2, 3)), c(1L, 2L, 3L))
  expect_identical(simplify_col(list(1.5, 2, NULL)), c(1.5, 2, NA))
  expect_identical(simplify_col(list("a", "b")), c("a", "b"))
  expect_identical(simplify_col(list()), logical())
})

test_that("raw_to_df builds a typed frame, including the zero-row case", {
  df <- raw_to_df(list(
    columns = list("a", "b"),
    rows = list(list(1, "x"), list(2, "y"))
  ))
  expect_named(df, c("a", "b"))
  expect_identical(df$a, c(1L, 2L))

  empty <- raw_to_df(list(columns = list("a", "b"), rows = list()))
  expect_named(empty, c("a", "b"))
  expect_equal(nrow(empty), 0)
})

test_that("obj_to_df rectangles row objects", {
  df <- obj_to_df(list(list(a = 1, b = "x"), list(a = 2, b = "y")))
  expect_named(df, c("a", "b"))
  expect_identical(df$b, c("x", "y"))
})

test_that("d1_query runs SQL and returns a data frame", {
  skip_if_no_d1()
  id <- local_test_db()
  d1_query(id, "CREATE TABLE t (x INTEGER, y TEXT)")
  d1_query(id, "INSERT INTO t VALUES (?, ?)", params = list(1L, "a"))
  out <- d1_query(id, "SELECT * FROM t")
  expect_equal(out, data.frame(x = 1L, y = "a"))
})
