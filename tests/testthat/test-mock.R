# These tests mock the HTTP layer with httr2, so they need no credentials or
# network. They cover branches the live tests can't reach reliably: error
# handling, the export/import polling loops, and request construction.

uuid <- "00000000-0000-0000-0000-000000000000"

test_that("a failed envelope is surfaced as an error", {
  httr2::local_mocked_responses(list(httr2::response_json(
    body = list(
      success = FALSE,
      errors = list(list(code = 7003, message = "Could not route to endpoint")),
      messages = list(),
      result = NULL
    )
  )))
  expect_snapshot(
    d1_query(uuid, "SELECT 1", account_id = "a", token = "t"),
    error = TRUE
  )
})

test_that("d1_export polls until the export is complete", {
  httr2::local_mocked_responses(list(
    httr2::response_json(
      body = list(
        success = TRUE,
        result = list(status = "active", at_bookmark = "bm1", messages = list())
      )
    ),
    httr2::response_json(
      body = list(
        success = TRUE,
        result = list(
          status = "complete",
          result = list(
            filename = "db.sql",
            signed_url = "https://example/db.sql"
          )
        )
      )
    )
  ))
  res <- d1_export(uuid, account_id = "a", token = "t", poll_interval = 0)
  expect_equal(res$signed_url, "https://example/db.sql")
})

test_that("d1_export aborts when the export reports an error", {
  httr2::local_mocked_responses(list(httr2::response_json(
    body = list(
      success = TRUE,
      result = list(status = "error", messages = list("disk full"))
    )
  )))
  expect_snapshot(
    d1_export(uuid, account_id = "a", token = "t", poll_interval = 0),
    error = TRUE
  )
})

test_that("d1_import runs the init / upload / ingest flow", {
  file <- withr::local_tempfile(lines = "CREATE TABLE t (x);")
  httr2::local_mocked_responses(list(
    httr2::response_json(
      body = list(
        success = TRUE,
        result = list(upload_url = "https://upload/target", filename = "f.sql")
      )
    ),
    httr2::response(status_code = 200), # the PUT upload
    httr2::response_json(
      body = list(
        success = TRUE,
        result = list(
          status = "complete",
          result = list(num_queries = 2, final_bookmark = "bm9")
        )
      )
    )
  ))
  res <- d1_import(uuid, file, account_id = "a", token = "t", poll_interval = 0)
  expect_equal(res$final_bookmark, "bm9")
})

test_that("d1_query posts sql and params to the /query endpoint", {
  seen <- NULL
  httr2::local_mocked_responses(function(req) {
    seen <<- req
    httr2::response_json(
      body = list(
        success = TRUE,
        result = list(list(success = TRUE, results = list(), meta = list()))
      )
    )
  })
  d1_query(
    uuid,
    "SELECT * FROM t WHERE a = ?",
    params = list(5L),
    account_id = "a",
    token = "t"
  )
  req <- seen
  expect_equal(req$method, "POST")
  expect_match(req$url, "/query$")
  expect_equal(req$body$data$sql, "SELECT * FROM t WHERE a = ?")
  expect_equal(req$body$data$params, list(5L))
})

test_that("d1_bookmark sends the timestamp as a query parameter", {
  seen <- NULL
  httr2::local_mocked_responses(function(req) {
    seen <<- req
    httr2::response_json(
      body = list(success = TRUE, result = list(bookmark = "bm"))
    )
  })
  out <- d1_bookmark(
    uuid,
    timestamp = as.POSIXct("2024-01-02 03:04:05", tz = "UTC"),
    account_id = "a",
    token = "t"
  )
  req <- seen
  expect_equal(out, "bm")
  expect_match(req$url, "time_travel/bookmark")
  expect_match(req$url, "timestamp=2024-01-02")
})
