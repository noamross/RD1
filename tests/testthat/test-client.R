test_that("credentials resolve from argument, then option, then env var", {
  withr::local_envvar(CLOUDFLARE_API_TOKEN = "from-env")
  withr::local_options(RD1.token = NULL)
  expect_equal(d1_token("from-arg"), "from-arg")
  expect_equal(d1_token(), "from-env")
  withr::local_options(RD1.token = "from-opt")
  expect_equal(d1_token(), "from-opt")
})

test_that("missing credentials raise an informative error", {
  withr::local_envvar(CLOUDFLARE_API_TOKEN = "")
  withr::local_options(RD1.token = NULL)
  expect_snapshot(d1_token(), error = TRUE)
})

test_that("compact() drops NULL entries only", {
  expect_equal(
    compact(list(a = 1, b = NULL, c = FALSE)),
    list(a = 1, c = FALSE)
  )
})
