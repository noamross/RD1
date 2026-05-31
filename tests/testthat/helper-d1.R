# Skip a test unless we are online and have Cloudflare credentials.
skip_if_no_d1 <- function() {
  testthat::skip_if_offline()
  if (
    !nzchar(Sys.getenv("CLOUDFLARE_API_TOKEN")) ||
      !nzchar(Sys.getenv("CLOUDFLARE_ACCOUNT_ID"))
  ) {
    testthat::skip("No Cloudflare credentials")
  }
}

# Create a throwaway D1 database, deleting it when the calling test ends.
local_test_db <- function(env = parent.frame()) {
  name <- paste0("r2d1-test-", as.integer(Sys.time()), "-", Sys.getpid())
  db <- d1_create_database(name)
  withr::defer(d1_delete_database(db$uuid), envir = env)
  db$uuid
}
