#' Cloudflare credentials
#'
#' Resolve the API token and account ID used for D1 requests. Each looks first
#' at its argument, then an option, then an environment variable.
#'
#' @param token A Cloudflare API token. Defaults to the option `RD1.token` or
#'   the environment variable `CLOUDFLARE_API_TOKEN`.
#' @param account_id A Cloudflare account ID. Defaults to the option
#'   `RD1.account_id` or the environment variable `CLOUDFLARE_ACCOUNT_ID`.
#' @return A length-one character string.
#' @name credentials
NULL

#' @rdname credentials
#' @export
d1_token <- function(token = NULL) {
  resolve_cred(token, "RD1.token", "CLOUDFLARE_API_TOKEN", "token")
}

#' @rdname credentials
#' @export
d1_account <- function(account_id = NULL) {
  resolve_cred(
    account_id,
    "RD1.account_id",
    "CLOUDFLARE_ACCOUNT_ID",
    "account_id"
  )
}

resolve_cred <- function(value, opt, env, arg) {
  value <- value %||% getOption(opt) %||% nzchar_or_null(Sys.getenv(env))
  if (is.null(value)) {
    cli::cli_abort(c(
      "No Cloudflare {arg} found.",
      i = "Pass {.arg {arg}}, set {.code options({opt} = )}, or the {.envvar {env}} environment variable."
    ))
  }
  value
}

# httr2 request against the D1 database resource, with auth and error parsing.
d1_req <- function(account_id, ..., token = d1_token()) {
  httr2::request("https://api.cloudflare.com/client/v4") |>
    httr2::req_auth_bearer_token(token) |>
    httr2::req_user_agent("RD1 (https://github.com/noamross/RD1)") |>
    httr2::req_url_path_append("accounts", account_id, "d1", "database", ...) |>
    httr2::req_error(body = d1_error_body) |>
    httr2::req_retry(max_tries = 3)
}

# Build, perform, and unwrap a D1 API call in one step.
d1_call <- function(
  account_id,
  ...,
  token = d1_token(),
  method = NULL,
  body = NULL,
  query = NULL
) {
  req <- d1_req(account_id, ..., token = token)
  if (!is.null(body)) {
    req <- httr2::req_body_json(req, compact(body))
  }
  if (!is.null(query)) {
    req <- httr2::req_url_query(req, !!!compact(query))
  }
  if (!is.null(method)) {
    req <- httr2::req_method(req, method)
  }
  d1_perform(req)
}

# Perform a request and return the `result` from the Cloudflare envelope.
d1_perform <- function(req) {
  resp <- httr2::req_perform(req)
  body <- httr2::resp_body_json(resp, simplifyVector = FALSE)
  if (!isTRUE(body$success)) {
    d1_abort(body$errors)
  }
  body$result
}

# Extract human-readable error strings from a failed response for httr2.
d1_error_body <- function(resp) {
  if (!grepl("json", httr2::resp_content_type(resp), fixed = TRUE)) {
    return(NULL)
  }
  fmt_errors(httr2::resp_body_json(resp)$errors)
}

d1_abort <- function(errors) {
  cli::cli_abort(c(
    "D1 API request failed.",
    set_names(fmt_errors(errors), "x")
  ))
}

fmt_errors <- function(errors) {
  if (length(errors) == 0) {
    return("Unknown error.")
  }
  vapply(
    errors,
    function(e) {
      paste0(e$message, if (length(e$code)) paste0(" [", e$code, "]"))
    },
    character(1)
  )
}
