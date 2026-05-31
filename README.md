# RD1

A [DBI](https://dbi.r-dbi.org) client API
wrapper for [Cloudflare D1](https://developers.cloudflare.com/d1/), a
cloud-hosted SQLite database service.

_Nearly fully LLM-developed_

## Installation


Install from R-Universe with:

```r
install.packages("RD1", repos = c('https://noamross.r-universe.dev', 'https://cloud.r-project.org'))
```

Install the development versions from GitHub with

```r
remotes::install_github("noamross/D1")
```

## Credentials

Every function takes `token` and `account_id` arguments. By default they are
read from the `RD1.token` / `RD1.account_id` options or the
`CLOUDFLARE_API_TOKEN` / `CLOUDFLARE_ACCOUNT_ID` environment variables. The
token needs the **D1 : Edit** permission.

## DBI interface

The driver constructor is `d1()` (with aliases `D1()`, `Rd1()`, `RD1()`):

```r
library(DBI)
con <- dbConnect(RD1::d1(), database_id = "your-db-uuid")

dbWriteTable(con, "mtcars", mtcars)
dbListTables(con)
dbGetQuery(con, "SELECT mpg, cyl FROM mtcars WHERE cyl = ?", params = list(6))
```

Column types are coerced to match RSQLite, so a table read from D1 is identical
to the same table read from a downloaded SQLite file. `{dplyr}` verbs work as
expected on D1 tables with SQLite-type SQL translation.  RStudio/Positron
connection pane integration is supported.

## D1 vs SQLite

D1 is SQLite over an HTTP API, which differs from a local file in a few ways:
connections are stateless (no transactions, `dbDisconnect()` is a no-op),
results are materialised in full, and JSON responses lose SQLite storage
classes — so `dbReadTable()` consults `PRAGMA table_info()` to coerce columns
back to the types RSQLite would return. D1 also adds capabilities SQLite lacks,
such as time travel and HTTP export/import. See `?d1` for the full list.

## API wrappers

All D1 endpoints are wrapped as `d1_*()` functions:

```r
library(RD1)

d1_list_databases()
db <- d1_create_database("my-db")
d1_query(db$uuid, "SELECT 1 AS x")

# Time travel
bm <- d1_bookmark(db$uuid)
d1_restore(db$uuid, bookmark = bm)

# Export / import, bridging local SQLite
d1_download_sqlite(db$uuid, "local.sqlite")
d1_upload_sqlite(db$uuid, "local.sqlite")
```

Key functions like bookmarkding and download are also surfaced as actions in
RStudio's connections pane.
