# RD1 0.0.0.9000

* First release. Provides a DBI-compliant client for Cloudflare D1 (`d1()`,
  `dbConnect()`, and the usual DBI verbs) alongside wrappers for every D1 REST
  endpoint: database management, `d1_query()`/`d1_raw()`, export/import, and
  time travel (`d1_bookmark()`, `d1_restore()`).
* Convenience wrappers bridge D1 and local SQLite files: `d1_download_sqlite()`
  and `d1_upload_sqlite()`. Table reads coerce column types to match RSQLite so
  databases round-trip seamlessly.
