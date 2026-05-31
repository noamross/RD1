# Run DBItest's foundational specifications against a live database. Deeper
# groups (SQL, data types, transactions) are not claimed: D1 is stateless over
# HTTP and conveys SQLite storage classes only approximately through JSON.
test_that("RD1 conforms to DBItest foundational specs", {
  skip_if_no_d1()
  skip_if_not_installed("DBItest")
  id <- local_test_db()
  ctx <- DBItest::make_context(
    d1(),
    connect_args = list(database_id = id),
    name = "RD1"
  )
  # D1 has no `bigint` connection argument for tuning 64-bit integer return.
  DBItest::test_getting_started(ctx = ctx)
  DBItest::test_driver(ctx = ctx, skip = "connect_bigint_.*")
  DBItest::test_connection(ctx = ctx)
})
