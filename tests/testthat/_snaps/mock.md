# a failed envelope is surfaced as an error

    Code
      d1_query(uuid, "SELECT 1", account_id = "a", token = "t")
    Condition
      Error in `d1_abort()`:
      ! D1 API request failed.
      x Could not route to endpoint [7003]

# d1_export aborts when the export reports an error

    Code
      d1_export(uuid, account_id = "a", token = "t", poll_interval = 0)
    Condition
      Error in `d1_export()`:
      ! D1 export failed.
      x disk full

