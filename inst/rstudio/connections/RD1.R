library(RD1)
con <- DBI::dbConnect(RD1::d1(), database_id = "${1:Database name or UUID}")
