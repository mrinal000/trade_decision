if (!requireNamespace("R6", quietly = TRUE)) {
  cat("R6 package not installed, skipping db_module test\n")
  quit(status = 0)
}

if (!requireNamespace("duckdb", quietly = TRUE)) {
  cat("duckdb package not installed, skipping db_module test\n")
  quit(status = 0)
}

source("R/db_module.R")

tmpdb <- tempfile(fileext = ".duckdb")

db <- DBModule$new(tmpdb)

db$log_trade(
  ts = Sys.time(),
  order_id = "1",
  regime = "Trending",
  side = "Long",
  htf_dir = "D2S",
  itf_dir = "D2S",
  htf_trend = "Up",
  itf_trend = "Up",
  curve = "LC",
  confluence = "None",
  scenario = "TL_FULL_ALIGN",
  eligible = TRUE,
  reasons = "",
  base_count = 3,
  leg_out = "Strong",
  voz = TRUE,
  fresh5m = TRUE,
  rr = 3.5,
  risk_pct = 1.5,
  oe = 15,
  checklist_pass = TRUE
)

logs <- db$get_log()
stopifnot(nrow(logs) == 1, logs$order_id[1] == "1")

# Clear the log and ensure it's empty
db$clear_log()
logs <- db$get_log()
stopifnot(nrow(logs) == 0)

db$disconnect()
unlink(tmpdb)

cat("db_module test passed\n")
