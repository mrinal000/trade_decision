if (!requireNamespace("R6", quietly = TRUE)) {
  cat("R6 package not installed, skipping checklist_module test\n")
  quit(status = 0)
}

source("R/checklist_module.R")

cm <- ChecklistModule$new()
res <- cm$evaluate(
  base_count = 3,
  leg_out = "Strong",
  voz = TRUE,
  fresh5m = TRUE,
  rr = 3.5,
  risk_pct = 1.5,
  oe = 15,
  decision_match = TRUE
)

stopifnot(res$passed)
cat("checklist_module test passed\n")
