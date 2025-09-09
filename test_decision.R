if (!requireNamespace("R6", quietly = TRUE)) {
  cat("R6 package not installed, skipping decision_module test\n")
  quit(status = 0)
}

source("R/decision_module.R")

dm <- DecisionModule$new()
res <- dm$determine(
  regime = "Trending",
  side = "Long",
  htf_dir = "D2S",
  itf_dir = "D2S",
  htf_trend = "Up",
  itf_trend = "Up",
  curve = "LC",
  confluence = TRUE
)

stopifnot(res$eligible, res$scenario == "TL_FULL_ALIGN")
cat("decision_module test passed\n")
