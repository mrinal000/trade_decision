##' ChecklistModule
##'
##' This R6 class encapsulates the eight‑point trade acceptance
##' checklist from the Neostock course.  Given a set of quantitative
##' and qualitative inputs describing the proposed trade, it
##' determines whether the trade passes all criteria.  If any
##' criterion fails, the module returns informative reasons.  The
##' checklist is independent of the scenario determination logic,
##' except that it expects a logical indicator from the DecisionModule
##' signalling that the proposed trade matches a valid decision table.
##'
##' @section Inputs:
##' * `base_count` – Integer.  Number of small bodies in the entry zone base.
##' * `leg_out`   – Character: "Strong" or "Sluggish".  Only "Strong" is accepted.
##' * `voz`       – Logical.  Whether a violated opposing zone achievement is present.
##' * `fresh5m`   – Logical.  Whether the entry zone is fresh and fine‑tuned on the 5‑minute timeframe.
##' * `rr`        – Numeric.  Planned risk/reward ratio.
##' * `risk_pct`  – Numeric.  Percentage of trade capital risked.
##' * `oe`        – Integer.  Odd Enhancer score.
##' * `decision_match` – Logical.  TRUE if the DecisionModule found a matching scenario.
##'
##' @section Returns:
##' A list with elements:
##'   * `passed` – Logical.  TRUE only if all checklist criteria pass.
##'   * `reasons` – Character vector of human readable messages for any
##'                 failed criteria.

ChecklistModule <- R6::R6Class(
  classname = "ChecklistModule",
  public = list(
    initialize = function() {
      invisible(self)
    },
    #' Evaluate checklist
    evaluate = function(base_count, leg_out, voz, fresh5m,
                        rr, risk_pct, oe, decision_match) {
      reasons <- character()
      passed <- TRUE
      # Base candle count <= 6
      if (is.na(base_count) || base_count > 6) {
        reasons <- c(reasons, sprintf("Base candle count (%s) exceeds maximum of 6", base_count))
        passed <- FALSE
      }
      # Leg‑out must be strong
      if (tolower(leg_out) != "strong") {
        reasons <- c(reasons, sprintf("Leg‑out is %s but must be Strong", leg_out))
        passed <- FALSE
      }
      # VOZ must be TRUE
      if (!isTRUE(voz)) {
        reasons <- c(reasons, "Violated opposing zone (VOZ) not achieved")
        passed <- FALSE
      }
      # Fresh 5‑minute zone must be TRUE
      if (!isTRUE(fresh5m)) {
        reasons <- c(reasons, "Zone is not fresh/fine‑tuned on 5‑minute timeframe")
        passed <- FALSE
      }
      # Risk/reward ratio must be >= 3
      if (is.na(rr) || rr < 3) {
        reasons <- c(reasons, sprintf("Risk/reward ratio (%.2f) below minimum of 3", rr))
        passed <- FALSE
      }
      # Risk percentage must be <= 2
      if (is.na(risk_pct) || risk_pct > 2) {
        reasons <- c(reasons, sprintf("Risk percentage (%.2f%%) exceeds maximum of 2%%", risk_pct))
        passed <- FALSE
      }
      # Odd Enhancer score must be >= 12
      if (is.na(oe) || oe < 12) {
        reasons <- c(reasons, sprintf("OE score (%s) below minimum of 12", oe))
        passed <- FALSE
      }
      # Decision table must match
      if (!isTRUE(decision_match)) {
        reasons <- c(reasons, "Trade does not match any valid decision table scenario")
        passed <- FALSE
      }
      list(
        passed = passed,
        reasons = reasons
      )
    }
  )
)