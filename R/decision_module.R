##' DecisionModule
##'
##' This R6 class encapsulates the logic for determining the appropriate
##' trading scenario given a set of market context inputs.  It encodes
##' the rules distilled from the Neostock course materials for trending
##' (bullish/bearish) and sideways trade setups.  Users of this class
##' supply high‑level descriptors (trend, price direction, curve
##' location, etc.) and the method returns a structured result with
##' both the identified scenario code and a human readable status.  If
##' the context violates one or more core rules (e.g. trading
##' against the intermediate time frame direction) the result will
##' indicate failure and include reasons.
##'
##' @section Inputs:
##' * `regime`   – "Trending" or "Sideways" (character)
##' * `side`     – "Long" or "Short" (character)
##' * `htf_dir`  – Higher time frame price direction token, one of
##'                "D2S", "S2D", "D2ATH", "ATH2D", "S2ATL", "ATL2S"
##' * `itf_dir`  – Intermediate time frame price direction token, one of
##'                "D2S" or "S2D" (for sideways setups the ITF price direction
##'                is implicit in the ITF trend)
##' * `htf_trend` – Higher time frame trend: "Up", "Down" or "Sideways"
##' * `itf_trend` – Intermediate time frame trend: "Up", "Down" or
##'                 "Sideways"
##' * `curve`    – Location on the curve: "LC" (Low), "EQ" (Equilibrium),
##'                or "HC" (High)
##' * `confluence` – Logical.  For sideways setups this should
##'                  indicate whether the lower time frame zone is in
##'                  confluence with the intermediate time frame zone
##'                  (demand zones for long trades, supply zones for short
##'                  trades).  This argument is ignored for trending
##'                  scenarios.
##'
##' @section Returns:
##' A list with elements:
##'   * `scenario` – A short code representing the matched scenario,
##'                  or `NA_character_` if no scenario matches.
##'   * `eligible` – Logical.  `TRUE` if a valid scenario was found
##'                  and all required directional/trend/curve rules
##'                  passed.
##'   * `candidate_side` – The side implied by the ITF price direction
##'                        ("Long" for D2S, "Short" for S2D).  If this
##'                        does not agree with the supplied `side`,
##'                        eligibility will be `FALSE`.
##'   * `reasons`  – Character vector describing any rule violations.

DecisionModule <- R6::R6Class(
  classname = "DecisionModule",
  public = list(
    initialize = function() {
      invisible(self)
    },

    #' Determine the scenario and eligibility
    #' @inheritParams DecisionModule
    determine = function(regime, side, htf_dir, itf_dir,
                         htf_trend, itf_trend,
                         curve, confluence = TRUE) {
      reasons <- character()
      scenario <- NA_character_
      eligible <- TRUE

      # Normalize input case
      regime <- tolower(regime)
      side <- tolower(side)
      htf_trend <- tolower(htf_trend)
      itf_trend <- tolower(itf_trend)
      curve <- toupper(curve)
      htf_dir <- toupper(htf_dir)
      itf_dir <- toupper(itf_dir)

      # Determine candidate side from ITF direction
      candidate_side <- ifelse(itf_dir == "D2S", "long",
                               ifelse(itf_dir == "S2D", "short", NA_character_))
      if (is.na(candidate_side)) {
        reasons <- c(reasons, sprintf("Invalid ITF direction: %s", itf_dir))
        eligible <- FALSE
      }

      # Check candidate vs supplied side for trending setups
      if (regime == "trending") {
        if (!is.na(candidate_side) && candidate_side != side) {
          reasons <- c(reasons, sprintf(
            "Supplied side (%s) does not match ITF direction implied side (%s)",
            side, candidate_side))
          eligible <- FALSE
        }
        # ITF trend must align with side
        if (candidate_side == "long" && itf_trend != "up") {
          reasons <- c(reasons, "ITF trend must be Up for long trades")
          eligible <- FALSE
        }
        if (candidate_side == "short" && itf_trend != "down") {
          reasons <- c(reasons, "ITF trend must be Down for short trades")
          eligible <- FALSE
        }
        # Determine HTF alignment category
        if (candidate_side == "long") {
          full_align_dirs <- c("D2S", "D2ATH")
          against_dirs <- c("S2D", "ATH2D")
          if (htf_dir %in% full_align_dirs) {
            htf_relation <- "full"
          } else if (htf_dir %in% against_dirs) {
            htf_relation <- "against"
          } else {
            # Unknown direction combination implies no scenario
            reasons <- c(reasons, sprintf(
              "HTF direction %s not recognised for long trades", htf_dir))
            eligible <- FALSE
            htf_relation <- NA_character_
          }
          # Allowed curves based on relation
          allowed_curves <- c()
          if (!is.na(htf_relation)) {
            if (htf_relation == "full") {
              # All curves allowed; except remove HC for D2ATH (no supply above)
              allowed_curves <- c("LC", "EQ", if (htf_dir == "D2S") "HC")
            } else if (htf_relation == "against") {
              # Only LC and EQ allowed
              allowed_curves <- c("LC", "EQ")
            }
          }
          # Check curve in allowed set
          if (!curve %in% allowed_curves) {
            reasons <- c(reasons, sprintf(
              "Curve location %s not allowed for this long scenario", curve))
            eligible <- FALSE
          }
          # Determine scenario code
          if (eligible) {
            if (htf_relation == "full" && htf_dir == "D2S" && itf_dir == "D2S") {
              scenario <- "TL_FULL_ALIGN"
            } else if (htf_relation == "full" && htf_dir == "D2ATH" && itf_dir == "D2S") {
              scenario <- "TL_ATH_ALIGN"
            } else if (htf_relation == "against" && htf_dir == "S2D" && itf_dir == "D2S") {
              scenario <- "TL_AGAINST_HTF"
            } else if (htf_relation == "against" && htf_dir == "ATH2D" && itf_dir == "D2S") {
              scenario <- "TL_ATH_PULLBACK"
            } else if (htf_relation == "full" && htf_dir == "D2ATH" && itf_dir == "D2ATH") {
              scenario <- "TL_BOTH_TO_ATH"
            } else if (htf_relation == "against" && htf_dir == "ATH2D" && itf_dir == "D2ATH") {
              scenario <- "TL_HTF_ATH2D_ITF_ATH"
            } else {
              reasons <- c(reasons, "No matching long scenario for inputs")
              eligible <- FALSE
            }
          }
        }
        if (candidate_side == "short") {
          full_align_dirs <- c("S2D", "S2ATL")
          against_dirs <- c("D2S", "ATL2S")
          if (htf_dir %in% full_align_dirs) {
            htf_relation <- "full"
          } else if (htf_dir %in% against_dirs) {
            htf_relation <- "against"
          } else {
            reasons <- c(reasons, sprintf(
              "HTF direction %s not recognised for short trades", htf_dir))
            eligible <- FALSE
            htf_relation <- NA_character_
          }
          # Allowed curves based on relation
          allowed_curves <- c()
          if (!is.na(htf_relation)) {
            if (htf_relation == "full") {
              allowed_curves <- c("HC", "EQ", if (htf_dir == "S2D") "LC")
            } else if (htf_relation == "against") {
              allowed_curves <- c("HC", "EQ")
            }
          }
          if (!curve %in% allowed_curves) {
            reasons <- c(reasons, sprintf(
              "Curve location %s not allowed for this short scenario", curve))
            eligible <- FALSE
          }
          if (eligible) {
            if (htf_relation == "full" && htf_dir == "S2D" && itf_dir == "S2D") {
              scenario <- "TS_FULL_ALIGN"
            } else if (htf_relation == "full" && htf_dir == "S2ATL" && itf_dir == "S2D") {
              scenario <- "TS_ATL_ALIGN"
            } else if (htf_relation == "against" && htf_dir == "D2S" && itf_dir == "S2D") {
              scenario <- "TS_AGAINST_HTF"
            } else if (htf_relation == "against" && htf_dir == "ATL2S" && itf_dir == "S2D") {
              scenario <- "TS_ATL_BOUNCE"
            } else if (htf_relation == "full" && htf_dir == "S2ATL" && itf_dir == "S2ATL") {
              scenario <- "TS_BOTH_TO_ATL"
            } else if (htf_relation == "against" && htf_dir == "ATL2S" && itf_dir == "S2ATL") {
              scenario <- "TS_HTF_ATL2S_ITF_ATL"
            } else {
              reasons <- c(reasons, "No matching short scenario for inputs")
              eligible <- FALSE
            }
          }
        }
      } # end trending

      if (regime == "sideways") {
        # ITF trend must be sideways
        if (itf_trend != "sideways") {
          reasons <- c(reasons, "For sideways trades the ITF trend must be Sideways")
          eligible <- FALSE
        }
        # Determine candidate side from side argument (explicit)
        # Sideways uses HTF trend and price direction separately
        if (side == "long") {
          # Confluence must be TRUE for long
          if (!isTRUE(confluence)) {
            reasons <- c(reasons, "Long sideways trades require demand zone confluence")
            eligible <- FALSE
          }
          # Allowed htf trends and directions based on the 5 cases
          scenario_candidates <- list(
            SL_UP_D2S  = list(htf_trend = "up", htf_dir = "D2S", curves = c("LC", "EQ")),
            SL_UP_S2D_AGAINST = list(htf_trend = "up", htf_dir = "S2D", curves = c("LC")),
            SL_SW_D2S = list(htf_trend = "sideways", htf_dir = "D2S", curves = c("LC")),
            SL_UP_D2ATH = list(htf_trend = "up", htf_dir = "D2ATH", curves = c("LC")),
            SL_UP_ATH2D = list(htf_trend = "up", htf_dir = "ATH2D", curves = c("LC"))
          )
          match_found <- FALSE
          for (sc in names(scenario_candidates)) {
            cond <- scenario_candidates[[sc]]
            if (htf_trend == cond$htf_trend && htf_dir == cond$htf_dir) {
              # check curve allowed
              if (!curve %in% cond$curves) {
                reasons <- c(reasons, sprintf(
                  "Curve location %s not allowed for scenario %s", curve, sc))
                eligible <- FALSE
              }
              scenario <- sc
              match_found <- TRUE
              break
            }
          }
          if (!match_found) {
            reasons <- c(reasons, "No matching sideways long scenario for inputs")
            eligible <- FALSE
          }
        }
        if (side == "short") {
          if (!isTRUE(confluence)) {
            reasons <- c(reasons, "Short sideways trades require supply zone confluence")
            eligible <- FALSE
          }
          scenario_candidates <- list(
            SS_DN_S2D  = list(htf_trend = "down", htf_dir = "S2D", curves = c("HC", "EQ")),
            SS_DN_D2S_AGAINST = list(htf_trend = "down", htf_dir = "D2S", curves = c("HC")),
            SS_SW_S2D = list(htf_trend = "sideways", htf_dir = "S2D", curves = c("HC")),
            SS_DN_S2ATL = list(htf_trend = "down", htf_dir = "S2ATL", curves = c("HC")),
            SS_DN_ATL2S = list(htf_trend = "down", htf_dir = "ATL2S", curves = c("HC"))
          )
          match_found <- FALSE
          for (sc in names(scenario_candidates)) {
            cond <- scenario_candidates[[sc]]
            if (htf_trend == cond$htf_trend && htf_dir == cond$htf_dir) {
              if (!curve %in% cond$curves) {
                reasons <- c(reasons, sprintf(
                  "Curve location %s not allowed for scenario %s", curve, sc))
                eligible <- FALSE
              }
              scenario <- sc
              match_found <- TRUE
              break
            }
          }
          if (!match_found) {
            reasons <- c(reasons, "No matching sideways short scenario for inputs")
            eligible <- FALSE
          }
        }
      } # end sideways

      # Compose result
      list(
        scenario = ifelse(is.na(scenario), NA_character_, scenario),
        eligible = eligible && !is.na(scenario),
        candidate_side = candidate_side,
        reasons = reasons
      )
    }
  )
)