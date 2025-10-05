#' Bundle resolver helpers
#'
#' Provides static mappings between execution timeframes (LTF) and their
#' corresponding intermediate (ITF) and higher (HTF) timeframes for the
#' multi-timeframe evaluation workflow. These bundle definitions are used by
#' the Multi-TF tab to determine which snapshot rows should be paired together
#' when running the decision table and checklist logic.
get_bundle_map <- function(bundle_id) {
  if (identical(bundle_id, "intra_v1")) {
    list(
      "1m" = list(LTF = "1m", ITF = "15m", HTF = "75m"),
      "5m" = list(LTF = "5m", ITF = "75m", HTF = "Daily"),
      "15m" = list(LTF = "15m", ITF = "75m", HTF = "Daily"),
      "75m" = list(LTF = "75m", ITF = "Daily", HTF = "Weekly")
    )
  } else {
    list(
      "Daily" = list(LTF = "Daily", ITF = "Weekly", HTF = "Monthly"),
      "Weekly" = list(LTF = "Weekly", ITF = "Monthly", HTF = "Monthly")
    )
  }
}
