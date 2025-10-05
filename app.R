# app.R
# ------------------------------------------------------------------------------
# NeoStock Decision Assistant (Shiny)
# - Two-step workflow: Evaluate (logic only) and Save (persist with Order ID).
# - Uses R6 modules:
#     * DecisionModule      -> chooses scenario + eligibility based on DMT rules
#     * ChecklistModule     -> applies the 8-point checklist gates
#     * DBModule            -> handles DuckDB storage (journal)
# - UI/UX via bslib; persistence via DuckDB.
# ------------------------------------------------------------------------------

# ---- Libraries ----------------------------------------------------------------
library(shiny)
library(bslib)
library(duckdb)
library(DBI)
library(DT)
library(memoise)
library(magrittr)
library(writexl)

# ---- Helpers ------------------------------------------------------------------
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) {
    return(y)
  }
  if (length(x) == 1 && (is.na(x) || (is.character(x) && !nzchar(x)))) {
    return(y)
  }
  x
}

pretty_scenario <- function(code) {
  if (is.null(code) || is.na(code) || !nzchar(code)) return("—")
  prefix <- substr(code, 1, 2)
  prefix_name <- switch(prefix,
    TL = "Trending Long",
    TS = "Trending Short",
    SL = "Sideways Long",
    SS = "Sideways Short",
    prefix
  )
  rest <- substr(code, 4, nchar(code))
  if (!nzchar(rest)) return(prefix_name)
  rest <- gsub("_", " ", rest, fixed = TRUE)
  rest <- tools::toTitleCase(tolower(rest))
  paste(prefix_name, rest, sep = " - ")
}

# ---- Modules ------------------------------------------------------------------
# These files must exist: R/decision_module.R, R/checklist_module.R, R/db_module.R
source("R/decision_module.R", local = TRUE)
source("R/checklist_module.R", local = TRUE)
source("R/db_module.R",      local = TRUE)
source("R/bundle_resolver.R", local = TRUE)

# ---- Engines ------------------------------------------------------------------
decision_engine  <- DecisionModule$new()
checklist_engine <- ChecklistModule$new()
db_engine        <- DBModule$new()  # creates/opens DuckDB and ensures schema

# Memoised wrappers for expensive computations
decision_cached  <- memoise(decision_engine$determine)
checklist_cached <- memoise(checklist_engine$evaluate)

# ---- UI -----------------------------------------------------------------------
ui <- page_sidebar(
  title = "NeoStock Decision Assistant",
  theme = bs_theme(bootswatch = "minty"),
  
  # Sidebar (inputs)
  sidebar = sidebar(
    
    # ---- Decision Table Inputs ----
    selectInput("regime",   "Regime",     choices = c("Trending", "Sideways")),
    selectInput("side",     "Side",       choices = c("Long", "Short")),
    selectInput("htf_dir",  "HTF Direction",
                choices = c("D2S", "S2D", "D2ATH", "ATH2D", "S2ATL", "ATL2S")),
    selectInput("itf_dir",  "ITF Direction",
                choices = c("D2S", "S2D")),
    selectInput("htf_trend","HTF Trend",  choices = c("Up", "Down", "Sideways")),
    selectInput("itf_trend","ITF Trend",  choices = c("Up", "Down", "Sideways")),
    selectInput("curve",    "Curve",      choices = c("LC", "EQ", "HC")),
    selectInput("confluence","Confluence (Sideways)",
                choices = c("None", "LTF_DZ_with_ITF_DZ", "LTF_SZ_with_ITF_SZ")),
    
    # ---- Checklist Inputs ----
    numericInput("base_count", "Base candles (<= 6)",               value = 4,  min = 0, step = 1),
    selectInput("leg_out",     "Leg-out",                           choices = c("Strong","Sluggish")),
    checkboxInput("voz",       "VOZ achieved?",                     value = TRUE),
    checkboxInput("fresh5m",   "Fresh fine-tuned 5m?",              value = TRUE),
    numericInput("rr",         "Planned RR (>= 3; >= 5 if beginner)", value = 3, min = 0, step = 0.5),
    numericInput("risk_pct",   "Max Risk % of Capital (<= 2)",      value = 2,  min = 0, step = 0.1),
    numericInput("oe",         "OE Score (>= 12)",                  value = 12, min = 0, step = 1),
    
    # ---- Save Controls ----
    textInput("order_id",      "Order ID (required to save)", value = ""),
    div(
      class = "mt-2",
      actionButton("evaluate",    "Evaluate"),
      actionButton("save_record", "Save", class = "btn-primary")
    )
  ),
  
  # Main panel (outputs)
  navset_tab(
    id = "main_tabs",
    nav_panel(
      "Single TF",
      div(
        class = "mt-4",
        h3("Decision Output"),
        uiOutput("decision_ui"),

        h3("Checklist Output"),
        uiOutput("checklist_ui"),

        h3("Summary"),
        uiOutput("summary_ui"),

        h3("Journal"),
        div(
          actionButton("clear_log", "Clear Journal", class = "btn-danger mb-2"),
          DT::dataTableOutput("log_table"),
          div(
            class = "mt-2",
            downloadButton("download_log_csv", "Download CSV"),
            downloadButton("download_log_xlsx", "Download Excel", class = "ms-2")
          )
        )
      )
    ),
    nav_panel(
      "Decision Pairs",
      div(
        class = "mt-4",
        card(
          card_header("Snapshot Editor"),
          card_body(
            div(
              class = "d-flex justify-content-end mb-3",
              actionButton("dp_evaluate", "Evaluate", class = "btn-primary")
            ),
            uiOutput("dp_snapshot_editor")
          )
        ),
        card(
          card_header("Results"),
          card_body(
            DTOutput("dp_results")
          )
        )
      )
    )
  )
)

# ---- Server -------------------------------------------------------------------
server <- function(input, output, session) {

  # Snapshot of last evaluation (used when saving)
  last_decision  <- reactiveVal(NULL)
  last_checklist <- reactiveVal(NULL)
  last_confluence <- reactiveVal(NULL)
  log_data <- reactiveVal(db_engine$get_log(200))

  # ---- Decision Pairs state -------------------------------------------------
  dp_default_snapshots <- data.frame(
    tf_label = c("1m", "5m", "15m", "75m", "Daily", "Weekly", "Monthly"),
    trend = rep("", 7),
    direction = rep("", 7),
    curve = rep("", 7),
    confluence = rep("None", 7),
    stringsAsFactors = FALSE
  )
  dp_results <- reactiveVal(NULL)

  tf_to_id <- function(tf) {
    gsub("[^A-Za-z0-9]", "_", tolower(tf))
  }
  snapshot_input_id <- function(tf, field) {
    paste0("dp_", tf_to_id(tf), "_", field)
  }

  trend_choices <- c("Select…" = "", "Up", "Down", "Sideways")
  direction_choices <- c(
    "Select…" = "",
    "D2S", "S2D", "D2ATH", "ATH2D", "S2ATL", "ATL2S"
  )
  curve_choices <- c("Select…" = "", "LC", "EQ", "HC")
  confluence_choices <- c(
    "None", "LTF_DZ_with_ITF_DZ", "LTF_SZ_with_ITF_SZ"
  )

  output$dp_snapshot_editor <- renderUI({
    snapshots <- dp_default_snapshots
    tagList(
      lapply(seq_len(nrow(snapshots)), function(i) {
        row <- snapshots[i, ]
        tf <- row$tf_label
        card(
          class = "mb-3",
          card_header(tf),
          card_body(
            layout_column_wrap(
              width = 1/3,
              selectInput(
                snapshot_input_id(tf, "trend"),
                "Trend",
                choices = trend_choices,
                selected = row$trend
              ),
              selectInput(
                snapshot_input_id(tf, "direction"),
                "Direction",
                choices = direction_choices,
                selected = row$direction
              ),
              selectInput(
                snapshot_input_id(tf, "curve"),
                "Curve",
                choices = curve_choices,
                selected = row$curve
              ),
              selectInput(
                snapshot_input_id(tf, "confluence"),
                "Confluence",
                choices = confluence_choices,
                selected = row$confluence
              )
            )
          )
        )
      })
    )
  })

  collect_dp_snapshots <- function() {
    df <- dp_default_snapshots
    for (i in seq_len(nrow(df))) {
      tf <- df$tf_label[i]
      df$trend[i] <- input[[snapshot_input_id(tf, "trend")]] %||% ""
      df$direction[i] <- input[[snapshot_input_id(tf, "direction")]] %||% ""
      df$curve[i] <- input[[snapshot_input_id(tf, "curve")]] %||% ""
      df$confluence[i] <- input[[snapshot_input_id(tf, "confluence")]] %||% "None"
    }
    df
  }

  output$dp_results <- DT::renderDT({
    res <- dp_results()
    if (is.null(res)) {
      return(NULL)
    }
    DT::datatable(res, rownames = FALSE, options = list(dom = "t", paging = FALSE, ordering = FALSE))
  })

  normalize_trend <- function(value, tf_label) {
    raw <- trimws(as.character(value %||% ""))
    if (!nzchar(raw)) {
      return(list(value = NA_character_, ok = FALSE, note = sprintf("Trend missing for %s", tf_label)))
    }
    mapped <- c("up" = "Up", "down" = "Down", "sideways" = "Sideways")[[tolower(raw)]]
    if (is.null(mapped)) {
      return(list(value = raw, ok = FALSE, note = sprintf("Invalid trend '%s' for %s", raw, tf_label)))
    }
    list(value = mapped, ok = TRUE, note = NULL)
  }

  normalize_direction <- function(value, tf_label) {
    raw <- trimws(as.character(value %||% ""))
    if (!nzchar(raw)) {
      return(list(value = NA_character_, ok = FALSE, note = sprintf("Direction missing for %s", tf_label)))
    }
    normalized <- toupper(gsub("\\s+", "", raw))
    allowed <- c("D2S", "S2D", "D2ATH", "ATH2D", "S2ATL", "ATL2S")
    if (!normalized %in% allowed) {
      return(list(value = normalized, ok = FALSE, note = sprintf("Invalid direction '%s' for %s", raw, tf_label)))
    }
    list(value = normalized, ok = TRUE, note = NULL)
  }

  normalize_curve <- function(value, tf_label, required = TRUE) {
    raw <- trimws(as.character(value %||% ""))
    if (!nzchar(raw)) {
      if (required) {
        return(list(value = NA_character_, ok = FALSE, note = "HTF curve missing"))
      }
      return(list(value = NA_character_, ok = TRUE, note = NULL))
    }
    normalized <- toupper(gsub("\\s+", "", raw))
    allowed <- c("LC", "EQ", "HC")
    if (!normalized %in% allowed) {
      return(list(value = normalized, ok = FALSE, note = sprintf("Invalid curve '%s' for %s", raw, tf_label)))
    }
    list(value = normalized, ok = TRUE, note = NULL)
  }

  normalize_confluence <- function(value, tf_label) {
    raw <- trimws(as.character(value %||% ""))
    if (!nzchar(raw)) {
      return(list(value = "None", ok = TRUE, note = NULL))
    }
    mapping <- c(
      "NONE" = "None",
      "LTF_DZ_WITH_ITF_DZ" = "LTF_DZ_with_ITF_DZ",
      "LTF_SZ_WITH_ITF_SZ" = "LTF_SZ_with_ITF_SZ"
    )
    normalized <- mapping[[toupper(raw)]]
    if (is.null(normalized)) {
      return(list(value = raw, ok = FALSE, note = sprintf("Invalid confluence '%s' for %s", raw, tf_label)))
    }
    list(value = normalized, ok = TRUE, note = NULL)
  }

  infer_side <- function(itf_direction) {
    raw <- toupper(trimws(as.character(itf_direction %||% "")))
    if (!nzchar(raw)) {
      return(list(value = NA_character_, ok = FALSE, note = "ITF direction missing for side inference"))
    }
    long_dirs <- c("D2S", "D2ATH", "ATH2D")
    short_dirs <- c("S2D", "S2ATL", "ATL2S")
    if (raw %in% long_dirs) {
      return(list(value = "Long", ok = TRUE, note = NULL))
    }
    if (raw %in% short_dirs) {
      return(list(value = "Short", ok = TRUE, note = NULL))
    }
    list(value = NA_character_, ok = FALSE, note = sprintf("Unable to infer trade side from ITF direction '%s'", raw))
  }

  observeEvent(input$dp_evaluate, {
    snapshots <- collect_dp_snapshots()
    snapshot_lookup <- split(snapshots, snapshots$tf_label)

    trios <- list(
      list(exec_tf = "5m", itf = "15m", htf = "75m"),
      list(exec_tf = "15m", itf = "75m", htf = "Daily"),
      list(exec_tf = "75m", itf = "Daily", htf = "Weekly"),
      list(exec_tf = "Daily", itf = "Weekly", htf = "Monthly")
    )

    results <- lapply(trios, function(trio) {
      notes <- character()
      valid <- TRUE

      ltf_row <- snapshot_lookup[[trio$exec_tf]]
      itf_row <- snapshot_lookup[[trio$itf]]
      htf_row <- snapshot_lookup[[trio$htf]]

      if (is.null(ltf_row)) {
        notes <- c(notes, sprintf("Snapshot missing for %s", trio$exec_tf))
        valid <- FALSE
      }
      if (is.null(itf_row)) {
        notes <- c(notes, sprintf("Snapshot missing for %s", trio$itf))
        valid <- FALSE
      }
      if (is.null(htf_row)) {
        notes <- c(notes, sprintf("Snapshot missing for %s", trio$htf))
        valid <- FALSE
      }

      ltf_trend <- NA_character_
      ltf_dir <- NA_character_
      itf_trend <- NA_character_
      itf_dir <- NA_character_
      itf_confluence <- "None"
      htf_trend <- NA_character_
      htf_dir <- NA_character_
      htf_curve <- NA_character_

      if (!is.null(ltf_row)) {
        trend_info <- normalize_trend(ltf_row$trend, trio$exec_tf)
        dir_info <- normalize_direction(ltf_row$direction, trio$exec_tf)
        if (!trend_info$ok) {
          notes <- c(notes, trend_info$note)
          valid <- FALSE
        }
        if (!dir_info$ok) {
          notes <- c(notes, dir_info$note)
          valid <- FALSE
        }
        ltf_trend <- trend_info$value
        ltf_dir <- dir_info$value
      }

      inferred_side <- list(value = NA_character_, ok = FALSE, note = "ITF context missing")

      if (!is.null(itf_row)) {
        trend_info <- normalize_trend(itf_row$trend, trio$itf)
        dir_info <- normalize_direction(itf_row$direction, trio$itf)
        conf_info <- normalize_confluence(itf_row$confluence, trio$itf)
        if (!trend_info$ok) {
          notes <- c(notes, trend_info$note)
          valid <- FALSE
        }
        if (!dir_info$ok) {
          notes <- c(notes, dir_info$note)
          valid <- FALSE
        }
        if (!conf_info$ok) {
          notes <- c(notes, conf_info$note)
          valid <- FALSE
        }
        itf_trend <- trend_info$value
        itf_dir <- dir_info$value
        itf_confluence <- conf_info$value
        inferred_side <- infer_side(itf_dir)
        if (!inferred_side$ok) {
          notes <- c(notes, inferred_side$note)
          valid <- FALSE
        }
      }

      if (!is.null(htf_row)) {
        trend_info <- normalize_trend(htf_row$trend, trio$htf)
        dir_info <- normalize_direction(htf_row$direction, trio$htf)
        curve_info <- normalize_curve(htf_row$curve, trio$htf, required = TRUE)
        if (!trend_info$ok) {
          notes <- c(notes, trend_info$note)
          valid <- FALSE
        }
        if (!dir_info$ok) {
          notes <- c(notes, dir_info$note)
          valid <- FALSE
        }
        if (!curve_info$ok) {
          notes <- c(notes, curve_info$note)
          valid <- FALSE
        }
        htf_trend <- trend_info$value
        htf_dir <- dir_info$value
        htf_curve <- curve_info$value
      }

      regime <- if (!is.na(itf_trend) && identical(itf_trend, "Sideways")) "Sideways" else "Trending"

      if (regime == "Sideways") {
        if (!inferred_side$ok) {
          notes <- c(notes, "Unable to determine trade side for sideways evaluation")
          valid <- FALSE
        } else {
          expected_conf <- if (identical(inferred_side$value, "Long")) "LTF_DZ_with_ITF_DZ" else "LTF_SZ_with_ITF_SZ"
          if (!identical(itf_confluence, expected_conf)) {
            notes <- c(notes, "Sideways ITF requires proper confluence")
            valid <- FALSE
          }
        }
      }

      scenario <- NA_character_
      decision_notes <- character()
      decision_eligible <- FALSE
      trade_side_display <- inferred_side$value

      if (valid) {
        dec <- decision_engine$determine(
          regime = regime,
          side = inferred_side$value,
          htf_dir = htf_dir,
          itf_dir = itf_dir,
          htf_trend = htf_trend,
          itf_trend = itf_trend,
          curve = htf_curve,
          confluence = !identical(itf_confluence, "None")
        )
        scenario <- dec$scenario %||% NA_character_
        decision_notes <- dec$reasons %||% character(0)
        decision_eligible <- isTRUE(dec$eligible)
        if (!is.null(dec$candidate_side) && !is.na(dec$candidate_side)) {
          trade_side_display <- tools::toTitleCase(dec$candidate_side)
        }
      }

      all_notes <- unique(c(notes, decision_notes))
      all_notes <- all_notes[nzchar(all_notes)]

      eligible_flag <- valid && decision_eligible

      human_readable <- paste0(
        "Exec TF ", trio$exec_tf, " (ITF ", trio$itf, ", HTF ", trio$htf, "): ",
        if (eligible_flag) "Eligible" else "Not eligible", " for ", trade_side_display %||% "Unknown",
        " — Scenario: ", scenario %||% "N/A",
        if (length(all_notes)) paste0(" | Notes: ", paste(all_notes, collapse = "; ")) else ""
      )

      data.frame(
        exec_tf = trio$exec_tf,
        itf = trio$itf,
        htf = trio$htf,
        inferred_side = inferred_side$value %||% NA_character_,
        trade_side = trade_side_display %||% NA_character_,
        regime = regime,
        scenario = scenario %||% NA_character_,
        eligible = eligible_flag,
        notes = if (length(all_notes)) paste(all_notes, collapse = "; ") else "",
        human_readable = human_readable,
        stringsAsFactors = FALSE
      )
    })

    dp_results(do.call(rbind, results))
  })
  # Utility: render reasons as a bullet list (if any)
  render_reasons <- function(items) {
    items <- items %||% character(0)
    if (!length(items)) return(NULL)
    tags$ul(lapply(items, tags$li))
  }
  
  # ---- Evaluate (no DB write) ----
  observeEvent(input$evaluate, {
    confluence_str <- input$confluence
    # 1) Decision table logic
    dec <- decision_cached(
      regime    = input$regime,
      side      = input$side,
      htf_dir   = input$htf_dir,
      itf_dir   = input$itf_dir,
      htf_trend = input$htf_trend,
      itf_trend = input$itf_trend,
      curve     = input$curve,
      confluence = confluence_str != "None"
    )

    # 2) Checklist logic
    chk <- checklist_cached(
      base_count    = input$base_count,
      leg_out       = input$leg_out,
      voz           = input$voz,
      fresh5m       = input$fresh5m,
      rr            = input$rr,
      risk_pct      = input$risk_pct,
      oe            = input$oe,
      decision_match = dec$eligible
    )
    
    # 3) Outputs (UI)
    output$decision_ui <- renderUI({
      tagList(
        p(strong("Scenario: "), pretty_scenario(dec$scenario)),
        p(strong("Eligible: "), if (isTRUE(dec$eligible)) "Yes" else "No"),
        render_reasons(dec$reasons)
      )
    })
    
    output$checklist_ui <- renderUI({
      tagList(
        p(strong("Checklist pass: "), if (isTRUE(chk$passed)) "Yes" else "No"),
        render_reasons(chk$reasons)
      )
    })
    
    output$summary_ui <- renderUI({
      tagList(
        p(
          strong("Side: "), input$side, "  |  ",
          strong("Regime: "), input$regime, "  |  ",
          strong("Curve: "), input$curve, "  |  ",
          strong("Confluence: "), confluence_str
        )
      )
    })

    # 4) Cache for Save
    last_decision(dec)
    last_checklist(chk)
    last_confluence(confluence_str)
  })
  
  # ---- Save (requires Order ID + prior Evaluate) ----
  observeEvent(input$save_record, {
    # Require evaluation
    if (is.null(last_decision()) || is.null(last_checklist())) {
      showNotification("Please Evaluate before saving.", type = "warning")
      return()
    }
    # Require Order ID
    if (!nzchar(trimws(input$order_id))) {
      showNotification("Order ID is required to save.", type = "error")
      return()
    }
    
    dec <- last_decision()
    chk <- last_checklist()
    reasons_concat <- paste(c(dec$reasons %||% character(0), chk$reasons %||% character(0)),
                            collapse = "; ")
    
    # Attempt to write to DB
    ok <- try({
      db_engine$log_trade(
        ts            = Sys.time(),
        order_id      = input$order_id,
        regime        = input$regime,
        side          = input$side,
        htf_dir       = input$htf_dir,
        itf_dir       = input$itf_dir,
        htf_trend     = input$htf_trend,
        itf_trend     = input$itf_trend,
        curve         = input$curve,
        confluence    = last_confluence(),
        scenario      = dec$scenario %||% NA_character_,
        eligible      = isTRUE(dec$eligible),
        reasons       = reasons_concat,
        base_count    = input$base_count,
        leg_out       = input$leg_out,
        voz           = isTRUE(input$voz),
        fresh5m       = isTRUE(input$fresh5m),
        rr            = input$rr,
        risk_pct      = input$risk_pct,
        oe            = input$oe,
        checklist_pass = isTRUE(chk$passed)
      )
      TRUE
    }, silent = TRUE)
    
    if (identical(ok, TRUE)) {
      showNotification("Saved.", type = "message")
      # refresh journal data
      log_data(db_engine$get_log(200))
    } else {
      showNotification("Save failed. See console for details.", type = "error")
      message("Save error: ", conditionMessage(attr(ok, "condition") %||% simpleError("unknown")))
    }
  })

  # ---- Clear Journal ----
  observeEvent(input$clear_log, {
    db_engine$clear_log()
    log_data(db_engine$get_log(200))
    showNotification("Journal cleared.", type = "message")
  })

  # ---- Journal (initial render) ----
  output$log_table <- DT::renderDataTable({
    df <- log_data()
    if (!is.null(df) && "scenario" %in% names(df)) {
      df$scenario <- vapply(df$scenario, pretty_scenario, character(1))
    }
    DT::datatable(df, options = list(pageLength = 10))
  })

  output$download_log_csv <- downloadHandler(
    filename = function() paste0("journal-", Sys.Date(), ".csv"),
    content = function(file) {
      df <- log_data()
      if (!is.null(df) && "scenario" %in% names(df)) {
        df$scenario <- vapply(df$scenario, pretty_scenario, character(1))
      }
      write.csv(df, file, row.names = FALSE)
    }
  )

  output$download_log_xlsx <- downloadHandler(
    filename = function() paste0("journal-", Sys.Date(), ".xlsx"),
    content = function(file) {
      df <- log_data()
      if (!is.null(df) && "scenario" %in% names(df)) {
        df$scenario <- vapply(df$scenario, pretty_scenario, character(1))
      }
      writexl::write_xlsx(df, file)
    }
  )
  
  # ---- Cleanup ----
  onStop(function() db_engine$disconnect())
}

# ---- Run app ------------------------------------------------------------------
shinyApp(ui, server)
