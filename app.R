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
`%||%` <- function(x, y) if (is.null(x)) y else x

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
      "Multi-TF",
      div(
        class = "mt-4",
        h3("Multi-Timeframe Evaluation"),
        card(
          card_header("Controls"),
          layout_column_wrap(
            width = 1/3,
            textInput("mtf_symbol", "Symbol"),
            selectInput("mtf_side", "Side", choices = c("Long", "Short")),
            selectInput("mtf_bundle", "Bundle", choices = c("intra_v1", "swing_v1")),
            dateInput("mtf_as_of", "As of", value = Sys.Date()),
            numericInput("mtf_base_count", "Base candles (<= 6)", value = 4, min = 0, step = 1),
            selectInput("mtf_leg_out", "Leg-out", choices = c("Strong", "Sluggish")),
            checkboxInput("mtf_voz", "VOZ achieved?", value = TRUE),
            checkboxInput("mtf_fresh5m", "Fresh fine-tuned 5m?", value = TRUE),
            numericInput("mtf_rr", "Planned RR (>= 3; >= 5 if beginner)", value = 3, min = 0, step = 0.5),
            numericInput("mtf_risk_pct", "Max Risk % of Capital (<= 2)", value = 2, min = 0, step = 0.1),
            numericInput("mtf_oe", "OE Score (>= 12)", value = 12, min = 0, step = 1)
          )
        ),
        card(
          card_header("Snapshot Editor"),
          card_body(
            DTOutput("mtf_snapshots")
          )
        ),
        card(
          card_header("Actions"),
          card_body(
            layout_columns(
              col_widths = c(6, 3, 3),
              textInput("mtf_base_order_id", "Base Order ID (required to save)", value = ""),
              div(class = "mt-4", actionButton("mtf_eval", "Evaluate Candidates")),
              div(class = "mt-4", actionButton("mtf_save_taken", "Save Taken", class = "btn-primary"))
            )
          )
        ),
        card(
          card_header("Results"),
          card_body(
            DTOutput("mtf_results")
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

  # ---- Multi-TF state -------------------------------------------------------
  mtf_default_snapshots <- data.frame(
    tf_label = c("1m", "5m", "15m", "75m", "Daily", "Weekly", "Monthly"),
    trend = c("Up", "Up", "Up", "Up", "Up", "Up", "Up"),
    direction = c("D2S", "D2S", "D2S", "D2S", "D2S", "D2S", "D2S"),
    curve = c("", "", "", "EQ", "EQ", "EQ", "EQ"),
    confluence = rep("None", 7),
    ath_flag = rep(FALSE, 7),
    atl_flag = rep(FALSE, 7),
    stringsAsFactors = FALSE
  )
  mtf_snapshots <- reactiveVal(mtf_default_snapshots)
  mtf_results <- reactiveVal(NULL)

  mtf_snapshot_proxy <- dataTableProxy("mtf_snapshots")
  
  output$mtf_snapshots <- DT::renderDT({
    DT::datatable(
      mtf_snapshots(),
      rownames = FALSE,
      options = list(dom = "t", paging = FALSE, ordering = FALSE),
      editable = list(target = "cell", disable = list(columns = c(0)))
    )
  })

  observeEvent(input$mtf_snapshots_cell_edit, {
    info <- input$mtf_snapshots_cell_edit
    i <- info$row
    j <- info$col + 1
    v <- info$value
    df <- mtf_snapshots()
    if (i < 1 || i > nrow(df) || j < 1 || j > ncol(df)) {
      return()
    }
    col_name <- names(df)[j]
    if (col_name %in% c("ath_flag", "atl_flag")) {
      df[i, j] <- tolower(as.character(v)) %in% c("true", "t", "1", "yes")
    } else {
      df[i, j] <- v
    }
    mtf_snapshots(df)
    DT::replaceData(mtf_snapshot_proxy, df, resetPaging = FALSE, rownames = FALSE)
  })

  output$mtf_results <- DT::renderDT({
    res <- mtf_results()
    if (is.null(res)) {
      return(NULL)
    }
    display <- res[, c("exec_tf", "scenario", "eligible", "checklist_pass", "take", "why_not"), drop = FALSE]
    DT::datatable(display, rownames = FALSE, options = list(dom = "t", paging = FALSE, ordering = FALSE))
  })

  normalize_trend <- function(x) {
    val <- trimws(as.character(x %||% ""))
    val_lower <- tolower(val)
    if (val_lower == "up") return("Up")
    if (val_lower == "down") return("Down")
    if (val_lower == "sideways") return("Sideways")
    val
  }

  normalize_direction <- function(x) {
    toupper(trimws(as.character(x %||% "")))
  }

  normalize_curve <- function(x) {
    toupper(trimws(as.character(x %||% "")))
  }

  normalize_confluence <- function(x) {
    val <- trimws(as.character(x %||% ""))
    if (!nzchar(val)) {
      return("None")
    }
    val_upper <- toupper(val)
    mapping <- c(
      "NONE" = "None",
      "LTF_DZ_WITH_ITF_DZ" = "LTF_DZ_with_ITF_DZ",
      "LTF_SZ_WITH_ITF_SZ" = "LTF_SZ_with_ITF_SZ"
    )
    mapping[[val_upper]] %||% val
  }

  allowed_directions <- c("D2S", "S2D", "D2ATH", "ATH2D", "S2ATL", "ATL2S")
  allowed_itf_directions <- c("D2S", "S2D")
  allowed_trends <- c("Up", "Down", "Sideways")
  allowed_curves <- c("LC", "EQ", "HC")
  allowed_confluence <- c("None", "LTF_DZ_with_ITF_DZ", "LTF_SZ_with_ITF_SZ")

  observeEvent(input$mtf_eval, {
    snapshots <- mtf_snapshots()
    bundle_map <- get_bundle_map(input$mtf_bundle)
    snapshot_lookup <- split(snapshots, snapshots$tf_label)

    results <- lapply(names(bundle_map), function(exec_tf) {
      trio <- bundle_map[[exec_tf]]
      ltf_row <- snapshot_lookup[[trio$LTF]]
      itf_row <- snapshot_lookup[[trio$ITF]]
      htf_row <- snapshot_lookup[[trio$HTF]]

      validation_reasons <- character()
      valid <- TRUE

      if (is.null(ltf_row)) {
        validation_reasons <- c(validation_reasons, sprintf("Snapshot missing for %s", trio$LTF))
        valid <- FALSE
      }
      if (is.null(itf_row)) {
        validation_reasons <- c(validation_reasons, sprintf("Snapshot missing for %s", trio$ITF))
        valid <- FALSE
      }
      if (is.null(htf_row)) {
        validation_reasons <- c(validation_reasons, sprintf("Snapshot missing for %s", trio$HTF))
        valid <- FALSE
      }

      ltf_trend <- if (!is.null(ltf_row)) normalize_trend(ltf_row$trend) else ""
      itf_trend <- if (!is.null(itf_row)) normalize_trend(itf_row$trend) else ""
      htf_trend <- if (!is.null(htf_row)) normalize_trend(htf_row$trend) else ""

      itf_dir <- if (!is.null(itf_row)) normalize_direction(itf_row$direction) else ""
      htf_dir <- if (!is.null(htf_row)) normalize_direction(htf_row$direction) else ""
      curve_val <- if (!is.null(htf_row)) normalize_curve(htf_row$curve) else ""
      confluence_val <- if (!is.null(itf_row)) normalize_confluence(itf_row$confluence) else "None"

      if (nzchar(itf_trend) && !itf_trend %in% allowed_trends) {
        validation_reasons <- c(validation_reasons, sprintf("Invalid ITF trend '%s'", itf_trend))
        valid <- FALSE
      }
      if (nzchar(htf_trend) && !htf_trend %in% allowed_trends) {
        validation_reasons <- c(validation_reasons, sprintf("Invalid HTF trend '%s'", htf_trend))
        valid <- FALSE
      }
      if (!itf_dir %in% allowed_itf_directions) {
        validation_reasons <- c(validation_reasons, sprintf("Invalid ITF direction '%s'", itf_dir))
        showNotification(sprintf("%s: Invalid ITF direction", exec_tf), type = "error")
        valid <- FALSE
      }
      if (!htf_dir %in% allowed_directions) {
        validation_reasons <- c(validation_reasons, sprintf("Invalid HTF direction '%s'", htf_dir))
        showNotification(sprintf("%s: Invalid HTF direction", exec_tf), type = "error")
        valid <- FALSE
      }

      regime <- if (tolower(itf_trend) == "sideways") "Sideways" else "Trending"

      if (!curve_val %in% allowed_curves) {
        validation_reasons <- c(validation_reasons, "HTF curve missing")
        showNotification(sprintf("%s: HTF curve missing", exec_tf), type = "error")
        valid <- FALSE
      }

      if (!confluence_val %in% allowed_confluence) {
        validation_reasons <- c(validation_reasons, sprintf("Invalid confluence token '%s'", confluence_val))
        valid <- FALSE
      }

      if (regime == "Sideways") {
        expected_conf <- if (identical(input$mtf_side, "Long")) "LTF_DZ_with_ITF_DZ" else "LTF_SZ_with_ITF_SZ"
        if (!confluence_val %in% allowed_confluence || confluence_val == "None") {
          validation_reasons <- c(validation_reasons, "Confluence required for sideways regime")
          showNotification(sprintf("%s: Confluence required when ITF trend is Sideways", exec_tf), type = "error")
          valid <- FALSE
        } else if (!identical(confluence_val, expected_conf)) {
          validation_reasons <- c(validation_reasons, sprintf("Confluence token '%s' does not match trade side", confluence_val))
          showNotification(sprintf("%s: Confluence token does not align with %s side", exec_tf, input$mtf_side), type = "error")
          valid <- FALSE
        }
      }

      scenario <- NA_character_
      eligible_flag <- FALSE
      dec_reasons <- character()

      if (valid) {
        dec <- decision_cached(
          regime = regime,
          side = input$mtf_side,
          htf_dir = htf_dir,
          itf_dir = itf_dir,
          htf_trend = htf_trend,
          itf_trend = itf_trend,
          curve = curve_val,
          confluence = confluence_val != "None"
        )
        scenario <- dec$scenario %||% NA_character_
        eligible_flag <- isTRUE(dec$eligible)
        dec_reasons <- dec$reasons %||% character(0)
      }

      chk <- checklist_cached(
        base_count = input$mtf_base_count,
        leg_out = input$mtf_leg_out,
        voz = input$mtf_voz,
        fresh5m = input$mtf_fresh5m,
        rr = input$mtf_rr,
        risk_pct = input$mtf_risk_pct,
        oe = input$mtf_oe,
        decision_match = eligible_flag && valid
      )
      checklist_pass <- isTRUE(chk$passed)
      chk_reasons <- chk$reasons %||% character(0)

      combined_reasons <- unique(c(validation_reasons, dec_reasons, chk_reasons))
      combined_reasons <- combined_reasons[nzchar(combined_reasons)]
      why_not <- if (length(combined_reasons) && !(eligible_flag && checklist_pass)) {
        paste(combined_reasons, collapse = "; ")
      } else {
        ""
      }

      take_flag <- eligible_flag && checklist_pass

      data.frame(
        exec_tf = exec_tf,
        scenario = scenario,
        eligible = eligible_flag,
        checklist_pass = checklist_pass,
        take = take_flag,
        why_not = if (nzchar(why_not)) why_not else "",
        regime = regime,
        side = input$mtf_side,
        htf_dir = htf_dir,
        itf_dir = itf_dir,
        htf_trend = htf_trend,
        itf_trend = itf_trend,
        curve = curve_val,
        confluence = confluence_val,
        reasons = if (length(combined_reasons)) paste(combined_reasons, collapse = "; ") else "",
        stringsAsFactors = FALSE
      )
    })

    mtf_results(do.call(rbind, results))
  })

  observeEvent(input$mtf_save_taken, {
    results <- mtf_results()
    if (is.null(results)) {
      showNotification("Please evaluate candidates before saving.", type = "warning")
      return()
    }
    base_id <- trimws(input$mtf_base_order_id %||% "")
    if (!nzchar(base_id)) {
      showNotification("Base Order ID is required to save taken trades.", type = "error")
      return()
    }

    takes <- results[isTRUE(results$take), , drop = FALSE]
    if (!nrow(takes)) {
      showNotification("No take candidates to save.", type = "warning")
      return()
    }

    save_ok <- TRUE
    for (idx in seq_len(nrow(takes))) {
      row <- takes[idx, , drop = FALSE]
      order_id <- paste(base_id, row$exec_tf, sep = "-")
      attempt <- try({
        db_engine$log_trade(
          ts = Sys.time(),
          order_id = order_id,
          regime = row$regime,
          side = row$side,
          htf_dir = row$htf_dir,
          itf_dir = row$itf_dir,
          htf_trend = row$htf_trend,
          itf_trend = row$itf_trend,
          curve = row$curve,
          confluence = row$confluence,
          scenario = row$scenario %||% NA_character_,
          eligible = isTRUE(row$eligible),
          reasons = row$reasons,
          base_count = input$mtf_base_count,
          leg_out = input$mtf_leg_out,
          voz = isTRUE(input$mtf_voz),
          fresh5m = isTRUE(input$mtf_fresh5m),
          rr = input$mtf_rr,
          risk_pct = input$mtf_risk_pct,
          oe = input$mtf_oe,
          checklist_pass = isTRUE(row$checklist_pass)
        )
        TRUE
      }, silent = TRUE)
      if (!identical(attempt, TRUE)) {
        save_ok <- FALSE
        showNotification(sprintf("Failed to save %s", order_id), type = "error")
        message(
          "Multi-TF save error for ", order_id, ": ",
          conditionMessage(attr(attempt, "condition") %||% simpleError("unknown"))
        )
      }
    }

    if (save_ok) {
      showNotification("Multi-TF trades saved.", type = "message")
      log_data(db_engine$get_log(200))
    }
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
