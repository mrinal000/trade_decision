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
  if (is.null(x)) return(y)
  if (length(x) == 0) return(y)
  if (is.atomic(x) && length(x) == 1) {
    if (is.na(x)) return(y)
    if (is.character(x) && !nzchar(x)) return(y)
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
        h3("Decision Pairs"),
        card(
          card_header("Controls"),
          layout_column_wrap(
            width = 1/3,
            textInput("dp_symbol", "Symbol"),
            selectInput("dp_side", "Side", choices = c("Long", "Short")),
            dateInput("dp_as_of", "As of", value = Sys.Date()),
            div(class = "mt-4", actionButton("dp_evaluate", "Evaluate"))
          )
        ),
        card(
          card_header("Snapshot Editor"),
          card_body(
            DTOutput("dp_snapshots")
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

  # ---- Decision Pairs state --------------------------------------------------
  dp_default_snapshots <- data.frame(
    tf_label = c("1m", "5m", "15m", "75m", "Daily", "Weekly", "Monthly"),
    trend = rep("Up", 7),
    direction = rep("D2S", 7),
    curve = c("", "", "", "EQ", "EQ", "EQ", "EQ"),
    confluence = rep("None", 7),
    ath_flag = rep(FALSE, 7),
    atl_flag = rep(FALSE, 7),
    stringsAsFactors = FALSE
  )

  dp_snapshots <- reactiveVal(dp_default_snapshots)
  dp_results <- reactiveVal(NULL)

  dp_snapshot_proxy <- dataTableProxy("dp_snapshots")

  output$dp_snapshots <- DT::renderDT({
    DT::datatable(
      dp_snapshots(),
      rownames = FALSE,
      options = list(dom = "t", paging = FALSE, ordering = FALSE),
      editable = list(target = "cell", disable = list(columns = c(0)))
    )
  })

  observeEvent(input$dp_snapshots_cell_edit, {
    info <- input$dp_snapshots_cell_edit
    i <- info$row
    j <- info$col + 1
    v <- info$value
    df <- dp_snapshots()
    if (i < 1 || i > nrow(df) || j < 1 || j > ncol(df)) {
      return()
    }
    col_name <- names(df)[j]
    if (col_name %in% c("ath_flag", "atl_flag")) {
      df[i, j] <- tolower(as.character(v)) %in% c("true", "t", "1", "yes")
    } else {
      df[i, j] <- v
    }
    dp_snapshots(df)
    DT::replaceData(dp_snapshot_proxy, df, resetPaging = FALSE, rownames = FALSE)
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

  allowed_trends <- c("Up", "Down", "Sideways")
  allowed_directions <- c("D2S", "S2D", "D2ATH", "ATH2D", "S2ATL", "ATL2S")
  allowed_curves <- c("LC", "EQ", "HC")
  allowed_confluence <- c("None", "LTF_DZ_with_ITF_DZ", "LTF_SZ_with_ITF_SZ")

  trio_definitions <- list(
    list(exec = "5m", itf = "15m", htf = "75m"),
    list(exec = "15m", itf = "75m", htf = "Daily"),
    list(exec = "75m", itf = "Daily", htf = "Weekly"),
    list(exec = "Daily", itf = "Weekly", htf = "Monthly")
  )

  output$dp_results <- DT::renderDT({
    res <- dp_results()
    if (is.null(res)) {
      return(NULL)
    }
    DT::datatable(res, rownames = FALSE, options = list(dom = "t", paging = FALSE, ordering = FALSE))
  })

  observeEvent(input$dp_evaluate, {
    snapshots <- dp_snapshots()
    snapshot_lookup <- split(snapshots, snapshots$tf_label)

    sideways_notifications <- character()

    results <- lapply(trio_definitions, function(trio) {
      ltf_row <- snapshot_lookup[[trio$exec]]
      itf_row <- snapshot_lookup[[trio$itf]]
      htf_row <- snapshot_lookup[[trio$htf]]

      notes <- character()
      valid <- TRUE

      if (is.null(ltf_row)) {
        notes <- c(notes, sprintf("Snapshot missing for %s", trio$exec))
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

      ltf_trend <- if (!is.null(ltf_row)) normalize_trend(ltf_row$trend) else NA_character_
      itf_trend <- if (!is.null(itf_row)) normalize_trend(itf_row$trend) else NA_character_
      htf_trend <- if (!is.null(htf_row)) normalize_trend(htf_row$trend) else NA_character_

      itf_dir <- if (!is.null(itf_row)) normalize_direction(itf_row$direction) else NA_character_
      htf_dir <- if (!is.null(htf_row)) normalize_direction(htf_row$direction) else NA_character_
      curve_val <- if (!is.null(htf_row)) normalize_curve(htf_row$curve) else NA_character_
      confluence_val <- if (!is.null(itf_row)) normalize_confluence(itf_row$confluence) else "None"

      if (!is.null(itf_row)) {
        if (is.na(itf_trend) || !itf_trend %in% allowed_trends) {
          notes <- c(notes, sprintf("Invalid ITF trend '%s'", itf_row$trend))
          valid <- FALSE
        }
        if (is.na(itf_dir) || !itf_dir %in% allowed_directions) {
          notes <- c(notes, sprintf("Invalid ITF direction '%s'", itf_row$direction))
          valid <- FALSE
        }
      }

      if (!is.null(htf_row)) {
        if (is.na(htf_trend) || !htf_trend %in% allowed_trends) {
          notes <- c(notes, sprintf("Invalid HTF trend '%s'", htf_row$trend))
          valid <- FALSE
        }
        if (!nzchar(curve_val)) {
          notes <- c(notes, "HTF curve missing")
          valid <- FALSE
        } else if (!curve_val %in% allowed_curves) {
          notes <- c(notes, sprintf("Invalid HTF curve '%s'", htf_row$curve))
          valid <- FALSE
        }
        if (is.na(htf_dir) || !htf_dir %in% allowed_directions) {
          notes <- c(notes, sprintf("Invalid HTF direction '%s'", htf_row$direction))
          valid <- FALSE
        }
      }

      if (!confluence_val %in% allowed_confluence) {
        notes <- c(notes, sprintf("Invalid confluence token '%s'", confluence_val))
        valid <- FALSE
      }

      regime <- if (tolower(itf_trend %||% "") == "sideways") "Sideways" else "Trending"

      if (regime == "Sideways") {
        expected_conf <- if (identical(input$dp_side, "Long")) "LTF_DZ_with_ITF_DZ" else "LTF_SZ_with_ITF_SZ"
        if (!identical(confluence_val, expected_conf)) {
          note <- "Sideways ITF requires proper confluence"
          notes <- c(notes, note)
          valid <- FALSE
          sideways_notifications <<- c(sideways_notifications, sprintf("%s trio: %s", trio$exec, note))
        }
      }

      scenario <- NA_character_
      eligible_flag <- FALSE

      if (valid) {
        confluence_token <- as.character(confluence_val %||% "None")
        decision_args <- list(
          regime = regime,
          side = input$dp_side,
          htf_dir = htf_dir,
          itf_dir = itf_dir,
          htf_trend = htf_trend,
          itf_trend = itf_trend,
          curve = curve_val,
          confluence = confluence_token
        )
        decision_formals <- names(formals(decision_engine$determine))
        if ("ath_atl" %in% decision_formals) {
          decision_args$ath_atl <- list(
            htf_ath = isTRUE(htf_row$ath_flag),
            htf_atl = isTRUE(htf_row$atl_flag)
          )
        }
        # Decision module expects a logical confluence flag; convert if needed.
        if (is.character(decision_args$confluence) && length(decision_args$confluence) == 1) {
          decision_args$confluence <- !identical(decision_args$confluence, "None")
        }
        dec <- do.call(decision_cached, decision_args)
        scenario <- dec$scenario %||% NA_character_
        eligible_flag <- isTRUE(dec$eligible)
        notes <- c(notes, dec$reasons %||% character(0))
      }

      notes <- notes[!is.na(notes) & nzchar(notes)]
      notes <- unique(notes)

      result_row <- data.frame(
        exec_tf = trio$exec,
        itf = trio$itf,
        htf = trio$htf,
        side = input$dp_side,
        regime = regime,
        scenario = scenario %||% NA_character_,
        eligible = eligible_flag && valid,
        notes = if (length(notes)) paste(notes, collapse = "; ") else "",
        stringsAsFactors = FALSE
      )

      result_row$human_readable <- paste0(
        "Exec TF ", trio$exec, " (ITF ", trio$itf, ", HTF ", trio$htf, "): ",
        if (result_row$eligible) "Eligible" else "Not eligible", " for ", input$dp_side,
        " — Scenario: ", scenario %||% "N/A",
        if (nzchar(result_row$notes)) paste0(" | Notes: ", result_row$notes) else ""
      )

      result_row
    })

    if (length(sideways_notifications)) {
      for (msg in unique(sideways_notifications)) {
        showNotification(msg, type = "warning")
      }
    }

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
