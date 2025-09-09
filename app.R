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
  div(
    class = "mt-4",
    h3("Decision Output"),
    uiOutput("decision_ui"),
    
    h3("Checklist Output"),
    uiOutput("checklist_ui"),
    
    h3("Summary"),
    uiOutput("summary_ui"),
    
    h3("Journal"),
    DT::dataTableOutput("log_table")
  )
)

# ---- Server -------------------------------------------------------------------
server <- function(input, output, session) {

  # Snapshot of last evaluation (used when saving)
  last_decision  <- reactiveVal(NULL)
  last_checklist <- reactiveVal(NULL)
  last_confluence <- reactiveVal(NULL)
  log_data <- reactiveVal(db_engine$get_log(200))
  
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

  # ---- Journal (initial render) ----
  output$log_table <- DT::renderDataTable({
    df <- log_data()
    if (!is.null(df) && "scenario" %in% names(df)) {
      df$scenario <- vapply(df$scenario, pretty_scenario, character(1))
    }
    DT::datatable(df, options = list(pageLength = 10))
    DT::datatable(log_data(), options = list(pageLength = 10))
  })
  
  # ---- Cleanup ----
  onStop(function() db_engine$disconnect())
}

# ---- Run app ------------------------------------------------------------------
shinyApp(ui, server)
