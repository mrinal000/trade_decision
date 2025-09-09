# R/db_module.R
# ------------------------------------------------------------------------------
# DBModule: Encapsulates all DuckDB I/O for the trade decision assistant.
# - Opens/initializes a DuckDB database file.
# - Ensures the `trade_log` table exists (adds columns if an older schema).
# - Provides methods to insert a trade record and read recent logs.
# - Cleans up the connection on shutdown.
# ------------------------------------------------------------------------------

# Dependencies
library(R6)
library(DBI)
library(duckdb)
library(memoise)

DBModule <- R6::R6Class(
  classname = "DBModule",
  
  public = list(
    
    # --------------------------------------------------------------------------
    # Public fields
    # --------------------------------------------------------------------------
    con  = NULL,     # DBI connection handle
    path = NULL,     # path to the DuckDB file
    log_cache = NULL, # memoised function for fetching logs
    
    # --------------------------------------------------------------------------
    # Constructor
    # --------------------------------------------------------------------------
    initialize = function(db_path = "trade_decision_db.duckdb") {
      self$path <- db_path
      self$ensure_connection()

      # Memoised log fetcher to avoid repeat disk reads
      self$log_cache <- memoise(function(limit = 200) {
        limit <- as.integer(limit)
        DBI::dbGetQuery(
          self$con,
          sprintf("SELECT * FROM trade_log ORDER BY ts DESC LIMIT %d", limit)
        )
      })
    },

    # Reconnect to DuckDB if the connection has been closed
    ensure_connection = function() {
      if (is.null(self$con) || !DBI::dbIsValid(self$con)) {
        self$con <- DBI::dbConnect(
          duckdb::duckdb(),
          dbdir   = self$path,
          read_only = FALSE
        )

        DBI::dbExecute(self$con, "
          CREATE TABLE IF NOT EXISTS trade_log (
            ts            TIMESTAMP,
            order_id      TEXT,
            regime        TEXT,
            side          TEXT,
            htf_dir       TEXT,
            itf_dir       TEXT,
            htf_trend     TEXT,
            itf_trend     TEXT,
            curve         TEXT,
            confluence    TEXT,
            scenario      TEXT,
            eligible      BOOLEAN,
            reasons       TEXT,
            base_count    INTEGER,
            leg_out       TEXT,
            voz           BOOLEAN,
            fresh5m       BOOLEAN,
            rr            DOUBLE,
            risk_pct      DOUBLE,
            oe            INTEGER,
            checklist_pass BOOLEAN
          )
        ")

        cols <- try(DBI::dbGetQuery(self$con, "PRAGMA table_info('trade_log')"), silent = TRUE)
        if (inherits(cols, "try-error")) {
          stop("Unable to inspect trade_log schema.")
        }

        if (!"order_id" %in% cols$name) {
          DBI::dbExecute(self$con, "ALTER TABLE trade_log ADD COLUMN order_id TEXT")
        }

        DBI::dbExecute(self$con, "CREATE INDEX IF NOT EXISTS idx_trade_log_ts ON trade_log(ts)")
        DBI::dbExecute(self$con, "CREATE INDEX IF NOT EXISTS idx_trade_log_order_id ON trade_log(order_id)")

        if (!is.null(self$log_cache)) memoise::forget(self$log_cache)
      }
      invisible(TRUE)
    },
    
    # --------------------------------------------------------------------------
    # Insert a single trade record
    # --------------------------------------------------------------------------
    log_trade = function(
    ts, order_id,
    regime, side, htf_dir, itf_dir, htf_trend, itf_trend, curve, confluence,
    scenario, eligible, reasons,
    base_count, leg_out, voz, fresh5m, rr, risk_pct, oe, checklist_pass
    ) {
      # Validate required argument: order_id must be present to save
      if (is.null(order_id) || !nzchar(trimws(order_id))) {
        stop("Order ID is required to save a record.")
      }
      
      # Parameterized insert for safety and correctness
      self$ensure_connection()
      DBI::dbExecute(
        self$con,
        "
        INSERT INTO trade_log (
          ts, order_id, regime, side, htf_dir, itf_dir, htf_trend, itf_trend,
          curve, confluence, scenario, eligible, reasons,
          base_count, leg_out, voz, fresh5m, rr, risk_pct, oe, checklist_pass
        )
        VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)
        ",
        params = list(
          ts, order_id,
          regime, side, htf_dir, itf_dir, htf_trend, itf_trend,
          curve, confluence, scenario, as.logical(eligible), reasons,
          as.integer(base_count), leg_out, as.logical(voz), as.logical(fresh5m),
          as.numeric(rr), as.numeric(risk_pct), as.integer(oe), as.logical(checklist_pass)
        )
      )

      # Invalidate cached log reads since data changed
      memoise::forget(self$log_cache)

      invisible(TRUE)
    },
    
    # --------------------------------------------------------------------------
    # Fetch recent log rows (default 200)
    # --------------------------------------------------------------------------
    get_log = function(limit = 200) {
      self$ensure_connection()
      self$log_cache(limit)
    },

    # --------------------------------------------------------------------------
    # Delete all rows from trade_log
    # --------------------------------------------------------------------------
    clear_log = function() {
      self$ensure_connection()
      DBI::dbExecute(self$con, "DELETE FROM trade_log")
      memoise::forget(self$log_cache)
      invisible(TRUE)
    },

    # --------------------------------------------------------------------------
    # Disconnect and clean up
    # --------------------------------------------------------------------------
    disconnect = function() {
      if (!is.null(self$con)) {
        DBI::dbDisconnect(self$con, shutdown = TRUE)
        self$con <- NULL
      }
      invisible(TRUE)
    }
  )
)
