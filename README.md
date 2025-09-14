# Trade Decision Assistant – Repository Documentation

## Overview
This repository implements a Shiny-based **NeoStock Decision Assistant** that guides traders through scenario determination, an eight‑point checklist, and journaling of trades in DuckDB.

## Project Structure
- **`app.R`** – Shiny application and UI/server wiring.  
- **`R/`** – R6 modules:
  - `decision_module.R`
  - `checklist_module.R`
  - `db_module.R`
- **Tests** – `test_decision.R`, `test_checklist.R`, `test_db.R`
- **Environment** – `renv.lock` for dependency pinning, `renv/` for library storage
- **Data** – example DuckDB database `trade_decision_db.duckdb`
- **Misc** – `Trade-decision.Rproj` project file

## Application (`app.R`)
- Top‑level comments summarize the two‑step workflow (Evaluate and Save) and the roles of the R6 modules

- Loads libraries for Shiny UI, styling, data tables, memoisation, and DuckDB persistence

- Defines helper utilities `%||%` and `pretty_scenario()` for safe defaults and human-readable scenario labels

- Sources R6 modules and initializes engines for decision logic, checklist evaluation, and database access

- **UI**: sidebar inputs capture regime, side, HTF/ITF directions and trends, curve, confluence, checklist metrics, and order ID; main panel displays decision results, checklist evaluation, summary, and trade journal

- **Server**:
  - Stores last evaluation results and renders bullet‑listed reasons for failures

  - Evaluation step calls memoized decision and checklist modules, then renders their outputs and summary

  - Save step requires prior evaluation and non‑empty order ID, assembles a combined reason string, writes to DuckDB, and refreshes the journal

  - Journal maintenance includes clearing logs, rendering the journal table, and disconnecting the database on shutdown

## Modules

### DecisionModule (`R/decision_module.R`)
Determines trade scenarios and eligibility based on regime, side, directional tokens, trends, curve location, and (for sideways trades) zone confluence. Returns scenario code, eligibility flag, implied side, and reasons for failures

Key method `determine()`:
- Normalizes inputs; infers candidate side from ITF direction

- **Trending regime** checks side alignment, ITF trend, allowable curves, and matches specific scenario codes (e.g., `TL_FULL_ALIGN`, `TS_ATL_ALIGN`)

- **Sideways regime** enforces sideways ITF trend, requires zone confluence, and matches long (`SL_*`) or short (`SS_*`) scenarios with permitted curve locations

- Returns structured result with scenario, eligibility, candidate side, and reasons

### ChecklistModule (`R/checklist_module.R`)
Implements the eight-point trade checklist. Fails any criterion by collecting reasons and setting `passed = FALSE`

`evaluate()` checks:
- Base candle count ≤ 6
- Strong leg‑out
- VOZ achieved
- Fresh 5‑minute zone
- Risk/reward ratio ≥ 3
- Risk percentage ≤ 2%
- OE score ≥ 12
- DecisionModule match  
and returns pass flag plus reasons

### DBModule (`R/db_module.R`)
Manages DuckDB storage for the trade journal

Features:
- `initialize()` opens/creates the database and memoizes a log-fetch function

- `ensure_connection()` lazily reconnects, creates/updates schema, and indexes `trade_log`

- `log_trade()` inserts validated records via parameterized SQL and invalidates the cache

- `get_log()`, `clear_log()`, and `disconnect()` expose journal retrieval, wiping, and cleanup

## Tests
- `test_decision.R`: verifies a “Trending Long” setup yields scenario `TL_FULL_ALIGN` and is eligible

- `test_checklist.R`: confirms valid inputs pass the checklist

- `test_db.R`: exercises DuckDB logging, reconnection, retrieval, and clearing routines

## Environment & Dependencies
Project uses `renv` to lock the R version (4.5.1) and packages such as R6 for R6 classes and duckdb for storage

## Data
Sample DuckDB database (`trade_decision_db.duckdb`) demonstrates the expected schema and supports quick testing.

## Testing
⚠️ `N/A` – Static review only; no programmatic tests were executed.

---

This documentation captures the repository’s architecture, module responsibilities, and key workflows for future reference or extension.
