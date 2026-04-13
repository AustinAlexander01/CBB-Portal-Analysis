# server.R
# STARTUP PERFORMANCE IMPROVEMENTS:
# - Lazy-load rarely-used packages: ggtext, fmsb, gt (loaded on-demand via requireNamespace)
# - Radar-only runtime: single-game + season-window modules removed
# - Multi-select filter: String-coerce filterValue.values to prevent numeric/string mismatch
# - Reset filters: Added onRender handler + custom message listener for radar_reset_reactable_filters
#
# Goals:
# 1) FIX missing functions (e.g., get_profile_labels), bad input IDs, recursion/stack issues
# 2) MAKE APP FAST: keep startup focused on radar/profile workflows
# 3) MATCH ui.R (navbarPage id = "main_nav"; radar-only tab)
# 4) ELIMINATE bindCache/bindEvent pitfalls; rely on memoised disk reads + tab gating
#
# Required packages (keep minimal but sufficient for startup)
# Lazy-loaded on-demand: ggtext, fmsb, gt (only used in specific outputrenderers)
# --------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(shiny)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readr)
  library(glue)
  library(scales)
  library(forcats)
  library(ggplot2)
  library(DBI)
  library(RPostgres)
  library(pool)
  # ggtext, fmsb loaded on-demand via requireNamespace (rich text in viz only)
  library(reactable)
  library(memoise)
  library(cachem)
  library(purrr)
  library(htmltools)
  library(lubridate)
  library(plotly)
  library(shinycssloaders)
  # gt loaded on-demand via requireNamespace (used only in advanced stats render)
})

# --------------------------------------------------------------------------------
# Guard: prevent accidental top-level `output$...` assignments from crashing when
# server.R is sourced (e.g., via Rscript or during debugging). Any real Shiny
# outputs must still be defined inside `shinyServer(function(input, output, session) { ... })`.
# --------------------------------------------------------------------------------
if (!exists("output", inherits = FALSE)) output <- list()

# --------------------------------------------------------------------------------
# Small helpers
# --------------------------------------------------------------------------------

# --- lightweight timing + memory logging --------------------------------------
profiling_enabled <- function() {
  isTRUE(getOption("app.profile", FALSE))
}

prof_log <- function(label) {
  if (!profiling_enabled()) {
    return(invisible(NULL))
  }
  gc_out <- gc()
  message(sprintf("[prof] %s | mem_used=%sMB", label, round(sum(gc_out[, 2]) / 1024, 2)))
  invisible(NULL)
}

with_timing <- function(label, meta = list(), expr) {
  t0 <- proc.time()
  res <- eval.parent(substitute(expr))
  elapsed_ms <- (proc.time() - t0)[["elapsed"]] * 1000

  meta_str <- ""
  if (length(meta) > 0) {
    meta_vals <- unlist(meta, use.names = TRUE)
    meta_str <- paste0(names(meta_vals), "=", meta_vals, collapse = " ")
  }

  row_str <- ""
  if (is.data.frame(res)) {
    row_str <- paste0("rows=", nrow(res))
  } else if (is.list(res) && all(c("shifts", "events") %in% names(res))) {
    row_str <- paste0("shifts=", nrow(res$shifts), " events=", nrow(res$events))
  }

  if (profiling_enabled()) {
    msg <- paste0("[timing] ", label, " | ", sprintf("%.1f", elapsed_ms), " ms")
    if (nzchar(meta_str)) msg <- paste0(msg, " | ", meta_str)
    if (nzchar(row_str)) msg <- paste0(msg, " | ", row_str)
    message(msg)
    prof_log(label)
  }

  res
}

chr1 <- function(x, default = "") {
  if (is.null(x) || length(x) == 0) {
    return(default)
  }
  x <- as.character(x[1])
  if (is.na(x) || !nzchar(x)) default else x
}

format_height <- function(ht_inches) {
  h <- suppressWarnings(as.numeric(ht_inches))
  if (!is.finite(h)) {
    return(NA_character_)
  }
  h <- round(h)
  feet <- h %/% 12
  inches <- h %% 12
  paste0(feet, "'", inches, "\"")
}

collapse_ids <- function(x) {
  if (is.null(x)) {
    return("")
  }
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(trimws(x))]
  if (length(x) == 0) {
    return("")
  }
  paste(sort(unique(x)), collapse = "|")
}

sanitize_filter_id <- function(var) {
  # safe Shiny input id for arbitrary column names (e.g., `3PP`, `FC/40`, `D-PRPG`)
  paste0("flt__", gsub("[^A-Za-z0-9]+", "_", as.character(var)))
}

sanitize_composite_id <- function(x) {
  paste0("comp__", gsub("[^A-Za-z0-9]+", "_", as.character(x)))
}

source("plotly_helpers.R")


# --------------------------------------------------------------------------------
# Labels + composites (kept from your prior server)
# --------------------------------------------------------------------------------
stat_labels <- c(
  PRPG = "PRPG!",
  Ht = "Ht",
  ORtg = "ORtg",
  OBPM = "OBPM",
  `BPM` = "BPM",
  `D-PRPG` = "DPRPG!",
  `DRtg` = "DRtg",
  DBPM = "DBPM",
  Blk = "BLK%",
  OR_pct = "ORB%",
  DR_pct = "DRB%",
  Reb = "`REBs`",
  Stl = "STL%",
  Stps = "Stps",
  `FC/40` = "Fouls/40m",
  foul_efficiency = "PF EFF",
  Usg = "USG%",
  Ast = "ASTs",
  Ast_per = "AST%",
  ATO = "A:TO",
  TO = "TO%",
  eFG = "eFG%",
  TS = "TS%",
  FTP = "FT%",
  FTM = "FTM",
  FTA = "FTA",
  FTR = "FTR",
  `3PR` = "3PR",
  `3P/100` = "3PT/100",
  `3PP` = "3P%",
  `3PA` = "3PA",
  `3PM` = "3PM",
  three_unastd_pct = "Unastd 3PM%",
  Far2_P = "Mid FG%",
  Far2_A = "Mid FGA",
  Far2_M = "Mid FGM",
  Far2_Rt = "Mid Att Rate",
  mid_unastd_pct = "Unastd Mid%",
  Close2_P = "Rim FG%",
  Close2_A = "Rim FGA",
  Close2_M = "Rim FGM",
  Close2_Rt = "Rim Att Rate",
  rim_unastd_pct = "Unastd Rim%",
  unastd_fgs = "Unastd FGM",
  unastd_fg_pct = "Unastd FG Rate",
  astd_fgs = "Assisted FGMs",
  Dunk_M = "Made Dunks",
  `2PP` = "2P%",
  `2PA` = "2PA",
  `2PM` = "2PM",
  FGA = "FGA",
  FGM = "FGM",
  Pts = "PTS",
  Shooting = "Shooting",
  Playmaking = "Playmaking",
  `Rim Pressure` = "Rim Pressure",
  `Ball Security` = "Ball Security",
  `3-pt shooting` = "3-pt shooting",
  Defense = "Defense"
)
composite_display_names <- c(
  `Ball Handling` = "Play Making"
)
composite_table_labels <- c(
  `Ball Handling` = "Play Making"
)

relabel_with_map <- function(x, label_map) {
  if (length(x) == 0) {
    return(character(0))
  }
  x_chr <- as.character(x)
  out <- x_chr
  hit <- !is.na(x_chr) & x_chr %in% names(label_map)
  out[hit] <- unname(label_map[x_chr[hit]])
  out
}

display_composite_name <- function(x) {
  relabel_with_map(x, composite_display_names)
}

rank_vars <- c("DraftSignalRank", "PredTransferImpactRating", "DraftPickModelRank", "EstimatedDraftModelRating")

composite_groups <- list(
  Scoring = c(Pts = 0.50, FTR = 0.025, eFG = 0.15, TS = 0.15, Usg = 0.10, FGA = 0.05, FGM = 0.125),
  `Defensive Efficiency` = c(`D-PRPG` = 0.30, `DRtg` = 0.25, DBPM = 0.30, Stl = 0.025, Blk = 0.025, Stps = 0.10),
  `Perimeter Defense` = c(Stl = 0.55, foul_efficiency = 0.45),
  `Interior Defense` = c(`D-PRPG` = 0.10, `DRtg` = 0.10, DBPM = 0.05, Blk = 0.55, DR_pct = 0.20),
  Rebounding = c(OR_pct = 0.45, DR_pct = 0.45, Reb = 0.10),
  `Ball Handling` = c(Ast = 0.15, ATO = 0.30, TO = 0.15, Ast_per = 0.30, Usg = 0.10),
  `Unassisted Scoring` = c(unastd_fg_pct = 0.70, rim_unastd_pct = 0.10, mid_unastd_pct = 0.10, unastd_fgs = 0.10),
  `Midrange Offense` = c(Far2_M = 0.25, Far2_A = 0.25, Far2_P = 0.25, Far2_Rt = 0.25),
  `Interior Offense` = c(`Close2_M` = 0.125, `Close2_P` = 0.30, FTR = 0.25, `Close2_Rt` = 0.30, Dunk_M = 0.025),
  `Perimeter Offense` = c(`3PM` = 0.175, `3P/100` = 0.25, `3PR` = 0.225, `3PP` = 0.40, three_unastd_pct = 0.00),
  `Offensive Efficiency` = c(
    eFG = 0.075, TS = 0.075, ATO = 0.075, TO = 0.025, `2PP` = 0.05, FTP = 0.025, PRPG = 0.025,
    OBPM = 0.2625, ORtg = 0.2875, Usg = 0.10
  )
)

radar_vars_all <- c(
  "PRPG", "Ht", "ORtg", "OBPM", "BPM", "D-PRPG", "DRtg", "DBPM", "Blk",
  "OR_pct", "DR_pct", "Reb", "Stl", "Stps", "FC/40", "Usg", "Ast", "Ast_per",
  "ATO", "TO", "eFG", "TS", "FTP", "FTM", "FTR", "3PR", "3P/100", "3PP",
  "3PA", "3PM", "three_unastd_pct", "Far2_P", "Far2_A", "Far2_M", "Far2_Rt",
  "mid_unastd_pct", "Close2_M", "Close2_A", "Close2_P", "Close2_Rt", "Dunk_M",
  "dunk_unastd_pct", "rim_unastd_pct", "2PP", "2PA", "2PM", "FGA", "FGM", "Pts",
  "unastd_fg_pct", "unastd_fgs", "astd_fgs", "foul_efficiency"
)

# Smaller default set for user-selectable raw radar
radar_vars_all_small <- intersect(
  c(
    "ORtg", "Usg","OBPM","Ast_per", "ATO", "TO",
    "eFG","TS","FTP", "2PP","3PP","Close2_P","3P/100","3PR","FTR","Close2_Rt","Far2_Rt","unastd_fg_pct",
    "DRtg", "DBPM","Stl", "Stps", "Blk","FC/40","DR_pct","OR_pct"
  ),
  radar_vars_all
)


# --- desired composite axis order (matches your 2nd image, clockwise) ----------

composite_labels <- c(
  `Offensive Efficiency` = "OFF EFF",
  `Defensive Efficiency` = "DEF EFF",
  `Perimeter Defense`    = "PERIM DEF",
  `Interior Defense`     = "INT DEF",
  `Interior Offense`     = "INT OFF",
  `Perimeter Offense`    = "PERIM OFF",
  `Unassisted Scoring`   = "UNAST SCOR",
  `Midrange Offense`     = "MID OFF",
  `Ball Handling`        = "PLAY MAKING",
  Rebounding             = "REB",
  Scoring                = "SCOR"
)


composite_axis_order <- c(
  "Perimeter Offense",
  "Interior Offense",
  "Offensive Efficiency",
  "Scoring",
  "Ball Handling",
  "Midrange Offense",
  "Defensive Efficiency",
  "Perimeter Defense",
  "Interior Defense",
  "Unassisted Scoring",
  "Rebounding"
)


composite_col_order <- c(
  "Offensive Efficiency",
  "Defensive Efficiency",
  "Rebounding",
  "Scoring",
  "Ball Handling",
  "Perimeter Offense",
  "Interior Offense",
  "Unassisted Scoring",
  "Midrange Offense",
  "Perimeter Defense",
  "Interior Defense"
)

rank_composite_groups <- list(
  ptir = c(ptir = 1),
  edmr = c(edmr = 1)
)
composite_groups_radar <- c(composite_groups, rank_composite_groups)
composite_axis_order_radar <- c(composite_axis_order, names(rank_composite_groups))
composite_labels_radar <- c(
  composite_labels,
  ptir = "PTIR",
  edmr = "EDMR"
)

`%||%` <- function(a, b) if (!is.null(a)) a else b
timed <- function(label, expr) {
  t0 <- proc.time()[["elapsed"]]
  out <- force(expr)
  dt <- proc.time()[["elapsed"]] - t0
  if (profiling_enabled()) {
    message("[timing] ", label, ": ", sprintf("%.3fs", dt))
    prof_log(label)
  }
  out
}

# Columns fetched at startup for the reactable table, similarity, and profile labels.
# Intentionally excludes *_pct (percentile) columns — those are fetched on demand
# per selected player when the radar chart is rendered.
# Defined here so load_player_stats_source() can reference it.
TABLE_DISPLAY_COLS <- unique(c(
  # Identity / metadata
  "Name", "Team", "Conf", "Role", "Year", "Player", "Ht", "G", "Min_pct", "Pick", "pid",
  # Composite rank metrics
  "ptir", "edmr", "dsr", "dpmr","Stps",
  # Rank model variables
  rank_vars,
  # Composite score columns (stored as "Scoring_score", "Defensive Efficiency_score", etc.)
  paste0(names(composite_groups), "_score"),
  # All raw stat columns used in the reactable, radar, and similarity functions
  radar_vars_all,
  names(stat_labels)
))

apply_plotly_theme <- function(p, dark_mode = FALSE) {
  if (isTRUE(dark_mode)) {
    p %>% plotly::layout(
      paper_bgcolor = "#111827",
      plot_bgcolor = "#111827",
      font = list(color = "#e5e7eb"),
      legend = list(bgcolor = "rgba(17,24,39,0.65)", font = list(color = "#e5e7eb")),
      xaxis = list(gridcolor = "#334155", linecolor = "#475569", tickfont = list(color = "#e5e7eb"), title = list(font = list(color = "#e5e7eb"))),
      yaxis = list(gridcolor = "#334155", linecolor = "#475569", tickfont = list(color = "#e5e7eb"), title = list(font = list(color = "#e5e7eb"))),
      polar = list(
        bgcolor = "#111827",
        angularaxis = list(gridcolor = "#334155", linecolor = "#475569", tickfont = list(color = "#e5e7eb")),
        radialaxis = list(gridcolor = "#334155", linecolor = "#475569", tickfont = list(color = "#e5e7eb"))
      )
    )
  } else {
    p %>% plotly::layout(
      paper_bgcolor = "#ffffff",
      plot_bgcolor = "#ffffff",
      font = list(color = "#222222"),
      legend = list(bgcolor = "rgba(255,255,255,0.65)", font = list(color = "#222222")),
      xaxis = list(gridcolor = "#d1d5db", linecolor = "#9ca3af", tickfont = list(color = "#222222"), title = list(font = list(color = "#222222"))),
      yaxis = list(gridcolor = "#d1d5db", linecolor = "#9ca3af", tickfont = list(color = "#222222"), title = list(font = list(color = "#222222"))),
      polar = list(
        bgcolor = "#ffffff",
        angularaxis = list(gridcolor = "#d1d5db", linecolor = "#9ca3af", tickfont = list(color = "#222222")),
        radialaxis = list(gridcolor = "#d1d5db", linecolor = "#9ca3af", tickfont = list(color = "#222222"))
      )
    )
  }
}

label_stat <- function(x, stat_labels) {
  if (is.null(x) || length(x) == 0 || is.na(x)) {
    return(NA_character_)
  }
  x_chr <- as.character(x)
  if (!is.null(stat_labels) && x_chr %in% names(stat_labels)) stat_labels[[x_chr]] else x_chr
}

# Build rich hover text for a composite axis for one player
build_composite_hover <- function(player_name,
                                  player_row_pct,
                                  player_row_raw,
                                  composite_name,
                                  composite_value,
                                  composite_groups,
                                  stat_labels) {
  composite_key <- as.character(composite_name)[1]
  composite_display <- display_composite_name(composite_key)
  if (length(composite_display) == 0 || is.na(composite_display) || !nzchar(composite_display)) {
    composite_display <- composite_key
  }
  if (is.na(composite_key) || !nzchar(composite_key) || !(composite_key %in% names(composite_groups))) {
    return(paste0(
      "<b>", htmlEscape(player_name), "</b>",
      "<br><b>", htmlEscape(composite_display %||% "Composite"), "</b>",
      "<br>Composite: ", ifelse(is.na(composite_value), "NA", sprintf("%.1f", composite_value))
    ))
  }
  w <- composite_groups[[composite_key]]

  if (is.null(w) || length(w) == 0) {
    return(paste0(
      "<b>", htmlEscape(player_name), "</b>",
      "<br><b>", htmlEscape(composite_display), "</b>",
      "<br>Composite: ", sprintf("%.1f", composite_value)
    ))
  }

  get_one_pct <- function(row, s) {
    if (!s %in% names(row)) {
      return(NA_real_)
    }
    suppressWarnings(as.numeric(row[[s]][1]))
  }

  get_one_raw <- function(row, s) {
    if (!s %in% names(row)) {
      return(NA_real_)
    }
    suppressWarnings(as.numeric(row[[s]][1]))
  }

  comps <- tibble(
    stat = names(w)
  ) %>%
    mutate(
      pct = purrr::map_dbl(stat, ~ get_one_pct(player_row_pct, .x)),
      raw = purrr::map_dbl(stat, ~ get_one_raw(player_row_raw, .x)),
      stat_label = purrr::map_chr(stat, ~ label_stat(.x, stat_labels))
    ) %>%
    arrange(stat_label) %>% # cleaner visually
    slice_head(n = 10)

  comp_lines <- purrr::pmap_chr(
    list(comps$stat_label, comps$raw, comps$pct),
    function(lbl, raw, pct) {
      raw_txt <- if (is.na(raw)) "NA" else round(raw, 2)
      pct_txt <- if (is.na(pct)) "NA" else sprintf("%.1f", pct)
      paste0(
        "• ", htmlEscape(lbl),
        ": ", raw_txt,
        " | ", pct_txt
      )
    }
  )

  # MODIFIED: Map short axis keys to full display names for hover text
  hover_display_names <- c(
    composite_display_names,
    ptir = "PredTransferImpactRating",
    edmr = "EstimatedDraftModelRating"
  )
  hover_label <- if (composite_key %in% names(hover_display_names)) {
    hover_display_names[[composite_key]]
  } else {
    composite_display
  }

  paste0(
    "<b>", htmlEscape(player_name), "</b>",
    "<br><b>", htmlEscape(hover_label), "</b>",
    "<br>Composite: ", sprintf("%.1f", composite_value),
    "<br><br>",
    paste(comp_lines, collapse = "<br>")
  )
}

# ------------------------------------------------------------------------------
# Radar app data bootstrap (Supabase-first, with cache fallback)
# ------------------------------------------------------------------------------
rotation_cutoff <- 15
is_benchmark_row <- function(nm) {
  nm %in% benchmark_names
}

coerce_to_date <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(as.Date(NA))
  }
  x <- x[[1]]
  if (inherits(x, "Date")) {
    return(as.Date(x))
  }
  if (inherits(x, c("POSIXct", "POSIXt"))) {
    return(as.Date(x))
  }
  d <- suppressWarnings(as.Date(x))
  if (!is.na(d)) {
    return(d)
  }
  suppressWarnings(as.Date(as.POSIXct(x, tz = "UTC")))
}

make_supabase_con <- function() {
  host <- Sys.getenv("SUPABASE_HOST", unset = "aws-1-us-east-2.pooler.supabase.com")
  user <- Sys.getenv("SUPABASE_USER", unset = "postgres.mkrllsjvjliyxgukwfme")
  pw <- Sys.getenv("SUPABASE_DB_PASSWORD", unset = "")
  if (!nzchar(pw)) {
    stop("SUPABASE_DB_PASSWORD environment variable is not set.")
  }
  DBI::dbConnect(
    RPostgres::Postgres(),
    host = host,
    port = 5432L,
    dbname = "postgres",
    user = user,
    password = pw,
    sslmode = "require"
  )
}


supabase_last_update <- function(con, table_name = "basketball_players") {
  cols <- DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT column_name
       FROM information_schema.columns
       WHERE table_schema = 'public' AND table_name = '%s'",
      table_name
    )
  )$column_name

  candidates <- c(
    "last_update", "updated_at", "updated_on", "updatedon",
    "modified_at", "modified_on", "date_updated",
    "load_date", "as_of_date", "ingested_at", "ingest_ts", "created_at"
  )
  present <- intersect(candidates, cols)

  for (col in present) {
    q <- sprintf('SELECT MAX("%s") AS value FROM "%s"', col, table_name)
    res <- tryCatch(DBI::dbGetQuery(con, q), error = function(e) NULL)
    if (is.null(res) || !"value" %in% names(res) || nrow(res) == 0) {
      next
    }
    d <- coerce_to_date(res$value[[1]])
    if (!is.na(d)) {
      return(d)
    }
  }

  # Fallback to DB current date when no timestamp-like column exists.
  fallback <- tryCatch(DBI::dbGetQuery(con, "SELECT CURRENT_DATE AS value"), error = function(e) NULL)
  if (!is.null(fallback) && "value" %in% names(fallback) && nrow(fallback) > 0) {
    d <- coerce_to_date(fallback$value[[1]])
    if (!is.na(d)) {
      return(d)
    }
  }
  Sys.Date()
}

# ---------------------------------------------------------------------------
# Benchmark row names — loaded at startup in full (all cols incl *_pct).
# Used for profile labels and radar chart comparisons.
# ---------------------------------------------------------------------------
benchmark_names <- c(
  "D1 Avg (career) - C",
  "D1 Avg (career) - Combo G",
  "D1 Avg (career) - PF/C",
  "D1 Avg (career) - Pure PG",
  "D1 Avg (career) - Scoring PG",
  "D1 Avg (career) - Stretch 4",
  "D1 Avg (career) - Wing F",
  "D1 Avg (career) - Wing G",
  "1st Rder Avg (career) - C",
  "1st Rder Avg (career) - Combo G",
  "1st Rder Avg (career) - PF/C",
  "1st Rder Avg (career) - Pure PG",
  "1st Rder Avg (career) - Scoring PG",
  "1st Rder Avg (career) - Stretch 4",
  "1st Rder Avg (career) - Wing F",
  "1st Rder Avg (career) - Wing G",
  "2nd Rder Avg (career) - C",
  "2nd Rder Avg (career) - Combo G",
  "2nd Rder Avg (career) - PF/C",
  "2nd Rder Avg (career) - Pure PG",
  "2nd Rder Avg (career) - Scoring PG",
  "2nd Rder Avg (career) - Stretch 4",
  "2nd Rder Avg (career) - Wing F",
  "2nd Rder Avg (career) - Wing G"
)
# ---------------------------------------------------------------------------
# Persistent connection pool — shared across all sessions.
# Replaces per-query make_supabase_con() calls in reactive paths.
# ---------------------------------------------------------------------------
supabase_pool <- {
  host <- Sys.getenv("SUPABASE_HOST", unset = "aws-1-us-east-2.pooler.supabase.com")
  user <- Sys.getenv("SUPABASE_USER", unset = "postgres.mkrllsjvjliyxgukwfme")
  pw <- Sys.getenv("SUPABASE_DB_PASSWORD", unset = "")
  
  if (!nzchar(pw)) {
    stop("SUPABASE_DB_PASSWORD not set")
  }
  
  tryCatch({
    pool::dbPool(
      drv        = RPostgres::Postgres(),
      host       = host,
      port       = 5432L,
      dbname     = "postgres",
      user       = user,
      password   = pw,
      sslmode    = "require",
      minSize    = 1L,
      maxSize    = 5L,
      idleTimeout = 300
    )
  }, error = function(e) {
    stop("Failed to create Supabase pool: ", conditionMessage(e))
  })
}

# Fetch actual DB column names once at startup so build_table_query() can
# filter TABLE_DISPLAY_COLS to only columns that exist in Supabase.
DB_BASKETBALL_COLS <- tryCatch({
  DBI::dbGetQuery(
    supabase_pool,
    "SELECT column_name FROM information_schema.columns
     WHERE table_name = 'basketball_players' AND table_schema = 'public'"
  )$column_name
}, error = function(e) {
  warning("Could not fetch DB column list: ", conditionMessage(e))
  NULL  # NULL means no filtering applied — callers must handle
})

# Rebuild TABLE_DISPLAY_COLS filtered to only columns that actually exist in DB
if (!is.null(DB_BASKETBALL_COLS)) {
  TABLE_DISPLAY_COLS <- intersect(TABLE_DISPLAY_COLS, DB_BASKETBALL_COLS)
}

# ---------------------------------------------------------------------------
# Team logo lookup maps — loaded once at startup from mbb_institution_crosswalk.
# barttorvik_logo_map: barttorvik_team_name  -> logo_url  (for "Team" column)
# portal_logo_map:     portal_team_name      -> logo_url  (for "To Team" column)
# ---------------------------------------------------------------------------
.logo_df <- tryCatch({
  if (!is.null(supabase_pool)) {
    DBI::dbGetQuery(
      supabase_pool,
      "SELECT barttorvik_team_name, portal_team_name, logo_url
       FROM mbb_institution_crosswalk
       WHERE logo_url IS NOT NULL AND logo_url <> ''"
    )
  } else {
    data.frame(barttorvik_team_name = character(), portal_team_name = character(), logo_url = character())
  }
}, error = function(e) {
  warning("Could not load team logos: ", conditionMessage(e))
  data.frame(barttorvik_team_name = character(), portal_team_name = character(), logo_url = character())
})
barttorvik_logo_map <- if (nrow(.logo_df) > 0) setNames(.logo_df$logo_url, .logo_df$barttorvik_team_name) else character(0)
portal_logo_map     <- if (nrow(.logo_df) > 0) setNames(.logo_df$logo_url, .logo_df$portal_team_name)     else character(0)
rm(.logo_df)

# to_team_logo_map: to_team (raw portal string) -> logo_url
# Built by JOINing directly on mbb_transfer_portal.to_team so that lookup
# keys exactly match the strings portal_to_team_rv() returns at runtime,
# bypassing any crosswalk portal_team_name string-matching ambiguity.
to_team_logo_map <- tryCatch({
  if (!is.null(supabase_pool)) {
    .tt_df <- DBI::dbGetQuery(
      supabase_pool,
      "SELECT DISTINCT p.to_team, c.logo_url
       FROM mbb_transfer_portal p
       JOIN mbb_institution_crosswalk c
         ON LOWER(TRIM(p.to_team)) = LOWER(TRIM(c.portal_team_name))
       WHERE c.logo_url IS NOT NULL AND c.logo_url <> ''
         AND p.to_team IS NOT NULL AND p.to_team <> ''"
    )
    if (nrow(.tt_df) > 0) setNames(.tt_df$logo_url, .tt_df$to_team) else character(0)
  } else character(0)
}, error = function(e) {
  warning("Could not load to_team logo map: ", conditionMessage(e))
  character(0)
})

load_player_stats_source <- function() {
  cache_path <- ".player_stats_cache.rds"
  cache_meta_path <- ".player_stats_cache_meta.rds"
  cache_hours <- suppressWarnings(as.numeric(Sys.getenv("PLAYER_STATS_CACHE_HOURS", unset = "1")))
  if (is.na(cache_hours)) {
    cache_hours <- 1
  }

  read_cached <- function() {
    df <- readRDS(cache_path)
    md <- if (file.exists(cache_meta_path)) {
      tryCatch(readRDS(cache_meta_path), error = function(e) NULL)
    } else {
      NULL
    }
    lu <- if (is.list(md) && !is.null(md$last_update)) {
      coerce_to_date(md$last_update)
    } else {
      as.Date(file.info(cache_path)$mtime)
    }
    if (is.na(lu)) {
      lu <- Sys.Date()
    }
    list(data = df, last_update = lu, source = "cache")
  }

  if (cache_hours > 0 && file.exists(cache_path)) {
    age_h <- as.numeric(difftime(Sys.time(), file.info(cache_path)$mtime, units = "hours"))
    if (is.finite(age_h) && age_h < cache_hours) {
      message(sprintf("[player_stats] loaded from local cache (%.1fh old)", age_h))
      return(read_cached())
    }
  }

  out <- tryCatch(
    {
      con <- make_supabase_con()
      on.exit(DBI::dbDisconnect(con), add = TRUE)

      all_cols <- DBI::dbGetQuery(
        con,
        "SELECT column_name
         FROM information_schema.columns
         WHERE table_schema = 'public' AND table_name = 'basketball_players'
         ORDER BY ordinal_position"
      )$column_name
      if (length(all_cols) == 0) {
        stop("No columns found for Supabase table public.basketball_players")
      }

      # Select only the columns needed for the table, similarity, and profiles.
      # *_pct (percentile) columns are excluded here — they are fetched on demand
      # per selected player when the radar chart renders (see radar_percentile_data reactive).
      fetch_cols <- intersect(TABLE_DISPLAY_COLS, all_cols)
      if (length(fetch_cols) == 0) fetch_cols <- all_cols  # safe fallback
      col_sql <- paste(sprintf('"%s"', fetch_cols), collapse = ", ")
      df <- DBI::dbGetQuery(con, sprintf('SELECT %s FROM "basketball_players"', col_sql))
      lu <- supabase_last_update(con, "basketball_players")

      if (cache_hours > 0) {
        tryCatch(saveRDS(df, cache_path), error = function(e) NULL)
        tryCatch(saveRDS(list(last_update = lu, fetched_at = Sys.time()), cache_meta_path), error = function(e) NULL)
      }
      list(data = df, last_update = lu, source = "supabase")
    },
    error = function(e) {
      if (file.exists(cache_path)) {
        warning("Supabase load failed; falling back to cached player stats: ", conditionMessage(e))
        return(read_cached())
      }
      stop("Unable to load basketball_players from Supabase and no cache is available: ", conditionMessage(e))
    }
  )

  out
}

# Preserve the internal composite key expected throughout the app, even if an
# upstream source starts emitting the newer display name.
ensure_composite_source_aliases <- function(df) {
  if (!is.data.frame(df)) {
    return(df)
  }

  alias_sources <- c(
    `Ball Handling` = "Play Making",
    `Ball Handling_score` = "Play Making_score"
  )

  for (target_col in names(alias_sources)) {
    source_col <- alias_sources[[target_col]]
    if (!target_col %in% names(df) && source_col %in% names(df)) {
      df[[target_col]] <- df[[source_col]]
    }
  }

  df
}

# --------------------------------------------------------------------------------
# Helper: join composite percentile columns onto any player stats table
# --------------------------------------------------------------------------------
add_composites_to_player_stats <- function(df,
                                           composite_percentiles_df,
                                           composite_groups,
                                           name_col = "Name") {
  stopifnot(is.data.frame(df))
  stopifnot(is.data.frame(composite_percentiles_df))
  df <- ensure_composite_source_aliases(df)
  composite_percentiles_df <- ensure_composite_source_aliases(composite_percentiles_df)
  comp_cols <- names(composite_groups)

  if (!name_col %in% names(df)) {
    stop(glue::glue("add_composites_to_player_stats: '{name_col}' not found in df"))
  }
  if (!name_col %in% names(composite_percentiles_df)) {
    stop(glue::glue("add_composites_to_player_stats: '{name_col}' not found in composite_percentiles_df"))
  }

  comp_keep <- intersect(c(name_col, comp_cols), names(composite_percentiles_df))
  comp_tbl <- composite_percentiles_df %>%
    dplyr::select(dplyr::all_of(comp_keep)) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(setdiff(comp_keep, name_col)), ~ suppressWarnings(as.numeric(.x))))

  df %>%
    dplyr::select(-dplyr::any_of(comp_cols)) %>%
    dplyr::left_join(comp_tbl, by = stats::setNames(name_col, name_col))
}

compute_app_data <- function() {
  loaded <- load_player_stats_source()
  player_stats_all <- ensure_composite_source_aliases(loaded$data)
  last_update <- coerce_to_date(loaded$last_update)
  if (is.na(last_update)) {
    last_update <- Sys.Date()
  }

  if (!is.data.frame(player_stats_all) || nrow(player_stats_all) == 0) {
    stop("Supabase basketball_players returned no rows.")
  }
  if (!"Name" %in% names(player_stats_all)) {
    stop("Supabase basketball_players is missing required column: Name")
  }

  player_stats_all$Name <- as.character(player_stats_all$Name)
  for (nm in intersect(c("Team", "Role", "Player"), names(player_stats_all))) {
    player_stats_all[[nm]] <- as.character(player_stats_all[[nm]])
  }
  for (nm in intersect(c("Year", "Pick", "ptir", "dsr", "dpmr", "edmr", rank_vars), names(player_stats_all))) {
    player_stats_all[[nm]] <- suppressWarnings(as.numeric(player_stats_all[[nm]]))
  }

  # Some raw percentage/rate fields can arrive on a 0-1 scale from source systems.
  # Normalize those to 0-100 so table display + filtering behave consistently.
  fraction_percent_cols <- c(
    "Min_pct", "eFG", "TS", "FTP", "FTR", "3PR", "3PP", "Far2_P", "Close2_P", "2PP",
    "OR_pct", "DR_pct", "Ast_per", "TO", "Stl", "Stps", "Blk", "Usg",
    "three_unastd_pct", "mid_unastd_pct", "rim_unastd_pct", "dunk_unastd_pct", "unastd_fg_pct"
  )
  for (nm in intersect(fraction_percent_cols, names(player_stats_all))) {
    x <- suppressWarnings(as.numeric(player_stats_all[[nm]]))
    finite <- is.finite(x)
    if (!any(finite)) {
      next
    }
    xmin <- suppressWarnings(min(x[finite], na.rm = TRUE))
    xmax <- suppressWarnings(max(x[finite], na.rm = TRUE))
    if (is.finite(xmin) && is.finite(xmax) && xmin >= 0 && xmax <= 1.5) {
      player_stats_all[[nm]] <- x * 100
    } else {
      player_stats_all[[nm]] <- x
    }
  }

  if (all(c("Year", "Team") %in% names(player_stats_all))) {
    y <- suppressWarnings(as.numeric(player_stats_all$Year))
    current_year <- suppressWarnings(max(y, na.rm = TRUE))
    if (is.finite(current_year)) {
      fill_idx <- is.na(y) & player_stats_all$Team == "001 - Averages"
      y[fill_idx] <- current_year
      player_stats_all$Year <- y
    }
  }

  pct_cols_needed <- paste0(radar_vars_all, "_pct")
  if (all(pct_cols_needed %in% names(player_stats_all))) {
    percentile_stats <- player_stats_all %>%
      dplyr::select(Name, dplyr::any_of(c("Year", "Pick")), dplyr::any_of(pct_cols_needed))
    for (stat in radar_vars_all) {
      pct_col <- paste0(stat, "_pct")
      if (pct_col %in% names(percentile_stats)) {
        names(percentile_stats)[names(percentile_stats) == pct_col] <- stat
      }
    }
  } else {
    message("[compute_app_data] pre-computed *_pct columns missing; deriving percentiles from raw stats.")
    stat_cols <- intersect(radar_vars_all, names(player_stats_all))
    percentile_stats <- player_stats_all %>%
      dplyr::select(Name, dplyr::any_of(c("Year", "Pick")), dplyr::any_of(stat_cols)) %>%
      dplyr::mutate(dplyr::across(dplyr::any_of(stat_cols), ~ suppressWarnings(as.numeric(.x))))

    inverse_stats <- intersect(c("D-PRPG", "DRtg", "FC/40", "TO"), stat_cols)
    if (length(inverse_stats) > 0) {
      percentile_stats <- percentile_stats %>%
        dplyr::mutate(dplyr::across(dplyr::all_of(inverse_stats), ~ -.x))
    }

    pct_transform <- function(x) {
      x <- suppressWarnings(as.numeric(x))
      if (all(is.na(x))) {
        return(rep(NA_real_, length(x)))
      }
      dplyr::percent_rank(x) * 100
    }

    if ("Year" %in% names(percentile_stats)) {
      percentile_stats <- percentile_stats %>%
        dplyr::group_by(.data$Year) %>%
        dplyr::mutate(dplyr::across(dplyr::any_of(stat_cols), pct_transform)) %>%
        dplyr::ungroup()
    } else {
      percentile_stats <- percentile_stats %>%
        dplyr::mutate(dplyr::across(dplyr::any_of(stat_cols), pct_transform))
    }
  }

  score_cols_needed <- paste0(names(composite_groups), "_score")
  if (all(score_cols_needed %in% names(player_stats_all))) {
    composite_percentiles_df <- player_stats_all %>%
      dplyr::select(Name, dplyr::any_of(score_cols_needed))
    for (grp in names(composite_groups)) {
      score_col <- paste0(grp, "_score")
      if (score_col %in% names(composite_percentiles_df)) {
        names(composite_percentiles_df)[names(composite_percentiles_df) == score_col] <- grp
      }
    }
  } else {
    message("[compute_app_data] pre-computed *_score columns missing; deriving composite scores from percentiles.")
    composite_percentiles_df <- percentile_stats %>%
      dplyr::select(Name)

    for (grp in names(composite_groups)) {
      w <- composite_groups[[grp]]
      vars <- intersect(names(w), names(percentile_stats))
      if (length(vars) == 0) {
        composite_percentiles_df[[grp]] <- NA_real_
        next
      }
      vals <- percentile_stats %>%
        dplyr::select(dplyr::all_of(vars)) %>%
        dplyr::mutate(dplyr::across(dplyr::everything(), ~ suppressWarnings(as.numeric(.x))))
      mat <- as.matrix(vals)
      ww <- as.numeric(w[vars])
      ww_mat <- matrix(ww, nrow = nrow(mat), ncol = length(ww), byrow = TRUE)
      num <- rowSums(mat * ww_mat, na.rm = TRUE)
      den <- rowSums((!is.na(mat)) * ww_mat, na.rm = TRUE)
      composite_percentiles_df[[grp]] <- ifelse(den > 0, num / den, NA_real_)
    }
  }

  rank_cols <- intersect(c("Name", rank_vars, "dsr", "ptir", "dpmr", "edmr"), names(player_stats_all))
  if (length(rank_cols) > 1) {
    rank_tbl <- player_stats_all %>%
      dplyr::select(dplyr::any_of(rank_cols)) %>%
      dplyr::distinct(.data$Name, .keep_all = TRUE)
    composite_percentiles_df <- composite_percentiles_df %>%
      dplyr::left_join(rank_tbl, by = "Name")
  }

  if (all(names(composite_groups) %in% names(player_stats_all))) {
    player_stats_all_with_composites <- player_stats_all
  } else {
    player_stats_all_with_composites <- add_composites_to_player_stats(
      player_stats_all,
      composite_percentiles_df,
      composite_groups
    )
  }

  list(
    player_stats_all = player_stats_all,
    last_update = last_update,
    percentile_stats = percentile_stats,
    composite_percentiles_df = composite_percentiles_df,
    player_stats_all_with_composites = player_stats_all_with_composites
  )
}

app_data <- compute_app_data()
player_stats_all <- app_data$player_stats_all
last_update <- app_data$last_update
percentile_stats <- app_data$percentile_stats
composite_percentiles_df <- app_data$composite_percentiles_df
player_stats_all_with_composites <- app_data$player_stats_all_with_composites
rm(app_data)

# portal_pids_vec is intentionally NOT set at global scope.
# It is fetched fresh per session inside shinyServer() as a reactive,
# so the app picks up newly confirmed matches without a process restart.
# See portal_pids_rv reactive defined inside shinyServer below.

# ---------------------------------------------------------------------------
# Benchmark rows — loaded once at startup with all columns (incl *_pct).
# Used by radar_percentile_data() to supply comparison averages to the radar
# chart and profile labels without re-querying the full table.
# ---------------------------------------------------------------------------
benchmark_data <- tryCatch({
  if (!is.null(supabase_pool)) {
    safe_names <- paste(sprintf("'%s'", gsub("'", "''", benchmark_names)), collapse = ", ")
    DBI::dbGetQuery(
      supabase_pool,
      sprintf('SELECT * FROM "basketball_players" WHERE "Name" IN (%s)', safe_names)
    )
  } else {
    # Pool not available — fall back to rows already in player_stats_all
    player_stats_all %>% dplyr::filter(is_benchmark_row(.data$Name))
  }
}, error = function(e) {
  warning("Could not load benchmark_data: ", conditionMessage(e))
  player_stats_all %>% dplyr::filter(is_benchmark_row(.data$Name))
})

# --------------------------------------------------------------------------------
# Profile labels
# --------------------------------------------------------------------------------
strength_threshold <- 85
weakness_threshold <- 20

get_profile_labels <- function(player_row, role, percentile_stats_df) {
  player_row <- unlist(player_row, use.names = TRUE)
  overlap <- intersect(names(player_row), names(percentile_stats_df))
  if (length(overlap) == 0) {
    return(list(strengths = character(0), weaknesses = character(0)))
  }

  frontcourt_roles2 <- c("C", "PF/C")
  wings_roles2 <- c("Wing F", "Wing G", "Stretch 4")
  guard_roles2 <- c("Pure PG", "Scoring PG", "Combo G")

  role_bucket <- dplyr::case_when(
    is.na(role) ~ NA_character_,
    role %in% frontcourt_roles2 ~ "Frontcourt",
    role %in% wings_roles2 ~ "Wings",
    role %in% guard_roles2 ~ "Guards",
    TRUE ~ role
  )

  is_frontcourt_role <- !is.na(role) && role %in% c("C", "PF/C")

  make_comp_names <- function(prefix, bucket, exact_role) {
    out <- c()
    if (!is.na(bucket) && bucket %in% c("Frontcourt", "Wings", "Guards")) {
      out <- c(out, paste0(prefix, " - ", bucket), paste0(prefix, " (career) - ", bucket))
    }
    if (!is.na(exact_role) && nzchar(exact_role)) {
      out <- c(out, paste0(prefix, " - ", exact_role), paste0(prefix, " (career) - ", exact_role))
    }
    unique(out)
  }

  d1_names <- make_comp_names("D1 Avg", role_bucket, role)
  rd1_names <- make_comp_names("1st Rder Avg", role_bucket, role)

  safe_comp_row <- function(df, name_vec, overlap_cols) {
    hit <- df %>%
      dplyr::filter(.data$Name %in% name_vec) %>%
      dplyr::select(dplyr::all_of(overlap_cols))
    if (nrow(hit) == 0) {
      out <- rep(NA_real_, length(overlap_cols))
      names(out) <- overlap_cols
      return(out)
    }
    hit %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ suppressWarnings(as.numeric(.x)))) %>%
      dplyr::summarise(dplyr::across(dplyr::everything(), ~ mean(.x, na.rm = TRUE))) %>%
      unlist(use.names = TRUE)
  }

  d1_avg_row <- safe_comp_row(percentile_stats_df, d1_names, overlap)
  first_rd_row <- safe_comp_row(percentile_stats_df, rd1_names, overlap)

  player_row_num <- suppressWarnings(as.numeric(player_row[overlap]))
  names(player_row_num) <- overlap

  dist_from_d1 <- player_row_num - d1_avg_row
  dist_from_rd1 <- player_row_num - first_rd_row

  composite_groups_local <- list(
    `Ball Handling` = c("Ast", "ATO", "TO", "Ast_per"),
    `Off. Efficiency` = c("eFG", "TS", "2PP", "PRPG", "OBPM", "ORtg", "Usg", "OR_pct", "ATO"),
    Rebounding = c("OR_pct", "DR_pct", "Reb"),
    Defense = c("D-PRPG", "DRtg", "DBPM", "Stl", "Stps", "Blk", "DR_pct", "FC/40", "foul_efficiency"),
    Scoring = c("Pts", "FTR", "Usg", "eFG", "TS", "2PM", "2PP", "3PM", "FGA"),
    `Perimeter Off.` = c("3PM", "3PA", "3P/100", "3PR"),
    `Interior Offense` = c("Close2_A", "Close2_P", "FTR", "Close2_Rt", "Close2_M"),
    `Unassisted FGs` = c("unastd_fg_pct", "unastd_fgs")
  )

  composite_strengths <- character(0)
  composite_weaknesses <- character(0)
  raw_stats_to_exclude <- character(0)

  for (group in names(composite_groups_local)) {
    vars <- composite_groups_local[[group]]
    common <- intersect(vars, names(player_row_num))
    if (length(common) == 0) next
    avg_score <- mean(player_row_num[common], na.rm = TRUE)
    if (!is.na(avg_score)) {
      if (avg_score >= strength_threshold) {
        composite_strengths <- c(composite_strengths, group)
        raw_stats_to_exclude <- c(raw_stats_to_exclude, common)
      } else if (avg_score <= weakness_threshold) {
        composite_weaknesses <- c(composite_weaknesses, group)
        raw_stats_to_exclude <- c(raw_stats_to_exclude, common)
      }
    }
  }

  if (is_frontcourt_role) {
    composite_weaknesses <- setdiff(composite_weaknesses, "Perimeter Off.")
  }

  manually_exclude <- c(
    "Close2_A", "Far2_A", "Year", "Pick",
    "dsr", "ptir", "dpmr", "edmr", "PRPG", "D-PRPG"
  )
  raw_stats_to_exclude <- unique(c(raw_stats_to_exclude, manually_exclude))

  dist_from_d1 <- dist_from_d1[setdiff(names(dist_from_d1), raw_stats_to_exclude)]
  dist_from_rd1 <- dist_from_rd1[setdiff(names(dist_from_rd1), raw_stats_to_exclude)]

  raw_strengths <- names(sort(dist_from_rd1, decreasing = TRUE))
  raw_strengths <- raw_strengths[!is.na(dist_from_rd1[raw_strengths])]
  raw_strengths <- head(raw_strengths, 5)

  raw_weaknesses <- names(sort(dist_from_d1, decreasing = FALSE))
  raw_weaknesses <- raw_weaknesses[!is.na(dist_from_d1[raw_weaknesses])]
  if (is_frontcourt_role) {
    no_3pt_weak_stats <- c("3PA", "3PM", "3PR", "3PP", "3P%", "3P/100", "3PT/100", "three_unastd_pct")
    raw_weaknesses <- setdiff(raw_weaknesses, no_3pt_weak_stats)
  }
  raw_weaknesses <- head(raw_weaknesses, 5)

  abs_weak_cutoff <- 40
  abs_strength_cutoff <- 70

  player_row_num_filtered <- player_row_num[setdiff(names(player_row_num), manually_exclude)]
  abs_weak <- names(sort(player_row_num_filtered, decreasing = FALSE))
  abs_weak <- abs_weak[!is.na(player_row_num_filtered[abs_weak]) & player_row_num_filtered[abs_weak] <= abs_weak_cutoff]
  if (is_frontcourt_role) {
    no_3pt_weak_stats <- c("3PA", "3PM", "3PR", "3PP", "3P%", "3P/100", "3PT/100", "three_unastd_pct")
    abs_weak <- setdiff(abs_weak, no_3pt_weak_stats)
  }
  abs_weak <- head(abs_weak, 5)

  abs_str <- names(sort(player_row_num_filtered, decreasing = TRUE))
  abs_str <- abs_str[!is.na(player_row_num_filtered[abs_str]) & player_row_num_filtered[abs_str] >= abs_strength_cutoff]
  abs_str <- head(abs_str, 5)

  if (length(raw_weaknesses) == 0) raw_weaknesses <- abs_weak
  if (length(raw_strengths) == 0) raw_strengths <- abs_str

  rename_labels <- function(x) {
    rename_map <- c(
      Close2_Rt = "Rim Att Rt",
      Far2_Rt = "Mid Att Rt",
      `3PR` = "3PA Rt",
      Close2_M = "Rim FGs",
      Close2_P = "Rim FG%",
      Far2_P = "Mid FG%",
      Far2_M = "Mid FGs"
    )
    x <- ifelse(x %in% names(rename_map), rename_map[x], x)
    x <- display_composite_name(x)
    unname(x)
  }

  list(
    strengths = rename_labels(unique(c(composite_strengths, raw_strengths))),
    weaknesses = rename_labels(unique(c(composite_weaknesses, raw_weaknesses)))
  )
}

# ------------------------------------------------------------------------------
# Fixes for dplyr >= 1.1.0:
# 1) replace cur_data() with pick()
# 2) replace summarise() that returns >1 row with reframe()
# ------------------------------------------------------------------------------

prepare_radar_long <- function(players,
                               view_mode = c("raw", "composite"),
                               percentile_stats,
                               composite_percentiles_df,
                               player_stats_all,
                               radar_vars_all,
                               composite_groups,
                               stat_labels) {
  view_mode <- match.arg(view_mode)
  players <- unique(players)

  if (view_mode == "raw") {
    cols <- intersect(radar_vars_all, names(percentile_stats))

    raw_lookup <- player_stats_all %>%
      filter(Name %in% players) %>%
      select(Name, any_of(cols))

    df <- percentile_stats %>%
      filter(Name %in% players) %>%
      select(Name, all_of(cols)) %>%
      pivot_longer(-Name, names_to = "axis", values_to = "percentile") %>%
      left_join(
        raw_lookup %>%
          pivot_longer(-Name, names_to = "axis", values_to = "raw_value"),
        by = c("Name", "axis")
      ) %>%
      mutate(
        axis_label = purrr::map_chr(axis, ~ label_stat(.x, stat_labels)),
        percentile = suppressWarnings(as.numeric(percentile)),
        hover = paste0(
          "<b>", htmlEscape(Name), "</b>",
          "<br>", htmlEscape(axis_label),
          "<br>Raw: <b>", ifelse(is.na(raw_value), "NA", round(raw_value, 2)), "</b>",
          "<br>Percentile: ", ifelse(is.na(percentile), "NA", sprintf("%.1f", percentile))
        ),
        value = percentile
      )

    return(df)
  }

  # ----------------------------- COMPOSITE ------------------------------------
  comp_axes <- names(composite_groups)
  component_cols <- unique(unlist(map(composite_groups, names)))

  # Lookup tables (KEEP SEPARATE): percentiles vs raw values
  pct_lookup <- percentile_stats %>%
    filter(Name %in% players) %>%
    select(Name, any_of(component_cols))

  raw_lookup <- player_stats_all %>%
    filter(Name %in% players) %>%
    select(Name, any_of(component_cols))

  dfc <- composite_percentiles_df %>%
    filter(Name %in% players) %>%
    select(Name, any_of(comp_axes)) %>%
    pivot_longer(-Name, names_to = "axis", values_to = "value") %>%
    rowwise() %>%
    mutate(
      axis_label = as.character(axis),
      value = suppressWarnings(as.numeric(value)),
      hover = {
        nm <- Name
        build_composite_hover(
          player_name = nm,
          player_row_pct = pct_lookup %>% filter(.data$Name == nm) %>% select(-Name),
          player_row_raw = raw_lookup %>% filter(.data$Name == nm) %>% select(-Name),
          composite_name = as.character(axis),
          composite_value = value,
          composite_groups = composite_groups,
          stat_labels = stat_labels
        )
      }
    ) %>%
    ungroup() %>%
    select(Name, axis, axis_label, value, hover)

  dfc
}

# --- plot function: add axis_order + enforce factor levels ---------------------
plot_radar_plotly <- function(players,
                              view_mode = c("raw", "composite"),
                              percentile_stats,
                              composite_percentiles_df,
                              radar_vars_all,
                              composite_groups,
                              stat_labels,
                              title = NULL,
                              axis_order = NULL,
                              axis_labels = NULL,
                              palette = NULL,
                              fill_alpha = 0.16,
                              line_width = 2.8,
                              marker_size = 6) {
  view_mode <- match.arg(view_mode)

  df_long <- prepare_radar_long(
    players = players,
    view_mode = view_mode,
    percentile_stats = percentile_stats,
    composite_percentiles_df = composite_percentiles_df,
    player_stats_all = player_stats_all, # relies on global already loaded
    radar_vars_all = radar_vars_all,
    composite_groups = composite_groups,
    stat_labels = stat_labels
  ) %>%
    filter(!is.na(axis_label)) %>%
    mutate(axis_label = as.character(axis_label))

  # For composite mode, allow a named label map: names = raw composite axis names,
  # values = display labels (e.g., "Offensive Efficiency" -> "OFF EFF").
  if (identical(view_mode, "composite") && !is.null(axis_labels)) {
    # axis_label is the RAW composite axis name (e.g., "Offensive Efficiency")
    df_long <- df_long %>%
      mutate(
        axis_display = dplyr::if_else(
          .data$axis_label %in% names(axis_labels),
          unname(axis_labels[.data$axis_label]),
          .data$axis_label
        )
      )
  } else {
    df_long <- df_long %>% mutate(axis_display = .data$axis_label)
  }

  # --- enforce axis order (only if provided) ----------------------------------
  # IMPORTANT: axis_order should always be expressed in RAW axis names.
  if (!is.null(axis_order)) {
    axis_levels_raw <- axis_order[axis_order %in% df_long$axis_label]
  } else {
    axis_levels_raw <- df_long %>%
      distinct(axis_label) %>%
      pull(axis_label)
  }

  df_long <- df_long %>%
    mutate(
      axis_factor = factor(.data$axis_label, levels = axis_levels_raw)
    )

  # Close polygon in explicit order
  df_closed <- df_long %>%
    group_by(Name) %>%
    arrange(axis_factor) %>%
    reframe(
      axis_factor  = c(as.character(axis_factor), as.character(axis_factor)[1]),
      axis_display = c(as.character(axis_display), as.character(axis_display)[1]),
      value        = c(value, value[1]),
      hover        = c(hover, hover[1])
    )

  default_palette <- c("#0077BB", "#EE7733", "#009988", "#CC3311", "#33BBEE", "#EE3377", "#BBBBBB")
  pal <- if (!is.null(palette)) palette else default_palette

  p <- plot_ly()
  player_names <- unique(df_closed$Name)
  for (i in seq_along(player_names)) {
    nm <- player_names[[i]]
    one <- df_closed %>% filter(Name == nm)
    trace_col <- pal[((i - 1) %% length(pal)) + 1]

    p <- p %>%
      add_trace(
        data = one,
        type = "scatterpolar",
        mode = "lines+markers",
        r = ~value,
        theta = ~axis_display,
        name = nm,
        showlegend = TRUE,
        text = ~hover,
        hoverinfo = "text",
        fill = "toself",
        fillcolor = grDevices::adjustcolor(trace_col, alpha.f = fill_alpha),
        line = list(color = trace_col, width = line_width),
        marker = list(color = trace_col, size = marker_size)
      )
  }

  p %>%
    layout(
      # MODIFIED: Allow title to be a list (e.g., with font size for mobile)
      title = if (is.list(title)) title else list(text = title %||% paste0("Radar (", view_mode, ")")),
      showlegend = TRUE,
      polar = list(
        domain = list(x = c(0.05, 0.95), y = c(0, 0.87)),
        radialaxis = list(
          visible  = TRUE,
          range    = c(0, 100),
          tickvals = c(0, 25, 50, 75, 100),
          tickfont = list(size = 13)
        ),
        angularaxis = list(tickfont = list(size = 19))
      ),
      legend = list(
        orientation   = "v",
        x             = 0.5,
        xanchor       = "center",
        y             = 1.0,
        yanchor       = "bottom",
        font          = list(size = 17),
        tracegroupgap = 8
      ),
      margin = list(l = 40, r = 40, t = 115, b = 10)
    )
}

# --------------------------------------------------------------------------------
# Similarity Helpers
# --------------------------------------------------------------------------------
find_similar_drafted_by_distance <- function(player_name, top_n = 10) {
  if (!exists("composite_groups", inherits = TRUE)) stop("composite_groups not found")
  if (!exists("composite_percentiles_df", inherits = TRUE)) stop("composite_percentiles_df not found")
  if (!exists("player_stats_all", inherits = TRUE)) stop("player_stats_all not found")

  comp_cols <- names(composite_groups)

  # --- helpers ---------------------------------------------------------------
  to_num <- function(x) suppressWarnings(as.numeric(x))
  safe_num_col <- function(df, col) {
    if (col %in% names(df)) to_num(df[[col]]) else rep(NA_real_, nrow(df))
  }

  # class normalization from Player field
  norm_class <- function(x) {
    x <- toupper(trimws(as.character(x)))
    dplyr::case_when(
      x %in% c("FR", "FRESHMAN") ~ "FR",
      x %in% c("SO", "SOPHOMORE") ~ "SO",
      x %in% c("JR", "JUNIOR") ~ "JR",
      x %in% c("SR", "SENIOR") ~ "SR",
      TRUE ~ NA_character_
    )
  }

  class_ok_set <- function(cls) {
    dplyr::case_when(
      is.na(cls) ~ NA_character_, # handled separately
      cls == "FR" ~ "FR,SO",
      cls == "SO" ~ "FR,SO,JR",
      cls == "JR" ~ "SO,JR,SR",
      cls == "SR" ~ "JR,SR",
      TRUE ~ NA_character_
    ) |>
      as.character() |>
      strsplit(",", fixed = TRUE) |>
      (\(z) if (length(z) == 0) character(0) else z[[1]])()
  }

  # --- target lookup (raw table) --------------------------------------------
  target_raw <- player_stats_all %>%
    dplyr::filter(.data$Name == player_name) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::select(dplyr::any_of(c("Name", "Role", "Ht", "Player")))

  if (nrow(target_raw) == 0) {
    return(tibble::tibble(Name = character(), distance = numeric()))
  }

  frontcourt_roles <- c("C", "PF/C")
  wings_roles <- c("Wing F", "Wing G", "Stretch 4")
  backcourt_roles <- c("Pure PG", "Scoring PG", "Combo G")

  target_bucket <- dplyr::case_when(
    target_raw$Role[1] %in% frontcourt_roles ~ "Frontcourt",
    target_raw$Role[1] %in% wings_roles ~ "Wings",
    target_raw$Role[1] %in% backcourt_roles ~ "Backcourt",
    TRUE ~ "Other"
  )

  target_ht <- if ("Ht" %in% names(target_raw)) to_num(target_raw$Ht[1]) else NA_real_
  target_class <- if ("Player" %in% names(target_raw)) norm_class(target_raw$Player[1]) else NA_character_
  ok_classes <- if (!is.na(target_class)) class_ok_set(target_class) else character(0)

  # --- build comp ONCE (this fixes "comp not found") -------------------------
  comp <- composite_percentiles_df %>%
    dplyr::left_join(
      player_stats_all %>%
        dplyr::select(dplyr::any_of(c("Name", "Role", "Pick", "Ht", "Player"))) %>%
        dplyr::distinct(),
      by = "Name"
    ) %>%
    dplyr::mutate(
      Pick = to_num(.data$Pick),
      Ht = if ("Ht" %in% names(.)) to_num(.data$Ht) else NA_real_,
      Player = if ("Player" %in% names(.)) as.character(.data$Player) else NA_character_,
      class_bucket = norm_class(.data$Player),
      is_agnostic_class = is.na(.data$class_bucket) | !nzchar(.data$class_bucket),
      role_bucket = dplyr::case_when(
        .data$Role %in% frontcourt_roles ~ "Frontcourt",
        .data$Role %in% wings_roles ~ "Wings",
        .data$Role %in% backcourt_roles ~ "Backcourt",
        TRUE ~ "Other"
      )
    )

  if (!(player_name %in% comp$Name)) {
    return(tibble::tibble(Name = character(), distance = numeric()))
  }

  # --- target vector ----------------------------------------------------------
  target_vec <- comp %>%
    dplyr::filter(.data$Name == player_name) %>%
    dplyr::select(dplyr::all_of(comp_cols)) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), to_num)) %>%
    as.matrix() %>%
    as.numeric()

  if (all(is.na(target_vec))) {
    return(tibble::tibble(Name = character(), distance = numeric()))
  }

  # --- candidate filtering ----------------------------------------------------
  others <- comp %>%
    dplyr::filter(
      .data$Name != player_name,
      .data$role_bucket == target_bucket,
      !is.na(.data$Pick)
    )

  # height ± 3 inches (only if we have both target and candidate heights)
  if (!is.na(target_ht) && "Ht" %in% names(others)) {
    others <- others %>%
      dplyr::filter(is.na(.data$Ht) | abs(.data$Ht - target_ht) <= 3)
  }

  # class filter:
  # - if target has a class, compare to allowed classes OR class-agnostic rows (averages)
  # - if target class is NA, do not class-filter at all
  if (!is.na(target_class)) {
    others <- others %>%
      dplyr::filter((.data$class_bucket %in% ok_classes) | .data$is_agnostic_class)
  }

  if (nrow(others) == 0) {
    return(tibble::tibble(Name = character(), distance = numeric()))
  }

  # --- distance ---------------------------------------------------------------
  out <- others %>%
    dplyr::select(Name, dplyr::all_of(comp_cols)) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(comp_cols), to_num)) %>%
    dplyr::filter(dplyr::if_any(dplyr::all_of(comp_cols), ~ !is.na(.x))) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(distance = {
      diffs <- c_across(dplyr::all_of(comp_cols)) - target_vec
      if (all(is.na(diffs))) NA_real_ else sqrt(sum(diffs^2, na.rm = TRUE))
    }) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(distance)) %>%
    dplyr::arrange(distance) %>%
    dplyr::slice_head(n = top_n) %>%
    dplyr::select(Name, distance)

  out
}


render_similar_players_ui <- function(player_name, top_n = 10) {
  similar_players <- find_similar_drafted_by_distance(player_name, top_n)
  if (nrow(similar_players) == 0 || all(is.na(similar_players$distance))) {
    return(NULL)
  }

  tags$ul(
    lapply(seq_len(nrow(similar_players)), function(i) {
      name <- similar_players$Name[i]
      dist <- round(similar_players$distance[i], 2)

      tags$li(
        tags$a(
          href = "#",
          onclick = sprintf(
            "Shiny.setInputValue('similar_player_click', '%s', {priority: 'event'})",
            name
          ),
          style = "cursor:pointer; font-weight:500;",
          paste0(name, " (dist: ", dist, ")")
        )
      )
    })
  )
}


# --- Similar CURRENT players (Year == 2026) by composite distance -------------
find_similar_current_by_distance <- function(player_name, top_n = 10, class_filter = NULL) {
  if (!exists("composite_groups", inherits = TRUE)) stop("composite_groups not found")
  if (!exists("composite_percentiles_df", inherits = TRUE)) stop("composite_percentiles_df not found")
  if (!exists("player_stats_all", inherits = TRUE)) stop("player_stats_all not found")

  comp_cols <- names(composite_groups)

  to_num <- function(x) suppressWarnings(as.numeric(x))

  norm_class <- function(x) {
    x <- toupper(trimws(as.character(x)))
    dplyr::case_when(
      x %in% c("FR", "FRESHMAN") ~ "FR",
      x %in% c("SO", "SOPHOMORE") ~ "SO",
      x %in% c("JR", "JUNIOR") ~ "JR",
      x %in% c("SR", "SENIOR") ~ "SR",
      TRUE ~ NA_character_
    )
  }

  # target lookup (raw)
  target_raw <- player_stats_all %>%
    dplyr::filter(.data$Name == player_name) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::select(dplyr::any_of(c("Name", "Role", "Ht", "Player")))

  if (nrow(target_raw) == 0) {
    return(tibble::tibble(Name = character(), distance = numeric()))
  }

  frontcourt_roles <- c("C", "PF/C")
  wings_roles <- c("Wing F", "Wing G", "Stretch 4")
  backcourt_roles <- c("Pure PG", "Scoring PG", "Combo G")

  target_bucket <- dplyr::case_when(
    target_raw$Role[1] %in% frontcourt_roles ~ "Frontcourt",
    target_raw$Role[1] %in% wings_roles ~ "Wings",
    target_raw$Role[1] %in% backcourt_roles ~ "Backcourt",
    TRUE ~ "Other"
  )

  target_ht <- if ("Ht" %in% names(target_raw)) to_num(target_raw$Ht[1]) else NA_real_

  # build comp with Year/Role/Ht/Class info
  comp <- composite_percentiles_df %>%
    dplyr::left_join(
      player_stats_all %>%
        dplyr::select(dplyr::any_of(c("Name", "Year", "Role", "Ht", "Player"))) %>%
        dplyr::distinct(),
      by = "Name"
    ) %>%
    dplyr::mutate(
      Year = to_num(.data$Year),
      Ht = if ("Ht" %in% names(.)) to_num(.data$Ht) else NA_real_,
      Player = if ("Player" %in% names(.)) as.character(.data$Player) else NA_character_,
      class_bucket = norm_class(.data$Player),
      role_bucket = dplyr::case_when(
        .data$Role %in% frontcourt_roles ~ "Frontcourt",
        .data$Role %in% wings_roles ~ "Wings",
        .data$Role %in% backcourt_roles ~ "Backcourt",
        TRUE ~ "Other"
      )
    )

  ptir_pred <- if ("PredTransferImpactRating" %in% names(comp)) to_num(comp$PredTransferImpactRating) else rep(NA_real_, nrow(comp))
  ptir_raw <- if ("ptir" %in% names(comp)) to_num(comp$ptir) else rep(NA_real_, nrow(comp))
  comp <- comp %>%
    dplyr::mutate(ptir_value = dplyr::coalesce(ptir_pred, ptir_raw))

  if (!(player_name %in% comp$Name)) {
    return(tibble::tibble(Name = character(), distance = numeric()))
  }

  # target vector
  target_vec <- comp %>%
    dplyr::filter(.data$Name == player_name) %>%
    dplyr::select(dplyr::all_of(comp_cols)) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), to_num)) %>%
    as.matrix() %>%
    as.numeric()

  if (all(is.na(target_vec))) {
    return(tibble::tibble(Name = character(), distance = numeric()))
  }

  # candidates: current year only, same role bucket
  others <- comp %>%
    dplyr::filter(
      .data$Name != player_name,
      .data$Year == 2026,
      .data$role_bucket == target_bucket
    )

  # optional height ± 3 inches
  if (!is.na(target_ht) && "Ht" %in% names(others)) {
    others <- others %>%
      dplyr::filter(is.na(.data$Ht) | abs(.data$Ht - target_ht) <= 3)
  }

  # Always restrict to transfer-eligible classes (FR/SO/JR only — exclude SR)
  others <- others %>%
    dplyr::filter(.data$class_bucket %in% c("FR", "SO", "JR"))

  # Optional further narrowing by specific class from UI
  cls <- toupper(trimws(as.character(class_filter %||% "")))
  if (nzchar(cls) && cls %in% c("FR", "SO", "JR")) {
    others <- others %>% dplyr::filter(.data$class_bucket == cls)
  }

  if (nrow(others) == 0) {
    return(tibble::tibble(Name = character(), distance = numeric()))
  }

  out <- others %>%
    dplyr::select(Name, ptir_value, dplyr::all_of(comp_cols)) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(comp_cols), to_num)) %>%
    dplyr::filter(dplyr::if_any(dplyr::all_of(comp_cols), ~ !is.na(.x))) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(distance = {
      diffs <- c_across(dplyr::all_of(comp_cols)) - target_vec
      if (all(is.na(diffs))) NA_real_ else sqrt(sum(diffs^2, na.rm = TRUE))
    }) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(distance)) %>%
    dplyr::arrange(distance) %>%
    dplyr::slice_head(n = top_n) %>%
    dplyr::mutate(PTIR = ifelse(is.finite(.data$ptir_value), round(.data$ptir_value, 1), NA_real_)) %>%
    dplyr::select(Name, distance, PTIR)

  out
}

render_similar_current_players_ui <- function(player_name, top_n = 10, class_filter = NULL) {
  similar_players <- find_similar_current_by_distance(player_name, top_n = top_n, class_filter = class_filter)
  if (nrow(similar_players) == 0 || all(is.na(similar_players$distance))) {
    return(NULL)
  }

  tags$ul(
    lapply(seq_len(nrow(similar_players)), function(i) {
      name <- similar_players$Name[i]
      dist <- round(similar_players$distance[i], 2)
      ptir <- if ("PTIR" %in% names(similar_players)) suppressWarnings(as.numeric(similar_players$PTIR[i])) else NA_real_
      ptir_txt <- if (is.finite(ptir)) sprintf("%.1f", ptir) else "n/a"

      tags$li(
        tags$a(
          href = "#",
          onclick = sprintf(
            "Shiny.setInputValue('similar_player_click', '%s', {priority: 'event'})",
            name
          ),
          style = "cursor:pointer; font-weight:500;",
          paste0(name, " (dist: ", dist, ")")
        ),
        tags$span(style = "color:#6b7280;", paste0(" | PTIR ", ptir_txt))
      )
    })
  )
}


# --------------------------------------------------------------------------------
# SQL query builder for the reactable table
# Translates all active filter inputs into a parameterized Postgres query.
# Returns a list(sql, params) safe for DBI::dbGetQuery(con, sql, params = params).
# --------------------------------------------------------------------------------
build_table_query <- function(
    shiny_year        = NULL,        # character vector of selected years
    shiny_team        = NULL,        # character vector of selected teams
    shiny_role        = NULL,        # character vector of selected roles
    shiny_prpg        = NULL,        # c(lo, hi) numeric or NULL
    shiny_portal_only = FALSE,       # logical; filters to portal players via pid IN (...)
    portal_pids       = character(0), # character vector of confirmed portal PIDs
    composites        = list(),      # named list of c(lo, hi) per composite group name
    table_state       = NULL,        # input$radar_table_state from JS (list with filters/search)
    display_cols      = TABLE_DISPLAY_COLS,
    count_only        = FALSE
) {
  conditions <- character(0)
  params     <- list()
  p          <- function() length(params)  # current param index helper

  safe_col <- function(nm) {
    # Return a quoted identifier string suitable for sprintf interpolation
    paste0('"', gsub('"', '""', nm), '"')
  }

  add_param <- function(val) {
    params[[p() + 1L]] <<- val
    p()
  }

  # Add each element of vals as a separate $N parameter; return "IN ($i, $j, ...)" body
  add_params_in <- function(vals) {
    indices <- integer(length(vals))
    for (k in seq_along(vals)) indices[k] <- add_param(vals[[k]])
    paste(sprintf("$%d", indices), collapse = ", ")
  }

  # ---- Exclude benchmark/average rows from the player table ----
  safe_bench <- paste(sprintf("'%s'", gsub("'", "''", benchmark_names)), collapse = ", ")
  conditions <- c(conditions, sprintf('"Name" NOT IN (%s)', safe_bench))

  # ---- Shiny-side external filters ----
  # Year is numeric in DB; pass as numeric and use IN with individual params
  if (!is.null(shiny_year) && length(shiny_year) > 0) {
    in_body <- add_params_in(as.numeric(shiny_year))
    conditions <- c(conditions, sprintf('"Year" IN (%s)', in_body))
  }
  if (!is.null(shiny_team) && length(shiny_team) > 0) {
    in_body <- add_params_in(as.character(shiny_team))
    conditions <- c(conditions, sprintf('"Team" IN (%s)', in_body))
  }
  if (!is.null(shiny_role) && length(shiny_role) > 0) {
    in_body <- add_params_in(as.character(shiny_role))
    conditions <- c(conditions, sprintf('"Role" IN (%s)', in_body))
  }
  if (!is.null(shiny_prpg) && length(shiny_prpg) == 2) {
    lo <- suppressWarnings(as.numeric(shiny_prpg[[1]]))
    hi <- suppressWarnings(as.numeric(shiny_prpg[[2]]))
    if (is.finite(lo) && is.finite(hi) && !(lo <= -2 && hi >= 10)) {
      i_lo <- add_param(lo); i_hi <- add_param(hi)
      conditions <- c(conditions, sprintf('"PRPG" BETWEEN $%d AND $%d', i_lo, i_hi))
    }
  }

  if (isTRUE(shiny_portal_only) && length(portal_pids) > 0) {
    in_body <- add_params_in(as.character(portal_pids))
    conditions <- c(conditions, sprintf('"pid" IN (%s)', in_body))
  }

  # ---- Composite slider filters (Shiny-side external sliders) ----
  # Composite group names in R (e.g. "Scoring") map to DB columns with "_score" suffix
  for (nm in names(composites)) {
    rng <- composites[[nm]]
    if (is.null(rng) || length(rng) != 2) next
    lo <- suppressWarnings(as.numeric(rng[[1]]))
    hi <- suppressWarnings(as.numeric(rng[[2]]))
    if (!is.finite(lo) || !is.finite(hi)) next
    # Skip if full range (default 0-100) — avoids unnecessary BETWEEN in SQL
    if (lo <= 0 && hi >= 100) next
    i_lo <- add_param(lo); i_hi <- add_param(hi)
    db_col <- paste0(nm, "_score")
    conditions <- c(conditions, sprintf('%s BETWEEN $%d AND $%d', safe_col(db_col), i_lo, i_hi))
  }

  # ---- In-table reactable filter state (captured from JS via Reactable.onStateChange) ----
  if (!is.list(table_state)) table_state <- list()

  # Per-column filters set inside the reactable
  # (Global name search is handled client-side by reactable — not routed through SQL)
  for (f in (table_state$filters %||% list())) {
    col_id <- f$id
    val    <- f$value
    if (is.null(col_id) || is.null(val)) next
    # Only allow filtering on columns we know exist in the table (security + correctness)
    if (!col_id %in% display_cols) next

    # Range filter — MUI range slider sends a 2-element numeric vector
    if (is.numeric(val) && length(val) == 2) {
      lo <- val[[1]]; hi <- val[[2]]
      if (is.finite(lo) && is.finite(hi)) {
        i_lo <- add_param(lo); i_hi <- add_param(hi)
        conditions <- c(conditions, sprintf('%s BETWEEN $%d AND $%d', safe_col(col_id), i_lo, i_hi))
      }
    # Multi-select filter — character vector of selected values
    } else if (is.character(val) && length(val) >= 1) {
      in_body <- add_params_in(as.character(val))
      conditions <- c(conditions, sprintf('%s IN (%s)', safe_col(col_id), in_body))
    }
  }

  where_sql <- if (length(conditions) > 0)
    paste("WHERE", paste(conditions, collapse = " AND "))
  else ""

  if (count_only) {
    return(list(
      sql    = sprintf('SELECT COUNT(*) AS n FROM "basketball_players" %s', where_sql),
      params = params
    ))
  }

  # Only select columns that are in both display_cols and the target list
  fetch_cols <- display_cols  # caller may pass TABLE_DISPLAY_COLS or a subset
  col_sql    <- paste(vapply(fetch_cols, safe_col, ""), collapse = ", ")
  list(
    sql    = sprintf('SELECT %s FROM "basketball_players" %s', col_sql, where_sql),
    params = params
  )
}

# --------------------------------------------------------------------------------
# SHINY SERVER
# --------------------------------------------------------------------------------
shinyServer(function(input, output, session) {
  # -------------------------- NAV TAB GATING -----------------------------------
  # ui.R has navbarPage(id="main_nav") with only "Radar tab"
  is_radar_tab_active <- reactive(identical(input$main_nav, "Radar tab"))
  radar_inputs_ready <- reactive({
    isTRUE(is_radar_tab_active()) # PERF: avoid radar data work until radar tab is actually active
  })
  # Debounce year filter so rapid multi-select clicks don't fire a full reactive
  # cascade on every keypress / click (each intermediate state → OOM on low-RAM hosts).
  year_filter_d <- shiny::debounce(reactive(input$year_filter), 300)
  is_mobile <- reactive({
    isTRUE(input$is_mobile)
  })

  # lightweight init logging (set TRUE for debugging)
  debug_init <- FALSE
  log_init <- function(...) {
    if (isTRUE(debug_init)) message("[init] ", paste(..., collapse = " "))
  }

  # Per-session reactive: fetch confirmed portal PIDs fresh from DB each time it is
  # invalidated (i.e. on each new session load), so newly confirmed matches are visible
  # without a process restart.
  portal_pids_rv <- reactive({
    tryCatch({
      if (!is.null(supabase_pool)) {
        res <- DBI::dbGetQuery(
          supabase_pool,
          'SELECT DISTINCT pid FROM mbb_portal_player_xref WHERE pid IS NOT NULL AND is_confirmed = TRUE'
        )
        as.character(res$pid)
      } else character(0)
    }, error = function(e) {
      warning("Could not load portal PIDs: ", conditionMessage(e))
      character(0)
    })
  })

  # Per-session reactive: pid -> to_team mapping for confirmed portal players.
  # Used to populate the "To Team" column in the reactable.
  portal_to_team_rv <- reactive({
    tryCatch({
      if (!is.null(supabase_pool)) {
        res <- DBI::dbGetQuery(
          supabase_pool,
          "SELECT x.pid::text AS pid, p.to_team
           FROM mbb_portal_player_xref x
           JOIN mbb_transfer_portal p ON p.id = x.portal_id
           WHERE x.is_confirmed = TRUE AND p.to_team IS NOT NULL AND p.to_team <> ''"
        )
        setNames(res$to_team, res$pid)
      } else character(0)
    }, error = function(e) {
      warning("Could not load portal to_team map: ", conditionMessage(e))
      character(0)
    })
  })

  default_radar_year_selection <- local({
    cache <- NULL
    function() {
      if (!is.null(cache)) {
        return(cache)
      }
      years <- sort(unique(suppressWarnings(as.integer(player_stats_all$Year))), decreasing = TRUE)
      years <- years[is.finite(years)]
      cache <<- as.character(head(years, 4))
      cache
    }
  })

  radar_player_pool <- function(year_selection = NULL,
                                team_selection = NULL,
                                role_selection = NULL,
                                prpg_selection = NULL) {
    df <- if (exists("player_stats_all_with_composites", inherits = TRUE)) {
      player_stats_all_with_composites
    } else {
      player_stats_all
    }

    years_int <- suppressWarnings(as.integer(year_selection))
    years_int <- years_int[is.finite(years_int)]
    if (length(years_int) > 0 && "Year" %in% names(df)) {
      df <- df %>% dplyr::filter(suppressWarnings(as.integer(.data$Year)) %in% years_int)
    }
    if (length(team_selection) > 0 && "Team" %in% names(df)) {
      df <- df %>% dplyr::filter(.data$Team %in% team_selection)
    }
    if (length(role_selection) > 0 && "Role" %in% names(df)) {
      df <- df %>% dplyr::filter(.data$Role %in% role_selection)
    }
    if (!is.null(prpg_selection) && length(prpg_selection) == 2 && "PRPG" %in% names(df)) {
      lo <- suppressWarnings(as.numeric(prpg_selection[[1]]))
      hi <- suppressWarnings(as.numeric(prpg_selection[[2]]))
      if (is.finite(lo) && is.finite(hi)) {
        df <- df %>% dplyr::filter(.data$PRPG >= lo, .data$PRPG <= hi)
      }
    }

    use_default_subset <- length(years_int) > 1 &&
      length(team_selection) == 0 &&
      length(role_selection) == 0 &&
      (is.null(prpg_selection) || identical(as.numeric(prpg_selection), c(-2, 10)))
    if (use_default_subset) {
      df <- table_default_comparison_subset(df)
    }

    if (!all(c("Year", "Team", "Name") %in% names(df))) {
      return(character(0))
    }

    df %>%
      dplyr::mutate(
        Year = suppressWarnings(as.integer(.data$Year)),
        Team = as.character(.data$Team),
        Name = as.character(.data$Name)
      ) %>%
      dplyr::arrange(dplyr::desc(.data$Year), .data$Team, .data$Name) %>%
      dplyr::distinct(.data$Name, .keep_all = TRUE) %>%
      dplyr::pull(.data$Name)
  }

  table_default_comparison_subset <- function(df) {
    if (!is.data.frame(df) || nrow(df) == 0 || !"Year" %in% names(df)) {
      return(df)
    }

    year_vals <- suppressWarnings(as.integer(df$Year))
    ptir_vals <- suppressWarnings(as.numeric(df$ptir %||% NA_real_))
    dpmr_vals <- suppressWarnings(as.numeric(df$dpmr %||% NA_real_))

    benchmark_rows <- if ("Name" %in% names(df)) is_benchmark_row(df$Name) else rep(FALSE, nrow(df))
    keep <- year_vals == 2026 | benchmark_rows

    prior_years <- sort(unique(year_vals[is.finite(year_vals) & year_vals < 2026]), decreasing = TRUE)
    for (yr in prior_years) {
      idx <- which(year_vals == yr)
      if (length(idx) == 0) next

      year_ptir <- ptir_vals[idx]
      # MODIFIED: Use top 75% by PTIR (cut at 25th percentile) for prior years
      ptir_cut <- suppressWarnings(stats::quantile(year_ptir, probs = 0.25, na.rm = TRUE, names = FALSE))

      keep[idx] <- keep[idx] |
        (is.finite(year_ptir) & is.finite(ptir_cut) & year_ptir >= ptir_cut)
    }

    df[keep %in% TRUE, , drop = FALSE]
  }

  # -------------------------- STATIC TABLES ------------------------------------
  render_data_updated_text <- function() {
    lu <- if (exists("last_update", inherits = TRUE)) last_update else NA
    lu <- suppressWarnings(as.Date(lu)[1])
    if (is.na(lu)) {
      return("Data updated through: n/a")
    }
    paste0("Data updated through: ", format(lu, "%m/%d/%Y"))
  }

  output$data_updated_radar <- renderText(render_data_updated_text())

  # -------------------------- RADAR TAB: INPUT INIT ----------------------------
  observeEvent(radar_inputs_ready(),
    {
      req(radar_inputs_ready())
      session$onFlushed(function() {
        radar_var_choices <- stats::setNames(radar_vars_all, purrr::map_chr(radar_vars_all, ~ label_stat(.x, stat_labels)))
        updateSelectizeInput(
          session,
          "radar_vars_select",
          choices = radar_var_choices,
          selected = radar_vars_all_small,
          server = FALSE
        )

        ord <- radar_player_pool(
          year_selection = default_radar_year_selection(),
          team_selection = NULL,
          role_selection = NULL,
          prpg_selection = c(-2, 10)
        )
        updateSelectizeInput(
          session,
          "players",
          choices = ord,
          selected = character(0),
          server = TRUE
        )

        for (nm in names(composite_groups)) {
          id <- sanitize_composite_id(nm)
          updateSliderInput(session, id, min = 0, max = 100, value = c(0, 100), step = 1)
        }
      }, once = TRUE)
    },
    ignoreInit = FALSE,
    once = TRUE
  )


  # -------------------------- RADAR TAB: RESET TABLE FILTERS -------------------
  # Requires an actionButton in ui.R with id = "radar_reset_filters"
  # Resets the *reactable column filters* (including the range sliders inside the table).
  # FIX: Added onRender handler to radarPlayerTable reactable to listen for custom message
  observeEvent(input$radar_reset_filters,
    {
      req(is_radar_tab_active())

      session$sendCustomMessage(
        "radar_reset_reactable_filters",
        list(
          table_id = "radarPlayerTable",
          # categorical/meta filters we want fully cleared (Year, Team, Role, Player/Class)
          clear_columns = unique(c("Year", "Team", "Role", "Player")),
          # numeric composite filters should reset their slider handles to full range
          numeric_columns = unique(c(names(composite_groups), "ptir", "edmr", "dsr", "dpmr", "Ht", "G", "Min_pct", names(stat_labels))),
          year_default = "2026" # Reset behavior: restore default Year filter after clearing
        )
      )
    },
    ignoreInit = TRUE
  )


  # -------------------------- RADAR TAB: COLUMN GLOSSARY MODAL -----------------
  # MODIFIED: Single glossary modal replaces per-column header tooltips.
  observeEvent(input$radar_glossary_btn, {
    showModal(modalDialog(
      title = "Column Glossary",
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close"),
      tags$div(
        tags$h5("Rank / Model Metrics", style = "margin-top:0;"),
        tags$dl(
          style = "margin-bottom:0;",
          tags$dt("PredTransferImpactRating (PTIR)"),
          tags$dd("Next-season Power-5 transfer impact projection rank. Model trained on year-over-year performance of High Major returners and transfers."),
          tags$dt("EstimatedDraftModelRating (EDMR)"),
          tags$dd("Combined draft model rating — average of DSR, PTIR, and DPMR."),
          tags$dt("DraftSignalRank (DSR)"),
          tags$dd("Class/Conference/Role-aware draft signal rank. Built from strength profile of position group, then adjusted with drafted-player outcomes."),
          tags$dt("DraftPickModelRank (DPMR)"),
          tags$dd("Draft-pick model rank. Supervised score trained on drafted players to predict stronger draft outcomes (lower pick numbers).")
        ),
        tags$hr(),
        tags$h5("Composite Skill Metrics"),
        tags$p(
          tags$em("All composites are percentile-based (0\u2013100). Each is a weighted average of individual stats grouped by season and converted to percentiles; lower-is-better stats are inverted before inclusion.")
        ),
        tags$dl(
          tags$dt("OFF EFF \u2014 Offensive Efficiency"),
          tags$dd("Measures overall offensive impact and efficiency."),
          tags$dt("DEF EFF \u2014 Defensive Efficiency"),
          tags$dd("Measures overall defensive contribution and impact."),
          tags$dt("REB \u2014 Rebounding"),
          tags$dd("Measures rebounding ability on both ends of the floor."),
          tags$dt("SCOR \u2014 Scoring"),
          tags$dd("Measures overall scoring ability and efficiency."),
          tags$dt("PLAY MAKING \u2014 Play Making"),
          tags$dd("Measures playmaking, decision-making, and turnover control."),
          tags$dt("PERIM OFF \u2014 Perimeter Offense"),
          tags$dd("Measures three-point shooting ability."),
          tags$dt("INT OFF \u2014 Interior Offense"),
          tags$dd("Measures finishing and scoring ability near the rim."),
          tags$dt("UNAST SCOR \u2014 Unassisted Scoring"),
          tags$dd("Measures ability to create and score without the benefit of an assist."),
          tags$dt("MID OFF \u2014 Midrange Offense"),
          tags$dd("Measures midrange shooting ability."),
          tags$dt("PERIM DEF \u2014 Perimeter Defense"),
          tags$dd("Measures ability to defend on the perimeter."),
          tags$dt("INT DEF \u2014 Interior Defense"),
          tags$dd("Measures rim protection and interior defensive impact.")
        )
      )
    ))
  })


  # -------------------------- RADAR TAB: RESET PLAYER SELECTIONS ---------------
  # Requires an actionButton in ui.R with id = "radar_reset_players"
  # Resets the Selectize "players" selection back to the default (top of player_choice_order).
  observeEvent(input$radar_reset_players,
    {
      req(is_radar_tab_active())

      ord <- player_choice_order()
      if (is.null(ord) || length(ord) == 0) {
        updateSelectizeInput(session, "players",
          choices  = character(0),
          selected = character(0),
          server   = TRUE
        )
        return()
      }

      # Default to the first player in the current ordering (Year desc, Team, Name)
      default_sel <- character(0)

      updateSelectizeInput(
        session,
        "players",
        choices  = ord,
        selected = default_sel,
        server   = TRUE
      )
    },
    ignoreInit = TRUE
  )


  # When user clicks a similar player name, ADD that player to the current radar selection
  # IMPORTANT: the clicked player may not be in the current selectize choices yet.
  # So we must update BOTH choices and selected.
  observeEvent(input$similar_player_click,
    {
      req(input$similar_player_click)

      clicked <- as.character(input$similar_player_click)
      current <- isolate(input$players)

      if (is.null(current)) current <- character(0)
      current <- as.character(current)

      # append + de-dup while preserving order
      new_sel <- unique(c(current, clicked))

      # cap at 3 players (keep earliest selections)
      if (length(new_sel) > 3) new_sel <- new_sel[seq_len(3)]

      # Ensure clicked player is present in choices; otherwise selectize will silently ignore it
      ord <- isolate(player_choice_order())
      if (is.null(ord)) ord <- character(0)
      ord <- as.character(ord)
      choices_all <- unique(c(ord, clicked))

      updateSelectizeInput(
        session,
        "players",
        choices  = choices_all,
        selected = new_sel,
        server   = TRUE
      )

      # bring user back to the radar tab
      updateNavbarPage(session, "main_nav", selected = "Radar tab")
    },
    ignoreInit = TRUE,
    priority = 1
  )

  filtered_stats <- reactive({
    req(radar_inputs_ready())
    # Prefer the composite-augmented table if available
    df <- if (exists("player_stats_all_with_composites", inherits = TRUE)) {
      player_stats_all_with_composites
    } else {
      player_stats_all
    }

    # External year selectize controls the radar/player universe.
    year_sel <- suppressWarnings(as.integer(year_filter_d()))
    year_sel <- year_sel[is.finite(year_sel)]
    if (length(year_sel) > 0) {
      df <- df %>% dplyr::filter(suppressWarnings(as.integer(.data$Year)) %in% year_sel)
    }
    if (length(input$team_filter) > 0) df <- df %>% dplyr::filter(.data$Team %in% input$team_filter)
    if (length(input$role_filter) > 0) df <- df %>% dplyr::filter(.data$Role %in% input$role_filter)
    if ("pid" %in% names(df)) {
      df$Portal <- ifelse(df$pid %in% portal_pids_rv(), "Yes", "No")
    } else {
      df$Portal <- "No"
    }
    if (isTRUE(input$portal_only) && "Portal" %in% names(df)) {
      df <- df %>% dplyr::filter(.data$Portal == "Yes")
    }
    if (!is.null(input$prpg_filter) && "PRPG" %in% names(df)) {
      df <- df %>% dplyr::filter(.data$PRPG >= input$prpg_filter[1], .data$PRPG <= input$prpg_filter[2])
    }

    # Apply composite percentile slider filters
    comp_cols <- names(composite_groups)

    # Pull missing composite-like columns from the full composite source first
    # (this preserves new columns like Overall_A / Overall_B when present).
    if (exists("player_stats_all_with_composites", inherits = TRUE)) {
      comp_source <- player_stats_all_with_composites
      desired_cols <- unique(c(comp_cols, "Overall_A", "Overall_B", rank_vars, c("dsr", "ptir", "dpmr", "edmr")))
      source_cols <- intersect(desired_cols, names(comp_source))
      missing_from_df <- setdiff(source_cols, names(df))
      if (length(missing_from_df) > 0) {
        df <- df %>%
          dplyr::left_join(
            comp_source %>%
              dplyr::select(Name, dplyr::any_of(missing_from_df)) %>%
              dplyr::distinct(.data$Name, .keep_all = TRUE),
            by = "Name"
          )
      }
    }

    # Backfill legacy composite percentile columns if still missing
    missing_comp <- setdiff(comp_cols, names(df))
    if (length(missing_comp) > 0) {
      comp_pct <- composite_percentiles_df %>%
        dplyr::select(Name, dplyr::any_of(missing_comp))
      df <- df %>% dplyr::left_join(comp_pct, by = "Name")
    }

    for (nm in comp_cols) {
      id <- sanitize_composite_id(nm)
      rng <- input[[id]]
      if (is.null(rng) || length(rng) != 2) next

      lo <- suppressWarnings(as.numeric(rng[[1]]))
      hi <- suppressWarnings(as.numeric(rng[[2]]))
      if (!is.finite(lo) || !is.finite(hi)) next

      # Optional operator selector per composite metric.
      # If ui.R defines it, expected values: "between", "ge", "le".
      # If not present, default to "between".
      op_id <- paste0("compop__", gsub("[^A-Za-z0-9]+", "_", as.character(nm)))
      op <- input[[op_id]]
      if (is.null(op) || is.na(op) || !nzchar(as.character(op))) op <- "between"
      op <- tolower(as.character(op))

      vv <- suppressWarnings(as.numeric(df[[nm]]))

      if (op == "ge") {
        df <- df %>% dplyr::filter(is.na(vv) | (vv >= lo))
      } else if (op == "le") {
        df <- df %>% dplyr::filter(is.na(vv) | (vv <= hi))
      } else {
        # between (default)
        df <- df %>% dplyr::filter(is.na(vv) | (vv >= lo & vv <= hi))
      }
    }

    df
  })

  # (dynamic filter UI output removed)

  radar_filtered <- reactive({
    req(radar_inputs_ready())
    filtered_stats()
  })

  # --- Watchlist ---
  watchlist_rv <- reactiveVal(character(0))

  observeEvent(input$watchlist_init, {
    pids <- as.character(input$watchlist_init)
    watchlist_rv(pids[!is.na(pids) & nzchar(pids)])
  }, ignoreNULL = TRUE, once = TRUE)

  observeEvent(input$watchlist_star_click, {
    req(input$watchlist_star_click)
    pid <- as.character(input$watchlist_star_click$pid)
    if (!nzchar(pid)) return()
    current <- watchlist_rv()
    watchlist_rv(if (pid %in% current) setdiff(current, pid) else c(current, pid))
  }, ignoreNULL = TRUE)

  observe({
    session$sendCustomMessage("watchlist_update", list(pids = watchlist_rv()))
  })

  # Debounce the in-table filter state so a fast-typing search waits 600ms before
  # triggering a SQL round-trip. Without this, every keystroke fires a query and
  # the table blanks between results.
  radar_table_state_d <- shiny::debounce(reactive(input$radar_table_state), 600)

  # radar_table_filtered: server-side SQL query driven by all active filter inputs.
  # Executes a parameterized Postgres query via the connection pool; only the rows
  # matching all current filters are returned (no full-table load per render).
  # The reactable then handles pagination and sorting client-side on this result set.
  radar_table_filtered <- reactive({
    req(radar_inputs_ready())
    df <- timed("radar_table_filtered_total", {

      # Collect Shiny-side composite slider values
      composite_filters <- lapply(
        setNames(names(composite_groups), names(composite_groups)),
        function(nm) {
          id  <- sanitize_composite_id(nm)
          rng <- input[[id]]
          if (!is.null(rng) && length(rng) == 2) rng else NULL
        }
      )

      q <- build_table_query(
        shiny_year        = year_filter_d(),
        shiny_team        = input$team_filter,
        shiny_role        = input$role_filter,
        shiny_prpg        = input$prpg_filter,
        shiny_portal_only = isTRUE(input$portal_only),
        portal_pids       = portal_pids_rv(),
        composites        = composite_filters,
        table_state       = radar_table_state_d(),
        display_cols      = TABLE_DISPLAY_COLS
      )

      result <- tryCatch(
        DBI::dbGetQuery(supabase_pool, q$sql, params = q$params),
        error = function(e) {
          warning("[radar_table_filtered] SQL query failed: ", conditionMessage(e),
                  "\nQuery: ", q$sql)
          # Fallback: filter the in-memory data if pool query fails
          df_fb <- if (exists("player_stats_all_with_composites", inherits = TRUE)) {
            player_stats_all_with_composites
          } else {
            player_stats_all
          }
          df_fb %>% dplyr::filter(!is_benchmark_row(.data$Name))
        }
      )

      # Add Portal column from pre-fetched portal pids
      if ("pid" %in% names(result)) {
        result$Portal <- ifelse(result$pid %in% portal_pids_rv(), "Yes", "No")
      } else {
        result$Portal <- "No"
      }

      # Rename *_score columns to composite names if needed (DB naming convention)
      result <- ensure_composite_source_aliases(result)
      for (grp in names(composite_groups)) {
        score_col <- paste0(grp, "_score")
        if (score_col %in% names(result) && !grp %in% names(result)) {
          result[[grp]] <- result[[score_col]]
        }
      }

      # Apply table_default_comparison_subset in R (complex per-year PTIR cutoff).
      # This runs on the already SQL-filtered set, so it's fast even for large results.
      result <- table_default_comparison_subset(result)

      result
    })

    df
  })

  output$radarPlayerTable <- reactable::renderReactable({
    with_timing("render_radarPlayerTable", meta = list(tab = "radar"), expr = {
      req(radar_inputs_ready())
      if (profiling_enabled()) message("[radarPlayerTable] start")
      df <- timed("radarPlayerTable_get_data", radar_table_filtered())
      if (profiling_enabled()) message("[radarPlayerTable] got data rows=", nrow(df), " cols=", ncol(df))
      if (!is.data.frame(df)) df <- data.frame()

      # --- Watchlist filter -----------------------------------------------------
      if (isTRUE(input$watchlist_only) && "pid" %in% names(df)) {
        wl <- watchlist_rv()
        if (length(wl) > 0) {
          df <- df[as.character(df$pid) %in% wl, , drop = FALSE]
        } else {
          df <- df[0L, , drop = FALSE]
        }
      }

      # --- Attach team logo URLs and To Team column ----------------------------
      # TeamLogo: barttorvik team name -> ESPN logo URL
      if ("Team" %in% names(df) && length(barttorvik_logo_map) > 0) {
        df$TeamLogo <- barttorvik_logo_map[as.character(df$Team)]
        df$TeamLogo[is.na(df$TeamLogo)] <- ""
      } else {
        df$TeamLogo <- character(nrow(df))
      }
      # ToTeam: pid -> to_team text (from reactive), then logo via to_team_logo_map
      if ("pid" %in% names(df)) {
        to_team_map   <- portal_to_team_rv()
        pids_chr      <- as.character(df$pid)
        df$ToTeam     <- if (length(to_team_map) > 0) to_team_map[pids_chr] else character(nrow(df))
        df$ToTeam[is.na(df$ToTeam)] <- ""
        df$ToTeamLogo <- to_team_logo_map[df$ToTeam]
        df$ToTeamLogo[is.na(df$ToTeamLogo)] <- ""
        # Mark confirmed portal players with no destination so the filter dropdown
        # can select for them; the JS cell renderer suppresses the display text.
        if ("Portal" %in% names(df))
          df$ToTeam[df$ToTeam == "" & as.character(df$Portal) == "Yes"] <- "(Uncommitted)"
      } else {
        df$ToTeam     <- character(nrow(df))
        df$ToTeamLogo <- character(nrow(df))
      }
      if (isTRUE(input$uncommitted_only) && "ToTeam" %in% names(df))
        df <- df[df$ToTeam == "(Uncommitted)", , drop = FALSE]

      # --- normalize Class for the table + restrict to a clean set --------------
      allowed_class <- c("Fr", "So", "Jr", "Sr", "1st Rder Avg", "2nd Rder Avg", "D1 Avg")

      # If your table already has Class, clean it; otherwise derive from Player
      if (!"Class" %in% names(df)) {
        if ("Player" %in% names(df)) {
          df <- df %>%
            dplyr::mutate(Class = dplyr::case_when(
              stringr::str_detect(.data$Player, "^\\s*1st\\s+Rder\\s+Avg") ~ "1st Rder Avg",
              stringr::str_detect(.data$Player, "^\\s*2nd\\s+Rder\\s+Avg") ~ "2nd Rder Avg",
              stringr::str_detect(.data$Player, "^\\s*D1\\s+Avg") ~ "D1 Avg",
              .data$Player %in% c("Fr", "So", "Jr", "Sr") ~ as.character(.data$Player),
              TRUE ~ NA_character_
            ))
        } else {
          df$Class <- rep(NA_character_, nrow(df))
        }
      } else {
        df <- df %>%
          dplyr::mutate(Class = dplyr::case_when(
            stringr::str_detect(as.character(.data$Class), "^\\s*1st\\s+Rder\\s+Avg") ~ "1st Rder Avg",
            stringr::str_detect(as.character(.data$Class), "^\\s*2nd\\s+Rder\\s+Avg") ~ "2nd Rder Avg",
            stringr::str_detect(as.character(.data$Class), "^\\s*D1\\s+Avg") ~ "D1 Avg",
            as.character(.data$Class) %in% c("Fr", "So", "Jr", "Sr") ~ as.character(.data$Class),
            TRUE ~ NA_character_
          ))
      }

      # Controlled factor = clean ordered filter list
      df$Class <- factor(df$Class, levels = allowed_class)
      # IMPORTANT: the table uses the existing meta column `Player` (renamed to "Class" in the UI)
      # Replace it with the cleaned Class values so the filter list is restricted to allowed_class
      if ("Player" %in% names(df)) {
        df$Player <- as.character(df$Class)
      }

      # composite columns to show (ordered)
      comp_cols <- {
        base <- {
          if (exists("composite_col_order", inherits = TRUE) && length(composite_col_order) > 0) {
            intersect(as.character(composite_col_order), names(df))
          } else {
            intersect(names(composite_groups), names(df))
          }
        }

        # Order requested: PTIR, EDMR, core composites, then DSR and DPMR.
        lead_extras <- intersect(c("ptir", "edmr"), names(df))
        tail_extras <- intersect(c("dsr", "dpmr"), names(df))
        unique(c(lead_extras, base, tail_extras))
      }

      # metadata columns (only if present) — display order: Portal, Year, Conf, Class, Role, Team, To Team
      meta_cols <- intersect(c("Portal", "Year", "Conf", "Player", "Role", "Team"), names(df))
      # Insert ToTeam right after Team (or at end if Team absent)
      if ("ToTeam" %in% names(df)) {
        team_pos <- which(meta_cols == "Team")
        if (length(team_pos) > 0) {
          tail_cols <- if (team_pos < length(meta_cols))
            meta_cols[seq.int(team_pos + 1L, length(meta_cols))]
          else
            character(0)
          meta_cols <- c(meta_cols[seq_len(team_pos)], "ToTeam", tail_cols)
        } else {
          meta_cols <- c(meta_cols, "ToTeam")
        }
      }
      raw_stat_order <- c("Ht", "G", "Min_pct", names(stat_labels))
      raw_stat_cols <- intersect(raw_stat_order, names(df))
      raw_stat_cols <- setdiff(raw_stat_cols, c(
        "PredTransferImpactRating", "R_PredTransferImpactRating",
        "DraftSignalRank", "DraftPickModelRank", "EstimatedDraftModelRating",
        "ptir", "ptir_value", "dsr", "dpmr", "edmr"
      ))
      percent_display_cols <- intersect(
        unique(c(
          "Min_pct",
          grep("_pct$", names(df), value = TRUE),
          names(stat_labels)[grepl("%", unname(stat_labels), fixed = TRUE)]
        )),
        names(df)
      )
      # Include hidden logo helper columns so JS cell renderers can access them
      logo_helper_cols <- intersect(c("TeamLogo", "ToTeamLogo"), names(df))
      pid_col          <- intersect("pid", names(df))
      table_cols <- unique(c("Name", meta_cols, comp_cols, raw_stat_cols, logo_helper_cols, pid_col))

      table_df <- df %>%
        dplyr::select(dplyr::any_of(table_cols)) %>%
        dplyr::mutate(
          Year = if ("Year" %in% names(.)) suppressWarnings(as.integer(.data$Year)) else NULL,
          Conf = if ("Conf" %in% names(.)) as.character(.data$Conf) else NULL
        )
      table_df$.idx  <- seq_len(nrow(table_df))
      table_df$Star  <- integer(nrow(table_df))
      table_df <- table_df %>% dplyr::select(.idx, Star, dplyr::everything())

      if (profiling_enabled()) {
        message("[radarPlayerTable] df cols contain DraftSignalRank? ", "DraftSignalRank" %in% names(df))
        message("[radarPlayerTable] df cols contain PredTransferImpactRating? ", "PredTransferImpactRating" %in% names(df))
        message("[radarPlayerTable] df cols contain DraftPickModelRank? ", "DraftPickModelRank" %in% names(df))
        message("[radarPlayerTable] df cols contain EstimatedDraftModelRating? ", "EstimatedDraftModelRating" %in% names(df))
        message("[radarPlayerTable] comp_cols: ", paste(comp_cols, collapse = ", "))
        message("[radarPlayerTable] raw_stat_cols: ", paste(raw_stat_cols, collapse = ", "))
        print(utils::head(df[, intersect(c("Name", "Player", rank_vars, "ptir", "edmr", "dsr", "dpmr", "Overall_A", "Overall_B"), names(df)), drop = FALSE]))
      }

      # column definitions
      rank_tooltips <- c(
        DraftSignalRank = "DraftSignalRank (DSR): Class/Conference/Role-aware draft signal rank. Built from strength profile of position group, then adjusted with drafted-player outcomes.",
        PredTransferImpactRating = "PredTransferImpactRating (PTIR): Next-season Power-5 transfer impact projection rank. Model trained on YoY performance of High Major returners/transfers",
        DraftPickModelRank = "DraftPickModelRank (DPMR): Draft-pick model rank. Supervised score trained on drafted players to predict stronger draft outcomes (lower pick numbers).",
        EstimatedDraftModelRating = "EstimatedDraftModelRating (EDMR): Average of DSR, PTIR, and DPMR.",
        ptir = "PredTransferImpactRating (PTIR): Next-season Power-5 transfer impact projection rank. Model trained on YoY performance of High Major returners/transfers",
        edmr = "EstimatedDraftModelRating (EDMR): Average of DSR, PTIR, and DPMR.",
        dsr = "DraftSignalRank (DSR): Class/Conference/Role-aware draft signal rank.",
        dpmr = "DraftPickModelRank (DPMR): Draft-pick model rank."
      )
      get_col_tooltip <- function(col_name, label) {
        tip <- NULL
        if (col_name %in% names(rank_tooltips)) {
          tip <- rank_tooltips[[col_name]]
        } else if (identical(col_name, "Name")) {
          tip <- "Player name. Click a row to add that player to the radar selection."
        } else if (identical(col_name, "Year")) {
          tip <- "Season for the player row."
        } else if (identical(col_name, "Team")) {
          tip <- "Team for that season."
        } else if (identical(col_name, "Role")) {
          tip <- "Role bucket used by the radar and comparison models."
        } else if (identical(col_name, "Player")) {
          tip <- "Class or benchmark bucket shown in the table filter."
        } else if (col_name %in% names(composite_groups)) {
          comps <- names(composite_groups[[col_name]])
          tip <- paste0(
            label, ": weighted composite of ",
            paste(comps, collapse = ", ")
          )
        }
        tip
      }
      if (profiling_enabled()) message("[radarPlayerTable] built col_defs count=", length(col_defs))
      make_header_with_tooltip <- function(tip_text) {
        force(tip_text)
        function(value) {
          label_text <- as.character(value)
          htmltools::tags$span(
            class = "radar-header-tooltip-wrap",
            htmltools::tags$span(
              class = "radar-header-tooltip-label",
              label_text
            ),
            htmltools::tags$button(
              type = "button",
              class = "radar-header-tooltip-btn",
              `aria-label` = paste0("Show description for ", label_text),
              `aria-expanded` = "false",
              onmousedown = "event.preventDefault(); event.stopPropagation();",
              onpointerdown = "event.preventDefault(); event.stopPropagation();",
              ontouchstart = "event.preventDefault(); event.stopPropagation();",
              onkeydown = "event.stopPropagation();",
              onclick = paste0(
                "event.preventDefault(); event.stopPropagation();",
                "var btn=this;",
                "var popup=btn.parentElement.querySelector('.radar-header-tooltip-popup');",
                "var isOpen=btn.getAttribute('aria-expanded')==='true';",
                "document.querySelectorAll('.radar-header-tooltip-btn[aria-expanded=\"true\"]').forEach(function(node){",
                "if(node!==btn){node.setAttribute('aria-expanded','false');",
                "var other=node.parentElement&&node.parentElement.querySelector('.radar-header-tooltip-popup');",
                "if(other){other.hidden=true;}}});",
                "btn.setAttribute('aria-expanded', isOpen ? 'false' : 'true');",
                "if(popup){popup.hidden=isOpen;}"
              ),
              "i"
            ),
            htmltools::tags$div(
              class = "radar-header-tooltip-popup",
              hidden = NA,
              tip_text
            )
          )
        }
      }

      col_defs <- list(
        # MODIFIED: Added sticky = "left" to pin index column during horizontal scroll
        .idx = reactable::colDef(
          name = "#",
          minWidth = 35,
          maxWidth = 45,
          align = "right",
          sortable = FALSE,
          filterable = FALSE,
          sticky = "left",
          style = list(fontSize = "9px", padding = "2px 4px", whiteSpace = "nowrap"),
          headerStyle = list(fontSize = "9px", padding = "2px 4px", whiteSpace = "nowrap"),
          cell = reactable::JS(
            "function(cellInfo) {
             var vi = cellInfo.viewIndex;
             if (vi === undefined || vi === null) vi = 0;
             try {
               var state = window.Reactable && window.Reactable.getState('radarPlayerTable');
               if (state) {
                 var pg = state.page !== undefined ? state.page : (state.pageIndex || 0);
                 var ps = state.pageSize || 15;
                 return pg * ps + vi + 1;
               }
             } catch(e) {}
             return vi + 1;
           }"
          )
        ),
        # MODIFIED: Name column also pinned (sticky = "left") - ordering: index first, then Name
        Name = reactable::colDef(
          name = "Player",
          minWidth = 110,
          maxWidth = 175,
          sticky = "left",
          filterable = TRUE,
          html = TRUE,
          cell = reactable::JS("function(cellInfo) {
            var name = String(cellInfo.value || '');
            var idx = name.indexOf(' (');
            name = idx > 0 ? name.substring(0, idx) : name;
            var fs = name.length > 22 ? '9px' : name.length > 17 ? '10px' : '11px';
            return '<span style=\"font-size:' + fs + ';white-space:nowrap;overflow:hidden;' +
              'text-overflow:ellipsis;display:block;width:100%\">' + name + '</span>';
          }")
        )
      )

      col_defs[["Star"]] <- reactable::colDef(
        name       = "\u2605",
        width      = 32,
        sortable   = FALSE,
        filterable = FALSE,
        html       = TRUE,
        cell       = reactable::JS(
          "function(cellInfo) {
            var pid = String(cellInfo.row['pid'] || '');
            if (!pid) return '';
            var wl = Array.isArray(window.__watchlistPids) ? window.__watchlistPids : [];
            var on = wl.indexOf(pid) >= 0;
            return '<button data-watchlist-pid=\"' + pid + '\" ' +
              'style=\"background:none;border:none;cursor:pointer;font-size:16px;padding:0;line-height:1;' +
              'color:' + (on ? '#f59e0b' : '#94a3b8') + '\" ' +
              'title=\"' + (on ? 'Remove from watchlist' : 'Add to watchlist') + '\">' +
              (on ? '&#9733;' : '&#9734;') + '</button>';
          }"
        )
      )

      col_defs[["pid"]] <- reactable::colDef(show = FALSE)

      col_defs[["Team"]] <- reactable::colDef(
        name = "Team",
        minWidth = 110, maxWidth = 150,
        filterable = TRUE,
        filterMethod = reactable::JS("filterMulti"),
        filterInput = reactable::JS("multiSelectFilter"),
        html = TRUE,
        cell = reactable::JS("function(cellInfo) {
          var logo = cellInfo.row['TeamLogo'] || '';
          var name = String(cellInfo.value || '');
          var nameSpan = '<span style=\"font-size:11px;white-space:nowrap;overflow:hidden;text-overflow:ellipsis\">' + name + '</span>';
          if (logo) {
            return '<div style=\"display:flex;align-items:center;gap:5px\">' +
              '<img src=\"' + logo + '\" style=\"width:20px;height:20px;object-fit:contain;flex-shrink:0\" />' +
              nameSpan + '</div>';
          }
          return nameSpan;
        }")
      )
      # Hidden helper columns — values accessible in JS cell renderers via cellInfo.row
      col_defs[["TeamLogo"]]   <- reactable::colDef(show = FALSE)
      col_defs[["ToTeamLogo"]] <- reactable::colDef(show = FALSE)
      # To Team column — shows destination logo + portal team name
      if ("ToTeam" %in% names(table_df)) {
        col_defs[["ToTeam"]] <- reactable::colDef(
          name = "To Team",
          minWidth = 70, maxWidth = 160,
          filterable = TRUE,
          filterMethod = reactable::JS("filterMulti"),
          filterInput = reactable::JS("multiSelectFilter"),
          html = TRUE,
          headerStyle = list(whiteSpace = "nowrap"),
          style = reactable::JS("function(rowInfo) {
            var logo = rowInfo.values['ToTeamLogo'] || '';
            return logo ? { background: '#ffffff' } : null;
          }"),
          cell = reactable::JS("function(cellInfo) {
            var logo = cellInfo.row['ToTeamLogo'] || '';
            var name = String(cellInfo.value || '');
            if (!name || name === '(Uncommitted)') return '';
            if (logo) {
              return '<div style=\"display:flex;justify-content:center;align-items:center;height:100%\">' +
                '<img src=\"' + logo + '\" title=\"' + name + '\"' +
                ' style=\"width:22px;height:22px;object-fit:contain\" /></div>';
            }
            var fs = name.length > 22 ? '9px' : name.length > 17 ? '10px' : '11px';
            return '<span style=\"font-size:' + fs + ';white-space:nowrap;overflow:hidden;' +
              'text-overflow:ellipsis;display:block;width:100%\">' + name + '</span>';
          }")
        )
      }
      col_defs[["Conf"]] <- reactable::colDef(
        name = "Conf",
        minWidth = 55, maxWidth = 90,
        filterable = TRUE,
        filterMethod = reactable::JS("filterMulti"),
        filterInput = reactable::JS("multiSelectFilter"),
        style = list(fontSize = "11px", whiteSpace = "nowrap", overflow = "hidden", textOverflow = "ellipsis")
      )
      col_defs[["Role"]] <- reactable::colDef(
        name = "Role",
        minWidth = 80, maxWidth = 120,
        filterable = TRUE,
        filterMethod = reactable::JS("filterMulti"),
        filterInput = reactable::JS("multiSelectFilter"),
        style = list(fontSize = "11px", whiteSpace = "nowrap", overflow = "hidden", textOverflow = "ellipsis")
      )
      col_defs[["Portal"]] <- reactable::colDef(
        name = "Portal",
        minWidth = 72,
        filterable = TRUE,
        filterMethod = reactable::JS("filterMulti"),
        filterInput = reactable::JS("multiSelectFilter"),
        style = list(whiteSpace = "nowrap")
      )
      for (mc in setdiff(meta_cols, c("Team", "Conf", "Role", "ToTeam", "Portal"))) {
        col_defs[[mc]] <- reactable::colDef(
          name = if (identical(mc, "Player")) "Class" else mc,
          minWidth = if (mc %in% c("Year", "Player")) 55 else 55,
          filterable = TRUE,
          filterMethod = reactable::JS("filterMulti"),
          filterInput = reactable::JS("multiSelectFilter")
        )
      }

      rank_alias_labels <- c(
        dsr = "DSR",
        ptir = "PTIR",
        dpmr = "DPMR",
        edmr = "EDMR"
      )
      vapormint_style_js <- reactable::JS(
        "function(rowInfo, column) {
         var vi = rowInfo.viewIndex;
         if (vi === undefined || vi === null) vi = rowInfo.index;
         if (vi === undefined || vi === null || vi >= 250) return null;
         var v = Number(rowInfo.values[column.id]);
         if (!isFinite(v)) return null;
         v = Math.max(0, Math.min(100, v));
         var palette = ['#23ff96','#4fffab','#7bffc0','#a7ffd5','#d3ffea','#ffe1e1','#ffc2c2','#ffa3a3','#ff8585','#ff6666'];
         var idx = Math.min(9, Math.max(0, Math.floor((100 - v) / 10)));
         return {
           background: palette[idx],
           color: '#0f172a',
           fontWeight: 600
         };
       }"
      )
      # MODIFIED: Removed per-column tooltip helpers (build_composite_tooltip, make_composite_header).
      # Glossary is now surfaced via a single modal button above the table instead.
      for (cc in comp_cols) {
        base_label <- if (cc %in% names(rank_alias_labels)) {
          rank_alias_labels[[cc]]
        } else if (cc %in% names(composite_table_labels)) {
          composite_table_labels[[cc]]
        } else if (cc %in% names(composite_labels)) {
          composite_labels[[cc]]
        } else {
          cc
        }
        col_defs[[cc]] <- reactable::colDef(
          name = base_label,
          minWidth = 50,
          maxWidth = 65,
          align = "center",
          filterable = TRUE,
          filterMethod = reactable::JS("filterRange"),
          filterInput = reactable::JS("muiRangeFilter"),
          sortable = TRUE,
          format = reactable::colFormat(digits = 1),
          style = vapormint_style_js
        )
      }
      for (rc in raw_stat_cols) {
        base_label <- if (identical(rc, "Min_pct")) {
          "Min%"
        } else if (rc %in% names(stat_labels)) {
          stat_labels[[rc]]
        } else {
          rc
        }

        if (is.numeric(table_df[[rc]]) || is.integer(table_df[[rc]])) {
          fmt <- if (rc %in% percent_display_cols) {
            reactable::colFormat(digits = 1, suffix = "%")
          } else {
            reactable::colFormat(digits = 1)
          }
          col_defs[[rc]] <- reactable::colDef(
            name = base_label,
            minWidth = 65,
            align = "center",
            filterable = TRUE,
            filterMethod = reactable::JS("filterRange"),
            filterInput = reactable::JS("muiRangeFilter"),
            sortable = TRUE,
            format = fmt,
            show = !isTRUE(input$is_mobile)
          )
        } else {
          col_defs[[rc]] <- reactable::colDef(
            name = base_label,
            minWidth = 65,
            filterable = TRUE,
            filterMethod = reactable::JS("filterMulti"),
            filterInput = reactable::JS("multiSelectFilter"),
            show = !isTRUE(input$is_mobile)
          )
        }
      }

      # default sort on PTIR if present, otherwise fall back to composite ordering
      default_sort_col <- {
        pref <- c("ptir", "PredTransferImpactRating")
        hit_pref <- pref[pref %in% names(table_df)]
        if (length(hit_pref) > 0) {
          hit_pref[[1]]
        } else {
          hit <- composite_col_order[composite_col_order %in% names(table_df)]
          if (length(hit) > 0) hit[[1]] else "Name"
        }
      }

      # reactable expects defaultSorted as a character vector OR a named list like list(`Col` = "desc")
      safe_default_sorted <- NULL
      if (!is.null(default_sort_col) && nzchar(default_sort_col) && default_sort_col %in% names(table_df)) {
        safe_default_sorted <- stats::setNames(list("desc"), default_sort_col)
      }

      out_tbl <- timed("radarPlayerTable_reactable_build", reactable::reactable(
        table_df,
        columns = col_defs,
        searchable = FALSE,
        filterable = TRUE,
        highlight = TRUE,
        striped = TRUE,
        bordered = TRUE,
        compact = TRUE,
        defaultPageSize = 15,
        pagination = TRUE,
        paginationType = "jump",
        showPageInfo = TRUE,
        showPageSizeOptions = TRUE,
        pageSizeOptions = c(15, 25, 50, 100, 250),
        language = reactable::reactableLang(noData = "No players match the current filters."),
        defaultSorted = safe_default_sorted,
        defaultSortOrder = "desc",
        onClick = htmlwidgets::JS(
          "function(rowInfo, colInfo) {
            if (!rowInfo || !rowInfo.row) return;
            if (colInfo && colInfo.id === 'Star') return;
            if (!window.Shiny) return;
            if (typeof Shiny.setInputValue === 'function') {
              Shiny.setInputValue('radar_table_click', rowInfo.row.Name, {priority: 'event'});
            } else if (typeof Shiny.onInputChange === 'function') {
              Shiny.onInputChange('radar_table_click', rowInfo.row.Name);
            }
          }"
        )
      ))

      # FIX: Use htmlwidgets::onRender directly, rather than passing onRender to reactable()
      out_tbl <- htmlwidgets::onRender(out_tbl, htmlwidgets::JS("
        function(el, x) {
          if (!window.Shiny) return;

          // ---- Server-side filter bridge ----------------------------------------
          // Only re-query Supabase when filters or search change.
          // Sort and page changes are handled client-side by reactable — sending
          // those to Shiny would cause a needless SQL round-trip and double-render.
          var __radarTableDebounce = null;
          var __lastSentFilterKey  = null;

          // Seed the key from the widget's initial state so the first onStateChange
          // after a re-render doesn't fire a redundant Shiny.setInputValue call.
          if (window.Reactable && typeof window.Reactable.getState === 'function') {
            try {
              var initSt   = window.Reactable.getState(el.id) || {};
              var initData = { filters: window.extractReactableFilters(initSt.filters || []).filter(function(f) { return f.id !== 'Name'; }) };
              __lastSentFilterKey = JSON.stringify(initData);
            } catch(e) {}
          }

          function sendRadarTableState(tableId) {
            if (!window.Reactable || typeof window.Reactable.getState !== 'function') return;
            try {
              var st = window.Reactable.getState(tableId) || {};
              // Only send column filters to Shiny — name search is handled
              // client-side by reactable's built-in searchable, so omitting
              // searchValue prevents SQL re-queries on every keystroke.
              var filterData = {
                filters: window.extractReactableFilters(st.filters || []).filter(function(f) {
                  return f.id !== 'Name';
                })
              };
              // Skip if column filters haven't changed
              var key = JSON.stringify(filterData);
              if (key === __lastSentFilterKey) return;
              __lastSentFilterKey = key;
              if (window.Shiny && typeof Shiny.setInputValue === 'function') {
                Shiny.setInputValue('radar_table_state', filterData);
              }
            } catch(e) {}
          }

          if (window.Reactable && typeof window.Reactable.onStateChange === 'function') {
            Reactable.onStateChange(el.id, function(state) {
              clearTimeout(__radarTableDebounce);
              __radarTableDebounce = setTimeout(function() {
                sendRadarTableState(el.id);
              }, 600);
            });
          }
        }"))
      if (profiling_enabled()) message("[radarPlayerTable] reactable built")
      session$sendCustomMessage("watchlist_update", list(pids = isolate(watchlist_rv())))
      out_tbl
    })
  })

  # Click a row -> add that player to the radar selection (do not replace)
  observeEvent(input$radar_table_click,
    {
      req(is_radar_tab_active())
      req(input$radar_table_click)

      clicked <- as.character(input$radar_table_click)

      current <- isolate(input$players)
      if (is.null(current)) current <- character(0)
      current <- as.character(current)

      # append + de-dup while preserving order
      new_sel <- unique(c(current, clicked))

      # cap at 3 players (keep earliest selections)
      if (length(new_sel) > 3) new_sel <- new_sel[seq_len(3)]

      # ensure clicked player is present in choices; otherwise selectize may ignore it
      ord <- isolate(player_choice_order())
      if (is.null(ord)) ord <- character(0)
      ord <- as.character(ord)
      choices_all <- unique(c(ord, clicked))

      updateSelectizeInput(
        session,
        "players",
        choices  = choices_all,
        selected = new_sel,
        server   = TRUE
      )

      updateNavbarPage(session, "main_nav", selected = "Radar tab")
    },
    ignoreInit = TRUE,
    priority = 1
  )

  # ---- palette you want (fixed order) ------------------------------------------
  radar_palette <- c("#0077BB", "#EE7733", "#009988", "#CC3311", "#33BBEE", "#EE3377", "#BBBBBB")

  # ---- 1) build the selector ordering: Year (desc), Team, Name -----------------
  player_choice_order <- reactive({
    req(radar_inputs_ready())
    radar_player_pool(
      year_selection = year_filter_d(),
      team_selection = input$team_filter,
      role_selection = input$role_filter,
      prpg_selection = input$prpg_filter
    )
  })

  # ---------------------------------------------------------------------------
  # radar_percentile_data: on-demand fetch of *_pct columns + raw stats for
  # the selected players and all benchmark rows.
  # Serves as both `percentile_stats` and `player_stats_all` for radar chart
  # rendering and profile label generation — replaces the former global
  # percentile_stats (which required loading *_pct for ALL players at startup).
  # ---------------------------------------------------------------------------
  radar_percentile_data <- reactive({
    req(input$players)
    player_names <- unique(c(as.character(input$players), benchmark_names))

    tryCatch({
      if (is.null(supabase_pool)) stop("pool unavailable")

      # Discover which *_pct columns exist in the DB (cached via memoisation if desired)
      all_db_cols <- DBI::dbGetQuery(
        supabase_pool,
        "SELECT column_name FROM information_schema.columns
         WHERE table_schema = 'public' AND table_name = 'basketball_players'"
      )$column_name

      pct_cols     <- intersect(paste0(radar_vars_all, "_pct"), all_db_cols)
      raw_stat_cols <- intersect(radar_vars_all, all_db_cols)
      id_cols      <- intersect(c("Name", "Year", "Pick"), all_db_cols)
      fetch_cols   <- unique(c(id_cols, pct_cols, raw_stat_cols))

      col_sql  <- paste(sprintf('"%s"', fetch_cols), collapse = ", ")
      safe_names <- paste(sprintf("'%s'", gsub("'", "''", player_names)), collapse = ", ")
      sql      <- sprintf(
        'SELECT %s FROM "basketball_players" WHERE "Name" IN (%s)',
        col_sql, safe_names
      )

      df <- DBI::dbGetQuery(supabase_pool, sql)

      # Rename *_pct columns to bare stat names so the df can serve as
      # percentile_stats (expected column format by prepare_radar_long).
      # Always overwrite the bare stat column with the *_pct value — both
      # raw and pct columns are fetched, so the naive rename guard
      # (!stat %in% names(df)) would leave pct_lookup reading raw stats.
      for (stat in radar_vars_all) {
        pct_col <- paste0(stat, "_pct")
        if (pct_col %in% names(df)) {
          df[[stat]] <- df[[pct_col]]
          df[[pct_col]] <- NULL
        }
      }

      df
    }, error = function(e) {
      warning("[radar_percentile_data] fetch failed, using global percentile_stats: ", conditionMessage(e))
      # Graceful fallback to global for the requested players
      req_names <- unique(c(as.character(input$players), benchmark_names))
      pst <- if (exists("percentile_stats", inherits = TRUE)) percentile_stats else data.frame()
      pst %>% dplyr::filter(.data$Name %in% req_names)
    })
  })

  # ---- 2) update selector choices in that order (Year not shown) ----------------
  observeEvent(
    list(radar_inputs_ready(), year_filter_d(), input$team_filter, input$role_filter, input$prpg_filter),
    {
      req(radar_inputs_ready())
      ord <- player_choice_order()

      if (length(ord) == 0) {
        updateSelectizeInput(session, "players",
          choices = character(0),
          selected = character(0),
          server = TRUE
        )
        return()
      }

      current <- isolate(input$players)
      current <- intersect(current, ord) # keep only still-valid players

      updateSelectizeInput(session, "players",
        choices = ord,
        selected = current,
        server = TRUE
      )
    },
    ignoreInit = FALSE
  )

  # ---- 3) radarPlot: enforce that same ordering + color assignment --------------
  output$radarPlot <- plotly::renderPlotly({
    with_timing("render_radarPlot", meta = list(tab = "radar"), expr = {
      req(is_radar_tab_active())
      req(input$players)

      players_to_plot <- head(input$players, 3)

      # Build dynamic title from selected player names
      make_radar_title <- function(main_text) {
        list(text = "")
      }

      # ADDED: Wrap and slightly shrink the caption for mobile widths
      format_radar_caption <- function(caption_text) {
        plot_w <- session$clientData$output_radarPlot_width
        if (!is.null(plot_w) && is.finite(plot_w) && plot_w < 520) {
          wrapped <- paste(strwrap(caption_text, width = 34), collapse = "<br>")
          return(list(text = wrapped, font_size = 8))
        }
        list(text = caption_text, font_size = 9)
      }

      # Build composite axis spec respecting PTIR/EDMR toggles
      eff_composite_groups <- composite_groups
      eff_axis_order       <- composite_axis_order
      eff_axis_labels      <- composite_labels
      if (isTRUE(input$include_ptir_axis)) {
        eff_composite_groups <- c(eff_composite_groups, list(ptir = c(ptir = 1)))
        eff_axis_order       <- c(eff_axis_order, "ptir")
        eff_axis_labels      <- c(eff_axis_labels, ptir = "PTIR")
      }
      if (isTRUE(input$include_edmr_axis)) {
        eff_composite_groups <- c(eff_composite_groups, list(edmr = c(edmr = 1)))
        eff_axis_order       <- c(eff_axis_order, "edmr")
        eff_axis_labels      <- c(eff_axis_labels, edmr = "EDMR")
      }

      # Use per-player on-demand data (*_pct columns + raw stats).
      # Falls back to global percentile_stats if fetch fails.
      pct_data <- radar_percentile_data()

      p <- if (identical(input$view_mode, "Composite Metrics")) {
        plot_radar_plotly(
          players = players_to_plot,
          view_mode = "composite",
          percentile_stats = pct_data,
          composite_percentiles_df = composite_percentiles_df,
          radar_vars_all = radar_vars_all,
          composite_groups = eff_composite_groups,
          stat_labels = stat_labels,
          title = make_radar_title(if (identical(input$view_mode, "Composite Metrics")) "Player Composite Strength Profile" else "Player Strength Profile"),
          axis_order = eff_axis_order,
          axis_labels = eff_axis_labels,
          palette     = radar_palette,
          fill_alpha  = if (isTRUE(input$dark_mode)) 0.22 else 0.16,
          line_width  = if (isTRUE(input$dark_mode)) 3.2 else 2.8,
          marker_size = if (isTRUE(input$dark_mode)) 7 else 6
        )
      } else {
        plot_radar_plotly(
          players = players_to_plot,
          view_mode = "raw",
          percentile_stats = pct_data,
          composite_percentiles_df = composite_percentiles_df,
          radar_vars_all = {
            sel <- input$radar_vars_select
            if (is.null(sel) || length(sel) == 0) radar_vars_all_small else sel
          },
          composite_groups = composite_groups,
          stat_labels = stat_labels,
          title = make_radar_title(if (identical(input$view_mode, "Composite Metrics")) "Player Composite Strength Profile" else "Player Strength Profile"),
          axis_order = NULL,
          palette     = radar_palette,
          fill_alpha  = if (isTRUE(input$dark_mode)) 0.22 else 0.16,
          line_width  = if (isTRUE(input$dark_mode)) 3.2 else 2.8,
          marker_size = if (isTRUE(input$dark_mode)) 7 else 6
        )
      }

      # ADDED: Mobile-aware caption formatting
      caption_cfg <- format_radar_caption("Data: barttorvik.com | Created on https://cbb.arkansasquant.com/")
      plot_w      <- session$clientData$output_radarPlot_width
      is_narrow   <- !is.null(plot_w) && is.finite(plot_w) && plot_w < 520

      # Mobile: narrow l/r so the polar circle isn't width-limited on a small
      # screen; larger b to ensure the caption annotation isn't clipped.
      margin_cfg <- if (is_narrow) {
        list(l = 15, r = 15, t = 5, b = 110)
      } else {
        list(l = 60, r = 60, t = 95, b = 65)
      }
      caption_y_val <- if (is_narrow) -0.10 else -0.12

      p <- p %>%
        plotly::layout(
          margin = margin_cfg,
          annotations = list(
            # --- Rotated Watermark ---
            list(
              text = "@ArkansasQuant",
              x = 0.5,
              y = 0.5,
              xref = "paper",
              yref = "paper",
              showarrow = FALSE,
              textangle = -45,
              font = list(
                size = if (is_narrow) 20L else 40L,
                color = if (isTRUE(input$dark_mode)) "rgba(255,255,255,0.18)" else "rgba(0,0,0,0.12)"
              ),
              xanchor = "center",
              yanchor = "middle",
              layer = "below"
            ),

            # --- Credit Line (Below Legend) ---
            list(
              text = caption_cfg$text,
              x = 1,
              y = caption_y_val,
              xref = "paper",
              yref = "paper",
              showarrow = FALSE,
              xanchor = "right",
              yanchor = "top",
              align = "right",
              font = list(
                size = caption_cfg$font_size,
                color = "gray60"
              )
            )
          )
        )

      # Keep radar-specific formatting, only overlay light/dark colors.
      p <- p %>% plotly::layout(
        paper_bgcolor = if (isTRUE(input$dark_mode)) "#111827" else "#ffffff",
        plot_bgcolor = if (isTRUE(input$dark_mode)) "#111827" else "#ffffff",
        font = list(color = if (isTRUE(input$dark_mode)) "#e5e7eb" else "#222222"),
        polar = list(
          domain = list(x = c(0.05, 0.95), y = c(0, 0.87)),
          bgcolor = if (isTRUE(input$dark_mode)) "#111827" else "#ffffff",
          angularaxis = list(
            gridcolor = if (isTRUE(input$dark_mode)) "#334155" else "#d1d5db",
            linecolor = if (isTRUE(input$dark_mode)) "#475569" else "#9ca3af",
            tickfont = list(
              color = if (isTRUE(input$dark_mode)) "#e5e7eb" else "#222222",
              size  = if (is_narrow) 9L else if (identical(input$view_mode, "Individual Stats")) 10L else 13L
            )
          ),
          radialaxis = list(
            gridcolor = if (isTRUE(input$dark_mode)) "#334155" else "#d1d5db",
            linecolor = if (isTRUE(input$dark_mode)) "#475569" else "#9ca3af",
            tickfont = list(color = if (isTRUE(input$dark_mode)) "#e5e7eb" else "#222222")
          )
        )
      )

      if (is_narrow) {
        p <- p %>% plotly::layout(
          margin = list(l = 52, r = 52, t = 60, b = 100),
          polar  = list(domain = list(x = c(0.10, 0.90))),
          legend = list(orientation = "h", x = 0, xanchor = "left",
                        y = 1.04, yanchor = "bottom", font = list(size = 9))
        )
      }

      p
    })
  })


  # ui.R references these outputs; keep them defined (empty)
  output$playerProfiles <- renderUI({
    req(input$players)

    players_to_show <- head(input$players, 3)

    # Use on-demand per-player percentile data (includes *_pct columns + raw stats
    # for selected players and all benchmark rows). This replaces the global
    # percentile_stats which required loading all *_pct columns at startup.
    pct_df <- radar_percentile_data()

    player_ui_list <- lapply(players_to_show, function(player_name) {
      row      <- pct_df %>% dplyr::filter(Name == player_name)

      if (nrow(row) == 0) {
        return(NULL)
      }

      # Use player_stats_all (global) for raw display stats — pct_df has
      # percentile values in the bare-name columns after radar_percentile_data()
      # renames *_pct → base stat name for the radar chart lookup.
      raw_row <- player_stats_all %>% dplyr::filter(Name == player_name)
      stat_src <- if (nrow(raw_row) > 0) raw_row else row

      role  <- chr1(stat_src$Role %||% NA_character_, NA_character_)
      class <- if ("Player" %in% names(stat_src)) chr1(stat_src$Player[1], NA_character_) else NA_character_
      ht    <- if ("Ht" %in% names(stat_src)) format_height(stat_src$Ht[1]) else NA
      pts   <- if ("Pts" %in% names(stat_src)) round(stat_src$Pts[1], 1) else NA
      reb   <- if ("Reb" %in% names(stat_src)) round(stat_src$Reb[1], 1) else NA
      ast   <- if ("Ast" %in% names(stat_src)) round(stat_src$Ast[1], 1) else NA
      stl   <- if ("Stl" %in% names(stat_src)) round(stat_src$Stl[1], 1) else NA
      blk   <- if ("Blk" %in% names(stat_src)) round(stat_src$Blk[1], 1) else NA
      ptir  <- if ("PredTransferImpactRating" %in% names(stat_src)) round(stat_src$PredTransferImpactRating[1], 1) else NA

      row_data <- row %>% dplyr::select(-Name)
      # pct_df includes both the player and benchmark rows, so get_profile_labels
      # can find D1 Avg / 1st Rder Avg reference rows for comparison.
      profile <- get_profile_labels(row_data, role, pct_df)

      strengths <- profile$strengths
      weaknesses <- profile$weaknesses

      strengths <- strengths[!is.na(strengths) & nzchar(strengths)]
      weaknesses <- weaknesses[!is.na(weaknesses) & nzchar(weaknesses)]

      # Build "Role | Class" subtitle line
      role_class_parts <- na.omit(c(
        if (!is.na(role) && nzchar(role)) role else NA_character_,
        if (!is.na(class) && nzchar(class)) class else NA_character_,
        if (!is.na(ptir)) paste0("PTIR: ", ptir) else NA_character_
      ))
      role_class_line <- if (length(role_class_parts) > 0) {
        tags$p(em(paste(role_class_parts, collapse = " | ")))
      } else NULL

      wellPanel(
        tags$h4(player_name),
        role_class_line,
        fluidRow(
          column(width = 6,
            if (!is.na(ht))  tags$p(strong("Height:"), ht),
            if (!is.na(pts)) tags$p(strong("PTS:"),    pts),
            if (!is.na(reb)) tags$p(strong("REB:"),    reb)
          ),
          column(width = 6,
            if (!is.na(ast)) tags$p(strong("AST:"),  ast),
            if (!is.na(stl)) tags$p(strong("STL%:"), stl),
            if (!is.na(blk)) tags$p(strong("BLK%:"), blk)
          )
        ),
        tags$h5("Strengths"),
        if (length(strengths) > 0) {
          do.call(tagList, lapply(strengths, function(stat) {
            label <- if (stat %in% names(stat_labels)) stat_labels[[stat]] else stat
            tags$span(class = "label label-success", chr1(label, stat))
          }))
        } else {
          tags$em("No standout strengths")
        },
        tags$h5("Weaknesses"),
        if (length(weaknesses) > 0) {
          do.call(tagList, lapply(weaknesses, function(stat) {
            label <- if (stat %in% names(stat_labels)) stat_labels[[stat]] else stat
            tags$span(class = "label label-danger", chr1(label, stat))
          }))
        } else {
          tags$em("No major weaknesses")
        },
        tags$hr(style = "margin: 10px 0;"),
        fluidRow(
          column(
            width = 6,
            tags$h5("Most Similar Players (Drafted)"),
            render_similar_players_ui(player_name = player_name, top_n = 10)
          ),
          column(
            width = 6,
            tags$h5("Most Similar Players (2026)"),
            render_similar_current_players_ui(
              player_name = player_name,
              top_n = 10,
              class_filter = input$similar_current_class
            )
          )
        )
      )
    })

    do.call(tagList, player_ui_list)
  })


  output$playerStrengths <- renderUI(NULL)
  output$playerWeaknesses <- renderUI(NULL)

  # Standalone front-office mode: Single-game and season-window
  # PBP views were intentionally removed from this app.
  observeEvent(input$radar_reactable_year_filter,
    {
      if (!is_radar_tab_active() || !profiling_enabled()) {
        return()
      }
      message("[radar] reactable Year filter:")
      print(input$radar_reactable_year_filter)
    },
    ignoreInit = TRUE
  )

  # ---------------------------------------------------------------------------
  # END SHINY SERVER
  # ---------------------------------------------------------------------------
})
