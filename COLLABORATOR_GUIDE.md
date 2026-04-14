# Collaborator Guide — CBB Portal Analysis

> **How to use this document**: Hand this file to an AI assistant along with the Supabase password (shared privately by the repo owner) and it can walk you through any setup, debugging, or development task without needing to read the source code itself.

---

## Repository Overview

**App name**: CBB Portal Analysis  
**Live URL**: https://cbb.arkansasquant.com/  
**GitHub**: https://github.com/AustinAlexander01/CBB-Portal-Analysis  
**Type**: R Shiny web application  
**Purpose**: Interactive scouting tool for NCAA Division I Men's Basketball players. Primary use case is evaluating transfer portal prospects — compare up to 3 players on 11 composite skill dimensions (percentile-scaled radar charts), filter by position/team/year/portal status, and discover similar players.

---

## File Inventory

### `app.R` (5 lines)
Entry point. Sources `ui.R` and `server.R`, then calls `shinyApp(ui, server)`. Never needs editing unless you rename the UI/server files.

### `ui.R` (~1,231 lines)
Defines the complete user interface using `fluidPage` with a custom dark/light theme. Key elements:

- **Dark mode toggle**: Fixed button top-right; uses `localStorage` to persist state; dispatches a `shiny:darkmode` custom event caught by `server.R`
- **Main panel**: Single-page layout with a left sidebar (filters) and main area (radar chart + player table + profiles)
- **Sidebar inputs**:
  - `input$year_filter` — multi-select, seasons 2010–2026
  - `input$team_filter` — multi-select NCAA team names
  - `input$role_filter` — multi-select: C, PG (Pure/Scoring), Wing G/F, Stretch 4, PF/C, Combo G
  - `input$prpg_filter` — slider, Points Responsible Per Game (-2 to 10)
  - `input$portal_only` — checkbox, portal players only
  - `input$uncommitted_only` — checkbox, further narrows to uncommitted portal players
  - `input$watchlist_only` — checkbox, shows only starred/watchlisted players (★)
  - `input$include_ptir_axis` / `input$include_edmr_axis` — checkboxes for optional radar axes
  - `input$radar_reset_filters` — action button, clears table search/sort state
  - `input$radar_reset_players` — action button, clears selected players
  - `input$radar_glossary_btn` — action button, opens column glossary modal
  - Per-composite sliders (11 total, range 0–100)
- **Player selector**: `input$radar_players` — Selectize input, up to 3 players, searches by name
- **Radar view toggle**: `input$radar_view` — "composite" or "individual stats"
- **Stat selector**: `input$radar_stat_cols` — shown only in "individual stats" mode
- **Outputs**:
  - `output$radarPlot` — Plotly radar chart
  - `output$radarPlayerTable` — Reactable table
  - `output$playerProfiles` — profile cards HTML
  - `output$data_updated_radar` — data freshness timestamp

### `server.R` (~3,438 lines)
All server logic. Key sections:

**Startup globals** (run once at app load, before any session):
- `supabase_pool` — DBI connection pool (1–5 connections, 5 min idle timeout) to Supabase PostgreSQL
- `player_stats_all` — full player table loaded from Supabase (or cache fallback)
- `player_stats_all_with_composites` — same as above with composite percentile columns added
- `benchmark_names` — vector of special non-player row names in the DB (see Benchmark Rows section)
- `composite_col_order` — ordered vector of the 11 composite column names
- `TABLE_DISPLAY_COLS` — character vector of column names fetched by the SQL query

**Key reactive values / reactives**:
- `radar_inputs_ready` — logical; gates all downstream reactives until Shiny UI is fully bound
- `is_mobile` — detected from browser user-agent
- `year_filter_d` — debounced (300ms) version of `input$year_filter`
- `filtered_stats` — applies year/team/role/PRPG/composite/portal filters to `player_stats_all_with_composites` (in-memory); feeds the player selector choices
- `radar_filtered` — same filters as `filtered_stats`, used for radar chart data
- `radar_table_state_d` — debounced (600ms) table search/sort state from client-side JS
- `radar_table_filtered` — executes parameterized SQL via `build_table_query()`, returns paginated result for Reactable
- `radar_percentile_data` — percentile lookup for the currently selected players
- `player_choice_order` — ordered list of up to 3 selected player names
- `watchlist_rv` — `reactiveVal(character(0))`, vector of watchlisted PIDs; initialized from `localStorage` via `watchlist_init` on session connect; toggled by `watchlist_star_click`; changes broadcast to client via `watchlist_update` custom message

**Key functions defined in server.R**:
- `load_player_stats_source()` — tries Supabase pool first, falls back to `.player_stats_cache.rds`, writes cache on success
- `compute_app_data()` — calls `load_player_stats_source()`, calls `add_composites_to_player_stats()`, returns the full enriched data frame
- `build_table_query(shiny_year, shiny_team, shiny_role, shiny_prpg, shiny_portal_only, composites, table_state, display_cols, count_only)` — builds a parameterized SQL SELECT with dynamic WHERE clauses and ORDER BY; uses `$1`-style positional params to prevent SQL injection; returns a list with `query` and `params`
- `add_params_in(vals)` — helper that generates `$N, $N+1, ...` placeholders for IN clauses
- `table_default_comparison_subset(df, year)` — applies per-year PTIR cutoffs for default visible rows

### `plotly_helpers.R` (~3,261 lines)
Visualization and analysis utilities. Key functions:

- `plot_radar_plotly(player_data, mode, stat_cols, dark_mode, show_ptir, show_edmr)` — renders the main Plotly radar chart; `mode` is "composite" or "individual"; traces are colored per player; supports 0–100 percentile scale
- `prepare_radar_long(df, cols)` — reshapes player data from wide to long format for Plotly polar axes
- `build_composite_hover(player_row)` — generates rich hover text showing stat context for each composite axis
- `apply_plotly_theme(plotly_obj, dark_mode)` — applies dark/light color scheme to an existing Plotly object via `plotlyProxy` / `Plotly.relayout`
- `find_similar_drafted_by_distance(player_vec, n)` — k-nearest-neighbors in composite space among historical draft picks
- `find_similar_current_by_distance(player_vec, n)` — k-nearest-neighbors among current active rosters
- `render_similar_players_ui(similar_df)` — returns HTML for the similar players panel
- `get_profile_labels(player_row, thresholds)` — returns strength/weakness badge labels based on composite percentile thresholds
- `render_player_profile_card(player_row, benchmark_rows)` — builds the full HTML profile card

### `renv.lock`
Package lockfile with exact versions for all 107 R package dependencies. **Never edit manually.** Always run `renv::restore()` to install from this file, and `renv::snapshot()` after adding new packages.

### `.Rprofile`
Single line: `source("renv/activate.R")`. Auto-activates the renv environment when the project is opened. Do not delete.

### `manifest.json`
Legacy artifact from the previous shinyapps.io deployment. No longer used. Can be ignored.

### `rsconnect/`
Legacy metadata folder from the previous shinyapps.io deployment. No longer used. Can be ignored.

---

## Prerequisites

### R
- **Version**: R 4.4 or higher required
- Download from https://cran.r-project.org/

### System Libraries (required for database packages)

**macOS** (via Homebrew):
```bash
brew install libpq openssl
```

**Ubuntu/Debian Linux**:
```bash
sudo apt-get install -y libpq-dev libssl-dev
```

**Windows**: Install RTools 4.4 from https://cran.r-project.org/bin/windows/Rtools/

### RStudio (optional but recommended)
Download from https://posit.co/download/rstudio-desktop/

---

## Setup Steps

1. **Clone the repository**
   ```bash
   git clone https://github.com/AustinAlexander01/CBB-Portal-Analysis.git
   cd CBB-Portal-Analysis
   ```

2. **Open the project**
   - In RStudio: File → Open Project → select `mbb_radar_app.Rproj`
   - Or in any R session: `setwd("path/to/CBB-Portal-Analysis")`

3. **Restore packages**
   ```r
   renv::restore()
   ```
   This installs all 107 dependencies at the exact versions in `renv.lock`. Takes 5–15 minutes on first run.

4. **Create `.Renviron`**
   Create a file named `.Renviron` in the project root (same folder as `app.R`). Add:
   ```
   SUPABASE_DB_PASSWORD=<password shared privately by repo owner>
   ```
   Optionally also set:
   ```
   PLAYER_STATS_CACHE_HOURS=1
   ```
   After creating `.Renviron`, restart your R session so the variable is loaded.

5. **Verify the password is loaded**
   ```r
   Sys.getenv("SUPABASE_DB_PASSWORD")
   # Should return the password, not an empty string
   ```

6. **Run the app**
   ```r
   shiny::runApp()
   ```
   Opens at `http://127.0.0.1:XXXX` in your browser. On first launch the app connects to Supabase, loads all player data (~2,000+ rows), builds composite percentiles, and writes a local cache file `.player_stats_cache.rds`.

---

## Environment Variables

| Variable | Required | Default | Description |
|---|---|---|---|
| `SUPABASE_DB_PASSWORD` | **Yes** | — | PostgreSQL password for the Supabase database. Shared privately by the repo owner. Without this, the app falls back to the local cache (or fails if no cache exists). |
| `PLAYER_STATS_CACHE_HOURS` | No | `1` | How many hours the local cache file is considered fresh before re-fetching from Supabase. Set to a large number (e.g. `9999`) to always use cache during development. |

---

## Data Architecture

### Database
- **Provider**: Supabase (managed PostgreSQL on AWS)
- **Host**: `aws-1-us-east-2.pooler.supabase.com`
- **Port**: `5432`
- **Database**: `postgres`
- **User**: `postgres.mkrllsjvjliyxgukwfme`
- **Main table**: `basketball_players` (public schema)
- **Connection**: DBI connection pool via `pool` package (1–5 connections, 5-minute idle timeout)

### Local Cache
- **File**: `.player_stats_cache.rds` (written to project root, ~20MB)
- **Metadata**: `.player_stats_cache_meta.rds` (timestamp, source info)
- **TTL**: Controlled by `PLAYER_STATS_CACHE_HOURS` (default 1 hour)
- **Behavior**: On startup, app checks if cache exists and is fresh. If yes, uses cache. If no (or stale), fetches from Supabase and writes new cache.
- **Offline fallback**: If Supabase is unreachable and a cache file exists, the app uses the cache silently without crashing.
- **Force refresh**: Delete `.player_stats_cache.rds` and restart the app.

---

## Key Database Columns

The main table `basketball_players` contains one row per player-season. Columns:

### Identifiers
| Column | Type | Description |
|---|---|---|
| `Name` | text | Player display name (e.g. "Cooper Flagg") |
| `Player` | text | Alternate/slug name used for matching |
| `Team` | text | NCAA team name |
| `Conf` | text | Conference abbreviation (e.g. "ACC", "Big Ten") |
| `Role` | text | Position role: "C", "PG - Pure", "PG - Scoring", "Wing G", "Wing F", "Stretch 4", "PF/C", "Combo G" |
| `Class` | text | Academic year: "Fr", "So", "Jr", "Sr", "Grad" |
| `Year` | integer | Season year (e.g. 2025 = 2024–25 season) |
| `Ht` | text | Height (e.g. "6-9") |
| `pid` | text | Transfer portal player ID — non-null if the player has entered or is eligible for the portal |

### Volume / Usage
| Column | Type | Description |
|---|---|---|
| `G` | integer | Games played |
| `Min_pct` | numeric | Minutes percentage (share of team minutes) |
| `USG` | numeric | Usage rate (% of possessions used while on floor) |
| `Stps` | numeric | Stops per 40 minutes (defensive possessions ended) |

### Scoring
| Column | Type | Description |
|---|---|---|
| `Pts` | numeric | Points per game |
| `ORtg` | numeric | Offensive rating (points per 100 possessions) |
| `DRtg` | numeric | Defensive rating (points allowed per 100 possessions, lower = better) |
| `BPM` | numeric | Box Plus/Minus (overall impact estimate) |
| `OBPM` | numeric | Offensive Box Plus/Minus |
| `DBPM` | numeric | Defensive Box Plus/Minus |
| `PRPG` | numeric | Points Responsible Per Game (scoring + creation) |

### Shooting Splits
| Column | Type | Description |
|---|---|---|
| `FG_pct` | numeric | Overall field goal percentage |
| `FT_pct` | numeric | Free throw percentage |
| `TP_pct` | numeric | Three-point percentage |
| `Rim_FG_pct` | numeric | At-rim FG% |
| `Rim_FGA` | numeric | At-rim attempts per game |
| `Mid_FG_pct` | numeric | Midrange FG% |
| `Mid_FGA` | numeric | Midrange attempts per game |
| `TP_FGA` | numeric | Three-point attempts per game |
| `Ast_FG_pct` | numeric | % of made field goals that were assisted |
| `UnAst_FG_pct` | numeric | % of made field goals that were unassisted |

### Rebounding & Defense
| Column | Type | Description |
|---|---|---|
| `TRB` | numeric | Total rebounds per game |
| `ORB_pct` | numeric | Offensive rebound rate |
| `DRB_pct` | numeric | Defensive rebound rate |
| `Blk_pct` | numeric | Block percentage |
| `Stl_pct` | numeric | Steal percentage |
| `FC40` | numeric | Fouls committed per 40 minutes |

### Playmaking
| Column | Type | Description |
|---|---|---|
| `Ast` | numeric | Assists per game |
| `A_TO` | numeric | Assist-to-turnover ratio |
| `TO_pct` | numeric | Turnover rate |

### Composite Scores (0–100 percentile within season)
| Column | Type | Description |
|---|---|---|
| `Scoring` | numeric | Overall scoring threat composite |
| `Defense` | numeric | Overall defensive impact composite |
| `Rebounding` | numeric | Rebounding composite |
| `BallHandling` | numeric | Ball handling and playmaking composite |
| `MidrangeOffense` | numeric | Midrange scoring composite |
| `InteriorOffense` | numeric | Paint/rim scoring composite |
| `PerimeterOffense` | numeric | Three-point and perimeter scoring composite |
| `PerimeterDefense` | numeric | On-ball perimeter defense composite |
| `InteriorDefense` | numeric | Shot-blocking and paint defense composite |
| `UnassistedScoring` | numeric | Self-created scoring composite |
| `OffensiveEfficiency` | numeric | Efficiency of offensive production |
| `DefensiveEfficiency` | numeric | Efficiency of defensive production |

### Rankings & Predictions
| Column | Type | Description |
|---|---|---|
| `ptir` | numeric | Predicted Transfer Impact Rating — model estimate of transfer value |
| `edmr` | numeric | Estimated Draft Model Rating — model estimate of draft prospect value |
| `dsr` | numeric | Draft Signal Rank — composite draft signal ranking |
| `dpmr` | numeric | Draft Pick Model Rank — model-based draft ranking |
| `Pick` | integer | Actual NBA draft pick number (if applicable) |

### Metadata
| Column | Type | Description |
|---|---|---|
| `load_date` | date | Date this row was loaded into the DB |
| `created_at` | timestamp | Row creation timestamp |
| `updated_at` | timestamp | Row last-update timestamp |

---

## Benchmark Rows

The `basketball_players` table contains special non-player benchmark rows used for comparison baselines. These rows have player-like structure but represent aggregate averages. They are loaded separately at startup into `benchmark_stats` and excluded from the main player table.

| Name value | Description |
|---|---|
| `"001 - Averages"` | All D1 average (not position-specific) |
| `"D1 Avg - Frontcourt"` | D1 average for frontcourt positions (C, PF/C, Stretch 4) |
| `"D1 Avg - Backcourt"` | D1 average for backcourt positions (PG variants, Combo G) |
| `"D1 Avg - Wings"` | D1 average for wing positions (Wing G, Wing F) |
| `"1st Rder Avg"` | Average stats for 1st-round NBA draft picks (all positions) |
| `"1st Rder Avg - Frontcourt"` | 1st-round draft pick average, frontcourt |
| `"1st Rder Avg - Backcourt"` | 1st-round draft pick average, backcourt |
| `"1st Rder Avg - Wings"` | 1st-round draft pick average, wings |
| Career variants | Career aggregates for each of the above groups |

In player profile cards, the app selects the positionally appropriate benchmark row to show alongside the player's stats.

---

## Composite Metrics (11 Radar Axes)

All composites are percentile-ranked (0–100) within the same season. 100 = best in D1 that year for that dimension.

| Axis | What it measures |
|---|---|
| **Offensive Efficiency** | Points produced per possession used — rewards efficient scorers over volume scorers |
| **Defensive Efficiency** | Stops created relative to opportunities — combines steals, blocks, and defensive ratings |
| **Rebounding** | Blended offensive + defensive rebounding rates adjusted for position |
| **Scoring** | Raw scoring output: points, usage, and scoring rate combined |
| **Ball Handling / Play Making** | Assists, assist rate, turnovers, and overall creation ability |
| **Midrange Offense** | Midrange shot volume and efficiency |
| **Interior Offense** | At-rim attempts, efficiency, and post scoring |
| **Perimeter Offense** | Three-point volume, efficiency, and off-ball perimeter scoring |
| **Perimeter Defense** | On-ball perimeter containment: steals, opponent 3P%, defensive assignments |
| **Interior Defense** | Paint protection: blocks, block rate, defensive rebounding |
| **Unassisted Scoring** | Self-created shot-making — unassisted FG% and volume |

---

## Reactive Architecture Summary

### Startup sequence (before any session opens)
1. Open Supabase connection pool (`supabase_pool`)
2. `compute_app_data()` → `load_player_stats_source()` → returns `player_stats_all`
3. `add_composites_to_player_stats()` → computes percentile composites → `player_stats_all_with_composites`
4. Extract `benchmark_names`, set `TABLE_DISPLAY_COLS`, build `composite_col_order`

### Per-session reactive chain
```
input$year_filter ──debounce──► year_filter_d
                                     │
input$team_filter ───────────────────┤
input$role_filter ───────────────────┤──► filtered_stats (in-memory filter)
input$prpg_filter ───────────────────┤        │
input$portal_only ───────────────────┤        ▼
composite sliders ───────────────────┘   player selector choices (input$radar_players)
                                              │
                                         radar_filtered ──► output$radarPlot
                                              │
input$radar_table_* ─debounce─► radar_table_state_d
                                     │
                                     ▼
                              build_table_query() → SQL → Supabase
                                     │
                                     ▼
                              radar_table_filtered ──► output$radarPlayerTable

input$radar_players ──────────► radar_percentile_data ──► output$playerProfiles

input$watchlist_star_click ───► watchlist_rv ──► watchlist_update (custom msg → client)
input$watchlist_only ─────────────────────────► filtered table rows
input$uncommitted_only ───────────────────────► filtered table rows
```

### Key globals (shared across all sessions, read-only after startup)
- `supabase_pool` — connection pool
- `player_stats_all_with_composites` — full enriched data frame (~2,000+ rows)
- `benchmark_stats` — benchmark rows data frame
- `benchmark_names` — character vector of benchmark row name values
- `TABLE_DISPLAY_COLS` — column names for SQL SELECT
- `composite_col_order` — ordered composite column names

---

## Key Functions Reference

### `server.R`

**`load_player_stats_source(pool, cache_path, cache_meta_path, cache_hours)`**
Loads player data. Priority: (1) fresh cache file, (2) Supabase query + write cache, (3) stale cache fallback, (4) error. Returns data frame.

**`build_table_query(shiny_year, shiny_team, shiny_role, shiny_prpg, shiny_portal_only, composites, table_state, display_cols, count_only)`**
Builds parameterized SQL. Returns `list(query = "SELECT ...", params = list(...))`. Uses `$1`-style positional params. Handles: IN clauses for multi-select filters, BETWEEN for range sliders, LIKE for text search, ORDER BY from table state. Guard: `length(portal_pids_vec) > 0` prevents empty `IN ()` syntax error.

**`compute_app_data()`**
Full startup pipeline. Returns named list: `player_stats`, `benchmark_stats`, `benchmark_names`.

**`add_composites_to_player_stats(df)`**
Takes raw stats data frame, computes all 11 composite percentiles within each season-year group, returns enriched data frame.

### `plotly_helpers.R`

**`plot_radar_plotly(player_data, benchmark_data, mode, stat_cols, dark_mode, show_ptir, show_edmr)`**
Renders the Plotly radar chart. `player_data` is a list of up to 3 row data frames. `mode = "composite"` uses the 11 axes; `mode = "individual"` uses `stat_cols`. Returns a Plotly figure object.

**`find_similar_drafted_by_distance(player_vec, all_players, n)`**
KNN in 11-dimensional composite space among historical NBA draft picks. Returns top-N most similar players with distance scores.

**`find_similar_current_by_distance(player_vec, all_players, n)`**
Same but searches current active rosters.

**`get_profile_labels(player_row, thresholds)`**
Returns a list of strength/weakness character labels. A dimension is a "strength" if its composite percentile exceeds the upper threshold, a "weakness" if below the lower threshold.

---

## Cache Management

| Action | How |
|---|---|
| Force full data refresh | Delete `.player_stats_cache.rds` and `.player_stats_cache_meta.rds`, then restart app |
| Always use cache (dev mode) | Set `PLAYER_STATS_CACHE_HOURS=9999` in `.Renviron` |
| Inspect cache age | Read `.player_stats_cache_meta.rds`: `readRDS(".player_stats_cache_meta.rds")` |
| Check cache size | `.player_stats_cache.rds` is typically ~20MB |

If Supabase is unreachable at startup and a cache file exists (even stale), the app loads the cached data and logs a warning. Users will see a stale data timestamp.

---

## Deployment

The app is containerized with Docker and deployed to Railway, fronted by Cloudflare.

**Live URL**: https://cbb.arkansasquant.com/

### How deployment works

1. A push to the `main` branch triggers `.github/workflows/docker-prebuild.yml`
2. GitHub Actions builds the Docker image and pushes it to GHCR:
   `ghcr.io/austinalexander01/cbb-portal-analysis:latest`
3. Railway pulls the updated image from GHCR and redeploys automatically
4. Cloudflare proxies traffic to the Railway service

### Docker image

- Base: `rocker/shiny:latest`
- System dependencies installed at build time (`libpq-dev`, `libssl-dev`, `libcurl4`, `libxml2`, etc.)
- R packages restored via `renv::restore()` at build time (cached by renv.lock hash — fast rebuilds if dependencies haven't changed)
- Supabase credentials injected as Docker build args: `SUPABASE_DB_PASSWORD`, `SUPABASE_HOST`, `SUPABASE_USER`
- Exposes port 3838, serves via `shiny-server`

### Triggering a redeploy

Merge or push to `main`. GitHub Actions handles the build and push. If `renv.lock` is unchanged, the cached image layer is reused and the build completes in under 2 minutes. A full rebuild (new packages) takes ~20 minutes.

### Health check

Configured in `railway.toml`: `GET /` with a 300-second timeout.

### Environment variables (Railway dashboard)

Set `SUPABASE_DB_PASSWORD`, `SUPABASE_HOST`, and `SUPABASE_USER` in the Railway service environment settings. These are passed as Docker build args and baked into the image at build time.

---

## Data Pipeline

Raw data flows from external sources into the app in three stages:

### 1. Raw Stats Collection
Player box scores and advanced stats are sourced from Barttorvik (torvik.com). Raw data is stored as JSON arrays in the working directory:
- `torvik_pbp_playerstat_array_YYYY.json` — one file per season

### 2. Composite Building
`build_player_composites.R` (in the parent workspace, not in this repo) reads the Torvik JSON files, computes the 11 composite percentile scores per player per season, and writes a processed data frame.

### 3. Supabase Upload
`upload_to_supabase.R` (in the parent workspace) takes the processed data frame and upserts it to the `basketball_players` table in Supabase. Upsert key is `(Name, Team, Year)`.

### 4. GitHub Actions
A GitHub Actions workflow (`.github/workflows/update-player-stats.yml`) automates steps 2–3 via manual dispatch (`workflow_dispatch` trigger). It:
1. Checks out the repo
2. Sets up R 4.4
3. Installs system dependencies (`libpq-dev`, `libssl-dev`)
4. Runs `build_player_composites.R`
5. Runs `upload_to_supabase.R`

The workflow requires `SUPABASE_DB_PASSWORD` set as a GitHub Actions secret.

---

## Watchlist Feature

The watchlist lets users star players in the table for quick recall. State is persisted to browser `localStorage` and survives session refresh.

### How it works

- Each row in the player table has a star button (☆/★) rendered via a Reactable JS cell renderer, keyed on a `data-watchlist-pid` attribute
- Clicking a star fires `Shiny.setInputValue('watchlist_star_click', {pid, ts})` to the server
- `watchlist_rv` (a server-side `reactiveVal`) toggles the PID in or out of the watchlist
- The server broadcasts the updated PID list via `session$sendCustomMessage('watchlist_update', ...)`
- The client JS handler updates all visible star buttons and persists the list to `localStorage`
- On reconnect (`shiny:connected`), the client reloads PIDs from `localStorage` and fires `watchlist_init` to the server; the server observer fires exactly once (`once = TRUE`) to restore prior state

### Filter integration

- `input$watchlist_only` (checkbox) — when checked, the table shows only rows whose `pid` is in `watchlist_rv()`
- If the watchlist is empty and the checkbox is checked, the table returns zero rows (empty data frame, not an error)

### Key signals

| Signal | Direction | Description |
|---|---|---|
| `watchlist_init` | client → server | Initial PID array from `localStorage` on session connect |
| `watchlist_star_click` | client → server | `{pid, ts}` — toggle a player in/out of watchlist |
| `watchlist_update` | server → client | Updated PID array after each toggle; client persists to `localStorage` |

### Implementation notes

- Star state is **not** stored in Supabase — it is browser-local only. Clearing site data or using private/incognito mode resets the watchlist.
- The `ts` (timestamp) field in `watchlist_star_click` ensures Shiny treats rapid repeated clicks as distinct events (prevents deduplication).

---

## Common Issues

| Symptom | Likely cause | Fix |
|---|---|---|
| App crashes at startup with DB error | `SUPABASE_DB_PASSWORD` not set or wrong | Verify `.Renviron` exists in project root and R session was restarted after creating it |
| App loads but shows no players | Cache is empty and Supabase unreachable | Check network access; verify password; delete stale cache files |
| `renv::restore()` fails with compilation error | Missing system libraries | Install `libpq-dev` / `libssl-dev` (Linux) or `brew install libpq` (Mac) |
| Radar chart blank after selecting players | Player names don't match DB names exactly | Use the table search to find exact names; names are case-sensitive in some lookups |
| Portal filter shows no players | `pid` column empty / `mbb_portal_player_xref` table not populated | Portal data is updated separately; contact repo owner |
| Watchlist stars reset after reload | `localStorage` unavailable (private/incognito mode, strict browser settings) | Expected behavior — watchlist is browser-local only, not server-persisted |
| Uncommitted filter shows no players | No portal players currently marked as uncommitted in Supabase | Data currency issue; contact repo owner to refresh portal data |
