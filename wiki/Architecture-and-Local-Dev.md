# Architecture & Local Development

> For the most complete reference — including all column definitions, key functions, and a common-issues table — see [`COLLABORATOR_GUIDE.md`](../COLLABORATOR_GUIDE.md) in the repo root. That file is designed to be handed to an AI assistant for guided setup.

---

## Prerequisites

### R
R 4.4 or higher. Download from https://cran.r-project.org/

### System Libraries

**macOS** (Homebrew):
```bash
brew install libpq openssl
```

**Linux (Ubuntu/Debian)**:
```bash
sudo apt-get install -y libpq-dev libssl-dev
```

**Windows**: Install RTools 4.4 from https://cran.r-project.org/bin/windows/Rtools/

### RStudio (optional)
https://posit.co/download/rstudio-desktop/

---

## Setup

```bash
git clone https://github.com/AustinAlexander01/CBB-Portal-Analysis.git
cd CBB-Portal-Analysis
```

In R or RStudio:
```r
renv::restore()   # installs all 107 dependencies at pinned versions
```

Create `.Renviron` in the project root:
```
SUPABASE_DB_PASSWORD=<password from repo owner>
PLAYER_STATS_CACHE_HOURS=1
```

Restart R to load the `.Renviron`, then:
```r
shiny::runApp()
```

The app opens in your browser at `http://127.0.0.1:XXXX`. On first launch it fetches all player data from Supabase and writes a local cache file.

---

## File Structure

| File | Purpose |
|---|---|
| `app.R` | Entry point — sources `ui.R` and `server.R`, calls `shinyApp()` |
| `ui.R` | All UI: layout, filter inputs, dark mode CSS/JS, output placeholders |
| `server.R` | All server logic: data loading, reactive filters, SQL queries, outputs |
| `plotly_helpers.R` | Radar chart rendering, similarity matching, player profile utilities |
| `renv.lock` | Package lockfile — 107 packages at exact versions |
| `.Rprofile` | Auto-activates renv on project open |
| `manifest.json` | shinyapps.io deployment manifest |
| `rsconnect/` | Deployment metadata (not needed for local dev) |

---

## Environment Variables

| Variable | Required | Default | Description |
|---|---|---|---|
| `SUPABASE_DB_PASSWORD` | Yes | — | PostgreSQL password. Shared privately by repo owner. |
| `PLAYER_STATS_CACHE_HOURS` | No | `1` | Hours before local cache is considered stale. Set to `9999` to always use cache. |

---

## How the App Starts

1. `app.R` sources `ui.R` and `server.R`
2. Before any user connects, `server.R` runs startup globals:
   - Opens a Supabase connection pool (1–5 connections)
   - Calls `compute_app_data()` → loads player stats from Supabase or cache, computes composite percentiles
   - Sets `TABLE_DISPLAY_COLS`, `benchmark_names`, `composite_col_order`
3. When a user opens the app, the session initializes and reactive values become live

---

## Reactive Architecture

### Key reactives (per session)

| Reactive | What it does |
|---|---|
| `year_filter_d` | Debounced (300ms) year filter input |
| `filtered_stats` | In-memory filter on the global data frame — year, team, role, PRPG, composites, portal |
| `radar_filtered` | Same filters as `filtered_stats`, used for radar chart data |
| `radar_table_state_d` | Debounced (600ms) table search/sort state from client JS |
| `radar_table_filtered` | Executes parameterized SQL via `build_table_query()`, feeds Reactable |
| `radar_percentile_data` | Looks up composite percentiles for selected players |

### Filter chain
```
Sidebar inputs ──► filtered_stats ──► player selector choices
                                  └──► radar_filtered ──► radar chart

Sidebar inputs + table search/sort ──► build_table_query() ──► SQL ──► player table
```

---

## Adding Packages

1. Install the package: `install.packages("packagename")`
2. Use it in code
3. Run `renv::snapshot()` to update `renv.lock`
4. Commit both your code change and the updated `renv.lock`

Never edit `renv.lock` manually.

---

## Deployment

The app is hosted on shinyapps.io under account `lunchbox`.

```r
rsconnect::writeManifest()     # regenerate manifest.json if files changed
rsconnect::deployApp(appName = "mbb_radar_app")
```

Set `SUPABASE_DB_PASSWORD` as an environment variable in the shinyapps.io dashboard (not in `.Renviron` — that file is not deployed).

---

## Cache Management

| Task | Action |
|---|---|
| Force full data refresh | Delete `.player_stats_cache.rds` and `.player_stats_cache_meta.rds`, restart app |
| Disable cache expiry (dev) | Set `PLAYER_STATS_CACHE_HOURS=9999` |
| Inspect cache age | `readRDS(".player_stats_cache_meta.rds")` |

If Supabase is unreachable, the app uses the most recent cache file silently.
