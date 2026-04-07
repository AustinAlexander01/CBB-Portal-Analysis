# Data Pipeline

This page describes how raw basketball statistics are collected, processed, and loaded into the Supabase database that powers the app.

> Note: The pipeline scripts (`build_player_composites.R`, `upload_to_supabase.R`) live in the parent workspace, not in this repository. This page documents how they work so collaborators can run or modify them.

---

## Overview

```
Torvik (barttorvik.com)
        │
        ▼
torvik_pbp_playerstat_array_YYYY.json   (raw per-player stats, one file per season)
        │
        ▼
build_player_composites.R               (compute composite percentile scores)
        │
        ▼
Processed data frame (in memory)
        │
        ▼
upload_to_supabase.R                    (upsert to basketball_players table)
        │
        ▼
Supabase PostgreSQL (basketball_players table)
        │
        ▼
CBB Portal Analysis app                 (reads on startup, caches locally)
```

---

## Step 1: Raw Stats Collection

Player-level box scores and advanced stats are sourced from **Barttorvik** (barttorvik.com), which provides granular play-by-play derived metrics for all D1 players.

Raw data is stored as JSON arrays, one file per season:
```
torvik_pbp_playerstat_array_2024.json
torvik_pbp_playerstat_array_2025.json
torvik_pbp_playerstat_array_2026.json
...
```

Each JSON file contains an array of player-season objects with all raw and advanced stat fields.

---

## Step 2: Composite Building (`build_player_composites.R`)

This script reads the Torvik JSON files and produces the enriched data frame written to Supabase.

**What it does**:
1. Reads all `torvik_pbp_playerstat_array_*.json` files
2. Normalizes column names and data types
3. Assigns `Role` classifications based on position, height, and usage patterns
4. Computes the **11 composite percentile scores** per player per season:
   - Each composite is a weighted combination of relevant raw stats
   - Percentile rank is computed within the same `Year` group (so a 75 always means "75th percentile among D1 players that season")
5. Computes model-based rankings: `ptir`, `edmr`, `dsr`, `dpmr`
6. Adds `pid` lookups from `mbb_portal_player_xref` where applicable
7. Writes benchmark rows (D1 averages, draft pick averages) as special rows in the output

**Output**: A data frame ready to upsert into `basketball_players`.

---

## Step 3: Supabase Upload (`upload_to_supabase.R`)

This script takes the processed data frame and upserts it to Supabase.

**What it does**:
1. Connects to Supabase using `SUPABASE_DB_PASSWORD`
2. Upserts all rows to `basketball_players` — upsert key is `(Name, Team, Year)`
3. New player-seasons are inserted; existing ones are updated
4. Logs row counts and any errors

**Requires**: `SUPABASE_DB_PASSWORD` set in the environment.

---

## Step 4: GitHub Actions (`update-player-stats.yml`)

A GitHub Actions workflow automates steps 2–3 via **manual dispatch** (no scheduled run — trigger it intentionally).

**Workflow file**: `.github/workflows/update-player-stats.yml`

**Trigger**: `workflow_dispatch` (run manually from GitHub Actions tab)

**Required secret**: `SUPABASE_DB_PASSWORD` must be set in the repository's GitHub Actions secrets (Settings → Secrets and variables → Actions).

**Jobs**:
1. Check out the repository
2. Set up R 4.4
3. Install system dependencies (`libpq-dev`, `libssl-dev`)
4. Run `Rscript build_player_composites.R`
5. Run `Rscript upload_to_supabase.R`

---

## Running the Pipeline Manually

To update player data outside of GitHub Actions:

```r
# From the parent workspace directory (not the CBB Portal Analysis subdirectory)
source("build_player_composites.R")
source("upload_to_supabase.R")
```

Ensure `SUPABASE_DB_PASSWORD` is set in your `.Renviron` before running.

---

## After Updating Data

Once new data is in Supabase, the app picks it up automatically the next time the cache expires (default: 1 hour). To force an immediate refresh:

1. Delete `.player_stats_cache.rds` and `.player_stats_cache_meta.rds` from the app's project directory
2. Restart the Shiny app (or the shinyapps.io instance)

---

## Portal Player Cross-Reference

Transfer portal eligibility is tracked in a separate Supabase table: `mbb_portal_player_xref`. This table maps player `pid` values to portal-eligible players. The app loads all portal `pid` values at startup and uses them to populate the `Portal` column and power the "Portal Players Only" filter.

To update portal data: add rows to `mbb_portal_player_xref` with the appropriate `pid` values. The app will reflect changes after the next cache refresh.
