# CBB Portal Analysis

An interactive R Shiny app for scouting NCAA Division I Men's Basketball players, with a focus on the transfer portal.

**[Live App](https://lunchbox.shinyapps.io/mbb_radar_app/)**

---

## Overview

CBB Portal Analysis lets coaches, analysts, and fans compare up to three D1 players side-by-side on 11 composite skill dimensions using interactive radar charts. Player data is pulled from a Supabase PostgreSQL database covering the 2010–2026 seasons. Filters for year, team, role, portal eligibility, and individual metrics let you narrow thousands of players down to the exact profile you're looking for.

---

## Features

- **Radar chart comparison** — visualize up to 3 players across 11 composite dimensions (Scoring, Defense, Rebounding, Ball Handling, Perimeter/Interior Offense & Defense, and more), scaled to D1 percentiles
- **Transfer portal filter** — toggle to show only portal-eligible players in both the table and radar player pool
- **Interactive player table** — server-side SQL filtering with range sliders, multi-select dropdowns, and column search; click a row to add a player to the radar
- **Player profile cards** — strength/weakness labels, benchmarks vs. D1 averages and 1st-round draft pick averages, similar player discovery
- **Individual stat view** — switch from composite metrics to raw/advanced stats on the radar axes
- **Dark mode** — full light/dark theme with persistence across sessions

---

## Tech Stack

| Layer | Technology |
|---|---|
| App framework | R Shiny |
| Visualization | Plotly (radar charts) |
| Table | Reactable + Material UI sliders |
| Database | Supabase (PostgreSQL) |
| DB connection | DBI / RPostgres / pool |
| Dependency management | renv |
| Hosting | shinyapps.io (Posit Cloud) |

---

## Project Structure

```
CBB Portal Analysis/
├── app.R               # Entry point — sources ui.R and server.R, calls shinyApp()
├── ui.R                # Full UI definition: layout, tabs, inputs, dark mode CSS/JS
├── server.R            # Server logic: data loading, reactive filters, SQL queries, outputs
├── plotly_helpers.R    # Radar chart rendering, similarity matching, player profile utilities
├── renv.lock           # Exact package versions (107 packages) — used by renv::restore()
├── renv/               # renv infrastructure (do not edit manually)
├── .Rprofile           # Auto-activates renv on project open
├── manifest.json       # shinyapps.io deployment manifest
└── rsconnect/          # Deployment metadata for Posit Connect
```

---

## Quick Start (Collaborators)

> Database credentials are shared privately by the repo owner. See `COLLABORATOR_GUIDE.md` for full setup details.

1. **Clone** the repository and open the project in RStudio (or any R environment)
2. **Restore packages**: `renv::restore()` — installs all 107 dependencies at pinned versions
3. **Set credentials**: create a `.Renviron` file in the project root containing:
   ```
   SUPABASE_DB_PASSWORD=<password from repo owner>
   ```
4. **Run**: `shiny::runApp()` — app opens at `http://localhost:XXXX`

---

## Contributing

Open an issue or pull request on [GitHub](https://github.com/AustinAlexander01/CBB-Portal-Analysis). When adding R packages, run `renv::snapshot()` to update `renv.lock` before committing.
