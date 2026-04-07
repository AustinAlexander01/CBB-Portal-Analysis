# Data Dictionary

All data is stored in the `basketball_players` table in Supabase PostgreSQL. One row = one player-season combination. Composite scores are percentile-ranked (0–100) within the same season year.

---

## Identifiers

| Column | Type | Description |
|---|---|---|
| `Name` | text | Player display name (e.g. "Cooper Flagg") |
| `Player` | text | Alternate/slug name used for internal matching |
| `Team` | text | NCAA team name |
| `Conf` | text | Conference abbreviation (e.g. "ACC", "Big Ten", "SEC") |
| `Role` | text | Position role — see values below |
| `Class` | text | Academic year: "Fr", "So", "Jr", "Sr", "Grad" |
| `Year` | integer | Season year (e.g. `2025` = the 2024–25 season) |
| `Ht` | text | Height in feet-inches format (e.g. "6-9") |
| `pid` | text | Transfer portal player ID. Non-null if the player has entered or is eligible for the portal. Used to join against `mbb_portal_player_xref`. |

### Role Values
- `C` — traditional center
- `PG - Pure` — pass-first point guard
- `PG - Scoring` — score-first point guard
- `Wing G` — shooting guard / small wing
- `Wing F` — small forward / versatile wing
- `Stretch 4` — perimeter-shooting power forward
- `PF/C` — power forward / center combo
- `Combo G` — guard who plays both backcourt spots

---

## Volume & Usage

| Column | Type | Description |
|---|---|---|
| `G` | integer | Games played |
| `Min_pct` | numeric | Minutes percentage — share of available team minutes the player was on the floor |
| `USG` | numeric | Usage rate — percentage of team possessions used while on the floor (shots, free throws, turnovers) |
| `PRPG` | numeric | Points Responsible Per Game — scoring output plus estimated points created for teammates |

---

## Scoring & Efficiency

| Column | Type | Description |
|---|---|---|
| `Pts` | numeric | Points per game |
| `ORtg` | numeric | Offensive rating — estimated points produced per 100 possessions |
| `DRtg` | numeric | Defensive rating — estimated points allowed per 100 possessions (lower = better) |
| `BPM` | numeric | Box Plus/Minus — overall impact estimate relative to average D1 player |
| `OBPM` | numeric | Offensive Box Plus/Minus |
| `DBPM` | numeric | Defensive Box Plus/Minus |

---

## Shooting Splits

| Column | Type | Description |
|---|---|---|
| `FG_pct` | numeric | Overall field goal percentage |
| `FT_pct` | numeric | Free throw percentage |
| `TP_pct` | numeric | Three-point field goal percentage |
| `Rim_FG_pct` | numeric | At-rim (within ~4 feet) field goal percentage |
| `Rim_FGA` | numeric | At-rim attempts per game |
| `Rim_FGM` | numeric | At-rim makes per game |
| `Mid_FG_pct` | numeric | Midrange (non-rim, non-3pt) field goal percentage |
| `Mid_FGA` | numeric | Midrange attempts per game |
| `Mid_FGM` | numeric | Midrange makes per game |
| `TP_FGA` | numeric | Three-point attempts per game |
| `TP_FGM` | numeric | Three-point makes per game |
| `Ast_FG_pct` | numeric | Percentage of made field goals that were assisted (catch-and-shoot) |
| `UnAst_FG_pct` | numeric | Percentage of made field goals that were unassisted (self-created) |

---

## Rebounding

| Column | Type | Description |
|---|---|---|
| `TRB` | numeric | Total rebounds per game |
| `ORB_pct` | numeric | Offensive rebound rate — % of available offensive rebounds grabbed |
| `DRB_pct` | numeric | Defensive rebound rate — % of available defensive rebounds grabbed |

---

## Defense

| Column | Type | Description |
|---|---|---|
| `Blk_pct` | numeric | Block percentage — % of opponent two-point attempts blocked while on floor |
| `Stl_pct` | numeric | Steal percentage — % of opponent possessions ending in a steal |
| `Stps` | numeric | Stops per 40 minutes — defensive possessions successfully ended |
| `FC40` | numeric | Fouls committed per 40 minutes |

---

## Playmaking

| Column | Type | Description |
|---|---|---|
| `Ast` | numeric | Assists per game |
| `A_TO` | numeric | Assist-to-turnover ratio |
| `TO_pct` | numeric | Turnover rate — % of possessions ending in a turnover |

---

## Composite Scores (Radar Axes)

All composite scores are **percentile-ranked 0–100 within the same season**. A score of 75 means the player is in the 75th percentile among all D1 players that year for that dimension.

| Column | Description |
|---|---|
| `OffensiveEfficiency` | Efficient scoring — rewards low-turnover, high-value shot selection over volume |
| `DefensiveEfficiency` | Stops created relative to opportunities — combines steals, blocks, and defensive ratings |
| `Rebounding` | Blended offensive + defensive rebounding rates, adjusted for position |
| `Scoring` | Raw scoring output — points, usage, and scoring rate combined |
| `BallHandling` | Ball handling and playmaking — assists, creation, turnover avoidance |
| `MidrangeOffense` | Midrange shot volume and efficiency |
| `InteriorOffense` | Paint and at-rim scoring — attempts, efficiency, post scoring |
| `PerimeterOffense` | Three-point volume, efficiency, and perimeter creation |
| `PerimeterDefense` | On-ball perimeter containment — steals, opponent three-point shooting |
| `InteriorDefense` | Paint protection — blocks, block rate, defensive rebounding |
| `UnassistedScoring` | Self-created shot-making — off-the-dribble makes and unassisted volume |

---

## Rankings & Predictions

| Column | Type | Description |
|---|---|---|
| `ptir` | numeric | Predicted Transfer Impact Rating — model estimate of how much impact this player would have if they transferred. Higher = more projected impact. Used to filter the default comparison subset. |
| `edmr` | numeric | Estimated Draft Model Rating — model estimate of the player's NBA draft prospect value. Higher = stronger draft prospect. |
| `dsr` | integer | Draft Signal Rank — composite ranking based on traditional draft signals (recruiting rank, positional scarcity, etc.) |
| `dpmr` | integer | Draft Pick Model Rank — model-based draft ranking combining multiple predictive features |
| `Pick` | integer | Actual NBA draft pick number, if the player was drafted. Null for undrafted or current players. |

---

## Metadata

| Column | Type | Description |
|---|---|---|
| `load_date` | date | Date this row was loaded into the database |
| `created_at` | timestamp | Row creation timestamp |
| `updated_at` | timestamp | Row last-update timestamp |

---

## Benchmark Rows

The following rows exist in `basketball_players` but are **not real players**. They represent aggregate averages used for profile card comparisons. They are identified at startup and stored separately — they never appear in the player table or radar selector.

| Name value | Description |
|---|---|
| `001 - Averages` | All-D1 average (position-agnostic) |
| `D1 Avg - Frontcourt` | D1 average for C / PF/C / Stretch 4 |
| `D1 Avg - Backcourt` | D1 average for PG variants / Combo G |
| `D1 Avg - Wings` | D1 average for Wing G / Wing F |
| `1st Rder Avg` | Average for all 1st-round NBA draft picks |
| `1st Rder Avg - Frontcourt` | 1st-round draft pick average, frontcourt |
| `1st Rder Avg - Backcourt` | 1st-round draft pick average, backcourt |
| `1st Rder Avg - Wings` | 1st-round draft pick average, wings |
| Career variants | Long-run career aggregates for each of the above |

The app selects the positionally appropriate benchmark row based on the selected player's `Role` value.
