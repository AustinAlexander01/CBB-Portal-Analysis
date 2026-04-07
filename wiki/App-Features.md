# App Features

**[Live App](https://lunchbox.shinyapps.io/mbb_radar_app/)**

---

## Radar Chart Comparison

The radar chart is the core of the app. It displays up to **3 players simultaneously** on a circular chart where each axis represents a different skill dimension. All values are **D1 percentiles within the same season** (0 = worst, 100 = best), so players from different eras are compared on equal footing.

### Composite View (default)
Shows all 11 composite dimensions:

| Axis | What it captures |
|---|---|
| Offensive Efficiency | Efficient scoring — rewards low-turnover, high-value shot selection |
| Defensive Efficiency | Stops created per possession — blocks, steals, defensive ratings |
| Rebounding | Blended offensive + defensive rebounding, adjusted for position |
| Scoring | Raw scoring output — points, usage rate, and scoring rate |
| Ball Handling / Play Making | Assists, creation, assist rate, turnover avoidance |
| Midrange Offense | Midrange shot volume and efficiency |
| Interior Offense | At-rim attempts, efficiency, and paint scoring |
| Perimeter Offense | Three-point volume, efficiency, and perimeter creation |
| Perimeter Defense | On-ball perimeter defense: steals, opponent shooting |
| Interior Defense | Paint protection: blocks, block rate, defensive rebounding |
| Unassisted Scoring | Self-created shot-making — off-the-dribble and unassisted makes |

### Individual Stats View
Switch from composite dimensions to raw or advanced stats using the **radar view toggle**. Select any combination of available stat columns to populate the axes.

### Optional Axes
Two additional axes can be toggled on/off:
- **PTIR axis** — Predicted Transfer Impact Rating (model estimate of portal value)
- **EDMR axis** — Estimated Draft Model Rating (model estimate of draft prospect value)

---

## Player Selection

Use the **player selector** (search box at the top of the sidebar) to find players by name. The selector autocompletes from the current filtered player pool — so if a year or team filter is active, only matching players appear.

- Select up to **3 players** at a time
- Players are color-coded on the radar (blue, orange, green)
- **Click any row** in the player table to add that player to the radar
- **Click a similar player** in a profile card to add them to the radar
- Use the **Reset Players** button to clear all selections

---

## Sidebar Filters

All filters work together. Changing any filter updates the player selector choices, the radar chart, and the player table simultaneously.

### Year
Multi-select dropdown. Choose one or more seasons (2010–2026). Default is the current season. Selecting multiple years allows cross-season comparisons.

### Team
Multi-select dropdown. Filter to one or more NCAA programs. Leave blank for all teams.

### Role
Multi-select dropdown. Filters by position role:
- **C** — traditional center
- **PG - Pure** — pass-first point guard
- **PG - Scoring** — score-first point guard
- **Wing G** — shooting guard / small wing
- **Wing F** — small forward / versatile wing
- **Stretch 4** — power forward with perimeter shooting
- **PF/C** — power forward / center
- **Combo G** — guard who plays both backcourt spots

### PRPG (Points Responsible Per Game)
Slider filter. Points Responsible Per Game = points scored + points created for teammates. Range is −2 to 10. Use this to filter out low-usage players or find high-impact scorers/creators.

### Portal Players Only
Checkbox. When checked, the player table and radar selector show **only players who have entered or are eligible for the transfer portal**. Portal status is determined by a match in the `mbb_portal_player_xref` table.

### Composite Metric Sliders
Below the main filters, there are 11 sliders — one per composite dimension. Each ranges from 0–100. Use these to find players who meet a specific profile (e.g. "perimeter defense ≥ 70, interior offense ≥ 60").

---

## Player Table

The interactive table below the radar shows all players matching the current filters.

### Columns
Standard display columns include: Name, Team, Conference, Role, Year, Class, Height, Games, Minutes%, and key stats and composite scores. See [[Data Dictionary]] for full column definitions.

### Searching and Filtering
- **Text search**: Type in the search box above the table to filter by player name, team, or any text column
- **Range sliders**: Numeric columns have range sliders in the column headers — drag to set min/max
- **Multi-select dropdowns**: Categorical columns (Year, Team, Role, Portal status) have dropdown filters in the headers
- **Sorting**: Click any column header to sort ascending/descending

### Adding to Radar
Click any row in the table to add that player to the radar chart. If 3 players are already selected, clicking a new player replaces the oldest selection.

### Reset Filters
The **Reset Filters** button clears all table search/sort state (text search, column filters, sort order) without affecting the sidebar filters.

### Glossary
The **Glossary** button opens a modal with definitions of all column abbreviations.

---

## Player Profile Cards

When one or more players are selected on the radar, profile cards appear below the chart. Each card shows:

### Strengths & Weaknesses
Color-coded badges indicating which composite dimensions are significantly above or below average for the player's position group.

### Benchmark Comparison
A stat table comparing the player against two baselines:
- **D1 Average** — positional average for all D1 players that season (Frontcourt, Backcourt, or Wings)
- **1st Round Draft Pick Average** — positional average for historical 1st-round NBA draft picks

### Similar Players
Two panels of similar players discovered via nearest-neighbor search in composite space:
- **Similar Draft Picks** — historical NBA draft picks with the closest composite profile
- **Similar Current Players** — active D1 players with the closest composite profile

Click any similar player name to add them to the radar.

---

## Dark Mode

Toggle between light and dark themes using the button in the top-right corner of the app. The setting is saved in your browser's local storage and persists across sessions.

Dark mode applies full theming to:
- Background, text, and panel colors
- Plotly radar chart colors and gridlines
- Reactable table
- Sidebar and filter controls
- Profile cards and badges
