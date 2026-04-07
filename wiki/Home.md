# CBB Portal Analysis — Wiki Home

**[Live App](https://lunchbox.shinyapps.io/mbb_radar_app/)** | **[GitHub Repository](https://github.com/AustinAlexander01/CBB-Portal-Analysis)**

---

## What Is This?

CBB Portal Analysis is an interactive R Shiny application for scouting NCAA Division I Men's Basketball players. It is built around the transfer portal — giving coaches, analysts, and fans a visual, data-driven way to evaluate portal prospects and compare players across seasons.

The app connects to a Supabase PostgreSQL database containing player stats and composite ratings for D1 players from 2010 through the current season. Key capabilities:

- **Radar chart comparison** of up to 3 players on 11 composite skill dimensions
- **Transfer portal filter** to restrict results to portal-eligible players
- **Advanced filtering** by year, team, position role, and per-metric thresholds
- **Player profiles** with strength/weakness labels and similar-player discovery
- **Dark mode** with full theming

---

## Wiki Pages

| Page | Description | Audience |
|---|---|---|
| [App Features](App-Features) | Walkthrough of every feature in the app | Users & analysts |
| [Data Dictionary](Data-Dictionary) | Every database column explained | Developers & analysts |
| [Architecture and Local Dev](Architecture-and-Local-Dev) | How to run the app locally and how the code is structured | Collaborators |
| [Data Pipeline](Data-Pipeline) | How player data is built and loaded into Supabase | Collaborators |

---

## Quick Links

- **Run locally**: See [Architecture and Local Dev](Architecture-and-Local-Dev)
- **Understand the columns**: See [Data Dictionary](Data-Dictionary)
- **Learn what the filters do**: See [App Features](App-Features)
- **Set up the data pipeline**: See [Data Pipeline](Data-Pipeline)
- **Full AI-readable onboarding doc**: [`COLLABORATOR_GUIDE.md`](../COLLABORATOR_GUIDE.md) in the repo root
