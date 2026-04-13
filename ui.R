# ui.R startup trim (Feb 2026): keep only required startup packages; use :: for everything else to reduce attach-time overhead.
library(shiny)
library(reactR)
library(htmltools)
library(shinycssloaders)

data_updated_footer <- function(output_id) {
  div(
    style = "text-align: right; font-size: 10px; color: #444; margin-top: 4px;",
    textOutput(output_id, inline = TRUE)
  )
}

ui_year_choices <- as.character(seq.int(2026, 2010, by = -1))
ui_role_choices <- c("C", "Combo G", "PF/C", "Pure PG", "Scoring PG", "Stretch 4", "Wing F", "Wing G")
ui_team_choices <- local({
  path <- "teamids.rds"
  if (!file.exists(path)) {
    return("001 - Averages")
  }
  teams <- tryCatch(readRDS(path)$Team, error = function(...) character(0))
  sort(unique(c("001 - Averages", as.character(teams))))
})

# --- Material UI slider filter for reactable (composite columns) --------------
muiDependency <- function() {
  list(
    # Material UI requires React
    reactR::html_dependency_react(),
    htmltools::htmlDependency(
      name = "mui",
      version = "5.6.3",
      src = c(href = "https://unpkg.com/@mui/material@5.6.3/umd/"),
      script = "material-ui.production.min.js"
    )
  )
}

# JS functions referenced in server.R via JS("muiRangeFilter") / JS("filterRange")
mui_filter_js <- "
window.muiRangeFilter = (column, state) => {
  const sliderMeta = React.useMemo(() => {
    const values = state.data
      .map(row => Number(row[column.id]))
      .filter(v => Number.isFinite(v));

    if (values.length === 0) {
      return { range: [0, 100], step: 1, digits: 0 };
    }

    let min = Math.min(...values);
    let max = Math.max(...values);
    const hasFraction = values.some(v => Math.abs(v - Math.round(v)) > 1e-9);

    const uniqueSorted = Array.from(new Set(values)).sort((a, b) => a - b);
    let minGap = Infinity;
    for (let i = 1; i < uniqueSorted.length; i += 1) {
      const gap = uniqueSorted[i] - uniqueSorted[i - 1];
      if (gap > 0 && gap < minGap) minGap = gap;
    }

    let step = 1;
    if (hasFraction) {
      if (Number.isFinite(minGap) && minGap > 0) {
        if (minGap < 0.05) step = 0.01;
        else if (minGap < 0.5) step = 0.1;
        else step = 0.5;
      } else {
        step = 0.1;
      }
    }

    const digits = step < 0.1 ? 2 : (step < 1 ? 1 : 0);
    const roundTo = (x, d) => Number(x.toFixed(d));

    min = Math.floor(min / step) * step;
    max = Math.ceil(max / step) * step;
    if (min === max) {
      max = min + (step < 1 ? step : 1);
    }

    return {
      range: [roundTo(min, digits), roundTo(max, digits)],
      step,
      digits
    };
  }, [state.data, column.id]);

  const value = Array.isArray(column.filterValue) && column.filterValue.length === 2
    ? column.filterValue
    : sliderMeta.range;
  const fmt = v => String(Math.round(Number(v)));
  const valueLabel = `${fmt(value[0])}...${fmt(value[1])}`;

  return React.createElement(
    'div',
    { style: { margin: (window.innerWidth <= 768) ? '0 4px' : '0 8px' } },
    [
      React.createElement('span', {
        style: { fontSize: '10px', whiteSpace: 'nowrap', display: 'block', lineHeight: 1.2 }
      }, valueLabel),
      React.createElement(
        MaterialUI.Slider,
        {
          value: value,
          min: sliderMeta.range[0],
          max: sliderMeta.range[1],
          step: sliderMeta.step,
          size: 'small',
          valueLabelDisplay: 'off',
          onChange: (e, val) => column.setFilter(val),
          'aria-label': `Filter ${column.name}`
        }
      )
    ]
  );
};

window.filterRange = (rows, columnId, filterValue) => {
  if (!Array.isArray(filterValue) || filterValue.length !== 2) return rows;
  const min = Number(filterValue[0]);
  const max = Number(filterValue[1]);
  if (!Number.isFinite(min) || !Number.isFinite(max)) return rows;
  return rows.filter(row => {
    const value = Number(row.values[columnId]);
    if (!Number.isFinite(value)) return true;
    return value >= min && value <= max;
  });
};

window.multiSelectFilter = (column, state) => {
  const values = React.useMemo(() => {
    const out = new Set();
    state.data.forEach(row => {
      const raw = row[column.id];
      if (raw === null || raw === undefined) return;
      const txt = String(raw).trim();
      if (!txt) return;
      out.add(txt);
    });
    const vals = Array.from(out);
    if (column && column.id === 'Player') {
      const pref = ['Fr', 'So', 'Jr', 'Sr', '1st Rder Avg', '2nd Rder Avg', 'D1 Avg'];
      const rank = Object.create(null);
      pref.forEach((v, i) => { rank[v] = i; });
      return vals.sort((a, b) => {
        const ai = Object.prototype.hasOwnProperty.call(rank, a) ? rank[a] : 999;
        const bi = Object.prototype.hasOwnProperty.call(rank, b) ? rank[b] : 999;
        if (ai !== bi) return ai - bi;
        return a.localeCompare(b);
      });
    }
    if (column && column.id === 'Role') {
      const pref = ['Pure PG', 'Scoring PG', 'Combo G', 'Wing G', 'Wing F', 'Stretch 4', 'PF/C', 'C'];
      const rank = Object.create(null);
      pref.forEach((v, i) => { rank[v] = i; });
      return vals.sort((a, b) => {
        const ai = Object.prototype.hasOwnProperty.call(rank, a) ? rank[a] : 999;
        const bi = Object.prototype.hasOwnProperty.call(rank, b) ? rank[b] : 999;
        if (ai !== bi) return ai - bi;
        return a.localeCompare(b);
      });
    }
    return vals.sort((a, b) => a.localeCompare(b));
  }, [state.data, column.id]);

  const selected = Array.isArray(column.filterValue)
    ? column.filterValue.map(x => String(x))
    : [];

  return React.createElement(
    'select',
    {
      multiple: true,
      value: selected,
      size: window.innerWidth <= 768 ? Math.min(3, Math.max(1, values.length)) : Math.min(6, Math.max(2, values.length)),
      style: { width: '100%', fontSize: '12px' },
      onChange: e => {
        const vals = Array.from(e.target.selectedOptions).map(opt => opt.value);
        column.setFilter(vals.length ? vals : undefined);
      }
    },
    values.map(v => React.createElement('option', { key: v, value: v }, v))
  );
};

window.filterMulti = (rows, columnId, filterValue) => {
  if (!Array.isArray(filterValue) || filterValue.length === 0) return rows;
  const keep = new Set(filterValue.map(x => String(x)));
  return rows.filter(row => keep.has(String(row.values[columnId])));
};

window.extractReactableFilters = function(filters) {
  if (!filters) return [];
  if (Array.isArray(filters)) {
    return filters
      .map(function(f) {
        if (!f) return null;
        if (typeof f === 'object' && Object.prototype.hasOwnProperty.call(f, 'id')) {
          return { id: String(f.id), value: f.value };
        }
        if (typeof f === 'object') {
          var keys = Object.keys(f);
          if (keys.length === 1) {
            return { id: String(keys[0]), value: f[keys[0]] };
          }
        }
        return null;
      })
      .filter(Boolean);
  }
  if (typeof filters === 'object') {
    if (Array.isArray(filters.id) && Array.isArray(filters.value)) {
      const n = Math.min(filters.id.length, filters.value.length);
      const out = [];
      for (let i = 0; i < n; i++) {
        const id = filters.id[i];
        if (id === null || id === undefined || String(id).length === 0) continue;
        out.push({ id: String(id), value: filters.value[i] });
      }
      return out;
    }
    if (
      Object.prototype.hasOwnProperty.call(filters, 'id') &&
      Object.prototype.hasOwnProperty.call(filters, 'value') &&
      !Array.isArray(filters.id)
    ) {
      if (filters.id === null || filters.id === undefined || String(filters.id).length === 0) {
        return [];
      }
      return [{ id: String(filters.id), value: filters.value }];
    }
    return Object.keys(filters).map(function(k) {
      return { id: String(k), value: filters[k] };
    });
  }
  return [];
};

window.normalizeReactableFilterValue = function(v) {
  if (Array.isArray(v)) return v;
  if (v === null || v === undefined) return v;
  if (typeof v === 'object') {
    var keys = Object.keys(v);
    if (keys.length === 0) return undefined;
    var isNumericKeys = keys.every(function(k) { return /^\\d+$/.test(String(k)); });
    if (isNumericKeys) {
      return keys
        .sort(function(a, b) { return Number(a) - Number(b); })
        .map(function(k) { return v[k]; });
    }
  }
  return v;
};

Shiny.addCustomMessageHandler('radar_reset_reactable_filters', function(msg) {
  const tableId = msg.table_id || 'radarPlayerTable';
  if (!(window.Reactable && typeof window.Reactable.setFilter === 'function')) return;
  const validCols = Array.isArray(msg.valid_columns)
    ? new Set(msg.valid_columns.map(function(x) { return String(x); }))
    : null;

  let cols = [];
  if (window.Reactable && typeof window.Reactable.getState === 'function') {
    try {
      const st = window.Reactable.getState(tableId) || {};
      cols = window.extractReactableFilters(st.filters).map(x => x.id);
    } catch (e) {}
  }

  if ((!cols || cols.length === 0) && Array.isArray(msg.clear_columns)) {
    cols = cols.concat(msg.clear_columns);
  }
  if ((!cols || cols.length === 0) && Array.isArray(msg.numeric_columns)) {
    cols = cols.concat(msg.numeric_columns);
  }

  cols = Array.from(new Set(cols.filter(Boolean)));
  if (validCols) {
    cols = cols.filter(function(col) { return validCols.has(String(col)); });
  }
  cols.forEach(function(col) {
    try { window.Reactable.setFilter(tableId, col, undefined); } catch (e) {}
  });

  if (window.Reactable && typeof window.Reactable.setSearch === 'function') {
    try { window.Reactable.setSearch(tableId, ''); } catch (e) {}
  }

  if (msg.year_default !== undefined && msg.year_default !== null && String(msg.year_default).length > 0) {
    if (!validCols || validCols.has('Year')) {
      try { window.Reactable.setFilter(tableId, 'Year', [String(msg.year_default)]); } catch (e) {}
    }
  }

  if (window.Reactable && typeof window.Reactable.gotoPage === 'function') {
    try { window.Reactable.gotoPage(tableId, 0); } catch (e) {}
  }
});
"
# -----------------------------------------------------------------------------

shinyUI(
  navbarPage(
    "NCAA Scouting App",
    id = "main_nav",
    fluid = TRUE,
    header = tagList(
      muiDependency(),
      tags$div(
        id = "dark-mode-toggle-wrap",
        tags$label(
          style = "margin:0; display:flex; align-items:center; gap:6px; cursor:pointer;",
          tags$input(id = "dark_mode_toggle", type = "checkbox"),
          "Dark mode"
        )
      ),
      tags$head(
        # App-wide light/dark mode styling
        tags$style(HTML("
    :root {
      --app-bg: #f8f8ff;
      --app-text: #222222;
      --muted-text: #444444;
      --panel-bg: #ffffff;
      --border-col: #e5e7eb;
    }
    html.dark-mode, body.dark-mode {
      --app-bg: #0f172a;
      --app-text: #e5e7eb;
      --muted-text: #cbd5e1;
      --panel-bg: #111827;
      --border-col: #334155;
      --panel-soft: #1f2937;
    }

    html, body { background-color: var(--app-bg) !important; color: var(--app-text) !important; }
    .tab-content { background: transparent !important; }
    .navbar-default { background-color: var(--panel-bg) !important; border-color: var(--border-col) !important; }
    .navbar-default .navbar-brand, .navbar-default .navbar-nav > li > a { color: var(--app-text) !important; }
    .navbar-default .navbar-nav > .active > a,
    .navbar-default .navbar-nav > .active > a:focus,
    .navbar-default .navbar-nav > .active > a:hover { background-color: var(--app-bg) !important; color: var(--app-text) !important; }
    .form-control, .selectize-input, .selectize-dropdown, .well {
      background-color: var(--panel-bg) !important;
      color: var(--app-text) !important;
      border-color: var(--border-col) !important;
    }
    html.dark-mode .btn-default,
    html.dark-mode .btn,
    html.dark-mode .dropdown-menu,
    html.dark-mode .selectize-dropdown-content,
    html.dark-mode .irs--shiny .irs-bar,
    html.dark-mode .irs--shiny .irs-line {
      background-color: var(--panel-soft) !important;
      color: var(--app-text) !important;
      border-color: var(--border-col) !important;
    }
    html.dark-mode .modal-content {
      background-color: var(--panel-bg) !important;
      color: var(--app-text) !important;
      border-color: var(--border-col) !important;
    }
    html.dark-mode .modal-header,
    html.dark-mode .modal-footer {
      border-color: var(--border-col) !important;
    }
    html.dark-mode .modal-title,
    html.dark-mode .modal-body,
    html.dark-mode .modal-body dt,
    html.dark-mode .modal-body dd,
    html.dark-mode .modal-body p,
    html.dark-mode .modal-body h5 {
      color: var(--app-text) !important;
    }
    html.dark-mode .modal-body hr {
      border-color: var(--border-col) !important;
    }
    .selectize-dropdown {
      max-height: 300px !important;
      overflow-y: auto !important;
    }
    .selectize-input input {
      font-size: 16px !important;
    }
    .radio label, .control-label, .help-block, h1, h2, h3, h4, h5, h6, label, p, span, div {
      color: var(--app-text);
    }

    .container, .container-fluid { max-width: 100% !important; }
    @media (min-width: 1200px) {
      .container-fluid { padding-left: 24px; padding-right: 24px; }
    }

    .help-block {
      color: var(--muted-text) !important;
      font-size: 0.9em;
      white-space: normal;
    }

    .radar-header-tooltip-wrap {
      display: inline-flex;
      align-items: center;
      gap: 6px;
      position: relative;
    }
    .radar-header-tooltip-label {
      line-height: 1.2;
      flex: 0 1 auto;
      min-width: 0;
    }
    .radar-header-tooltip-btn {
      flex: 0 0 auto;
      appearance: none;
      border: 1px solid var(--border-col);
      background: var(--panel-bg);
      color: var(--app-text);
      border-radius: 999px;
      width: 16px;
      height: 16px;
      padding: 0;
      display: inline-flex;
      align-items: center;
      justify-content: center;
      font-size: 10px;
      font-weight: 700;
      line-height: 1;
      cursor: pointer;
      position: relative;
    }
    .radar-header-tooltip-btn:focus-visible {
      outline: 2px solid #2563eb;
      outline-offset: 1px;
    }
    .radar-header-tooltip-popup {
      position: absolute;
      left: 50%;
      top: calc(100% + 8px);
      transform: translateX(-50%);
      width: min(260px, 70vw);
      padding: 8px 10px;
      border-radius: 8px;
      background: rgba(15, 23, 42, 0.96);
      color: #f8fafc;
      text-align: left;
      font-size: 12px;
      font-weight: 400;
      line-height: 1.35;
      white-space: normal;
      box-shadow: 0 10px 28px rgba(15, 23, 42, 0.28);
      z-index: 50;
    }
    .radar-header-tooltip-popup[hidden] {
      display: none !important;
    }
    @media (max-width: 768px) {
      .radar-header-tooltip-popup {
        left: auto;
        right: 0;
        transform: none;
        width: min(220px, 78vw);
      }
    }

    #advanced_stats_table,
    #advanced_stats_table .gt_table {
      background: var(--panel-bg) !important;
      color: var(--app-text) !important;
    }
    html.dark-mode .gt_table,
    html.dark-mode .gt_heading,
    html.dark-mode .gt_title,
    html.dark-mode .gt_subtitle,
    html.dark-mode .gt_col_headings,
    html.dark-mode .gt_col_heading,
    html.dark-mode .gt_stub,
    html.dark-mode .gt_summary_row,
    html.dark-mode .gt_footnotes,
    html.dark-mode .gt_source_notes {
      background-color: var(--panel-bg) !important;
      color: var(--app-text) !important;
      border-color: var(--border-col) !important;
    }
    /* Keep row/cell backgrounds untouched so gt heatmap shading still shows */
    html.dark-mode .gt_row,
    html.dark-mode .gt_row .gt_left,
    html.dark-mode .gt_row .gt_right,
    html.dark-mode .gt_row .gt_center {
      color: var(--app-text) !important;
      border-color: var(--border-col) !important;
    }
    /* Heatmap-colored gt cells: force dark foreground for readability */
    html.dark-mode .gt_table td[style*='background-color'],
    html.dark-mode .gt_table td[style*='background:'] {
      color: #0b1220 !important;
      font-weight: 600 !important;
    }

    #playerProfiles .well {
      max-width: 1000px;
      margin: 16px auto 0;
    }

    .label-success {
      background-color: #28a745;
      color: floralwhite;
      padding: 5px 10px;
      margin: 3px;
      display: inline-block;
      border-radius: 5px;
    }
    .label-danger {
      background-color: #dc3545;
      color: floralwhite;
      padding: 5px 10px;
      margin: 3px;
      display: inline-block;
      border-radius: 5px;
    }

    .reactable { background: var(--panel-bg) !important; color: var(--app-text) !important; }
    html.dark-mode .reactable .rt-table,
    html.dark-mode .reactable .rt-thead,
    html.dark-mode .reactable .rt-tr,
    html.dark-mode .reactable .rt-th,
    html.dark-mode .reactable .rt-pagination {
      background-color: var(--panel-bg) !important;
      color: var(--app-text) !important;
      border-color: var(--border-col) !important;
    }
    /* Don't override data-cell backgrounds; allow reactable inline heat shading */
    html.dark-mode .reactable .rt-td {
      color: var(--app-text) !important;
      border-color: var(--border-col) !important;
    }
    /* Keep darker text on heat-shaded cells in radar table */
    html.dark-mode .reactable .rt-td[style*='background'],
    html.dark-mode .reactable .rt-td[style*='background-color'] {
      color: #0b1220 !important;
      font-weight: 700 !important;
    }
    html.dark-mode .reactable .rt-td[style*='background'] *,
    html.dark-mode .reactable .rt-td[style*='background-color'] * {
      color: #0b1220 !important;
      font-weight: 700 !important;
      text-shadow: none !important;
    }
    html.dark-mode .reactable input,
    html.dark-mode .reactable select,
    html.dark-mode .reactable button {
      background-color: var(--panel-soft) !important;
      color: var(--app-text) !important;
      border-color: var(--border-col) !important;
    }

    /* Plotly containers and SVG text in dark mode */
    html.dark-mode .js-plotly-plot,
    html.dark-mode .plotly,
    html.dark-mode .plot-container,
    html.dark-mode .svg-container {
      background-color: var(--panel-bg) !important;
    }
    html.dark-mode #radarPlot {
      background-color: var(--panel-bg) !important;
      border: 1px solid var(--border-col);
      border-radius: 8px;
    }
    html.dark-mode #radarPlot svg text {
      fill: var(--app-text) !important;
    }

    #dark-mode-toggle-wrap {
      position: fixed;
      top: 10px;
      right: 16px;
      z-index: 2000;
      background: var(--panel-bg);
      color: var(--app-text);
      border: 1px solid var(--border-col);
      border-radius: 999px;
      padding: 6px 10px;
      font-size: 12px;
    }
    #dark-mode-toggle-wrap input { margin-right: 6px; }

    /* --- Radar chart responsive layout --- */
    .radar-plot-outer {
      overflow-x: hidden !important;
      width: 100% !important;
      max-width: 100% !important;
      display: flex;
      justify-content: center;
    }

    .radar-plot-wrap {
      width: 100% !important;
      max-width: 1000px;
      margin: 0 auto;
    }

    /* Base radar height (JS sets --radarPlotVh based on aspect ratio) */
    #radarPlot {
      height: calc(var(--radarPlotVh, 72) * 1vh) !important;
      min-height: 480px !important;
    }

    @media (max-width: 768px) {
      /* Give the radar compact vertical space on mobile (JS sets --radarPlotVh) */
      #radarPlot {
        height: calc(var(--radarPlotVh, 72) * 1vh) !important;
        min-height: 300px !important;
      }

      /* Font sizes for the radar are controlled via Plotly layout (is_narrow branch in server.R) */
    }

    /* --- Reactable table density --- */
    #radarPlayerTable .rt-table {
      font-size: 12px;
    }
    #radarPlayerTable .rt-td {
      padding: 3px 5px !important;
    }
    /* Reduce filter-row cell height — MUI sliders are the tallest element */
    #radarPlayerTable .rt-thead .rt-th {
      padding: 2px 4px !important;
    }
    @media (max-width: 768px) {
      #radarPlayerTable .rt-table { font-size: 9px; }
      #radarPlayerTable .rt-th,
      #radarPlayerTable .rt-td { font-size: 9px; }
      /* Stretch table to near-full width on mobile */
      #radarTableContainer { margin-left: 4px !important; margin-right: 4px !important; }
      /* Tighter row padding on mobile */
      #radarPlayerTable .rt-td { padding: 2px 3px !important; }
      /* Override both the outer widget div and the inner React container */
      #radarPlayerTable,
      #radarPlayerTable .Reactable {
        height: 625px !important;
      }
      /* Touch targets: bump small buttons/inputs to 36px min */
      .btn-sm {
        min-height: 36px;
        padding: 6px 10px;
        font-size: 12px;
      }
      .selectize-input {
        min-height: 36px;
      }
      /* Compact pagination row */
      .rt-pagination {
        font-size: 11px;
        flex-wrap: wrap;
        gap: 2px;
      }
      .rt-pagination-nav button {
        min-width: 32px;
        min-height: 32px;
      }
    }
    /* Portrait phones: derive chart height from viewport width so polar circle fills the space.
       100vw - 90px (Bootstrap + Plotly l/r padding) + 125px (Plotly t+b margins) = 100vw + 35px */
    @media (max-width: 768px) and (orientation: portrait) {
      #radarPlot {
        /* Square chart: viewport width minus Bootstrap side gutters (~30px) */
        height: calc(100vw - 30px) !important;
        min-height: 300px !important;
        max-height: 520px !important;
      }
    }
    /* Prevent iOS font inflation on orientation change */
    html { -webkit-text-size-adjust: 100%; text-size-adjust: 100%; }
    /* Push right panel down to align with the reactable on desktop */
    @media (min-width: 769px) {
      .right-panel-offset { margin-top: 370px; }
    }
    /* Save-photo modal */
    #radarPhotoModal {
      display: none;
      position: fixed;
      inset: 0;
      background: rgba(0,0,0,0.88);
      z-index: 9999;
      align-items: center;
      justify-content: center;
      flex-direction: column;
      padding: 16px;
    }
    #radarPhotoModal.open { display: flex; }
    #radarPhotoModalImg {
      max-width: 95vw;
      max-height: 75vh;
      border-radius: 8px;
      box-shadow: 0 4px 32px rgba(0,0,0,0.6);
      -webkit-user-select: none;
      user-select: none;
    }
    #radarPhotoModalHint {
      color: #e2e8f0;
      margin-top: 14px;
      font-size: 14px;
      text-align: center;
    }
    #radarPhotoModalClose {
      margin-top: 12px;
      padding: 8px 24px;
      border-radius: 6px;
      border: none;
      background: #475569;
      color: white;
      font-size: 14px;
      cursor: pointer;
    }
  ")),
        tags$script(HTML(mui_filter_js)),
        tags$script(HTML("
(function() {
  function setShinyInputValue(name, value, opts) {
    if (!window.Shiny) return;
    if (typeof Shiny.setInputValue === 'function') {
      Shiny.setInputValue(name, value, opts || {priority: 'event'});
    } else if (typeof Shiny.onInputChange === 'function') {
      Shiny.onInputChange(name, value);
    }
  }
  document.addEventListener('click', function(e) {
    var target = e.target;
    if (target && target.closest && target.closest('.radar-header-tooltip-wrap')) {
      return;
    }
    document.querySelectorAll('.radar-header-tooltip-btn[aria-expanded=\"true\"]').forEach(function(btn) {
      btn.setAttribute('aria-expanded', 'false');
      var popup = btn.parentElement && btn.parentElement.querySelector('.radar-header-tooltip-popup');
      if (popup) popup.hidden = true;
    });
  });

  document.addEventListener('keydown', function(e) {
    if (e.key !== 'Escape') return;
    document.querySelectorAll('.radar-header-tooltip-btn[aria-expanded=\"true\"]').forEach(function(btn) {
      btn.setAttribute('aria-expanded', 'false');
      var popup = btn.parentElement && btn.parentElement.querySelector('.radar-header-tooltip-popup');
      if (popup) popup.hidden = true;
    });
  });

  function relayoutPlotlyForTheme(isDark) {
    if (!window.Plotly || !window.Plotly.relayout) return;
    // Keep radarPlot layout untouched so custom formatting is preserved.
    var ids = [];
    var layoutUpdate = isDark ? {
      paper_bgcolor: '#111827',
      plot_bgcolor: '#111827',
      'font.color': '#e5e7eb',
      'legend.bgcolor': 'rgba(17,24,39,0.65)',
      'legend.font.color': '#e5e7eb',
      'xaxis.gridcolor': '#334155',
      'xaxis.linecolor': '#475569',
      'xaxis.tickfont.color': '#e5e7eb',
      'xaxis.title.font.color': '#e5e7eb',
      'yaxis.gridcolor': '#334155',
      'yaxis.linecolor': '#475569',
      'yaxis.tickfont.color': '#e5e7eb',
      'yaxis.title.font.color': '#e5e7eb',
      'polar.bgcolor': '#111827',
      'polar.angularaxis.gridcolor': '#334155',
      'polar.angularaxis.linecolor': '#475569',
      'polar.angularaxis.tickfont.color': '#e5e7eb',
      'polar.radialaxis.gridcolor': '#334155',
      'polar.radialaxis.linecolor': '#475569',
      'polar.radialaxis.tickfont.color': '#e5e7eb'
    } : {
      paper_bgcolor: '#ffffff',
      plot_bgcolor: '#ffffff',
      'font.color': '#222222',
      'legend.bgcolor': 'rgba(255,255,255,0.65)',
      'legend.font.color': '#222222',
      'xaxis.gridcolor': '#d1d5db',
      'xaxis.linecolor': '#9ca3af',
      'xaxis.tickfont.color': '#222222',
      'xaxis.title.font.color': '#222222',
      'yaxis.gridcolor': '#d1d5db',
      'yaxis.linecolor': '#9ca3af',
      'yaxis.tickfont.color': '#222222',
      'yaxis.title.font.color': '#222222',
      'polar.bgcolor': '#ffffff',
      'polar.angularaxis.gridcolor': '#d1d5db',
      'polar.angularaxis.linecolor': '#9ca3af',
      'polar.angularaxis.tickfont.color': '#222222',
      'polar.radialaxis.gridcolor': '#d1d5db',
      'polar.radialaxis.linecolor': '#9ca3af',
      'polar.radialaxis.tickfont.color': '#222222'
    };
    ids.forEach(function(id) {
      var el = document.getElementById(id);
      if (!el) return;
      try { Plotly.relayout(el, layoutUpdate); } catch (e) {}
    });
  }
  function applyDarkMode(on) {
    var html = document.documentElement;
    var body = document.body;
    if (on) {
      html.classList.add('dark-mode');
      body.classList.add('dark-mode');
    } else {
      html.classList.remove('dark-mode');
      body.classList.remove('dark-mode');
    }
    try { localStorage.setItem('radar_dark_mode', on ? '1' : '0'); } catch (e) {}
    setShinyInputValue('dark_mode', on, {priority: 'event'});
    // Plotly outputs may render asynchronously; relayout twice.
    setTimeout(function() { relayoutPlotlyForTheme(on); }, 120);
    setTimeout(function() { relayoutPlotlyForTheme(on); }, 600);
  }
  var _darkModeInited = false;
  function initDarkMode() {
    var chk = document.getElementById('dark_mode_toggle');
    if (!chk) return;
    var saved = null;
    try { saved = localStorage.getItem('radar_dark_mode'); } catch (e) {}
    var isOn = (saved !== '0');  // default ON; only off if user explicitly unchecked
    chk.checked = isOn;
    applyDarkMode(isOn);
    if (!_darkModeInited) {
      _darkModeInited = true;
      chk.addEventListener('change', function() { applyDarkMode(!!chk.checked); });
      document.addEventListener('shown.bs.tab', function() {
        var c = document.getElementById('dark_mode_toggle');
        var on = !!(c && c.checked);
        setTimeout(function() { relayoutPlotlyForTheme(on); }, 120);
        setTimeout(function() { relayoutPlotlyForTheme(on); }, 600);
      });
    }
  }
  // Apply CSS/checkbox state immediately so the page renders in dark mode
  // before Shiny's WebSocket session is established.
  window.addEventListener('load', function() {
    var saved = null;
    try { saved = localStorage.getItem('radar_dark_mode'); } catch(e) {}
    if (saved !== '0') {  // default ON; only skip if user explicitly unchecked
      document.documentElement.classList.add('dark-mode');
      document.body.classList.add('dark-mode');
      var chk = document.getElementById('dark_mode_toggle');
      if (chk) chk.checked = true;
    }
  });
  // Send dark_mode to Shiny only after the session is connected so the server
  // receives the correct value before its initial renderPlotly evaluation.
  $(document).on('shiny:connected', initDarkMode);
})();
")),
        tags$script(HTML("
(function () {
  window.__watchlistPids = [];

  $(document).on('shiny:connected', function () {
    var stored = [];
    try { stored = JSON.parse(localStorage.getItem('watchlist_pids') || '[]'); } catch (e) {}
    if (!Array.isArray(stored)) stored = [];
    window.__watchlistPids = stored;
    Shiny.setInputValue('watchlist_init', stored, { priority: 'event' });
  });

  Shiny.addCustomMessageHandler('watchlist_update', function (msg) {
    window.__watchlistPids = Array.isArray(msg.pids) ? msg.pids : [];
    try { localStorage.setItem('watchlist_pids', JSON.stringify(window.__watchlistPids)); } catch (e) {}
    document.querySelectorAll('[data-watchlist-pid]').forEach(function (el) {
      var pid = el.getAttribute('data-watchlist-pid');
      var on  = window.__watchlistPids.indexOf(pid) >= 0;
      el.textContent = on ? '\u2605' : '\u2606';
      el.style.color = on ? '#f59e0b' : '#94a3b8';
      el.title       = on ? 'Remove from watchlist' : 'Add to watchlist';
    });
  });

  // Event delegation: catch star button clicks without inline onclick (avoids HTML quote escaping)
  document.addEventListener('click', function (e) {
    var btn = e.target && e.target.closest ? e.target.closest('[data-watchlist-pid]') : null;
    if (!btn) return;
    var pid = btn.getAttribute('data-watchlist-pid');
    if (!pid || !window.Shiny || typeof Shiny.setInputValue !== 'function') return;
    Shiny.setInputValue('watchlist_star_click', { pid: pid, ts: Date.now() }, { priority: 'event' });
  });
})();
")),
        tags$script(HTML("
(function() {
  var last = null;
  function setShinyInputValue(name, value, opts) {
    if (!window.Shiny) return;
    if (typeof Shiny.setInputValue === 'function') {
      Shiny.setInputValue(name, value, opts || {priority: 'event'});
    } else if (typeof Shiny.onInputChange === 'function') {
      Shiny.onInputChange(name, value);
    }
  }
  function compute() {
    var w = document.documentElement.clientWidth || window.innerWidth || 9999;
    var h = document.documentElement.clientHeight || window.innerHeight || 9999;
    var isMobile = (w <= 768);
    var aspect = w / Math.max(h, 1);

    // Height in viewport units (vh) tuned by aspect ratio.
    // - Tall screens (portrait / iPad split view): give more height.
    // - Very wide screens: slightly less height.
    var vh;
    if (isMobile) {
      vh = (aspect < 0.70) ? 72 : 68;
    } else {
      if (aspect >= 1.80) vh = 68;       // very wide desktop
      else if (aspect >= 1.45) vh = 72;  // normal desktop
      else vh = 78;                      // taller / narrow windows
    }

    // Push to CSS so Plotly container grows/shrinks without hardcoding in plotlyOutput()
    document.documentElement.style.setProperty('--radarPlotVh', String(vh));

    // Only treat is_mobile as event-driven; height can update every resize
    if (last !== isMobile) {
      last = isMobile;
      setShinyInputValue('is_mobile', isMobile, {priority: 'event'});
    }

    // Optional: expose for debugging
    setShinyInputValue('radar_plot_vh', vh, {priority: 'event'});
  }

  window.addEventListener('load', compute);

  window.addEventListener('orientationchange', function() {
    setTimeout(compute, 200);
  });

  var t = null;
  window.addEventListener('resize', function() {
    clearTimeout(t);
    t = setTimeout(compute, 400);
  });
})();
")),
        tags$script(HTML("
$(document).on('shiny:disconnected', function() {
  setTimeout(function() { location.reload(); }, 2000);
});
"))
      )
    ),

    # ---- Tab 1: Radar Chart ----
    tabPanel(
      "Radar tab",
      fluidPage(
        titlePanel("Player Radar Chart Comparison"),
        fluidRow(
          column(
            width = 7,
            selectizeInput("year_filter", "Year", choices = ui_year_choices, selected = head(ui_year_choices, 1), multiple = TRUE),
            tags$script(HTML("
(function() {
  // Fire year_filter to Shiny only when the dropdown closes or a chip is
  // removed — not on every individual click within a multi-select session.
  // This prevents the reactive cascade from running on each intermediate
  // selection state, which causes OOM on memory-constrained hosts.
  $(document).on('shiny:bound', function(e) {
    if (e.target.id !== 'year_filter') return;
    var sel = e.target.selectize;
    if (!sel) return;

    // Remove Shiny's per-change jQuery listener on the underlying <select>.
    $(e.target).off('change');

    // Fire once when the user closes the dropdown (finished adding years).
    sel.on('dropdown_close', function() {
      Shiny.setInputValue('year_filter', sel.getValue(), {priority: 'event'});
    });

    // Fire on chip removal (x button — no dropdown opens for removes).
    sel.on('item_remove', function() {
      clearTimeout(sel._yearFireTimer);
      sel._yearFireTimer = setTimeout(function() {
        Shiny.setInputValue('year_filter', sel.getValue(), {priority: 'event'});
      }, 200);
    });
  });
})();
")),
            selectizeInput("players", "Select players",
              choices = character(0), selected = character(0), multiple = TRUE,
              options = list(
                placeholder = "Selected player(s)",
                maxOptions = 1000,
                closeAfterSelect = TRUE,
                plugins = list("remove_button")
              )
            ),
            helpText("Tip: Hold Ctrl/Cmd to select multiple players (max 3)."),
            tags$hr(),
            div(
              style = "display:flex; align-items:center; justify-content:space-between; gap:8px; flex-wrap:wrap;",
              h4("Players (Composite Percentiles)", style = "margin: 0;"),
              div(
                style = "display:flex; align-items:center; gap:6px; flex-wrap:wrap;",
                actionButton(
                  inputId = "radar_reset_players",
                  label   = "Clear selection(s)",
                  class   = "btn btn-default btn-sm"
                ),
                actionButton(
                  inputId = "radar_reset_filters",
                  label   = "Reset filters",
                  class   = "btn btn-default btn-sm"
                ),
                actionButton(
                  inputId = "radar_glossary_btn",
                  label   = "Column glossary",
                  class   = "btn btn-default btn-sm"
                )
              )
            ),
            tags$div(
              style = "margin-left: 16px; margin-right: 16px; margin-bottom: 6px;",
              tags$small(
                style = "color: var(--muted-text);",
                tags$b("How to use:"),
                " Click a row to add that player to the radar plot (up to 3 players).",
                " Use column filters to narrow the list.",
                " Use Reset Filters to clear table filters."
              )
            ),
            div(
              style = "margin-left: 16px; margin-right: 16px; margin-bottom: 6px; display: flex; gap: 24px; align-items: center;",
              checkboxInput("portal_only", "Portal Players Only", value = FALSE),
              checkboxInput("uncommitted_only", "Uncommitted Only", value = FALSE),
              checkboxInput("watchlist_only", "Watchlist Only \u2605", value = FALSE)
            ),
            div(
              id = "radarTableContainer",
              style = "margin-left: 16px; margin-right: 16px;",
              shinycssloaders::withSpinner(
                reactable::reactableOutput("radarPlayerTable", height = "650px"),
                type = 8,
                size = 1,
                caption = "Loading data"
              )
            ),
            tags$hr(),
            radioButtons(
              "view_mode", "Radar View:",
              choices = c("Composite Metrics", "Individual Stats"),
              selected = "Composite Metrics"
            ),
            conditionalPanel(
              condition = "input.view_mode == 'Composite Metrics'",
              div(
                style = "display:flex; gap:16px; margin-top:4px; margin-bottom:4px;",
                checkboxInput("include_ptir_axis", "Include PTIR Axis", value = TRUE),
                checkboxInput("include_edmr_axis", "Include EDMR Axis", value = TRUE)
              )
            ),
            conditionalPanel(
              condition = "input.view_mode == 'Individual Stats'",
              selectizeInput(
                "radar_vars_select",
                "Individual stats (radar axes)",
                choices = NULL,
                selected = NULL,
                multiple = TRUE,
                options = list(
                  placeholder = "Pick 6-12 stats (recommended)",
                  maxOptions = 5000,
                  closeAfterSelect = FALSE,
                  plugins = list("remove_button")
                )
              ),
              helpText("Tip: choose a smaller set of stats so labels are readable.")
            )
          ),
          column(
            width = 5,
            tags$div(
              class = "right-panel-offset",
              div(
                style = "margin-bottom: 6px; display: flex; gap: 6px; justify-content: flex-end;",
                tags$button(
                  id = "radarExportBtn",
                  class = "btn btn-default btn-sm",
                  onclick = "
                    var btn = this;
                    var el = document.getElementById('radarPlot');
                    if (el && window.Plotly) {
                      var orig = btn.textContent;
                      btn.disabled = true;
                      btn.textContent = 'Exporting...';
                      var w = Math.max(Math.round(el.offsetWidth) || 700, 680);
                      var h = Math.max(Math.round(el.offsetHeight) || 500, 480);
                      Plotly.downloadImage(el, {
                        format: 'png',
                        width: w,
                        height: h,
                        scale: 3,
                        filename: 'radar_export'
                      }).then(function() {
                        btn.disabled = false;
                        btn.textContent = orig;
                      }).catch(function() {
                        btn.disabled = false;
                        btn.textContent = orig;
                      });
                    }
                  ",
                  "\u2193 Export PNG"
                ),
                tags$button(
                  id = "radarSavePhotoBtn",
                  class = "btn btn-default btn-sm",
                  onclick = "
                    var btn = document.getElementById('radarSavePhotoBtn');
                    var el  = document.getElementById('radarPlot');
                    if (!el || !window.Plotly) return;
                    var orig = btn.textContent;
                    btn.disabled = true;
                    btn.textContent = 'Generating...';

                    // Capture the current live font sizes so we can restore them after export
                    var fl = el._fullLayout || {};
                    var liveAngSize = (fl.polar && fl.polar.angularaxis && fl.polar.angularaxis.tickfont)
                      ? (fl.polar.angularaxis.tickfont.size || 12) : 12;
                    var liveLegSize = (fl.legend && fl.legend.font)
                      ? (fl.legend.font.size || 17) : 17;

                    // Scale export fonts so text looks proportionally correct in the
                    // saved image. We take the smaller of:
                    //   - modalDisplayW: accounts for image being shrunk to fit the modal
                    //   - el.offsetWidth: accounts for the export canvas (700px) being
                    //     larger than the live chart, making text look smaller against
                    //     a proportionally bigger polar circle
                    // Using min() of both ensures we always scale UP enough for both effects.
                    var modalDisplayW = Math.min(700, window.innerWidth * 0.95);
                    var chartW        = el.offsetWidth || 700;
                    var divisor       = Math.min(chartW, modalDisplayW);
                    var exportAngSize = Math.round(liveAngSize * 700 / divisor);
                    var exportLegSize = Math.round(liveLegSize * 700 / divisor);

                    Plotly.relayout(el, {
                      'polar.angularaxis.tickfont.size': exportAngSize,
                      'legend.font.size': exportLegSize
                    })
                    .then(function() {
                      return Plotly.toImage(el, { format: 'png', width: 700, height: 700, scale: 1 });
                    })
                    .then(function(dataUrl) {
                      // Restore live-chart fonts before revealing the modal
                      return Plotly.relayout(el, {
                        'polar.angularaxis.tickfont.size': liveAngSize,
                        'legend.font.size': liveLegSize
                      }).then(function() { return dataUrl; });
                    })
                    .then(function(dataUrl) {
                      var modal = document.getElementById('radarPhotoModal');
                      var img   = document.getElementById('radarPhotoModalImg');
                      img.src = dataUrl;
                      modal.classList.add('open');
                      btn.disabled = false;
                      btn.textContent = orig;
                    })
                    .catch(function() {
                      // Restore even on error
                      Plotly.relayout(el, {
                        'polar.angularaxis.tickfont.size': liveAngSize,
                        'legend.font.size': liveLegSize
                      });
                      btn.disabled = false;
                      btn.textContent = orig;
                    });
                  ",
                  "\ud83d\udcf1 Save photo"
                )
              ),
              tags$div(
                class = "radar-plot-outer",
                tags$div(
                  class = "radar-plot-wrap",
                  shinycssloaders::withSpinner(
                    plotly::plotlyOutput("radarPlot", height = "100%"),
                    type = 8,
                    size = 1,
                    caption = "Loading data"
                  )
                )
              ),
              tags$div(
                style = "font-size: 0.88em;",
                uiOutput("playerProfiles"),
                uiOutput("careerSimilarity"),
                fluidRow(
                  column(width = 6, uiOutput("playerStrengths")),
                  column(width = 6, uiOutput("playerWeaknesses"))
                )
              )
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            tags$hr(),
            h4("Additional Filters"),
            selectizeInput("team_filter", "Team", choices = ui_team_choices, selected = NULL, multiple = TRUE),
            selectizeInput("role_filter", "Role", choices = ui_role_choices, selected = NULL, multiple = TRUE),
            sliderInput("prpg_filter", "PRPG Range", min = -2, max = 10, value = c(-2, 10), step = 0.1)
          )
        ),
        br(),
        data_updated_footer("data_updated_radar"),
        # Save-photo modal — shown over everything when user taps "Save photo"
        tags$div(
          id = "radarPhotoModal",
          tags$img(id = "radarPhotoModalImg", src = ""),
          tags$p(id = "radarPhotoModalHint", "Long-press the image and tap \u201cSave to Photos\u201d"),
          tags$button(
            id = "radarPhotoModalClose",
            onclick = "document.getElementById('radarPhotoModal').classList.remove('open');
                       document.getElementById('radarPhotoModalImg').src = '';",
            "Close"
          )
        )
      )
    )
  )
)
