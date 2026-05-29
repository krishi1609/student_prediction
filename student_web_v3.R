library(shiny)
library(shinyjs)
library(ggplot2)
library(DT)
library(DBI)
library(RMySQL)

# Load local environment variables from .env if present.
load_dot_env <- function(path = ".env") {
  if (!file.exists(path)) return(invisible(FALSE))

  lines <- readLines(path, warn = FALSE)
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]
  lines <- lines[!grepl("^#", lines)]

  for (line in lines) {
    if (!grepl("=", line, fixed = TRUE)) next
    parts <- strsplit(line, "=", fixed = TRUE)[[1]]
    key <- trimws(parts[[1]])
    value <- paste(parts[-1], collapse = "=")
    value <- trimws(value)
    value <- sub('^"(.*)"$', "\\1", value)
    value <- sub("^'(.*)'$", "\\1", value)
    if (nzchar(key)) Sys.setenv(setNames(value, key))
  }

  invisible(TRUE)
}

load_dot_env()

# ── DB helpers ────────────────────────────────────────────────────────────────
getConnection <- function() {
  dbConnect(
    RMySQL::MySQL(),
    host     = or_default(Sys.getenv("STUDENT_DB_HOST"), "127.0.0.1"),
    port     = as.integer(or_default(Sys.getenv("STUDENT_DB_PORT"), "3306")),
    dbname   = or_default(Sys.getenv("STUDENT_DB_NAME"), "student_db"),
    user     = or_default(Sys.getenv("STUDENT_DB_USER"), "root"),
    password = or_default(Sys.getenv("STUDENT_DB_PASSWORD"), "krishi1121")
  )
}

emptyStudents <- function() {
  data.frame(
    id                    = integer(0),
    study_hours           = numeric(0),
    attendance            = numeric(0),
    sleep_hours           = numeric(0),
    internet_usage        = numeric(0),
    assignments_completed = integer(0),
    previous_score        = numeric(0),
    exam_score            = numeric(0),
    placement_status      = character(0),
    stringsAsFactors      = FALSE
  )
}

fetchData <- function() {
  tryCatch({
    con  <- getConnection()
    on.exit(try(dbDisconnect(con), silent = TRUE))
    data <- dbGetQuery(con, "SELECT * FROM students ORDER BY id DESC")
    data$placement_status <- as.character(trimws(data$placement_status))
    data
  }, error = function(e) {
    message("[fetchData] DB error: ", e$message)
    emptyStudents()
  })
}

relationshipColumnChoices <- c(
  "Study Hours" = "study_hours",
  "Attendance (%)" = "attendance",
  "Sleep Hours" = "sleep_hours",
  "Internet Usage" = "internet_usage",
  "Assignments Completed" = "assignments_completed",
  "Previous Score" = "previous_score",
  "Exam Score" = "exam_score"
)

relationshipChartChoices <- c(
  "Scatter Plot" = "Scatter Plot",
  "Line Chart"   = "Line Chart",
  "Trend Line"   = "Trend Line",
  "Smooth Trend" = "Smooth Trend",
  "Bar Chart"    = "Bar Chart",
  "Area Chart"   = "Area Chart"
)

relationshipLabel <- function(value) {
  nm <- names(relationshipColumnChoices)[relationshipColumnChoices == value]
  if (length(nm) == 0) value else nm[[1]]
}

defaultCustomChart <- list(
  x = "attendance",
  y = "exam_score",
  type = "Scatter Plot",
  title = "Attendance vs Score"
)

or_default <- function(value, default) {
  if (is.null(value) || identical(value, "")) default else value
}

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  useShinyjs(),

  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&display=swap');
      *, *::before, *::after { box-sizing: border-box; margin: 0; padding: 0; }
      body { font-family: 'Inter', sans-serif; background: #f0f2f5; color: #1a1a2e; }

      .topnav {
        background: linear-gradient(135deg, #1a1a2e 0%, #16213e 100%);
        padding: 0 32px; height: 64px; display: flex; align-items: center;
        justify-content: space-between; box-shadow: 0 2px 12px rgba(0,0,0,.3);
        position: sticky; top: 0; z-index: 999;
      }
      .topnav-brand { color: #fff; font-size: 20px; font-weight: 700; display: flex; align-items: center; gap: 10px; }
      .topnav-brand span { color: #4ecca3; }
      .topnav-tabs { display: flex; gap: 4px; }
      .tab-btn {
        background: transparent; color: #94a3b8; border: none;
        padding: 8px 20px; border-radius: 6px; font-size: 14px; font-weight: 500;
        cursor: pointer; transition: all .2s; font-family: 'Inter', sans-serif;
      }
      .tab-btn:hover { background: rgba(255,255,255,.08); color: #fff; }
      .tab-btn.active-tab { background: #4ecca3; color: #1a1a2e; font-weight: 600; }
      .topnav-right { display: flex; align-items: center; gap: 12px; }
      .refresh-btn {
        background: rgba(78,204,163,.15); color: #4ecca3;
        border: 1.5px solid #4ecca3; padding: 7px 16px; border-radius: 7px;
        font-size: 13px; font-weight: 600; cursor: pointer;
        font-family: 'Inter', sans-serif; transition: all .2s;
      }
      .refresh-btn:hover { background: #4ecca3; color: #1a1a2e; }
      .last-updated { color: #64748b; font-size: 12px; white-space: nowrap; }

      .hero-banner {
        background: linear-gradient(135deg, #1a1a2e 0%, #0f3460 60%, #16213e 100%);
        border-radius: 16px; padding: 24px 36px; margin-bottom: 14px;
        position: relative; overflow: hidden;
      }
      .hero-banner::before {
        content: ''; position: absolute; top: -60px; right: -60px;
        width: 260px; height: 260px; border-radius: 50%;
        background: rgba(78,204,163,.08); pointer-events: none;
      }
      .hero-banner::after {
        content: ''; position: absolute; bottom: -80px; left: 30%;
        width: 200px; height: 200px; border-radius: 50%;
        background: rgba(59,130,246,.07); pointer-events: none;
      }
      .hero-top { display: flex; align-items: flex-start; justify-content: space-between; margin-bottom: 20px; }
      .hero-title { font-size: 22px; font-weight: 700; color: #fff; line-height: 1.2; margin-bottom: 4px; }
      .hero-title span { color: #4ecca3; }
      .hero-subtitle { font-size: 13px; color: #94a3b8; }
      .hero-badge {
        background: rgba(78,204,163,.15); border: 1px solid rgba(78,204,163,.35);
        color: #4ecca3; padding: 5px 12px; border-radius: 20px;
        font-size: 11px; font-weight: 600; letter-spacing: .5px; white-space: nowrap;
      }
      .hero-kpi-row { display: grid; grid-template-columns: repeat(4, 1fr); gap: 0; }
      .hero-kpi { padding: 0 24px 0 0; border-right: 1px solid rgba(255,255,255,.08); }
      .hero-kpi:last-child { border-right: none; padding-right: 0; }
      .hero-kpi:not(:first-child) { padding-left: 24px; }
      .kpi-icon { width: 30px; height: 30px; border-radius: 8px; display: flex; align-items: center; justify-content: center; margin-bottom: 6px; font-size: 15px; }
      .kpi-icon.blue   { background: rgba(59,130,246,.18);  color: #60a5fa; }
      .kpi-icon.green  { background: rgba(78,204,163,.18);  color: #4ecca3; }
      .kpi-icon.orange { background: rgba(245,158,11,.18);  color: #fbbf24; }
      .kpi-icon.purple { background: rgba(139,92,246,.18);  color: #a78bfa; }
      .kpi-number { font-size: 28px; font-weight: 700; color: #fff; line-height: 1; margin-bottom: 3px; min-height: 34px; }
      .kpi-label  { font-size: 11px; color: #64748b; font-weight: 500; text-transform: uppercase; letter-spacing: .8px; margin-bottom: 4px; }
      .kpi-trend  { font-size: 11px; font-weight: 600; display: inline-flex; align-items: center; gap: 3px; padding: 2px 7px; border-radius: 10px; }
      .kpi-trend.up   { background: rgba(78,204,163,.15);  color: #4ecca3; }
      .kpi-trend.info { background: rgba(59,130,246,.15);  color: #60a5fa; }
      @keyframes fadeSlideUp { from { opacity: 0; transform: translateY(16px); } to { opacity: 1; transform: translateY(0); } }
      .hero-kpi { animation: fadeSlideUp .5s ease both; }
      .hero-kpi:nth-child(1) { animation-delay: .05s; }
      .hero-kpi:nth-child(2) { animation-delay: .15s; }
      .hero-kpi:nth-child(3) { animation-delay: .25s; }
      .hero-kpi:nth-child(4) { animation-delay: .35s; }

      .page-wrap { max-width: 1280px; margin: 20px auto; padding: 0 20px; }

      .filter-bar {
        background: #fff; border-radius: 12px; padding: 12px 20px; margin-bottom: 14px;
        box-shadow: 0 1px 4px rgba(0,0,0,.07);
        display: flex; align-items: center; gap: 16px; flex-wrap: wrap;
      }
      .filter-bar label { font-size: 13px; font-weight: 600; color: #64748b; white-space: nowrap; }
      .filter-bar select {
        border: 1.5px solid #e2e8f0; border-radius: 8px; padding: 7px 12px;
        font-size: 14px; font-family: 'Inter', sans-serif; color: #1a1a2e;
        background: #f8fafc; cursor: pointer; outline: none;
      }
      .filter-bar select:focus { border-color: #4ecca3; }
      .active-filter-pill {
        display: inline-flex; align-items: center; gap: 6px;
        background: #f0fdf4; border: 1.5px solid #4ecca3; color: #166534;
        padding: 5px 12px; border-radius: 20px; font-size: 13px; font-weight: 600;
        animation: fadeSlideUp .3s ease;
      }
      .clear-filter-btn {
        background: none; border: none; cursor: pointer; color: #4ecca3;
        font-size: 16px; line-height: 1; padding: 0; font-weight: 700;
        font-family: 'Inter', sans-serif;
      }
      .clear-filter-btn:hover { color: #dc2626; }

      .chart-row { display: grid; grid-template-columns: 1fr 1fr; gap: 16px; margin-bottom: 16px; }
      .chart-card {
        background: #fff; border-radius: 14px; padding: 18px 20px;
        box-shadow: 0 1px 4px rgba(0,0,0,.07);
        transition: box-shadow .2s, border-color .2s;
        border: 2px solid transparent; display: flex; flex-direction: column;
      }
      .chart-card.clickable { cursor: pointer; }
      .chart-card.clickable:hover { box-shadow: 0 4px 16px rgba(78,204,163,.18); border-color: #b2f0dc; }
      .chart-card.chart-active { border-color: #4ecca3 !important; box-shadow: 0 4px 20px rgba(78,204,163,.25) !important; }
      .chart-card.full { grid-column: 1 / -1; }
      .chart-header { display: flex; align-items: center; justify-content: space-between; padding-bottom: 10px; border-bottom: 1px solid #f1f5f9; margin-bottom: 4px; }
      .chart-title  { font-size: 15px; font-weight: 600; color: #1a1a2e; }
      .chart-type-select {
        border: 1.5px solid #e2e8f0; border-radius: 7px; padding: 4px 10px;
        font-size: 12px; font-family: 'Inter', sans-serif; color: #475569;
        background: #f8fafc; cursor: pointer; outline: none; font-weight: 500;
        transition: border-color .2s;
      }
      .chart-type-select:focus { border-color: #4ecca3; }
      .chart-hint { font-size: 11px; color: #94a3b8; margin-bottom: 12px; font-style: italic; }
      .relationship-controls {
        display: grid;
        grid-template-columns: repeat(3, minmax(0, 1fr));
        gap: 12px;
        margin-bottom: 12px;
        align-items: end;
      }
      .relationship-controls .shiny-input-container { margin: 0; }
      .chart-reset-btn {
        background: #fff1f2; color: #be123c; border: 1px solid #fda4af;
        padding: 9px 14px; border-radius: 8px; font-size: 12px; font-weight: 700;
        cursor: pointer; font-family: 'Inter', sans-serif; transition: all .2s;
        width: 100%;
      }
      .chart-reset-btn:hover { background: #be123c; color: #fff; border-color: #be123c; }
      .relationship-note {
        font-size: 12px; color: #64748b; margin-top: 2px; margin-bottom: 10px;
      }
      .builder-wrap { max-width: 1100px; margin: 24px auto; padding: 0 20px; }
      .builder-grid { display: grid; grid-template-columns: 380px 1fr; gap: 18px; align-items: start; }
      .builder-card, .builder-preview-card {
        background: #fff; border-radius: 16px; box-shadow: 0 1px 4px rgba(0,0,0,.07);
        padding: 24px;
      }
      .builder-title { font-size: 20px; font-weight: 800; color: #1a1a2e; margin-bottom: 6px; }
      .builder-subtitle { font-size: 13px; color: #64748b; margin-bottom: 18px; }
      .builder-actions { display: flex; gap: 10px; flex-wrap: wrap; margin-top: 16px; }
      .builder-btn {
        border: none; border-radius: 10px; padding: 11px 16px; font-size: 13px; font-weight: 700;
        cursor: pointer; font-family: 'Inter', sans-serif; transition: transform .1s, opacity .2s;
      }
      .builder-btn:hover { opacity: .92; transform: translateY(-1px); }
      .builder-save { background: linear-gradient(135deg, #4ecca3, #2dd4bf); color: #1a1a2e; }
      .builder-clear { background: #fef2f2; color: #b91c1c; border: 1px solid #fca5a5; }
      .builder-note {
        background: #f8fafc; border: 1px solid #e2e8f0; border-radius: 12px;
        padding: 12px 14px; color: #475569; font-size: 13px; line-height: 1.5;
      }
      .custom-chart-head { display: flex; align-items: flex-start; justify-content: space-between; gap: 12px; }
      .custom-chart-remove { background: #fef2f2; color: #b91c1c; border: 1px solid #fca5a5; }

      .chart-toggle-bar {
        background: #fff; border-radius: 12px; padding: 12px 20px; margin-bottom: 14px;
        box-shadow: 0 1px 4px rgba(0,0,0,.07);
        display: flex; align-items: center; gap: 14px; flex-wrap: wrap;
      }
      .chart-toggle-label { font-size: 13px; font-weight: 600; color: #64748b; white-space: nowrap; }
      .chart-toggle-bar .shiny-input-container { margin: 0; }
      .chart-toggle-bar select {
        border: 1.5px solid #e2e8f0; border-radius: 8px; padding: 7px 12px;
        font-size: 14px; font-family: 'Inter', sans-serif; color: #1a1a2e;
        background: #f8fafc; outline: none; cursor: pointer;
        transition: border-color .2s;
      }
      .chart-toggle-bar select:focus { border-color: #4ecca3; }
      .chart-toggle-hint { font-size: 11px; color: #94a3b8; font-style: italic; white-space: nowrap; }
      .no-charts-msg {
        background: #fff; border-radius: 14px; padding: 48px 24px; text-align: center;
        color: #94a3b8; font-size: 15px; font-weight: 500;
        box-shadow: 0 1px 4px rgba(0,0,0,.07); margin-bottom: 20px;
      }

      .placement-filter-row { display: flex; gap: 10px; justify-content: center; margin-top: 10px; flex-wrap: wrap; }
      .placement-filter-btn {
        border: none; padding: 6px 20px; border-radius: 20px; font-size: 13px;
        font-weight: 600; cursor: pointer; font-family: 'Inter', sans-serif; transition: all .2s;
      }
      .placed-btn { background: #d1fae5; color: #065f46; }
      .placed-btn:hover, .placed-btn.active-pfbtn { background: #4ecca3; color: #1a1a2e; }
      .notplaced-btn { background: #fee2e2; color: #991b1b; }
      .notplaced-btn:hover, .notplaced-btn.active-pfbtn { background: #f87171; color: #fff; }

      .table-card { background: #fff; border-radius: 14px; padding: 18px 20px; box-shadow: 0 1px 4px rgba(0,0,0,.07); margin-bottom: 24px; }
      .table-header { display: flex; align-items: center; justify-content: space-between; margin-bottom: 16px; padding-bottom: 12px; border-bottom: 1px solid #f1f5f9; }
      .section-title { font-size: 15px; font-weight: 600; color: #1a1a2e; }
      .table-count { font-size: 13px; color: #94a3b8; }

      .insert-wrap { max-width: 900px; margin: 32px auto; padding: 0 24px; }
      .import-card { background: #fff; border-radius: 16px; padding: 28px; box-shadow: 0 1px 4px rgba(0,0,0,.07); margin-bottom: 24px; border-top: 4px solid #3b82f6; }
      .import-title    { font-size: 16px; font-weight: 700; color: #1a1a2e; margin-bottom: 4px; }
      .import-subtitle { font-size: 13px; color: #94a3b8; margin-bottom: 20px; }
      .csv-format { background: #f8fafc; border: 1.5px solid #e2e8f0; border-radius: 8px; padding: 12px 16px; font-size: 12px; color: #64748b; font-family: monospace; margin-bottom: 16px; line-height: 1.6; }
      .import-btn { background: linear-gradient(135deg, #3b82f6, #2563eb); color: #fff; border: none; padding: 10px 24px; border-radius: 8px; font-size: 14px; font-weight: 600; cursor: pointer; font-family: 'Inter', sans-serif; transition: opacity .2s; margin-top: 12px; width: 100%; }
      .import-btn:hover { opacity: .9; }
      .form-card { background: #fff; border-radius: 16px; padding: 36px; box-shadow: 0 1px 4px rgba(0,0,0,.07); margin-bottom: 24px; }
      .form-title    { font-size: 20px; font-weight: 700; color: #1a1a2e; margin-bottom: 4px; }
      .form-subtitle { font-size: 13px; color: #94a3b8; margin-bottom: 28px; }
      .form-grid { display: grid; grid-template-columns: 1fr 1fr; gap: 20px; }
      .form-group { display: flex; flex-direction: column; gap: 6px; }
      .form-group label { font-size: 13px; font-weight: 600; color: #475569; }
      .form-group input, .form-group select { border: 1.5px solid #e2e8f0; border-radius: 8px; padding: 10px 14px; font-size: 14px; font-family: 'Inter', sans-serif; color: #1a1a2e; background: #f8fafc; outline: none; transition: border-color .2s; width: 100%; }
      .form-group input:focus, .form-group select:focus { border-color: #4ecca3; background: #fff; }
      .submit-btn { background: linear-gradient(135deg, #4ecca3, #2dd4bf); color: #1a1a2e; border: none; padding: 13px 32px; border-radius: 10px; font-size: 15px; font-weight: 700; cursor: pointer; margin-top: 24px; width: 100%; font-family: 'Inter', sans-serif; transition: opacity .2s, transform .1s; }
      .submit-btn:hover  { opacity: .9; transform: translateY(-1px); }
      .submit-btn:active { transform: translateY(0); }
      .alert-success { background: #f0fdf4; border: 1.5px solid #4ecca3; color: #166534; padding: 14px 18px; border-radius: 10px; font-size: 14px; font-weight: 500; margin-top: 16px; }
      .alert-error   { background: #fef2f2; border: 1.5px solid #fca5a5; color: #991b1b; padding: 14px 18px; border-radius: 10px; font-size: 14px; font-weight: 500; margin-top: 16px; }
      .preview-label { font-size: 13px; color: #64748b; margin-bottom: 8px; font-weight: 500; }
      .recent-card { background: #fff; border-radius: 16px; padding: 28px; box-shadow: 0 1px 4px rgba(0,0,0,.07); }
      .delete-btn { background: #fef2f2; color: #dc2626; border: 1px solid #fca5a5; padding: 4px 10px; border-radius: 5px; font-size: 12px; font-weight: 600; cursor: pointer; font-family: 'Inter', sans-serif; transition: all .2s; }
      .delete-btn:hover { background: #dc2626; color: #fff; }
      .dataTables_wrapper .dataTables_filter input { border: 1.5px solid #e2e8f0; border-radius: 8px; padding: 6px 12px; font-family: 'Inter', sans-serif; outline: none; }
      .dataTables_wrapper .dataTables_filter input:focus { border-color: #4ecca3; }
      table.dataTable thead th { font-size: 12px; font-weight: 600; text-transform: uppercase; letter-spacing: .6px; color: #64748b; border-bottom: 2px solid #f1f5f9; }
      .shiny-input-container { margin-bottom: 0; }
      .confirm-overlay { display: none; position: fixed; top: 0; left: 0; width: 100%; height: 100%; background: rgba(0,0,0,.5); z-index: 9999; align-items: center; justify-content: center; }
      .confirm-overlay.show { display: flex; }
      .confirm-box { background: #fff; border-radius: 16px; padding: 32px; max-width: 400px; width: 90%; text-align: center; box-shadow: 0 20px 60px rgba(0,0,0,.3); }
      .confirm-title { font-size: 18px; font-weight: 700; color: #1a1a2e; margin-bottom: 8px; }
      .confirm-msg   { font-size: 14px; color: #64748b; margin-bottom: 24px; }
      .confirm-btns  { display: flex; gap: 12px; justify-content: center; }
      .confirm-yes { background: #dc2626; color: #fff; border: none; padding: 10px 28px; border-radius: 8px; font-size: 14px; font-weight: 600; cursor: pointer; font-family: 'Inter', sans-serif; }
      .confirm-no  { background: #f1f5f9; color: #475569; border: none; padding: 10px 28px; border-radius: 8px; font-size: 14px; font-weight: 600; cursor: pointer; font-family: 'Inter', sans-serif; }
      .confirm-yes:hover { background: #b91c1c; }
      .confirm-no:hover  { background: #e2e8f0; }
    ")),
    tags$script(HTML("
      /* ── Delete flow ── */
      $(document).on('click', '.delete-btn', function() {
        $('#deleteId').val($(this).data('id'));
        $('#confirmOverlay').addClass('show');
      });
      $(document).on('click', '#confirmNo',  function() { $('#confirmOverlay').removeClass('show'); });
      $(document).on('click', '#confirmYes', function() {
        Shiny.setInputValue('deleteConfirmed', $('#deleteId').val(), {priority:'event'});
        $('#confirmOverlay').removeClass('show');
      });

      /* ── Count-up animation ── */
      function animateCount(el, target, decimals, suffix) {
        var duration = 1400, startTime = null;
        suffix = suffix || '';
        function step(ts) {
          if (!startTime) startTime = ts;
          var p    = Math.min((ts - startTime) / duration, 1);
          var ease = 1 - Math.pow(1 - p, 3);
          el.textContent = (ease * target).toFixed(decimals) + suffix;
          if (p < 1) requestAnimationFrame(step);
          else el.textContent = target.toFixed(decimals) + suffix;
        }
        requestAnimationFrame(step);
      }
      Shiny.addCustomMessageHandler('runCountUp', function(d) {
        setTimeout(function() {
          var e1 = document.getElementById('heroTotal');
          var e2 = document.getElementById('heroScore');
          var e3 = document.getElementById('heroAttend');
          var e4 = document.getElementById('heroPlaced');
          if (e1) animateCount(e1, d.total,  0, '');
          if (e2) animateCount(e2, d.score,  1, '');
          if (e3) animateCount(e3, d.attend, 1, '%');
          if (e4) animateCount(e4, d.placed, 0, '');
        }, 120);
      });

      /* ── Highlight active chart card ── */
      Shiny.addCustomMessageHandler('highlightCard', function(d) {
        document.querySelectorAll('.chart-card').forEach(function(c) { c.classList.remove('chart-active'); });
        if (d.id) {
          var card = document.getElementById(d.id);
          if (card) card.classList.add('chart-active');
        }
      });

      function setActiveTopTab(activeId) {
        document.querySelectorAll('.tab-btn').forEach(function(b) { b.classList.remove('active-tab'); });
        var active = document.getElementById(activeId);
        if (active) active.classList.add('active-tab');
      }

      /* ── Chart type dropdowns: bind native <select> -> Shiny ── */
      function bindChartSelects() {
        ['studyChartType','attendChartType','scoreChartType','placementChartType'].forEach(function(id) {
          var el = document.getElementById(id);
          if (el && !el.dataset.bound) {
            el.dataset.bound = '1';
            el.addEventListener('change', function() {
              Shiny.setInputValue(id, this.value, {priority:'event'});
            });
          }
        });
      }
      /* Try binding immediately and also after Shiny updates DOM */
      $(document).on('shiny:value shiny:recalculated shiny:idle', function() {
        setTimeout(bindChartSelects, 200);
      });
      setTimeout(bindChartSelects, 800);

      /* ── Placement filter buttons ── */
      $(document).on('click', '.placement-filter-btn', function() {
        var val = $(this).data('val');
        $('.placement-filter-btn').removeClass('active-pfbtn');
        $(this).addClass('active-pfbtn');
        Shiny.setInputValue('placementPieFilter', val, {priority:'event'});
      });
    "))
  ),

  # Delete confirm modal
  div(id = "confirmOverlay", class = "confirm-overlay",
    div(class = "confirm-box",
      div(class = "confirm-title", "Delete Student"),
      div(class = "confirm-msg",   "Are you sure? This cannot be undone."),
      div(class = "confirm-btns",
        tags$button("Yes, Delete", class = "confirm-yes", id = "confirmYes"),
        tags$button("Cancel",      class = "confirm-no",  id = "confirmNo")
      )
    )
  ),
  tags$input(type = "hidden", id = "deleteId"),

  # Top nav
  div(class = "topnav",
    div(class = "topnav-brand",
      tags$svg(xmlns="http://www.w3.org/2000/svg", width="22", height="22",
               viewBox="0 0 24 24", fill="none", stroke="#4ecca3",
               `stroke-width`="2", `stroke-linecap`="round", `stroke-linejoin`="round",
               tags$path(d="M22 10v6M2 10l10-5 10 5-10 5z"),
               tags$path(d="M6 12v5c3 3 9 3 12 0v-5")),
      "Student", tags$span("MS")
    ),
    div(class = "topnav-tabs",
      actionButton("tabInsert",    "Insert Student", class = "tab-btn active-tab"),
      actionButton("tabDashboard", "Dashboard",      class = "tab-btn"),
      actionButton("tabBuilder",   "Chart Builder",  class = "tab-btn")
    ),
    div(class = "topnav-right",
      uiOutput("refreshUI"),
      uiOutput("lastUpdatedUI")
    )
  ),

  uiOutput("pageContent")
)

# ── SERVER ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  page          <- reactiveVal("insert")
  student_data  <- reactiveVal(fetchData())
  last_updated   <- reactiveVal(format(Sys.time(), "%d %b %Y  %H:%M:%S"))
  chart_filter   <- reactiveVal(NULL)
  custom_chart   <- reactiveVal(NULL)
  builder_msg    <- reactiveVal(NULL)

  sendKPIs <- function(df) {
    if (nrow(df) == 0) {
      session$sendCustomMessage("runCountUp", list(total=0, score=0, attend=0, placed=0))
      return()
    }
    session$sendCustomMessage("runCountUp", list(
      total  = nrow(df),
      score  = round(mean(df$exam_score,  na.rm = TRUE), 1),
      attend = round(mean(df$attendance,  na.rm = TRUE), 1),
      placed = nrow(df[df$placement_status == "Placed", ])
    ))
  }

  # ── Tab navigation ──────────────────────────────────────────────────────────
  observeEvent(input$tabInsert, {
    page("insert"); chart_filter(NULL)
    runjs("setActiveTopTab('tabInsert');")
  })
  observeEvent(input$tabDashboard, {
    page("dashboard")
    runjs("setActiveTopTab('tabDashboard');")
    sendKPIs(student_data())
  })
  observeEvent(input$tabBuilder, {
    page("builder")
    runjs("setActiveTopTab('tabBuilder');")
  })

  # ── Refresh ──────────────────────────────────────────────────────────────────
  observeEvent(input$refreshBtn, {
    student_data(fetchData()); last_updated(format(Sys.time(), "%d %b %Y  %H:%M:%S"))
    chart_filter(NULL); session$sendCustomMessage("highlightCard", list(id=NULL))
    sendKPIs(student_data())
  })
  output$refreshUI    <- renderUI({ req(page()=="dashboard"); actionButton("refreshBtn","Refresh Data",class="refresh-btn") })
  output$lastUpdatedUI <- renderUI({ req(page()=="dashboard"); div(class="last-updated", paste("Last updated:", last_updated())) })

  # ── Filters ──────────────────────────────────────────────────────────────────
  placement_filter <- reactive({
    if (!is.null(input$placement) && input$placement != "All") input$placement else NULL
  })
  base_data <- reactive({
    df <- student_data()
    if (!is.null(placement_filter())) df <- df[df$placement_status == placement_filter(), ]
    df
  })
  observeEvent(input$clearChartFilter, {
    chart_filter(NULL); session$sendCustomMessage("highlightCard", list(id=NULL))
    runjs("document.querySelectorAll('.placement-filter-btn').forEach(function(b){b.classList.remove('active-pfbtn');});")
  })
  filtered_data <- reactive({
    df <- base_data(); cf <- chart_filter()
    if (is.null(cf)) return(df)
    if      (cf$type == "study")      df <- df[df$study_hours  >= cf$lo & df$study_hours  < cf$hi, ]
    else if (cf$type == "attendance") df <- df[df$attendance   >= cf$lo & df$attendance   < cf$hi, ]
    else if (cf$type == "score")      df <- df[df$exam_score   >= cf$lo & df$exam_score   < cf$hi, ]
    else if (cf$type == "placement")  df <- df[df$placement_status == cf$value, ]
    df
  })

  # ── Chart click observers (cross-filter all charts) ──────────────────────────
  observeEvent(input$studyClick, {
    ct <- if (is.null(input$studyChartType)) "Bar Chart" else input$studyChartType
    req(ct == "Bar Chart"); click <- input$studyClick
    breaks <- c(0,2,4,6,8,10,12,24); labels <- c("0-2","2-4","4-6","6-8","8-10","10-12","12+")
    idx <- max(1, min(round(click$x), length(labels)))
    chart_filter(list(type="study", lo=breaks[idx], hi=breaks[idx+1],
                      sel_labels=labels[idx], source="study",
                      label=paste0("Study Hours: ", labels[idx])))
    session$sendCustomMessage("highlightCard", list(id="studyCard"))
  })
  observeEvent(input$attendClick, {
    ct <- if (is.null(input$attendChartType)) "Bar Chart" else input$attendChartType
    req(ct == "Bar Chart"); click <- input$attendClick
    breaks <- c(0,20,40,60,70,80,90,100); labels <- c("0-20%","20-40%","40-60%","60-70%","70-80%","80-90%","90-100%")
    idx <- max(1, min(round(click$x), length(labels)))
    chart_filter(list(type="attendance", lo=breaks[idx], hi=breaks[idx+1],
                      sel_labels=labels[idx], source="attend",
                      label=paste0("Attendance: ", labels[idx])))
    session$sendCustomMessage("highlightCard", list(id="attendCard"))
  })
  observeEvent(input$scoreClick, {
    ct <- if (is.null(input$scoreChartType)) "Bar Chart" else input$scoreChartType
    req(ct == "Bar Chart"); click <- input$scoreClick
    breaks <- c(0,20,40,50,60,70,80,90,100); labels <- c("0-20","20-40","40-50","50-60","60-70","70-80","80-90","90-100")
    idx <- max(1, min(round(click$x), length(labels)))
    chart_filter(list(type="score", lo=breaks[idx], hi=breaks[idx+1],
                      sel_labels=labels[idx], source="score",
                      label=paste0("Exam Score: ", labels[idx])))
    session$sendCustomMessage("highlightCard", list(id="scoreCard"))
  })

  # ── Placement filter buttons ─────────────────────────────────────────────────
  observeEvent(input$placementPieFilter, {
    val <- input$placementPieFilter
    req(!is.null(val) && val != "")
    chart_filter(list(type="placement", value=val, source="placement", label=paste("Status:", val)))
    session$sendCustomMessage("highlightCard", list(id="placementCard"))
  })

  observeEvent(input$previewCustomChart, {
    page("builder")
    runjs("setActiveTopTab('tabBuilder');")
  })

  observeEvent(input$addCustomChart, {
    custom_chart(list(
      x = or_default(input$builderXVar, "attendance"),
      y = or_default(input$builderYVar, "exam_score"),
      type = or_default(input$builderChartType, "Scatter Plot"),
      title = or_default(input$builderChartTitle, "Custom Chart")
    ))
    builder_msg(list(type="success", text="Chart added to the dashboard."))
    page("dashboard")
    runjs("setActiveTopTab('tabDashboard');")
  })

  observeEvent(input$clearCustomChart, {
    custom_chart(NULL)
    builder_msg(list(type="info", text="Custom chart removed from the dashboard."))
  })

  observeEvent(input$removeCustomChart, {
    custom_chart(NULL)
    builder_msg(list(type="info", text="Custom chart removed from the dashboard."))
  })

  observeEvent(input$resetRelationshipChart, {
    updateSelectInput(session, "builderXVar", selected = "attendance")
    updateSelectInput(session, "builderYVar", selected = "exam_score")
    updateSelectInput(session, "builderChartType", selected = "Scatter Plot")
    updateTextInput(session, "builderChartTitle", value = "Attendance vs Score")
  })

  output$builderMsg <- renderUI({
    msg <- builder_msg()
    if (is.null(msg)) return(NULL)
    cls <- if (identical(msg$type, "success")) "alert-success" else "alert-error"
    if (identical(msg$type, "info")) cls <- "alert-success"
    div(class = cls, msg$text)
  })

  # ── Active filter pill & table count ────────────────────────────────────────
  output$activeFilterUI <- renderUI({
    cf <- chart_filter()
    if (is.null(cf)) return(NULL)
    div(class="active-filter-pill",
      tags$span(paste0("Filtered: ", cf$label)),
      actionButton("clearChartFilter", "\u00d7", class="clear-filter-btn")
    )
  })
  output$tableCountUI <- renderUI({
    n <- nrow(filtered_data()); total <- nrow(base_data())
    label <- if (!is.null(chart_filter())) paste(n,"of",total,"students") else paste(total,"students")
    div(class="table-count", label)
  })

  # ── Chart themes ─────────────────────────────────────────────────────────────
  make_bar_theme <- function() {
    theme_minimal(base_family="sans") +
      theme(plot.background=element_rect(fill="white",color=NA), panel.grid.minor=element_blank(),
            panel.grid.major.x=element_blank(), panel.grid.major.y=element_line(color="#f1f5f9"),
            axis.text=element_text(color="#64748b",size=10), axis.title=element_text(color="#475569",size=12,face="bold"),
            plot.margin=margin(8,8,8,8))
  }
  make_pie_theme <- function() {
    theme_void(base_family="sans") +
      theme(plot.background=element_rect(fill="white",color=NA), legend.position="bottom",
            legend.text=element_text(size=11,color="#475569"), legend.title=element_blank(),
            plot.margin=margin(8,8,8,8))
  }

  make_relationship_plot <- function(df, xvar, yvar, chart_type) {
    plot_df <- df[, c(xvar, yvar)]
    plot_df <- plot_df[complete.cases(plot_df), , drop = FALSE]
    validate(need(nrow(plot_df) > 0, "No complete rows available for the selected columns."))

    plot_df <- plot_df[order(plot_df[[xvar]]), , drop = FALSE]

    gg <- ggplot(plot_df, aes(x = .data[[xvar]], y = .data[[yvar]]))
    if (chart_type == "Scatter Plot") {
      gg +
        geom_point(color = "#4ecca3", size = 3, alpha = 0.8) +
        labs(x = relationshipLabel(xvar), y = relationshipLabel(yvar)) +
        make_bar_theme()
    } else if (chart_type == "Line Chart") {
      gg +
        geom_line(color = "#4ecca3", linewidth = 1.4) +
        geom_point(color = "#0f3460", size = 2.6, alpha = 0.85) +
        labs(x = relationshipLabel(xvar), y = relationshipLabel(yvar)) +
        make_bar_theme()
    } else if (chart_type == "Trend Line") {
      gg +
        geom_smooth(method = "lm", se = FALSE, color = "#0f3460", linewidth = 1.2) +
        labs(x = relationshipLabel(xvar), y = relationshipLabel(yvar)) +
        make_bar_theme()
    } else if (chart_type == "Smooth Trend") {
      gg +
        geom_smooth(method = "loess", se = FALSE, color = "#4ecca3", linewidth = 1.2) +
        labs(x = relationshipLabel(xvar), y = relationshipLabel(yvar)) +
        make_bar_theme()
    } else if (chart_type == "Bar Chart") {
      gg +
        geom_col(fill = "#4ecca3", alpha = 0.9, width = 0.75) +
        labs(x = relationshipLabel(xvar), y = relationshipLabel(yvar)) +
        make_bar_theme()
    } else if (chart_type == "Area Chart") {
      gg +
        geom_area(fill = "#4ecca3", alpha = 0.3, color = "#4ecca3", linewidth = 1) +
        labs(x = relationshipLabel(xvar), y = relationshipLabel(yvar)) +
        make_bar_theme()
    } else {
      gg +
        geom_point(color = "#4ecca3", size = 3, alpha = 0.8) +
        labs(x = relationshipLabel(xvar), y = relationshipLabel(yvar)) +
        make_bar_theme()
    }
  }

  # Helper: render bar / pie / line for binned data
  render_binned_chart <- function(df, breaks, labels, x_lab, chart_type, cf_type, cf, bar_colors, is_source=FALSE) {
    df$bin <- cut(df[[names(df)[1]]], breaks=breaks, labels=labels, right=FALSE, include.lowest=TRUE)
    counts <- as.data.frame(table(df$bin)); colnames(counts) <- c("Bin","Count")
    counts$Pct <- round(counts$Count / max(sum(counts$Count),1) * 100, 1)

    if (chart_type == "Pie Chart") {
      ggplot(counts, aes(x="", y=Count, fill=Bin)) +
        geom_col(width=1, color="white", linewidth=0.5) + coord_polar("y") +
        geom_text(aes(label=ifelse(Pct>=5, paste0(Pct,"%"), "")),
                  position=position_stack(vjust=0.5), size=3.5, fontface="bold", color="white") +
        scale_fill_manual(values=bar_colors) + labs(x=NULL,y=NULL) + make_pie_theme()
    } else if (chart_type == "Line Chart") {
      ggplot(counts, aes(x=Bin, y=Count, group=1)) +
        geom_line(color=bar_colors[1], linewidth=1.5) +
        geom_point(color=bar_colors[1], size=4, fill="#fff", shape=21, stroke=2) +
        geom_text(aes(label=Count), vjust=-1, size=3.5, color="#475569", fontface="bold") +
        labs(x=x_lab, y="Students") + expand_limits(y=max(counts$Count)*1.15) + make_bar_theme()
    } else {
      # Bar — highlight selected range when this chart is the source of the brush
      if (is_source && !is.null(cf) && cf$type == cf_type && !is.null(cf$sel_labels)) {
        sel <- cf$sel_labels
        counts$fill  <- ifelse(as.character(counts$Bin) %in% sel, bar_colors[2], bar_colors[3])
        counts$alpha <- ifelse(as.character(counts$Bin) %in% sel, 1, 0.35)
      } else {
        counts$fill <- bar_colors[1]; counts$alpha <- 0.85
      }
      ggplot(counts, aes(x=Bin, y=Count, fill=fill, alpha=alpha)) +
        geom_col(width=0.72) +
        geom_text(aes(label=Count), vjust=-0.4, size=3.5, color="#475569", fontface="bold") +
        scale_fill_identity() + scale_alpha_identity() +
        labs(x=x_lab, y="Students") + make_bar_theme()
    }
  }

  # ── STUDY PLOT ───────────────────────────────────────────────────────────────
  output$studyPlot <- renderPlot({
    cf <- chart_filter()
    is_src <- !is.null(cf) && !is.null(cf$source) && cf$source == "study"
    df <- if (is_src) base_data() else filtered_data()
    validate(need(nrow(df)>0,"No data to display"))
    ct <- if (is.null(input$studyChartType)) "Bar Chart" else input$studyChartType
    df2 <- data.frame(study_hours = df$study_hours)
    render_binned_chart(df2, c(0,2,4,6,8,10,12,24), c("0-2","2-4","4-6","6-8","8-10","10-12","12+"),
                        "Study Hours / Day", ct, "study", cf,
                        c("#3b82f6","#4ecca3","#93c5fd","#60a5fa","#bfdbfe","#dbeafe","#1d4ed8"),
                        is_source=is_src)
  })

  # ── ATTENDANCE PLOT ──────────────────────────────────────────────────────────
  output$attendancePlot <- renderPlot({
    cf <- chart_filter()
    is_src <- !is.null(cf) && !is.null(cf$source) && cf$source == "attend"
    df <- if (is_src) base_data() else filtered_data()
    validate(need(nrow(df)>0,"No data to display"))
    ct <- if (is.null(input$attendChartType)) "Bar Chart" else input$attendChartType
    df2 <- data.frame(attendance = df$attendance)
    render_binned_chart(df2, c(0,20,40,60,70,80,90,100), c("0-20%","20-40%","40-60%","60-70%","70-80%","80-90%","90-100%"),
                        "Attendance Range", ct, "attendance", cf,
                        c("#f59e0b","#f59e0b","#fcd34d","#fbbf24","#fde68a","#fef3c7","#d97706"),
                        is_source=is_src)
  })

  # ── SCORE PLOT ───────────────────────────────────────────────────────────────
  output$scorePlot <- renderPlot({
    cf <- chart_filter()
    is_src <- !is.null(cf) && !is.null(cf$source) && cf$source == "score"
    df <- if (is_src) base_data() else filtered_data()
    validate(need(nrow(df)>0,"No data to display"))
    ct <- if (is.null(input$scoreChartType)) "Bar Chart" else input$scoreChartType
    df2 <- data.frame(exam_score = df$exam_score)
    render_binned_chart(df2, c(0,20,40,50,60,70,80,90,100), c("0-20","20-40","40-50","50-60","60-70","70-80","80-90","90-100"),
                        "Exam Score Range", ct, "score", cf,
                        c("#8b5cf6","#a78bfa","#c4b5fd","#ddd6fe","#ede9fe","#f5f3ff","#6d28d9","#7c3aed"),
                        is_source=is_src)
  })

  output$customBuilderPlot <- renderPlot({
    df <- filtered_data()
    validate(need(nrow(df) > 0, "No data to display"))

    xvar <- or_default(input$builderXVar, "attendance")
    yvar <- or_default(input$builderYVar, "exam_score")
    chart_type <- or_default(input$builderChartType, "Scatter Plot")

    validate(need(xvar != yvar, "Please choose two different columns."))
    validate(need(xvar %in% names(df) && yvar %in% names(df), "Selected columns are not available."))
    validate(need(is.numeric(df[[xvar]]) && is.numeric(df[[yvar]]), "Both selected columns must be numeric."))

    make_relationship_plot(df, xvar, yvar, chart_type)
  })

  output$customDashboardPlot <- renderPlot({
    cfg <- custom_chart()
    validate(need(!is.null(cfg), "No custom chart has been added yet."))
    df <- filtered_data()
    validate(need(nrow(df) > 0, "No data to display"))

    xvar <- cfg$x
    yvar <- cfg$y
    chart_type <- cfg$type

    validate(need(xvar %in% names(df) && yvar %in% names(df), "Saved chart columns are not available."))
    validate(need(is.numeric(df[[xvar]]) && is.numeric(df[[yvar]]), "Saved chart columns must be numeric."))

    make_relationship_plot(df, xvar, yvar, chart_type)
  })

  # ── PLACEMENT PLOT ───────────────────────────────────────────────────────────
  output$placementPlot <- renderPlot({
    cf <- chart_filter()
    is_src <- !is.null(cf) && !is.null(cf$source) && cf$source == "placement"
    df <- if (is_src) base_data() else filtered_data()
    validate(need(nrow(df)>0,"No data to display"))
    ct <- if (is.null(input$placementChartType)) "Pie Chart" else input$placementChartType
    df <- df[!is.na(df$placement_status) & df$placement_status != "", ]
    validate(need(nrow(df)>0,"No placement data"))

    counts <- as.data.frame(table(df$placement_status)); colnames(counts) <- c("Status","Count")
    counts$Pct <- round(counts$Count / sum(counts$Count) * 100, 1)
    base_colors <- c("Not Placed"="#f87171","Placed"="#4ecca3")

    if (ct == "Pie Chart") {
      counts$fill  <- base_colors[as.character(counts$Status)]
      counts$alpha <- if (!is.null(cf) && cf$type=="placement")
        ifelse(as.character(counts$Status)==cf$value, 1, 0.3) else 1
      ggplot(counts, aes(x="", y=Count, fill=Status, alpha=alpha)) +
        geom_col(width=1, color="white", linewidth=1) + coord_polar("y") +
        geom_text(aes(label=paste0(Pct,"%\n(",Count,")")),
                  position=position_stack(vjust=0.5), size=4.5, fontface="bold", color="white", lineheight=1.2) +
        scale_fill_manual(values=base_colors) + scale_alpha_identity() +
        labs(x=NULL,y=NULL) + make_pie_theme()
    } else if (ct == "Line Chart") {
      counts$Status <- factor(counts$Status, levels=c("Not Placed","Placed"))
      ggplot(counts, aes(x=Status, y=Count, group=1)) +
        geom_line(color="#4ecca3", linewidth=1.5) +
        geom_point(aes(color=Status), size=6) +
        geom_text(aes(label=paste0(Count,"\n(",Pct,"%)")), vjust=-1, size=4, color="#475569", fontface="bold", lineheight=1.1) +
        scale_color_manual(values=base_colors) + labs(x=NULL,y="Number of Students") +
        expand_limits(y=max(counts$Count)*1.25) + make_bar_theme() + theme(legend.position="none")
    } else {
      # Bar
      counts$Status <- factor(counts$Status, levels=c("Not Placed","Placed"))
      if (!is.null(cf) && cf$type=="placement") {
        sel <- cf$value
        counts$fill  <- ifelse(as.character(counts$Status)==sel, base_colors[as.character(counts$Status)], "#d1d5db")
        counts$alpha <- ifelse(as.character(counts$Status)==sel, 1, 0.4)
      } else {
        counts$fill <- base_colors[as.character(counts$Status)]; counts$alpha <- 0.85
      }
      ggplot(counts, aes(x=Status, y=Count, fill=fill, alpha=alpha)) +
        geom_col(width=0.55) +
        geom_text(aes(label=paste0(Count,"\n(",Pct,"%)")),
                  vjust=-0.3, size=4.5, color="#475569", fontface="bold", lineheight=1.1) +
        scale_fill_identity() + scale_alpha_identity() +
        labs(x=NULL, y="Number of Students") +
        expand_limits(y=max(counts$Count)*1.15) +
        make_bar_theme() + theme(axis.text.x=element_text(size=13,face="bold",color="#1a1a2e"))
    }
  })

  # ── Chart Grid (dynamic based on visible chart selection) ────────────────────
  output$chartGrid <- renderUI({
    sel <- if (is.null(input$visibleCharts)) "all" else input$visibleCharts
    # "All Charts" selected OR nothing selected → show every chart
    vis <- if ("all" %in% sel || length(sel) == 0)
      c("study","attend","score","placement") else sel

    # Build each card only if selected
    studyCard <- if ("study" %in% vis) {
      div(class="chart-card clickable", id="studyCard",
        div(class="chart-header",
          div(class="chart-title","Study Hours Distribution"),
          tags$select(id="studyChartType", class="chart-type-select",
            tags$option("Bar Chart",  value="Bar Chart"),
            tags$option("Pie Chart",  value="Pie Chart"),
            tags$option("Line Chart", value="Line Chart"))
        ),
        div(class="chart-hint","Click a bar to cross-filter all charts"),
        plotOutput("studyPlot", height="240px", click="studyClick")
      )
    } else NULL

    attendCard <- if ("attend" %in% vis) {
      div(class="chart-card clickable", id="attendCard",
        div(class="chart-header",
          div(class="chart-title","Attendance Distribution"),
          tags$select(id="attendChartType", class="chart-type-select",
            tags$option("Bar Chart",  value="Bar Chart"),
            tags$option("Pie Chart",  value="Pie Chart"),
            tags$option("Line Chart", value="Line Chart"))
        ),
        div(class="chart-hint","Click a bar to cross-filter all charts"),
        plotOutput("attendancePlot", height="240px", click="attendClick")
      )
    } else NULL

    scoreCard <- if ("score" %in% vis) {
      div(class="chart-card clickable", id="scoreCard",
        div(class="chart-header",
          div(class="chart-title","Exam Score Distribution"),
          tags$select(id="scoreChartType", class="chart-type-select",
            tags$option("Bar Chart",  value="Bar Chart"),
            tags$option("Pie Chart",  value="Pie Chart"),
            tags$option("Line Chart", value="Line Chart"))
        ),
        div(class="chart-hint","Click a bar to cross-filter all charts"),
        plotOutput("scorePlot", height="240px", click="scoreClick")
      )
    } else NULL

    placementCard <- if ("placement" %in% vis) {
      div(class="chart-card", id="placementCard",
        div(class="chart-header",
          div(class="chart-title","Placement Status"),
          tags$select(id="placementChartType", class="chart-type-select",
            tags$option("Pie Chart",  value="Pie Chart"),
            tags$option("Bar Chart",  value="Bar Chart"),
            tags$option("Line Chart", value="Line Chart"))
        ),
        div(class="chart-hint","Use buttons below to filter by placement status"),
        plotOutput("placementPlot", height="200px"),
        div(class="placement-filter-row",
          tags$button("Placed",     class="placement-filter-btn placed-btn",    `data-val`="Placed"),
          tags$button("Not Placed", class="placement-filter-btn notplaced-btn", `data-val`="Not Placed")
        )
      )
    } else NULL

    custom_cfg <- custom_chart()
    customCard <- if (!is.null(custom_cfg)) {
      div(class="chart-card full", id="customDashboardCard",
        div(class="custom-chart-head",
          div(
            div(class="chart-title", if (!is.null(custom_cfg$title) && nzchar(custom_cfg$title)) custom_cfg$title else "Custom Chart"),
            div(class="relationship-note", paste("X:", relationshipLabel(custom_cfg$x), "| Y:", relationshipLabel(custom_cfg$y), "| Type:", custom_cfg$type))
          ),
          actionButton("removeCustomChart", "Delete Chart", class="chart-reset-btn custom-chart-remove")
        ),
        plotOutput("customDashboardPlot", height="320px")
      )
    } else NULL

    # Collect only non-NULL cards in order
    cards <- Filter(Negate(is.null), list(studyCard, attendCard, scoreCard, placementCard, customCard))
    n <- length(cards)

    if (n == 0) {
      return(div(class="no-charts-msg",
        tags$span(style="font-size:28px;display:block;margin-bottom:10px;","Charts"),
        "No charts selected. Use the toggles above to show charts."
      ))
    }

    # Layout: pair cards into rows of 2; last row is full-width if odd number
    rows <- list()
    i <- 1
    while (i <= n) {
      if (i + 1 <= n) {
        rows <- c(rows, list(div(class="chart-row", cards[[i]], cards[[i+1]])))
        i <- i + 2
      } else {
        rows <- c(rows, list(div(class="chart-row", style="grid-template-columns:1fr;", cards[[i]])))
        i <- i + 1
      }
    }
    tagList(rows)
  })
  output$table <- renderDT({
    tryCatch({
      df <- filtered_data()
      validate(need(is.data.frame(df),"Data unavailable - check DB connection."))
      if (nrow(df)>0 && "id" %in% names(df))
        df$Delete <- paste0('<button class="delete-btn" data-id="',df$id,'">Delete</button>')
      else df$Delete <- character(0)
      datatable(df, escape=FALSE, options=list(pageLength=10,dom="ftip",scrollX=TRUE), rownames=FALSE, class="stripe hover")
    }, error=function(e) {
      datatable(data.frame(Notice=paste("Table error:",e$message)), options=list(dom="t"), rownames=FALSE)
    })
  })

  # ── Page layout ──────────────────────────────────────────────────────────────
  output$pageContent <- renderUI({

    if (page() == "insert") {
      div(class="insert-wrap",
        div(class="import-card",
          div(class="import-title","Import CSV File"),
          div(class="import-subtitle","Upload a CSV to insert multiple students at once"),
          div(class="csv-format",
            "Required columns:", tags$br(),
            "study_hours, attendance, sleep_hours, internet_usage,", tags$br(),
            "assignments_completed, previous_score, exam_score, placement_status"),
          fileInput("csvFile",NULL,accept=".csv",placeholder="Choose CSV file",width="100%"),
          uiOutput("csvPreview"), uiOutput("importMsg")
        ),
        div(class="form-card",
          div(class="form-title","Add New Student"),
          div(class="form-subtitle","Fill in all fields and submit to save to the database"),
          div(class="form-grid",
            div(class="form-group",tags$label("Study Hours"),           numericInput("study_hours",           NULL,5,0,24,  width="100%")),
            div(class="form-group",tags$label("Attendance (%)"),        numericInput("attendance",             NULL,75,0,100,width="100%")),
            div(class="form-group",tags$label("Sleep Hours"),           numericInput("sleep_hours",            NULL,7,0,24,  width="100%")),
            div(class="form-group",tags$label("Internet Usage (hrs)"),  numericInput("internet_usage",         NULL,3,0,24,  width="100%")),
            div(class="form-group",tags$label("Assignments Completed"), numericInput("assignments_completed",  NULL,5,0,20,  width="100%")),
            div(class="form-group",tags$label("Previous Score"),        numericInput("previous_score",         NULL,60,0,100,width="100%")),
            div(class="form-group",tags$label("Exam Score"),            numericInput("exam_score",             NULL,70,0,100,width="100%")),
            div(class="form-group",tags$label("Placement Status"),      selectInput("placement_status",NULL,c("Placed","Not Placed"),width="100%"))
          ),
          actionButton("submitBtn","Submit Student",class="submit-btn"),
          uiOutput("submitMsg")
        ),
        div(class="recent-card",
          div(class="section-title",style="margin-bottom:16px;","Recently Added Students"),
          DTOutput("recentTable")
        )
      )
    } else if (page() == "builder") {
      div(class="builder-wrap",
        div(class="builder-grid",
          div(class="builder-card",
            div(class="builder-title","Chart Builder"),
            div(class="builder-subtitle","Pick the columns and chart style, then add it to the dashboard."),
            div(class="builder-note",
              "Default setup: Attendance (%) on the X axis and Exam Score on the Y axis. ",
              "Use the reset button if you want to go back to that starting point."
            ),
            selectInput("builderXVar", "X Axis", choices = relationshipColumnChoices, selected = "attendance", width = "100%"),
            selectInput("builderYVar", "Y Axis", choices = relationshipColumnChoices, selected = "exam_score", width = "100%"),
            selectInput("builderChartType", "Chart Type", choices = relationshipChartChoices, selected = "Scatter Plot", width = "100%"),
            textInput("builderChartTitle", "Chart Title", value = "Attendance vs Score", width = "100%"),
            div(class="builder-actions",
              actionButton("previewCustomChart", "Preview Chart", class="builder-btn builder-save"),
              actionButton("addCustomChart", "Add to Dashboard", class="builder-btn builder-save"),
              actionButton("resetRelationshipChart", "Reset", class="builder-btn builder-clear"),
              actionButton("clearCustomChart", "Remove from Dashboard", class="builder-btn builder-clear")
            ),
            uiOutput("builderMsg")
          ),
          div(class="builder-preview-card",
            div(class="custom-chart-head",
              div(
                div(class="builder-title","Live Preview"),
                div(class="builder-subtitle","This is the chart that will be added to the dashboard.")
              )
            ),
            plotOutput("customBuilderPlot", height="360px")
          )
        )
      )
    } else {
      div(class="page-wrap",
        # Hero
        div(class="hero-banner",
          div(class="hero-top",
            div(div(class="hero-title","Student Performance ",tags$span("Analytics")),
                div(class="hero-subtitle","Real-time insights from your student database")),
            div(class="hero-badge","LIVE DATA")
          ),
          div(class="hero-kpi-row",
            div(class="hero-kpi",
              div(class="kpi-icon blue", tags$svg(xmlns="http://www.w3.org/2000/svg",width="18",height="18",viewBox="0 0 24 24",fill="none",stroke="currentColor",`stroke-width`="2",`stroke-linecap`="round",`stroke-linejoin`="round",
                tags$path(d="M17 21v-2a4 4 0 0 0-4-4H5a4 4 0 0 0-4 4v2"),tags$circle(cx="9",cy="7",r="4"),tags$path(d="M23 21v-2a4 4 0 0 0-3-3.87"),tags$path(d="M16 3.13a4 4 0 0 1 0 7.75"))),
              div(class="kpi-label","Total Students"), div(class="kpi-number",id="heroTotal","-"), div(class="kpi-trend info","enrolled")),
            div(class="hero-kpi",
              div(class="kpi-icon green", tags$svg(xmlns="http://www.w3.org/2000/svg",width="18",height="18",viewBox="0 0 24 24",fill="none",stroke="currentColor",`stroke-width`="2",`stroke-linecap`="round",`stroke-linejoin`="round",
                tags$polyline(points="22 12 18 12 15 21 9 3 6 12 2 12"))),
              div(class="kpi-label","Avg Exam Score"), div(class="kpi-number",id="heroScore","-"), div(class="kpi-trend up","out of 100")),
            div(class="hero-kpi",
              div(class="kpi-icon orange", tags$svg(xmlns="http://www.w3.org/2000/svg",width="18",height="18",viewBox="0 0 24 24",fill="none",stroke="currentColor",`stroke-width`="2",`stroke-linecap`="round",`stroke-linejoin`="round",
                tags$rect(x="3",y="4",width="18",height="18",rx="2",ry="2"),tags$line(x1="16",y1="2",x2="16",y2="6"),tags$line(x1="8",y1="2",x2="8",y2="6"),tags$line(x1="3",y1="10",x2="21",y2="10"))),
              div(class="kpi-label","Avg Attendance"), div(class="kpi-number",id="heroAttend","-"), div(class="kpi-trend up","class rate")),
            div(class="hero-kpi",
              div(class="kpi-icon purple", tags$svg(xmlns="http://www.w3.org/2000/svg",width="18",height="18",viewBox="0 0 24 24",fill="none",stroke="currentColor",`stroke-width`="2",`stroke-linecap`="round",`stroke-linejoin`="round",
                tags$path(d="M22 11.08V12a10 10 0 1 1-5.93-9.14"),tags$polyline(points="22 4 12 14.01 9 11.01"))),
              div(class="kpi-label","Students Placed"), div(class="kpi-number",id="heroPlaced","-"), div(class="kpi-trend up","placed"))
          )
        ),
        # Filter bar
        div(class="filter-bar",
          tags$label("Filter by Placement:"),
          selectInput("placement",NULL,choices=c("All","Placed","Not Placed"),selected="All",width="180px"),
          uiOutput("activeFilterUI")
        ),
        # Chart visibility toggle bar
        div(class="chart-toggle-bar",
          div(class="chart-toggle-label", "Show Charts:"),
          selectInput(
            "visibleCharts", label = NULL,
            choices  = c("All Charts"="all","Study Hours"="study","Attendance"="attend","Exam Score"="score","Placement"="placement"),
            selected = "all",
            width    = "220px"
          )
        ),
        # Dynamic chart grid
        uiOutput("chartGrid"),

        # Table
        div(class="table-card",
          div(class="table-header",
            div(class="section-title","Student Records"),
            uiOutput("tableCountUI")
          ),
          DTOutput("table")
        )
      )
    }
  })

  # ── CSV import ───────────────────────────────────────────────────────────────
  observeEvent(input$csvFile, {
    req(input$csvFile)
    tryCatch({
      csv_data <- read.csv(input$csvFile$datapath)
      output$csvPreview <- renderUI({
        div(div(class="preview-label",paste("Found",nrow(csv_data),"rows - preview of first 5:")),
            DTOutput("csvPreviewTable"),
            actionButton("importBtn",paste("Import All",nrow(csv_data),"Rows"),class="import-btn"))
      })
      output$csvPreviewTable <- renderDT({
        tryCatch(datatable(head(csv_data,5),options=list(dom="t",scrollX=TRUE),rownames=FALSE,class="stripe"),
                 error=function(e) datatable(data.frame(Error=e$message),options=list(dom="t"),rownames=FALSE))
      })
      output$importMsg <- renderUI({ NULL })
    }, error=function(e) {
      output$csvPreview <- renderUI({ div(class="alert-error",paste("Error reading CSV:",e$message)) })
    })
  })

  observeEvent(input$importBtn, {
    req(input$csvFile)
    tryCatch({
      csv_data <- read.csv(input$csvFile$datapath)
      csv_data$placement_status <- as.character(trimws(csv_data$placement_status))
      con <- getConnection(); on.exit(try(dbDisconnect(con),silent=TRUE))
      success <- 0L
      for (i in seq_len(nrow(csv_data))) {
        dbExecute(con, sprintf(
          "INSERT INTO students (study_hours,attendance,sleep_hours,internet_usage,assignments_completed,previous_score,exam_score,placement_status) VALUES (%f,%f,%f,%f,%d,%f,%f,'%s')",
          csv_data$study_hours[i],csv_data$attendance[i],csv_data$sleep_hours[i],csv_data$internet_usage[i],
          as.integer(csv_data$assignments_completed[i]),csv_data$previous_score[i],csv_data$exam_score[i],csv_data$placement_status[i]))
        success <- success + 1L
      }
      student_data(fetchData()); last_updated(format(Sys.time(),"%d %b %Y  %H:%M:%S"))
      output$importMsg  <- renderUI({ div(class="alert-success",paste(success,"students imported successfully!")) })
      output$csvPreview <- renderUI({ NULL })
    }, error=function(e) {
      output$importMsg <- renderUI({ div(class="alert-error",paste("Import failed:",e$message)) })
    })
  })

  # ── Manual insert ────────────────────────────────────────────────────────────
  observeEvent(input$submitBtn, {
    tryCatch({
      con <- getConnection(); on.exit(try(dbDisconnect(con),silent=TRUE))
      dbExecute(con, sprintf(
        "INSERT INTO students (study_hours,attendance,sleep_hours,internet_usage,assignments_completed,previous_score,exam_score,placement_status) VALUES (%f,%f,%f,%f,%d,%f,%f,'%s')",
        input$study_hours,input$attendance,input$sleep_hours,input$internet_usage,
        as.integer(input$assignments_completed),input$previous_score,input$exam_score,input$placement_status))
      student_data(fetchData()); last_updated(format(Sys.time(),"%d %b %Y  %H:%M:%S"))
      output$submitMsg <- renderUI({ div(class="alert-success","Student added successfully!") })
    }, error=function(e) {
      output$submitMsg <- renderUI({ div(class="alert-error",paste("Error:",e$message)) })
    })
  })

  # ── Delete ───────────────────────────────────────────────────────────────────
  observeEvent(input$deleteConfirmed, {
    id_to_delete <- as.integer(input$deleteConfirmed)
    tryCatch({
      con <- getConnection(); on.exit(try(dbDisconnect(con),silent=TRUE))
      dbExecute(con, sprintf("DELETE FROM students WHERE id = %d", id_to_delete))
      student_data(fetchData()); last_updated(format(Sys.time(),"%d %b %Y  %H:%M:%S"))
    }, error=function(e) { showNotification(paste("Delete failed:",e$message),type="error") })
  })

  # ── Recent table ─────────────────────────────────────────────────────────────
  output$recentTable <- renderDT({
    student_data()
    tryCatch({
      con <- getConnection(); on.exit(try(dbDisconnect(con),silent=TRUE))
      recent <- dbGetQuery(con,"SELECT * FROM students ORDER BY id DESC LIMIT 10")
      if (nrow(recent)==0) return(datatable(data.frame(Message="No students in database yet."),options=list(dom="t"),rownames=FALSE))
      recent$placement_status <- as.character(trimws(recent$placement_status))
      recent$Delete <- paste0('<button class="delete-btn" data-id="',recent$id,'">Delete</button>')
      datatable(recent,escape=FALSE,options=list(pageLength=5,dom="tip",scrollX=TRUE),rownames=FALSE,class="stripe hover")
    }, error=function(e) {
      datatable(data.frame(Notice=paste("Could not load recent students:",e$message)),options=list(dom="t"),rownames=FALSE)
    })
  })
}

shinyApp(ui, server, options = list(port = 3838, host = "0.0.0.0"))
