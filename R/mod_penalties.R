# ============================================================
# Beer League IQ | Penalty Statistics
# R/mod_penalties.R
# ============================================================
# Requires shiny, bslib, DBI, and the existing Supabase pool.
# All queries are read-only. Data loads on first visit and is
# cached for the user's session. No season ID is hard-coded.

mod_penalties_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "container-fluid py-4 penalty-stats-page",
    shiny::tags$style(shiny::HTML("\n      .penalty-stats-page .penalty-view-buttons { display:grid; grid-template-columns:repeat(3,minmax(0,1fr)); gap:8px; margin-bottom:20px; }\n      .penalty-stats-page .penalty-view-buttons .btn { width:100%; border-radius:9px; font-weight:600; padding:11px 6px; }\n      .penalty-stats-page .penalty-choice-buttons { display:flex; flex-wrap:wrap; gap:8px; margin:10px 0 18px; }\n      .penalty-stats-page .penalty-choice-buttons .btn { border-radius:9px; font-size:.86rem; font-weight:600; padding:9px 13px; white-space:normal; }\n      .penalty-stats-page .penalty-metrics { display:grid; grid-template-columns:repeat(4,minmax(0,1fr)); gap:12px; margin:0 0 18px; }\n      .penalty-stats-page .penalty-metric { background:#262626; border:1px solid #414850; border-radius:10px; padding:17px; min-width:0; }\n      .penalty-stats-page .penalty-metric-label { color:#aab4c4; font-size:.78rem; margin-bottom:8px; }\n      .penalty-stats-page .penalty-metric-value { color:#4DA3FF; font-size:1.65rem; font-weight:700; font-variant-numeric:tabular-nums; }\n      .penalty-stats-page .penalty-metric-note { color:#aab4c4; font-size:.72rem; margin-top:5px; }\n      .penalty-stats-page .penalty-grid { display:grid; grid-template-columns:repeat(2,minmax(0,1fr)); gap:16px; margin-bottom:16px; }\n      .penalty-stats-page .penalty-table-wrap { overflow-x:auto; }\n      .penalty-stats-page .penalty-table { width:100%; font-size:.86rem; font-variant-numeric:tabular-nums; }\n      .penalty-stats-page .penalty-table th { color:#aab4c4; font-size:.75rem; white-space:nowrap; }\n      .penalty-stats-page .penalty-table td { padding:10px 8px; vertical-align:middle; }\n      .penalty-stats-page .penalty-table th:not(:first-child), .penalty-stats-page .penalty-table td:not(:first-child) { text-align:right; }\n      .penalty-stats-page .penalty-note { color:#aab4c4; font-size:.78rem; margin:9px 0 16px; }\n      .penalty-stats-page .penalty-bar-list { display:grid; gap:13px; padding:7px 0; }\n      .penalty-stats-page .penalty-bar-head { display:flex; justify-content:space-between; gap:12px; font-size:.83rem; margin-bottom:5px; }\n      .penalty-stats-page .penalty-bar-head span:last-child { color:#cbd5e1; font-variant-numeric:tabular-nums; white-space:nowrap; }\n      .penalty-stats-page .penalty-track { background:#343d48; border-radius:6px; height:9px; overflow:hidden; }\n      .penalty-stats-page .penalty-fill { height:100%; background:#4DA3FF; border-radius:6px; }\n      .penalty-stats-page .penalty-loading-container { position:relative; min-height:260px; }
      .penalty-stats-page .penalty-loading { position:absolute; z-index:5; inset:0; background:#222; padding:42px 24px; text-align:center; }
      .penalty-stats-page .penalty-loading-icon { font-size:2.5rem; color:#4DA3FF; margin-bottom:18px; }
      .penalty-stats-page .penalty-loading p { color:#aab4c4; font-size:.9rem; margin-bottom:24px; }
      .penalty-stats-page .penalty-loading .progress { background:#2b3542; border-radius:6px; }
      .penalty-stats-page .penalty-time-chart { min-height:245px; }
      @media(max-width:800px) { .penalty-stats-page .penalty-metrics { grid-template-columns:repeat(2,minmax(0,1fr)); } .penalty-stats-page .penalty-grid { grid-template-columns:1fr; } }\n      @media(max-width:450px) { .penalty-stats-page .penalty-metric-value { font-size:1.35rem; } .penalty-stats-page .penalty-table { font-size:.78rem; } }\n    ")),
    shiny::tags$script(shiny::HTML(sprintf("
      (function() {
        const overlayId = '%s';
        const wrapperId = '%s';
        function initialize() {
          const overlay = document.getElementById(overlayId);
          const wrapper = document.getElementById(wrapperId);
          if (!overlay || !wrapper || overlay.dataset.initialized) return;
          overlay.dataset.initialized = 'true';
          function update() {
            if (wrapper.querySelector('.penalty-ready-marker')) overlay.style.display = 'none';
          }
          new MutationObserver(update).observe(wrapper, {childList:true, subtree:true});
          update();
        }
        const timer = setInterval(function() {
          initialize();
          const overlay = document.getElementById(overlayId);
          if (overlay && overlay.dataset.initialized) clearInterval(timer);
        }, 100);
      })();
    ", ns("penalties_loading_overlay"), ns("penalties_content_wrapper")))),
    shiny::h2("Penalty Statistics", class = "mb-2"),
    shiny::p(class = "text-muted mb-4",
             "Explore penalties, penalty minutes, game start times, and player discipline across the league."),
    shiny::uiOutput(ns("view_buttons")),
    shiny::uiOutput(ns("filters")),
    shiny::uiOutput(ns("status")),
    shiny::div(class = "penalty-loading-container",
      shiny::div(id = ns("penalties_loading_overlay"), class = "penalty-loading",
        shiny::div(class = "penalty-loading-icon", shiny::icon("whistle")),
        shiny::h5("Loading penalty statistics"),
        shiny::p("Retrieving current-season penalty data and summaries..."),
        shiny::div(class = "progress", style = "height:6px;",
          shiny::div(class = "progress-bar progress-bar-striped progress-bar-animated bg-info",
                     role = "progressbar", style = "width:100%;"))),
      shiny::div(id = ns("penalties_content_wrapper"),
        shiny::uiOutput(ns("ready_marker")),
        shiny::uiOutput(ns("metrics")),
        shiny::uiOutput(ns("content")))),
    shiny::div(class = "penalty-note",
               "All statistics are descriptive. Differences in penalties called do not establish officiating bias. ",
               "Only games eligible for the validated penalty summaries are included.")
  )
}

mod_penalties_server <- function(id, pool, active = shiny::reactive(TRUE)) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    current_view <- shiny::reactiveVal("league")
    selected_division <- shiny::reactiveVal(NULL)
    selected_team <- shiny::reactiveVal(NULL)
    loaded <- shiny::reactiveVal(NULL)
    load_error <- shiny::reactiveVal(NULL)

    # A view query is restricted to the same current-season convention
    # used by the other app modules. PostgreSQL views remain unchanged.
    get_view <- function(view, season_id) {
      allowed <- c("vw_penalty_game_stats", "vw_penalty_league_summary",
                   "vw_penalty_division_summary", "vw_penalty_team_summary",
                   "vw_penalty_type_summary", "vw_player_penalty_summary")
      stopifnot(view %in% allowed)
      sql <- paste0("SELECT * FROM public.", view, " WHERE season_id = $1")
      DBI::dbGetQuery(pool, sql, params = list(season_id))
    }

    shiny::observeEvent(active(), {
      if (!isTRUE(active()) || !is.null(loaded())) return()
      tryCatch({
        season <- DBI::dbGetQuery(pool,
          "SELECT id, season_name FROM public.seasons WHERE season_name = 'Current' ORDER BY id DESC LIMIT 1")
        if (!nrow(season)) stop("No season named 'Current' was found.")
        sid <- season$id[[1]]
        views <- c(game = "vw_penalty_game_stats",
                   league = "vw_penalty_league_summary",
                   division = "vw_penalty_division_summary",
                   team = "vw_penalty_team_summary",
                   type = "vw_penalty_type_summary",
                   player = "vw_player_penalty_summary")
        data <- lapply(views, get_view, season_id = sid)
        data$season_id <- sid
        loaded(data)
        load_error(NULL)
      }, error = function(e) {
        load_error(conditionMessage(e))
      })
    }, ignoreInit = FALSE)

    dataset <- shiny::reactive({
      shiny::req(isTRUE(active()))
      shiny::req(loaded())
      loaded()
    })

    # Accommodate differences in column labels across the validated views.
    # We never substitute a guessed metric when a required field is absent.
    colname <- function(d, candidates) {
      hit <- intersect(candidates, names(d))
      if (length(hit)) hit[[1]] else NULL
    }
    numeric_col <- function(d, candidates, default = NA_real_) {
      nm <- colname(d, candidates)
      if (is.null(nm)) rep(default, nrow(d)) else as.numeric(d[[nm]])
    }
    text_col <- function(d, candidates, default = "") {
      nm <- colname(d, candidates)
      if (is.null(nm)) rep(default, nrow(d)) else as.character(d[[nm]])
    }
    sum0 <- function(x) sum(x, na.rm = TRUE)
    rate <- function(n, d) if (is.finite(d) && d > 0) n / d else NA_real_
    fmt <- function(x, digits = 1L) {
      if (length(x) != 1L || !is.finite(x)) return("N/A")
      formatC(x, format = "f", digits = digits, big.mark = ",")
    }
    whole <- function(x) fmt(x, 0L)

    # The game view has two team-side rows per game. Deduplicate game IDs
    # for league-wide counts, while using team-side totals for penalties.
    game_data <- shiny::reactive({
      g <- dataset()$game
      if ("game_id" %in% names(g)) g else g[FALSE, , drop = FALSE]
    })
    game_team_penalties <- function(g) numeric_col(g,
      c("team_penalties", "penalties", "penalty_count", "penalties_called"))
    game_team_pim <- function(g) numeric_col(g,
      c("team_pim", "pim", "penalty_minutes", "total_pim"))

    divisions <- shiny::reactive({
      g <- game_data()
      if (!all(c("division_id", "division_name") %in% names(g)))
        return(stats::setNames(character(), character()))
      x <- unique(g[, c("division_id", "division_name"), drop = FALSE])
      x <- x[!is.na(x$division_id), , drop = FALSE]
      x <- x[order(x$division_name), , drop = FALSE]
      stats::setNames(as.character(x$division_id), as.character(x$division_name))
    })
    shiny::observe({
      ids <- unname(divisions())
      if (length(ids) && (is.null(selected_division()) ||
                          !selected_division() %in% ids)) selected_division(ids[[1]])
    })
    teams <- shiny::reactive({
      g <- game_data()
      shiny::req(selected_division())
      if (!all(c("division_id", "team_id", "team_name") %in% names(g)))
        return(stats::setNames(character(), character()))
      g <- g[as.character(g$division_id) == selected_division(), , drop = FALSE]
      x <- unique(g[, c("team_id", "team_name"), drop = FALSE])
      x <- x[!is.na(x$team_id), , drop = FALSE]
      x <- x[order(x$team_name), , drop = FALSE]
      stats::setNames(as.character(x$team_id), as.character(x$team_name))
    })
    shiny::observe({
      ids <- unname(teams())
      if (!length(ids)) selected_team(NULL)
      else if (is.null(selected_team()) || !selected_team() %in% ids)
        selected_team(ids[[1]])
    })

    output$view_buttons <- shiny::renderUI({
      views <- c(league = "League", division = "Division", team = "Team")
      shiny::div(class = "penalty-view-buttons", lapply(names(views), function(v) {
        shiny::actionButton(ns(paste0("view_", v)), views[[v]],
          class = if (identical(current_view(), v)) "btn-primary" else "btn-outline-primary")
      }))
    })
    for (v in c("league", "division", "team")) local({
      key <- v
      shiny::observeEvent(input[[paste0("view_", key)]], current_view(key), ignoreInit = TRUE)
    })
    output$filters <- shiny::renderUI({
      if (current_view() == "league") return(NULL)
      d <- divisions()
      division_buttons <- lapply(seq_along(d), function(i) {
        val <- unname(d[[i]])
        shiny::actionButton(ns(paste0("division_", val)), names(d)[[i]],
          class = if (identical(selected_division(), val)) "btn-primary" else "btn-outline-primary")
      })
      team_ui <- NULL
      if (current_view() == "team") {
        t <- teams()
        team_ui <- shiny::tagList(shiny::h5("Team"),
          shiny::div(class = "penalty-choice-buttons", lapply(seq_along(t), function(i) {
            val <- unname(t[[i]])
            shiny::actionButton(ns(paste0("team_", val)), names(t)[[i]],
              class = if (identical(selected_team(), val)) "btn-primary" else "btn-outline-primary")
          })))
      }
      shiny::tagList(shiny::h5("Division"),
        shiny::div(class = "penalty-choice-buttons", division_buttons), team_ui)
    })
    registered_divisions <- shiny::reactiveVal(character())
    shiny::observe({
      ids <- unname(divisions())
      fresh <- setdiff(ids, registered_divisions())
      registered_divisions(union(registered_divisions(), fresh))
      lapply(fresh, function(value) local({
        key <- value
        shiny::observeEvent(input[[paste0("division_", key)]], selected_division(key), ignoreInit = TRUE)
      }))
    })
    registered_teams <- shiny::reactiveVal(character())
    shiny::observe({
      ids <- unname(teams())
      fresh <- setdiff(ids, registered_teams())
      registered_teams(union(registered_teams(), fresh))
      lapply(fresh, function(value) local({
        key <- value
        shiny::observeEvent(input[[paste0("team_", key)]], selected_team(key), ignoreInit = TRUE)
      }))
    })

    scoped_games <- shiny::reactive({
      g <- game_data()
      if (current_view() != "league") {
        shiny::req(selected_division())
        g <- g[as.character(g$division_id) == selected_division(), , drop = FALSE]
      }
      if (current_view() == "team") {
        shiny::req(selected_team())
        g <- g[as.character(g$team_id) == selected_team(), , drop = FALSE]
      }
      g
    })
    output$ready_marker <- shiny::renderUI({
      if (!is.null(loaded()) || !is.null(load_error()))
        shiny::span(class = "penalty-ready-marker", style = "display:none;")
      else NULL
    })
    output$status <- shiny::renderUI({
      if (!is.null(load_error())) return(shiny::div(class = "alert alert-danger",
        shiny::strong("Unable to load penalty statistics: "), load_error()))
      if (is.null(loaded())) return(shiny::div(class = "text-muted mb-4",
        shiny::icon("spinner", class = "fa-spin"), " Loading current-season penalty statistics..."))
      NULL
    })
    metric_card <- function(label, value, note = "") {
      shiny::div(class = "penalty-metric",
        shiny::div(class = "penalty-metric-label", label),
        shiny::div(class = "penalty-metric-value", value),
        shiny::div(class = "penalty-metric-note", note))
    }
    output$metrics <- shiny::renderUI({
      g <- scoped_games()
      if (!nrow(g)) return(NULL)
      gp <- length(unique(as.character(g$game_id)))
      calls <- game_team_penalties(g)
      pim <- game_team_pim(g)
      # Team view has one side per game; league/division have two.
      # Per-game total calls are summed over both sides.
      shiny::div(class = "penalty-metrics",
        metric_card("Completed games", whole(gp), "Eligible scored games"),
        metric_card("Total penalties", if (all(is.na(calls))) "N/A" else whole(sum0(calls)),
                    if (current_view() == "team") "Assessed to selected team" else "Both teams combined"),
        metric_card("Penalties / game", if (all(is.na(calls))) "N/A" else fmt(rate(sum0(calls), gp), 2),
                    if (current_view() == "team") "Selected team" else "Both teams combined"),
        metric_card("PIM / game", if (all(is.na(pim))) "N/A" else fmt(rate(sum0(pim), gp), 1),
                    "Penalty minutes per game")
      )
    })

    # Safe compact table, preserving actual view column labels.
    data_table <- function(d, fields, labels, digits = character(), limit = 30L) {
      keep <- intersect(fields, names(d))
      if (!nrow(d) || !length(keep)) return(shiny::p(class = "text-muted", "No statistics available."))
      d <- utils::head(d[, keep, drop = FALSE], limit)
      names(d) <- unname(labels[keep])
      for (nm in names(d)) {
        if (inherits(d[[nm]], "integer64")) d[[nm]] <- as.numeric(d[[nm]])
        if (is.numeric(d[[nm]])) {
          dec <- if (nm %in% digits) 2L else 0L
          d[[nm]] <- vapply(d[[nm]], function(x) fmt(x, dec), character(1))
        } else {
          d[[nm]] <- ifelse(is.na(d[[nm]]), "N/A", as.character(d[[nm]]))
        }
      }
      shiny::div(class = "penalty-table-wrap", shiny::tags$table(
        class = "table table-dark table-hover penalty-table",
        shiny::tags$thead(shiny::tags$tr(lapply(names(d), shiny::tags$th))),
        shiny::tags$tbody(lapply(seq_len(nrow(d)), function(i)
          shiny::tags$tr(lapply(unname(as.list(d[i, , drop = FALSE])), shiny::tags$td))))
      ))
    }
    section <- function(title, content, note = NULL) {
      bslib::card(bslib::card_header(shiny::strong(title)),
        bslib::card_body(content, if (!is.null(note)) shiny::p(class = "penalty-note", note)))
    }
    bars <- function(names, values, digits = 2L) {
      valid <- !is.na(values) & is.finite(values)
      names <- names[valid]; values <- values[valid]
      if (!length(values)) return(shiny::p(class = "text-muted", "No data available."))
      maximum <- max(values, 0)
      shiny::div(class = "penalty-bar-list", lapply(seq_along(values), function(i) {
        width <- if (maximum > 0) 100 * values[[i]] / maximum else 0
        shiny::div(
          shiny::div(class = "penalty-bar-head", shiny::span(names[[i]]), shiny::span(fmt(values[[i]], digits))),
          shiny::div(class = "penalty-track", shiny::div(class = "penalty-fill",
            style = sprintf("width:%.2f%%", width)))
        )
      }))
    }
    # Aggregate raw team-side game rows so every division uses the
    # same denominator: unique games, not team appearances.
    division_rates <- shiny::reactive({
      g <- game_data()
      if (!nrow(g)) return(data.frame())
      groups <- split(g, as.character(g$division_id))
      do.call(rbind, lapply(groups, function(x) {
        p <- game_team_penalties(x); m <- game_team_pim(x)
        n <- length(unique(as.character(x$game_id)))
        data.frame(division_id = as.character(x$division_id[[1]]),
          division_name = as.character(x$division_name[[1]]),
          games = n, penalties = if (all(is.na(p))) NA_real_ else sum0(p),
          pim = if (all(is.na(m))) NA_real_ else sum0(m),
          penalties_per_game = if (all(is.na(p))) NA_real_ else rate(sum0(p), n),
          pim_per_game = if (all(is.na(m))) NA_real_ else rate(sum0(m), n))
      }))
    })
    time_rates <- shiny::reactive({
      g <- scoped_games()
      if (!nrow(g)) return(data.frame())
      hour <- numeric_col(g, c("start_hour"))
      if (all(is.na(hour)) && "game_time" %in% names(g)) {
        hour <- suppressWarnings(as.numeric(substr(as.character(g$game_time), 1, 2)))
      }
      g$hour_for_display <- hour
      g <- g[!is.na(g$hour_for_display), , drop = FALSE]
      if (!nrow(g)) return(data.frame())
      groups <- split(g, g$hour_for_display)
      d <- do.call(rbind, lapply(groups, function(x) {
        n <- length(unique(as.character(x$game_id)))
        p <- game_team_penalties(x)
        data.frame(hour = x$hour_for_display[[1]], games = n,
          penalties_per_game = if (all(is.na(p))) NA_real_ else rate(sum0(p), n))
      }))
      d[order(d$hour), , drop = FALSE]
    })
    type_data <- shiny::reactive({
      d <- dataset()$type
      if (current_view() != "league" && "division_id" %in% names(d)) {
        shiny::req(selected_division())
        d <- d[as.character(d$division_id) == selected_division(), , drop = FALSE]
      } else if (current_view() == "league" && "division_id" %in% names(d)) {
        # Prefer league-wide records, avoiding double-counting division rows.
        d <- d[is.na(d$division_id), , drop = FALSE]
      }
      d
    })
    penalty_types <- shiny::reactive({
      d <- type_data()
      if (!nrow(d)) return(data.frame())
      name <- text_col(d, c("penalty_type", "penalty_name", "penalty_description", "type_name"))
      calls <- numeric_col(d, c("total_calls", "penalty_calls", "calls", "total_penalties", "penalty_count"))
      if (all(is.na(calls)) || all(!nzchar(name))) return(data.frame())
      x <- stats::aggregate(calls, by = list(type = name), FUN = sum0)
      names(x)[[2]] <- "calls"
      x <- x[order(-x$calls, x$type), , drop = FALSE]
      utils::head(x, 5L)
    })
    team_comparison <- shiny::reactive({
      g <- game_data()
      shiny::req(selected_division())
      g <- g[as.character(g$division_id) == selected_division(), , drop = FALSE]
      if (!nrow(g)) return(data.frame())
      groups <- split(g, as.character(g$team_id))
      x <- do.call(rbind, lapply(groups, function(d) {
        n <- length(unique(as.character(d$game_id)))
        p <- game_team_penalties(d); m <- game_team_pim(d)
        data.frame(team_id = as.character(d$team_id[[1]]),
          team_name = as.character(d$team_name[[1]]), games = n,
          penalties = if (all(is.na(p))) NA_real_ else sum0(p),
          pim = if (all(is.na(m))) NA_real_ else sum0(m),
          penalties_per_game = if (all(is.na(p))) NA_real_ else rate(sum0(p), n),
          pim_per_game = if (all(is.na(m))) NA_real_ else rate(sum0(m), n))
      }))
      x[order(-x$penalties_per_game, x$team_name, na.last = TRUE), , drop = FALSE]
    })
    player_data <- shiny::reactive({
      d <- dataset()$player
      shiny::req(selected_division(), selected_team())
      d <- d[as.character(d$division_id) == selected_division() &
               as.character(d$team_id) == selected_team(), , drop = FALSE]
      d
    })

    output$time_chart <- shiny::renderPlot({
      d <- time_rates()
      d <- d[is.finite(d$hour) & is.finite(d$penalties_per_game), , drop = FALSE]
      if (!nrow(d)) return(invisible(NULL))
      d <- d[order(d$hour), , drop = FALSE]
      old <- graphics::par(c("mar", "bg", "fg", "col.axis", "col.lab"))
      on.exit(graphics::par(old), add = TRUE)
      graphics::par(mar = c(4.0, 4.2, 1.0, 1.0), bg = "#262626",
                    fg = "#cbd5e1", col.axis = "#aab4c4", col.lab = "#aab4c4")
      ymax <- max(d$penalties_per_game, 0, na.rm = TRUE)
      if (!is.finite(ymax) || ymax <= 0) ymax <- 1
      xrange <- range(d$hour)
      if (diff(xrange) == 0) xrange <- xrange + c(-0.5, 0.5)
      graphics::plot(NA, xlim = xrange, ylim = c(0, ymax * 1.15),
                     xlab = "Scheduled start time", ylab = "Penalties / game",
                     axes = FALSE, type = "n")
      graphics::axis(1, at = d$hour, labels = sprintf("%02d:00", as.integer(d$hour)),
                     cex.axis = .75)
      graphics::axis(2, las = 1, cex.axis = .8)
      graphics::abline(h = graphics::axTicks(2), col = "#39424d", lty = 3)
      graphics::lines(d$hour, d$penalties_per_game, col = "#4DA3FF", lwd = 2.5)
      graphics::points(d$hour, d$penalties_per_game, col = "#4DA3FF", pch = 16, cex = 1.15)
      graphics::box(col = "#46505d")
    }, res = 110)
    time_card <- function(note = NULL) {
      section("Penalties by game start time",
        if (nrow(time_rates())) shiny::div(class = "penalty-time-chart",
          shiny::plotOutput(ns("time_chart"), height = "245px", width = "100%"))
        else shiny::p(class = "text-muted", "No start-time data."), note)
    }

    output$content <- shiny::renderUI({
      shiny::req(dataset())
      view <- current_view()
      if (view == "league") {
        d <- division_rates()
        if (nrow(d)) d <- d[order(-d$penalties_per_game, d$division_name, na.last = TRUE), , drop = FALSE]
        times <- time_rates()
        types <- penalty_types()
        return(shiny::tagList(
          section("Penalty rates by division",
            if (nrow(d)) bars(d$division_name, d$penalties_per_game) else shiny::p("No division data."),
            "Total penalties assessed to both teams, divided by eligible games."),
          time_card("Rates are descriptive and may reflect differences in division schedules."),
          section("Five most common penalty types",
            if (nrow(types)) bars(types$type, types$calls, 0L)
            else shiny::p(class = "text-muted", "Penalty type fields are unavailable.")),
          section("Division summary", data_table(d,
            c("division_name", "games", "penalties", "pim", "penalties_per_game", "pim_per_game"),
            c(division_name = "Division", games = "GP", penalties = "Penalties", pim = "PIM",
              penalties_per_game = "Pen/G", pim_per_game = "PIM/G"),
            digits = c("Pen/G", "PIM/G")))
        ))
      }
      if (view == "division") {
        d <- team_comparison()
        times <- time_rates()
        types <- penalty_types()
        return(shiny::tagList(
          section("Team penalty rates",
            if (nrow(d)) bars(d$team_name, d$penalties_per_game)
            else shiny::p(class = "text-muted", "No team data."),
            "Penalties assessed to each team per game played."),
          time_card(),
          section("Five most common penalty types",
            if (nrow(types)) bars(types$type, types$calls, 0L)
            else shiny::p(class = "text-muted", "Penalty type fields are unavailable.")),
          section("Team comparison", data_table(d,
            c("team_name", "games", "penalties", "pim", "penalties_per_game", "pim_per_game"),
            c(team_name = "Team", games = "GP", penalties = "Penalties", pim = "PIM",
              penalties_per_game = "Pen/G", pim_per_game = "PIM/G"),
            digits = c("Pen/G", "PIM/G")))
        ))
      }
      g <- scoped_games()
      p <- player_data()
      if (nrow(p)) p <- p[order(p$most_penalized_rank, na.last = TRUE), , drop = FALSE]
      times <- time_rates()
      # Differential: opponent penalties minus team penalties. Positive
      # values mean opponents were assessed more penalties.
      opponent <- numeric_col(g, c("opponent_penalties", "opponent_penalty_count"))
      own <- game_team_penalties(g)
      differential <- if (all(is.na(opponent)) || all(is.na(own))) NA_real_
                      else sum0(opponent) - sum0(own)
      eligible <- p[!is.na(p$least_penalized_rank), , drop = FALSE]
      if (nrow(eligible)) eligible <- eligible[order(eligible$least_penalized_rank), , drop = FALSE]
      shiny::tagList(
        section("Team versus opponents", shiny::tagList(
          shiny::div(class = "penalty-metrics", style = "margin:0;grid-template-columns:repeat(3,minmax(0,1fr));",
            metric_card("Team penalties", if (all(is.na(own))) "N/A" else whole(sum0(own))),
            metric_card("Opponent penalties", if (all(is.na(opponent))) "N/A" else whole(sum0(opponent))),
            metric_card("Differential", if (is.na(differential)) "N/A" else sprintf("%+.0f", differential),
                        "Opponent minus team")),
          shiny::p(class = "penalty-note", "Positive differential means opponents received more penalties."))),
        time_card(),
        shiny::div(class = "penalty-grid",
          section("Most penalized players", data_table(p,
            c("player_name", "games_played", "total_penalties", "total_pim", "penalties_per_game"),
            c(player_name = "Player", games_played = "GP", total_penalties = "Penalties",
              total_pim = "PIM", penalties_per_game = "Pen/G"),
            digits = "Pen/G", limit = 3L)),
          section("Least penalized players (5+ GP)", data_table(eligible,
            c("player_name", "games_played", "total_penalties", "penalties_per_game"),
            c(player_name = "Player", games_played = "GP", total_penalties = "Penalties",
              penalties_per_game = "Pen/G"), digits = "Pen/G", limit = 3L)))
      )
    })
  })
}
