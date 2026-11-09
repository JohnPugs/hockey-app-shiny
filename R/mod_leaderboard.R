# Beer League IQ: Weekly and Season Leaderboards
# Drop-in replacement for R/mod_leaderboard.R.
# Uses shiny, DBI, pool, and base R graphics. No new database objects required.

blq_leaderboard_query <- function(con) {
  DBI::dbGetQuery(con, "
    WITH current_season AS (
      SELECT id FROM public.seasons WHERE season_name = 'Current'
    ),
    frozen_predictions AS (
      SELECT DISTINCT ON (gp.game_id)
        gp.game_id, gp.p_home_win, gp.p_tie, gp.p_away_win,
        gp.snapshot_date, gp.predicted_at
      FROM public.game_predictions gp
      JOIN public.games g ON g.id = gp.game_id
      JOIN current_season cs ON cs.id = g.season_id
      WHERE gp.snapshot_date <= g.game_date
        AND (gp.predicted_at AT TIME ZONE 'America/Los_Angeles')::date <= g.game_date
      ORDER BY gp.game_id, gp.snapshot_date ASC, gp.predicted_at ASC, gp.id ASC
    )
    SELECT
      up.user_id::text AS user_id,
      COALESCE(NULLIF(BTRIM(pr.display_name), ''),
               'Player ' || LEFT(up.user_id::text, 8)) AS display_name,
      g.id AS game_id, g.game_date,
      DATE_TRUNC('week', g.game_date::timestamp)::date AS week_start,
      COALESCE(NULLIF(g.home_team_name_raw, ''), 'Home') AS home_team,
      COALESCE(NULLIF(g.away_team_name_raw, ''), 'Away') AS away_team,
      g.home_score, g.away_score,
      up.picked_outcome,
      fp.p_home_win, fp.p_tie, fp.p_away_win,
      CASE WHEN g.home_score > g.away_score THEN 'home'
           WHEN g.home_score < g.away_score THEN 'away'
           ELSE 'tie' END AS actual_outcome,
      CASE WHEN fp.p_home_win >= fp.p_tie AND fp.p_home_win >= fp.p_away_win THEN 'home'
           WHEN fp.p_tie >= fp.p_home_win AND fp.p_tie >= fp.p_away_win THEN 'tie'
           ELSE 'away' END AS model_outcome
    FROM public.user_picks up
    JOIN public.games g ON g.id = up.game_id
    JOIN current_season cs ON cs.id = g.season_id
    JOIN frozen_predictions fp ON fp.game_id = g.id
    LEFT JOIN public.profiles pr ON pr.user_id = up.user_id
    WHERE g.has_final_score IS TRUE
      AND g.home_score IS NOT NULL AND g.away_score IS NOT NULL
      AND up.picked_outcome IN ('home', 'away', 'tie')
      AND up.created_at < (g.game_date::timestamp + COALESCE(g.game_time, TIME '00:00:00'))
          AT TIME ZONE 'America/Los_Angeles'
    ORDER BY g.game_date, g.id, up.user_id
  ")
}

blq_lb_aggregate <- function(d, by_week = FALSE) {
  if (!nrow(d)) return(data.frame())
  keys <- if (by_week) c('user_id', 'display_name', 'week_start') else c('user_id', 'display_name')
  groups <- split(seq_len(nrow(d)), interaction(d[keys], drop = TRUE, lex.order = TRUE))
  out <- lapply(groups, function(idx) {
    x <- d[idx, , drop = FALSE]
    n <- nrow(x)
    u <- sum(x$user_correct)
    m <- sum(x$model_correct)
    data.frame(user_id = x$user_id[1], display_name = x$display_name[1],
               week_start = if (by_week) as.Date(x$week_start[1]) else as.Date(NA),
               picks = n, user_correct = u, model_correct = m,
               user_accuracy = 100 * u / n, model_accuracy = 100 * m / n,
               advantage = 100 * (u - m) / n, net = u - m,
               stringsAsFactors = FALSE)
  })
  do.call(rbind, out)
}

blq_lb_pct <- function(x) sprintf('%.1f%%', x)
blq_lb_adv <- function(x) sprintf('%+.1f pp', x)
blq_lb_week <- function(x) {
  x <- as.Date(x)
  paste(format(x, '%b %d'), '-', format(x + 6, '%b %d, %Y'))
}
blq_lb_outcome <- function(side, home, away) {
  if (identical(side, 'home')) return(home)
  if (identical(side, 'away')) return(away)
  if (identical(side, 'tie')) return('Tie')
  'Unknown'
}

mod_leaderboard_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    id = ns('leaderboard_root'), class = 'container-fluid py-4 blq-leaderboard',
    shiny::tags$style(shiny::HTML(do.call(sprintf, c(list('
      #%s .blq-metrics {display:grid;grid-template-columns:repeat(3,minmax(0,1fr));gap:9px;margin:12px 0}
      #%s .blq-metric {background:#262626;border:1px solid #414850;border-radius:9px;padding:10px;text-align:center;min-width:0}
      #%s .blq-value {color:#4DA3FF;font-size:1.25rem;font-weight:700;font-variant-numeric:tabular-nums}
      #%s .blq-label {color:#aab4c4;font-size:.73rem;margin-top:3px}
      #%s .blq-panel {border:1px solid #414850;border-radius:12px;padding:15px;margin-bottom:18px}
      #%s .blq-heading {display:flex;justify-content:space-between;align-items:center;gap:10px;flex-wrap:wrap}
      #%s .blq-alpha {display:flex;flex-wrap:wrap;gap:5px;margin:12px 0}
      #%s .blq-alpha .btn {min-width:32px;padding:5px 8px;font-size:.78rem}
      #%s .blq-muted {color:#aab4c4;font-size:.8rem}
      #%s .blq-table-wrap {overflow-x:auto}
      #%s .blq-table {width:100%%;font-size:.86rem;font-variant-numeric:tabular-nums}
      #%s .blq-table th {color:#aab4c4;font-size:.75rem;white-space:nowrap}
      #%s .blq-table td {vertical-align:middle;padding:9px 7px}
      #%s .blq-table th:not(:nth-child(2)), #%s .blq-table td:not(:nth-child(2)) {text-align:right}
      #%s .blq-name {color:#4DA3FF;font-weight:600;text-decoration:none}
      #%s .blq-name:hover {text-decoration:underline}
      #%s .blq-positive {color:#53c68c;font-weight:700}
      #%s .blq-negative {color:#ff8585;font-weight:700}
      @media(max-width:540px){#%s .blq-metrics{gap:5px}#%s .blq-metric{padding:7px 3px}#%s .blq-value{font-size:1.05rem}}
    '), as.list(rep(ns('leaderboard_root'), 22)))))),
    shiny::div(class = 'blq-panel',
      shiny::div(class = 'blq-heading',
        shiny::h4('Model Performance', style = 'margin:0'),
        shiny::actionButton(ns('validation'), 'Validation results',
                            icon = shiny::icon('chart-column'), class = 'btn-sm btn-outline-info')
      ),
      shiny::uiOutput(ns('performance_content'))
    ),
    shiny::div(class = 'blq-heading mb-2',
      shiny::h3('Leaderboard', style = 'margin:0'),
      shiny::span('Compete against the model', class = 'blq-muted')
    ),
    shiny::div(class = 'd-flex gap-2 mb-3',
      shiny::actionButton(ns('view_weekly'), 'Weekly', class = 'btn-primary'),
      shiny::actionButton(ns('view_season'), 'Season', class = 'btn-outline-primary')
    ),
    shiny::uiOutput(ns('week_selector')),
    shiny::uiOutput(ns('navigation')),
    shiny::uiOutput(ns('leaderboard_table')),
    shiny::uiOutput(ns('detail_card'))
  )
}

mod_leaderboard_server <- function(id, active = shiny::reactive(TRUE), db = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    owns_pool <- is.null(db)
    con <- if (owns_pool) get_db_pool() else if (shiny::is.reactive(db)) db() else db
    if (owns_pool) session$onSessionEnded(function() pool::poolClose(con))

    current_view <- shiny::reactiveVal('weekly')
    current_letter <- shiny::reactiveVal('TOP')
    selected_user <- shiny::reactiveVal(NULL)
    selected_week <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$view_weekly, {
      current_view('weekly'); current_letter('TOP'); selected_user(NULL)
    })
    shiny::observeEvent(input$view_season, {
      current_view('season'); current_letter('TOP'); selected_user(NULL)
    })

    # Refresh on page entry and periodically while open. Keeps completed games current.
    data <- shiny::reactive({
      shiny::req(isTRUE(active()))
      shiny::invalidateLater(5 * 60 * 1000, session)
      d <- blq_leaderboard_query(con)
      if (nrow(d)) {
        d$week_start <- as.Date(d$week_start)
        d$user_correct <- as.integer(d$picked_outcome == d$actual_outcome)
        d$model_correct <- as.integer(d$model_outcome == d$actual_outcome)
        d$net <- d$user_correct - d$model_correct
      }
      d
    })

    performance <- shiny::reactive({
      shiny::req(isTRUE(active()))
      shiny::invalidateLater(5 * 60 * 1000, session)
      get_model_performance(con)
    })

    output$performance_content <- shiny::renderUI({
      p <- tryCatch(performance(), error = function(e) NULL)
      if (is.null(p) || !nrow(p))
        return(shiny::p(class = 'blq-muted mt-2', 'Current-season model results are not available.'))
      x <- p[1, , drop = FALSE]
      metric <- function(v, label) shiny::div(class = 'blq-metric',
        shiny::div(class = 'blq-value', v), shiny::div(class = 'blq-label', label))
      shiny::div(class = 'blq-metrics',
        metric(blq_lb_pct(x$outcome_accuracy_pct), 'Outcome accuracy'),
        metric(blq_lb_pct(x$winner_accuracy_pct), 'Winner accuracy'),
        metric(format(x$games_evaluated, big.mark = ','), 'Games evaluated'))
    })

    shiny::observeEvent(input$validation, {
      shiny::showModal(shiny::modalDialog(
        title = 'Historical Model Validation',
        shiny::p('Held-out validation of the combined multinomial game prediction model.'),
        shiny::tags$table(class = 'table table-sm',
          shiny::tags$tbody(
            shiny::tags$tr(shiny::tags$th('Games evaluated'), shiny::tags$td('3,536')),
            shiny::tags$tr(shiny::tags$th('Outcome accuracy'), shiny::tags$td('56.2%')),
            shiny::tags$tr(shiny::tags$th('Winner accuracy'), shiny::tags$td('60.9%')),
            shiny::tags$tr(shiny::tags$th('Multiclass log loss'), shiny::tags$td('0.8983')),
            shiny::tags$tr(shiny::tags$th('Multiclass Brier score'), shiny::tags$td('0.5464'))
          )
        ),
        shiny::p(class = 'blq-muted', 'Historical holdout benchmarks are fixed; current-season performance is calculated separately from completed games.'),
        easyClose = TRUE, footer = shiny::modalButton('Close')
      ))
    })

    weeks <- shiny::reactive({
      d <- data()
      if (!nrow(d)) return(as.Date(character()))
      sort(unique(as.Date(d$week_start)), decreasing = TRUE)
    })
    
    
    # shiny::observe({
    #   w <- weeks()
    #   if (!length(w)) return()
    #   if (is.null(selected_week()) || !as.Date(selected_week()) %in% w)
    #     selected_week(as.character(w[1]))
    # })
    # output$week_selector <- shiny::renderUI({
    #   if (current_view() != 'weekly') return(NULL)
    #   w <- weeks()
    #   if (!length(w)) return(NULL)
    #   shiny::div(class = 'mb-3', shiny::selectInput(ns('week'), 'Week',
    #     choices = stats::setNames(as.character(w), vapply(w, blq_lb_week, character(1))),
    #     selected = selected_week(), width = '260px'))
    # })
    
    # Automatically select the most recent available week
    shiny::observe({
      
      w <- weeks()
      
      if (!length(w)) return()
      
      latest_week <- as.character(w[1])
      
      if (!identical(selected_week(), latest_week)) {
        selected_week(latest_week)
        selected_user(NULL)
      }
      
    })
    
    output$week_selector <- shiny::renderUI({
      
      if (current_view() != 'weekly') return(NULL)
      
      shiny::req(selected_week())
      
      shiny::div(
        class = 'mb-3',
        shiny::span(
          class = 'badge bg-secondary',
          style = 'font-size: 0.9rem; padding: 9px 13px;',
          blq_lb_week(as.Date(selected_week()))
        )
      )
      
    })
    
    # shiny::observeEvent(input$week, {
    #   selected_week(input$week); selected_user(NULL)
    # })

    rankings <- shiny::reactive({
      d <- data()
      if (!nrow(d)) return(data.frame())
      if (current_view() == 'weekly') {
        shiny::req(selected_week())
        d <- d[as.character(d$week_start) == selected_week(), , drop = FALSE]
      }
      a <- blq_lb_aggregate(d)
      if (!nrow(a)) return(a)
      minimum <- if (current_view() == 'weekly') 5L else 10L
      a <- a[a$picks >= minimum, , drop = FALSE]
      if (!nrow(a)) return(a)
      a <- a[order(-a$advantage, -a$net, -a$picks, tolower(a$display_name)), , drop = FALSE]
      a$rank <- seq_len(nrow(a))
      a
    })

    output$navigation <- shiny::renderUI({
      buttons <- lapply(c('TOP', LETTERS), function(letter) {
        shiny::actionButton(ns(paste0('letter_', letter)),
          if (letter == 'TOP') 'Top 10' else letter,
          class = if (identical(current_letter(), letter)) 'btn-primary' else 'btn-outline-secondary')
      })
      shiny::div(class = 'blq-alpha', do.call(shiny::tagList, buttons))
    })
    for (letter in c('TOP', LETTERS)) local({
      l <- letter
      shiny::observeEvent(input[[paste0('letter_', l)]], {
        current_letter(l); selected_user(NULL)
      }, ignoreInit = TRUE)
    })

    displayed <- shiny::reactive({
      a <- rankings()
      if (!nrow(a)) return(a)
      if (current_letter() == 'TOP') return(utils::head(a, 10L))
      initials <- toupper(substr(trimws(a$display_name), 1, 1))
      a[initials == current_letter(), , drop = FALSE]
    })

    output$leaderboard_table <- shiny::renderUI({
      a <- displayed()
      if (!nrow(a)) return(shiny::div(class = 'blq-panel blq-muted',
        if (current_view() == 'weekly') 'No qualifying users for this week (5 completed picks required).'
        else 'No qualifying users yet (10 completed season picks required).'))
      head <- shiny::tags$tr(lapply(c('Rank', 'User', 'Picks', 'User %', 'Model %', 'Advantage'), shiny::tags$th))
      rows <- lapply(seq_len(nrow(a)), function(i) {
        x <- a[i, ]
        shiny::tags$tr(
          shiny::tags$td(x$rank),
          shiny::tags$td(shiny::actionLink(ns(paste0('user_', i)), x$display_name, class = 'blq-name')),
          shiny::tags$td(x$picks),
          shiny::tags$td(blq_lb_pct(x$user_accuracy)),
          shiny::tags$td(blq_lb_pct(x$model_accuracy)),
          shiny::tags$td(class = if (x$advantage > 0) 'blq-positive' else if (x$advantage < 0) 'blq-negative' else '',
                         blq_lb_adv(x$advantage))
        )
      })
      shiny::div(class = 'blq-panel',
        shiny::div(class = 'blq-heading mb-2',
          shiny::strong(if (current_view() == 'weekly') 'Weekly standings' else 'Season standings'),
          shiny::span(class = 'blq-muted', paste(nrow(a), 'shown'))),
        shiny::div(class = 'blq-table-wrap',
          shiny::tags$table(class = 'table table-hover blq-table',
            shiny::tags$thead(head), shiny::tags$tbody(rows))))
    })

    # One observer per displayed row. IDs are stable, but each click resolves
    # against the current displayed table rather than a stale captured user ID.
    for (i in seq_len(100L)) local({
      j <- i
      shiny::observeEvent(input[[paste0('user_', j)]], {
        a <- displayed()
        if (nrow(a) >= j) selected_user(a$user_id[j])
      }, ignoreInit = TRUE)
    })

    output$detail_card <- shiny::renderUI({
      uid <- selected_user()
      if (is.null(uid)) return(NULL)
      d <- data()
      d <- d[d$user_id == uid, , drop = FALSE]
      if (current_view() == 'weekly')
        d <- d[as.character(d$week_start) == selected_week(), , drop = FALSE]
      if (!nrow(d)) return(NULL)
      x <- blq_lb_aggregate(d)
      title <- paste0(x$display_name[1], if (current_view() == 'weekly') ' | Weekly picks' else ' | Season performance')
      stats <- shiny::div(class = 'blq-metrics',
        shiny::div(class = 'blq-metric', shiny::div(class = 'blq-value', blq_lb_pct(x$user_accuracy)), shiny::div(class = 'blq-label', 'User accuracy')),
        shiny::div(class = 'blq-metric', shiny::div(class = 'blq-value', blq_lb_pct(x$model_accuracy)), shiny::div(class = 'blq-label', 'Model accuracy')),
        shiny::div(class = 'blq-metric', shiny::div(class = 'blq-value', blq_lb_adv(x$advantage)), shiny::div(class = 'blq-label', 'Advantage')))
      shiny::div(class = 'blq-panel',
        shiny::div(class = 'blq-heading', shiny::h4(title, style = 'margin:0'),
          shiny::actionButton(ns('close_detail'), 'Close', class = 'btn-sm btn-outline-secondary')),
        stats,
        if (current_view() == 'weekly') shiny::uiOutput(ns('pick_details'))
        else shiny::tagList(
          shiny::h5('Weekly advantage over model'),
          shiny::plotOutput(ns('season_plot'), height = '245px'),
          shiny::uiOutput(ns('season_details')))
      )
    })
    shiny::observeEvent(input$close_detail, selected_user(NULL))

    output$pick_details <- shiny::renderUI({
      shiny::req(current_view() == 'weekly', selected_user(), selected_week())
      d <- data()
      d <- d[d$user_id == selected_user() & as.character(d$week_start) == selected_week(), , drop = FALSE]
      if (!nrow(d)) return(NULL)
      d <- d[order(d$game_date, d$game_id), , drop = FALSE]
      rows <- lapply(seq_len(nrow(d)), function(i) {
        x <- d[i, ]
        probability <- switch(x$model_outcome,
          home = x$p_home_win, away = x$p_away_win, tie = x$p_tie)
        shiny::tags$tr(
          shiny::tags$td(paste(x$away_team, '@', x$home_team)),
          shiny::tags$td(blq_lb_outcome(x$picked_outcome, x$home_team, x$away_team)),
          shiny::tags$td(paste0(blq_lb_outcome(x$model_outcome, x$home_team, x$away_team),
                                ' (', sprintf('%.1f%%', 100 * probability), ')')),
          shiny::tags$td(sprintf('%s %d, %s %d', x$away_team, x$away_score, x$home_team, x$home_score)),
          shiny::tags$td(class = if (x$net > 0) 'blq-positive' else if (x$net < 0) 'blq-negative' else '',
                         sprintf('%+d', x$net))
        )
      })
      shiny::tagList(
        shiny::p(class = 'blq-muted', 'Scoring: +1 user correct/model wrong; -1 model correct/user wrong; 0 otherwise.'),
        shiny::div(class = 'blq-table-wrap', shiny::tags$table(class = 'table table-sm blq-table',
          shiny::tags$thead(shiny::tags$tr(lapply(c('Matchup', 'User pick', 'Model (prob.)', 'Final score', '+/-'), shiny::tags$th))),
          shiny::tags$tbody(rows)))
      )
    })

    season_weeks <- shiny::reactive({
      shiny::req(selected_user(), current_view() == 'season')
      d <- data()
      d <- d[d$user_id == selected_user(), , drop = FALSE]
      if (!nrow(d)) return(data.frame())
      x <- blq_lb_aggregate(d, by_week = TRUE)
      x[order(x$week_start), , drop = FALSE]
    })

    output$season_plot <- shiny::renderPlot({
      x <- season_weeks()
      shiny::req(nrow(x))
      # Missing calendar weeks are gaps, not zero-valued performances.
      full <- seq(min(x$week_start), max(x$week_start), by = 'week')
      y <- rep(NA_real_, length(full))
      y[match(x$week_start, full)] <- x$advantage
      lim <- range(c(y, 0), na.rm = TRUE)
      pad <- max(5, diff(lim) * .15)
      graphics::par(mar = c(5, 4.5, 1.5, 1), bg = '#222222', fg = '#cbd5e1',
                    col.axis = '#cbd5e1', col.lab = '#cbd5e1')
      graphics::plot(seq_along(full), y, type = 'b', pch = 19, lwd = 2,
                     col = '#4DA3FF', ylim = c(lim[1] - pad, lim[2] + pad),
                     xaxt = 'n', xlab = 'Week beginning', ylab = 'Advantage (percentage points)')
      graphics::abline(h = 0, lty = 2, col = '#aab4c4')
      at <- unique(round(seq(1, length(full), length.out = min(7, length(full)))))
      graphics::axis(1, at = at, labels = format(full[at], '%b %d'), cex.axis = .8)
      graphics::grid(nx = NA, ny = NULL, col = '#414850', lty = 3)
    }, bg = '#222222')

    output$season_details <- shiny::renderUI({
      x <- season_weeks()
      if (!nrow(x)) return(NULL)
      rows <- lapply(seq_len(nrow(x)), function(i) {
        r <- x[i, ]
        shiny::tags$tr(shiny::tags$td(blq_lb_week(r$week_start)),
          shiny::tags$td(r$picks),
          shiny::tags$td(blq_lb_pct(r$user_accuracy)),
          shiny::tags$td(blq_lb_pct(r$model_accuracy)),
          shiny::tags$td(class = if (r$advantage > 0) 'blq-positive' else if (r$advantage < 0) 'blq-negative' else '',
                         blq_lb_adv(r$advantage)))
      })
      shiny::div(class = 'blq-table-wrap', shiny::tags$table(class = 'table table-sm blq-table',
        shiny::tags$thead(shiny::tags$tr(lapply(c('Week', 'Picks', 'User %', 'Model %', 'Advantage'), shiny::tags$th))),
        shiny::tags$tbody(rows)))
    })
    invisible(data)
  })
}
