# Beer League IQ: winner picks module
# Requires: shiny, DBI, httr, jsonlite; get_weekly_games(),
# get_matchup_stats(), get_game_predictions() from R/db.R.

picks_api <- function(method, access_token, game_id = NULL, user_id = NULL,
                      outcome = NULL) {
  base <- sub('/+$', '', Sys.getenv('SUPABASE_URL'))
  key <- Sys.getenv('SUPABASE_PUBLISHABLE_KEY', '')
  if (!nzchar(key)) key <- Sys.getenv('SUPABASE_ANON_KEY', '')
  if (!nzchar(base) || !nzchar(key))
    stop('Set SUPABASE_URL and SUPABASE_PUBLISHABLE_KEY (or SUPABASE_ANON_KEY).')
  if (is.null(access_token) || !nzchar(access_token)) stop('Sign in first.')

  url <- paste0(base, '/rest/v1/user_picks')
  hdr <- c('apikey' = key, 'Authorization' = paste('Bearer', access_token),
           'Accept' = 'application/json')
  if (identical(method, 'GET')) {
    resp <- httr::GET(url, httr::add_headers(.headers = hdr),
                      query = list(select = 'game_id,picked_outcome'))
  } else if (identical(method, 'UPSERT')) {
    hdr <- c(hdr, 'Content-Type' = 'application/json',
             'Prefer' = 'resolution=merge-duplicates,return=minimal')
    resp <- httr::POST(url, httr::add_headers(.headers = hdr),
                       query = list(on_conflict = 'user_id,game_id'),
                       body = jsonlite::toJSON(list(user_id = user_id,
                         game_id = as.numeric(game_id), picked_outcome = outcome),
                         auto_unbox = TRUE))
  } else if (identical(method, 'DELETE')) {
    hdr <- c(hdr, 'Prefer' = 'return=minimal')
    resp <- httr::DELETE(url, httr::add_headers(.headers = hdr),
                         query = list(game_id = paste0('eq.', game_id),
                                      user_id = paste0('eq.', user_id)))
  } else stop('Unsupported picks operation')

  if (httr::http_error(resp)) {
    raw <- httr::content(resp, as = 'text', encoding = 'UTF-8')
    parsed <- tryCatch(jsonlite::fromJSON(raw), error = function(e) NULL)
    detail <- if (is.list(parsed)) parsed$message %||picks% parsed$details %||picks% raw else raw
    stop(sprintf('Supabase (%s): %s', httr::status_code(resp), detail))
  }
  if (identical(method, 'GET')) {
    raw <- httr::content(resp, as = 'text', encoding = 'UTF-8')
    return(jsonlite::fromJSON(raw))
  }
  invisible(TRUE)
}

`%||picks%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

mod_picks_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(class = 'games-page picks-page',
    shiny::tags$style(shiny::HTML('
      .picks-actions {display:flex;flex-wrap:wrap;gap:10px;margin-top:14px;align-items:center}
      .picks-choice {flex:1;min-width:100px;border:1px solid #718096!important;background:transparent!important;color:#e9eef5!important;font-weight:650}
      .picks-choice.selected {background:#15803d!important;border-color:#22c55e!important;color:#fff!important}
      .picks-choice:disabled {opacity:.55;cursor:not-allowed}
      .picks-choice.selected:disabled {opacity:.85;background:#166534!important;color:#fff!important}
      .picks-clear {font-size:.8rem}
      .picks-note {font-size:.78rem;color:#aab4c4;margin-top:9px}
      .picks-progress {margin:10px 0 22px;color:#dbe7f5}
    ')),
    shiny::h2('Make Your Picks'),
    shiny::p(class = 'games-subtitle', 'Pick the winner of each matchup this week.'),
    shiny::actionButton(ns('rules'), 'Rules & Scoring', icon = shiny::icon('book-open'),
                        class = 'btn-outline-secondary btn-sm mb-3'),
    shiny::uiOutput(ns('progress')),
    shiny::div(class = 'division-tabs',
      shiny::radioButtons(ns('division'), NULL,
                          choices = c('All Divisions' = 'all'),
                          selected = 'all', inline = TRUE)),
    shiny::uiOutput(ns('game_cards'))
  )
}

mod_picks_server <- function(id, pool, auth_session, active = shiny::reactive(TRUE)) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    saved <- shiny::reactiveVal(setNames(character(), character()))

    shiny::observeEvent(input$rules, {
      
      shiny::showModal(shiny::modalDialog(
        title = 'Picks Rules & Scoring',
        
        shiny::tags$p(
          'Choose the home team, away team, or a tie for each matchup. ',
          'Your selection is saved immediately and may be changed or ',
          'cleared until 30 minutes before the scheduled start time ',
          '(Pacific Time).'
        ),
        
        shiny::tags$p(
          'Weekly Leaderboard: You need at least five completed ',
          'picks during the Monday-to-Sunday week to qualify. ',
          'Weekly results are published after the games are completed.'
        ),
        
        shiny::tags$p(
          'Season Leaderboard: You need at least 10 completed picks ',
          'during the season to qualify. All completed picks count, ',
          'even if you did not qualify for a particular weekly leaderboard.'
        ),
        
        shiny::tags$p(
          'Each completed pick is compared with the model prediction. ',
          'You earn +1 point when you are correct and the model is wrong, ',
          '-1 point when the model is correct and you are wrong, ',
          'and 0 points when both are correct or both are incorrect. ',
          'Ties are included in scoring.'
        ),
        
        shiny::tags$p(
          shiny::tags$strong(
            'Leaderboard ranking = 100 x ',
            '(Your correct picks - Model correct picks) / ',
            'Completed picks.'
          ),
          ' This is your percentage-point (PP) advantage over the model.'
        ),
        
        shiny::tags$p(
          'For example, if you correctly predict 7 of 10 games ',
          'and the model correctly predicts 5 of those same games, ',
          'your PP advantage is +20.0.'
        ),
        
        shiny::tags$p(
          'Players are ranked by PP advantage, not total points. ',
          'This allows players with different numbers of completed ',
          'picks to compete on the same scale.'
        ),
        
        easyClose = TRUE,
        footer = shiny::modalButton('Close')
      ))
      
    })

    games <- shiny::reactive({
      schedule <- get_weekly_games(pool)
      if (!nrow(schedule)) return(schedule)
      stats <- get_matchup_stats(pool)
      preds <- get_game_predictions(pool)
      if (nrow(stats)) {
        h <- stats[stats$side == 'home', , drop = FALSE]
        a <- stats[stats$side == 'away', , drop = FALSE]
        sc <- setdiff(names(stats), c('game_id', 'team_id', 'side'))
        names(h)[match(sc, names(h))] <- paste0('home_', sc)
        names(a)[match(sc, names(a))] <- paste0('away_', sc)
        h <- h[, !names(h) %in% c('team_id', 'side'), drop = FALSE]
        a <- a[, !names(a) %in% c('team_id', 'side'), drop = FALSE]
        schedule$.order <- seq_len(nrow(schedule))
        schedule <- merge(schedule, h, by = 'game_id', all.x = TRUE)
        schedule <- merge(schedule, a, by = 'game_id', all.x = TRUE)
      } else schedule$.order <- seq_len(nrow(schedule))
      schedule <- merge(schedule, preds[, intersect(c('game_id', 'snapshot_date',
        'home_eto_100', 'home_etd_100', 'away_eto_100', 'away_etd_100'), names(preds)),
        drop = FALSE], by = 'game_id', all.x = TRUE)
      schedule <- schedule[order(schedule$.order), , drop = FALSE]
      schedule$.order <- NULL
      schedule
    })

    shiny::observeEvent(list(auth_session(), active()), {
      if (!isTRUE(active())) return()
      a <- auth_session()
      if (is.null(a) || is.null(a$access_token)) {
        saved(setNames(character(), character()))
        return()
      }
      tryCatch({
        x <- picks_api('GET', a$access_token)
        if (is.data.frame(x) && nrow(x)) {
          saved(stats::setNames(as.character(x$picked_outcome),
                                as.character(x$game_id)))
        } else saved(setNames(character(), character()))
      }, error = function(e) shiny::showNotification(
        paste('Could not load picks:', conditionMessage(e)), type = 'error'))
    }, ignoreInit = FALSE)

    shiny::observe({
      if (!isTRUE(active())) return()
      x <- games()
      if (!nrow(x)) return()
      divs <- unique(x[order(x$regular_weekday, x$division_name),
                       c('division_id', 'division_name')])
      choices <- c('All Divisions' = 'all', stats::setNames(
        as.character(divs$division_id), divs$division_name))
      selected <- shiny::isolate(input$division)
      if (is.null(selected) || !selected %in% unname(choices)) selected <- 'all'
      shiny::updateRadioButtons(session, 'division', choices = choices,
                                selected = selected, inline = TRUE)
    })

    filtered <- shiny::reactive({
      x <- games()
      if (!is.null(input$division) && input$division != 'all')
        x <- x[as.character(x$division_id) == input$division, , drop = FALSE]
      x
    })

    # Game times from database are league-local wall-clock times, not UTC.
    start_at <- function(g) {
      as.POSIXct(paste(g$game_date, g$game_time),
                 format = '%Y-%m-%d %H:%M:%S', tz = 'America/Los_Angeles')
    }
    locked <- function(g) {
      st <- start_at(g)
      is.na(st) || isTRUE(g$has_final_score) ||
        as.numeric(difftime(Sys.time(), st, units = 'secs')) >= -1800
    }
    txt <- function(x, fallback = 'N/A') {
      if (is.null(x) || !length(x) || is.na(x[1])) fallback else as.character(x[1])
    }
    field <- function(g, name) if (name %in% names(g)) g[[name]] else NA
    record <- function(g, side, prefix = '') paste(vapply(c('wins','losses','ties'),
      function(k) txt(field(g, paste0(side, '_', prefix, k)), '0'), character(1)),
      collapse = '-')
    ratings <- function(g, side) {
      offense <- field(g, paste0(side, '_eto_100'))
      defense <- field(g, paste0(side, '_etd_100'))
      if (all(is.na(c(offense, defense)))) return(NULL)
      shiny::div(class = 'team-ratings', style = 'margin-top:12px;padding-top:10px;border-top:1px solid #394454;font-size:.8rem;',
        shiny::div('Offensive Rating: ', shiny::strong(if (is.na(offense)) 'N/A' else sprintf('%.1f', offense))),
        shiny::div('Defensive Rating: ', shiny::strong(if (is.na(defense)) 'N/A' else sprintf('%.1f', defense))))
    }
    card <- function(g) {
      gid <- as.character(g$game_id)
      pick <- saved()[gid]
      if (!length(pick) || is.na(pick)) pick <- ''
      is_locked <- locked(g)
      st <- start_at(g)
      game_time <- if (!is.na(st)) format(st, '%a, %b %d · %I:%M %p') else txt(g$game_date)
      deadline <- if (!is.na(st)) format(st - 1800, '%a %I:%M %p', tz = 'America/Los_Angeles') else 'Unavailable'
      can_pick <- !is_locked && !is.null(auth_session()) &&
        !is.na(field(g, 'snapshot_date'))
      pick_button <- function(side) {
        selected <- identical(unname(pick), side)
        shiny::actionButton(ns(paste0('pick_', side, '_', gid)),
          label = paste0(if (selected) '✓ ' else '', txt(field(g, paste0(side, '_team')))),
          class = paste('picks-choice btn', if (selected) 'selected' else ''),
          disabled = if (!can_pick) "disabled" else NULL)
      }
      shiny::div(class = 'matchup-card',
        shiny::div(class = 'matchup-header', shiny::span(game_time),
          shiny::span(class = 'matchup-status', if (is_locked) 'Picks Locked' else 'Open')),
        shiny::div(class = 'matchup-teams',
          shiny::div(class = 'matchup-team', shiny::strong(txt(g$away_team)),
            shiny::tags$small('Away'),
            shiny::div(class = 'team-record', record(g, 'away')),
            shiny::div(class = 'team-goals', paste0('GF/GA: ',
              txt(field(g, 'away_goals_for'), '0'), ' / ',
              txt(field(g, 'away_goals_against'), '0'))), ratings(g, 'away')),
          shiny::div(class = 'matchup-score', if (isTRUE(g$has_final_score))
            paste(txt(g$away_score), '-', txt(g$home_score)) else 'VS'),
          shiny::div(class = 'matchup-team', shiny::strong(txt(g$home_team)),
            shiny::tags$small('Home'),
            shiny::div(class = 'team-record', record(g, 'home')),
            shiny::div(class = 'team-goals', paste0('GF/GA: ',
              txt(field(g, 'home_goals_for'), '0'), ' / ',
              txt(field(g, 'home_goals_against'), '0'))), ratings(g, 'home'))),
        shiny::div(class = 'matchup-h2h',
          shiny::div(class = 'h2h-title', 'HEAD-TO-HEAD THIS SEASON'),
          shiny::div(class = 'h2h-record', record(g, 'away', 'h2h_'),
                     shiny::span('vs'), record(g, 'home', 'h2h_'))),
        shiny::div(class = 'picks-actions', pick_button('away'), pick_button('home'),
          shiny::actionButton(ns(paste0('clear_', gid)), 'Clear Pick',
            class = 'btn-outline-secondary btn-sm picks-clear',
            disabled = if (!can_pick || !nzchar(pick)) "disabled" else NULL)),
        shiny::div(class = 'picks-note', if (is_locked) 'Selections locked' else
          paste('Picks close', deadline, 'Pacific')),
        shiny::div(class = 'matchup-footer', txt(g$rink, '')))
    }

    output$progress <- shiny::renderUI({
      if (is.null(auth_session())) return(shiny::p('Sign in to make picks.'))
      x <- games()
      n <- sum(as.character(x$game_id) %in% names(saved()))
      shiny::div(class = 'picks-progress',
        shiny::strong(sprintf('%d of 5 picks required', min(n, 5L))),
        shiny::span(sprintf('  ·  %d selected this week', n)),
        if (n >= 5) shiny::span('  ✓ Weekly eligibility reached'))
    })

    output$game_cards <- shiny::renderUI({
      if (!isTRUE(active())) return(NULL)
      x <- filtered()
      if (!nrow(x)) return(shiny::p('No games scheduled for this week.'))
      render_section <- function(d) {
        if (!nrow(d)) return(NULL)
        shiny::tagList(lapply(unique(d$division_id), function(id) {
          sub <- d[d$division_id == id, , drop = FALSE]
          shiny::div(class = 'division-section', shiny::h4(sub$division_name[1]),
            shiny::div(class = 'matchup-grid',
              lapply(seq_len(nrow(sub)), function(i) card(sub[i, , drop = FALSE]))))
        }))
      }
      off <- !is.na(x$is_off_day) & x$is_off_day
      shiny::tagList(render_section(x[!off, , drop = FALSE]),
        if (any(off)) shiny::div(class = 'offday-section',
          shiny::h3('Off-Day Games'),
          shiny::p('Games scheduled outside their division\'s regular playing day.'),
          render_section(x[off, , drop = FALSE])))
    })

    # One handler per game and action. Avoid duplicate handlers on navigation.
    shiny::observeEvent(games(), {
      x <- games()
      if (!nrow(x)) return()
      for (i in seq_len(nrow(x))) {
        local({
          g <- x[i, , drop = FALSE]
          gid <- as.character(g$game_id)
          for (side in c('home', 'away', 'clear')) {
            local({
              action <- side
              shiny::observeEvent(input[[paste0(if (action == 'clear') 'clear_' else
                paste0('pick_', action, '_'), gid)]], {
                a <- auth_session()
                if (is.null(a) || is.null(a$access_token)) {
                  shiny::showNotification('Please sign in first.', type = 'warning')
                  return()
                }
                if (locked(g)) return()
                tryCatch({
                  if (action == 'clear') {
                    picks_api('DELETE', a$access_token, gid, a$user_id)
                    old <- saved(); old <- old[names(old) != gid]; saved(old)
                  } else {
                    picks_api('UPSERT', a$access_token, gid, a$user_id, action)
                    old <- saved(); old[gid] <- action; saved(old)
                  }
                }, error = function(e) shiny::showNotification(
                  paste('Could not save pick:', conditionMessage(e)),
                  type = 'error', duration = 8))
              }, ignoreInit = TRUE)
            })
          }
        })
      }
    })

  })
}
