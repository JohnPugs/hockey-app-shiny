# R/mod_leaderboard.R
# Beer League IQ: Leaderboard landing section, model performance.
# Requires: shiny, DBI. Works with a DBI connection or pool::Pool.
# The caller supplies the same database connection used elsewhere in the app.

mod_leaderboard_ui <- function(id) {
  ns <- shiny::NS(id)
  #message("Leaderboard UI initialized")
  shiny::tagList(
    
    shiny::tags$style(shiny::HTML(
      do.call(
        sprintf,
        c(
          list('
        #%s .blq-metrics { display:grid; grid-template-columns:repeat(3,minmax(0,1fr)); gap:14px; margin:16px 0; }
        #%s .blq-metric {
          background: #262626;
          border: 1px solid #414850;
          border-radius: 10px;
          padding: 17px;
          min-width: 0;
          text-align: center;
        }
        #%s .blq-value {
          color: #4DA3FF;
          font-size: 1.65rem;
          font-weight: 700;
          font-variant-numeric: tabular-nums;
        }
        #%s .blq-label {
          color: #aab4c4;
          font-size: .78rem;
          font-weight: 600;
          margin-top: 8px;
        }
        #%s .blq-detail {
          color: #aab4c4;
          font-size: .72rem;
          margin-top: 5px;
        }
        #%s .blq-panel { border:1px solid var(--bs-border-color, #e1e5eb); border-radius:14px; padding:22px; margin-bottom:20px; }
        #%s .blq-heading { display:flex; justify-content:space-between; align-items:center; gap:12px; flex-wrap:wrap; }
        #%s .blq-subhead { font-weight:650; margin-bottom:10px; }
        #%s .blq-history { display:flex; gap:18px; flex-wrap:wrap; font-size:.93rem; }
        #%s .blq-muted { opacity:.72; font-size:.83rem; }
        @media(max-width:600px) { #%s .blq-metrics { grid-template-columns:1fr; } }
      '),
          as.list(rep(ns('leaderboard_root'), 11))
        )
      )
    )),
    shiny::div(
      id = ns('leaderboard_root'),
      shiny::div(
        class = 'blq-panel',
        shiny::div(
          class = 'blq-heading',
          shiny::h3('Model Performance', style = 'margin:0;'),
          shiny::span('Current season', class = 'badge bg-secondary')
        ),
        shiny::uiOutput(ns('performance_content')),
        shiny::tags$hr(),
        shiny::div(class = 'blq-subhead', 'Historical Model Validation'),
        shiny::div(
          class = 'blq-history',
          shiny::span('Outcome accuracy: ', shiny::strong('56.2%')),
          shiny::span('Winner accuracy: ', shiny::strong('60.9%')),
          shiny::span('Games evaluated: ', shiny::strong('3,536'))
        ),
        shiny::div(
          class = 'blq-muted', style = 'margin-top:10px;',
          'Historical results are fixed validation benchmarks. Current-season results update as final scores become available.'
        )
      ),
      shiny::h3('Leaderboard'),
      shiny::p(class = 'blq-muted', 'Player picks leaderboard coming soon.')
    )
  )
}

# db: a reactive returning a live DBI connection / pool, or a connection itself.
# season_id: optional reactive or scalar season ID. If NULL, uses the season
# associated with the most recent game date among the model's predictions.
# refresh: optional reactive invalidation trigger, e.g. a refresh button counter.

mod_leaderboard_server <- function(
    id,
    active = shiny::reactive(TRUE),
    db = NULL
) {
  shiny::moduleServer(id, function(input, output, session) {
    
    # Use the existing app pool when supplied.
    # Otherwise create a session-specific pool.
    owns_pool <- is.null(db)
    
    con <- if (owns_pool) {
      get_db_pool()
    } else if (shiny::is.reactive(db)) {
      db()
    } else {
      db
    }
    
    if (owns_pool) {
      session$onSessionEnded(function() {
        pool::poolClose(con)
      })
    }
    
    performance <- shiny::reactive({
      
      shiny::req(active())
      
      get_model_performance(con)
      
    })
    

    output$performance_content <- shiny::renderUI({
      result <- tryCatch(performance(), error = function(e) e)
      if (inherits(result, 'error')) {
        return(shiny::div(
          class = 'alert alert-warning', role = 'alert',
          'Model performance is temporarily unavailable. Please try again later.'
        ))
      }
      if (nrow(result) == 0L) {
        return(shiny::p(class = 'blq-muted',
                        'No completed games with eligible frozen predictions yet.'))
      }
      x <- result[1L, , drop = FALSE]
      fmt_pct <- function(v) if (is.na(v)) 'N/A' else sprintf('%.1f%%', as.numeric(v))
      fmt_n <- function(v) format(as.integer(v), big.mark = ',', trim = TRUE)
      metric <- function(value, label, detail) {
        shiny::div(class = 'blq-metric',
          shiny::div(class = 'blq-value', value),
          shiny::div(class = 'blq-label', label),
          shiny::div(class = 'blq-detail', detail)
        )
      }
      date_label <- if (is.na(x$first_game_date) || is.na(x$last_game_date)) {
        'Current season'
      } else {
        paste(format(as.Date(x$first_game_date), '%b %d'),
              '\u2013', format(as.Date(x$last_game_date), '%b %d, %Y'))
      }
      shiny::div(class = 'blq-metrics',
        metric(fmt_pct(x$outcome_accuracy_pct), 'Outcome Accuracy',
               sprintf('%s of %s correct', fmt_n(x$correct_outcomes), fmt_n(x$games_evaluated))),
        metric(fmt_pct(x$winner_accuracy_pct), 'Winner Accuracy',
               sprintf('%s of %s correct', fmt_n(x$correct_winners), fmt_n(x$games_without_ties))),
        metric(fmt_n(x$games_evaluated), 'Games Evaluated', date_label)
      )
    })

    invisible(performance)
  })
}
