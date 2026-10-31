
# ============================================================
# Beer League IQ
# Team Statistics Module
# Division-first, mobile-friendly design
# ============================================================

mod_teams_ui <- function(id) {
  
  ns <- shiny::NS(id)
  
  shiny::div(
    class = "container-fluid py-4 team-stats-page",
    
    shiny::tags$style(shiny::HTML("
      .team-stats-page .division-buttons {
        display: flex;
        flex-wrap: wrap;
        gap: 8px;
        margin-bottom: 18px;
      }

      .team-stats-page .division-buttons .btn {
        border-radius: 9px;
        font-size: 0.86rem;
        font-weight: 600;
        padding: 9px 13px;
      }

      .team-stats-page .view-buttons {
        display: grid;
        grid-template-columns: repeat(3, minmax(0, 1fr));
        gap: 8px;
        margin: 18px 0;
      }

      .team-stats-page .view-buttons .btn {
        width: 100%;
        padding: 11px 6px;
        border-radius: 9px;
        font-weight: 600;
      }

      .team-stats-page .stats-table {
        width: 100%;
        font-size: 0.9rem;
        font-variant-numeric: tabular-nums;
      }

      .team-stats-page .stats-table th {
        color: #aab4c4;
        font-size: 0.76rem;
        font-weight: 600;
        white-space: nowrap;
      }

      .team-stats-page .stats-table td {
        vertical-align: middle;
        padding-top: 12px;
        padding-bottom: 12px;
      }

      .team-stats-page .stats-table th:not(:first-child),
      .team-stats-page .stats-table td:not(:first-child) {
        text-align: right;
      }

      .team-stats-page .team-link {
        font-weight: 600;
        text-decoration: none;
        overflow-wrap: anywhere;
      }

      .team-stats-page .team-selected td {
        background: rgba(77, 163, 255, 0.14);
      }

      .team-stats-page .rank-card {
        background: #263241;
        border: 1px solid #374151;
        border-radius: 11px;
        padding: 15px;
        height: 100%;
      }

      .team-stats-page .rank-label {
        color: #aab4c4;
        font-size: 0.76rem;
        margin-bottom: 7px;
      }

      .team-stats-page .rank-value {
        font-size: 1.65rem;
        font-weight: 700;
        line-height: 1.2;
        font-variant-numeric: tabular-nums;
      }

      .team-stats-page .rank-position {
        color: #4DA3FF;
        font-size: 0.78rem;
        font-weight: 600;
        margin-top: 7px;
      }

      .team-stats-page .chart-card {
        margin-bottom: 12px;
      }

      @media (max-width: 576px) {
        .team-stats-page {
          padding-left: 10px !important;
          padding-right: 10px !important;
        }

        .team-stats-page .division-buttons {
          display: grid;
          grid-template-columns: repeat(2, minmax(0, 1fr));
        }

        .team-stats-page .division-buttons .btn {
          width: 100%;
          font-size: 0.79rem;
          padding: 10px 5px;
        }

        .team-stats-page .stats-table {
          font-size: 0.78rem;
        }

        .team-stats-page .stats-table th {
          font-size: 0.69rem;
        }

        .team-stats-page .stats-table th,
        .team-stats-page .stats-table td {
          padding-left: 5px;
          padding-right: 5px;
        }

        .team-stats-page .rank-card {
          padding: 10px;
        }

        .team-stats-page .rank-label {
          font-size: 0.68rem;
        }

        .team-stats-page .rank-value {
          font-size: 1.3rem;
        }

        .team-stats-page .rank-position {
          font-size: 0.69rem;
        }
      }
    ")),
    
    shiny::h2("Team Statistics"),
    
    shiny::p(
      class = "text-muted mb-4",
      "Compare team records, offensive performance, ",
      "and defensive strength."
    ),
    
    shiny::h5("Select Division"),
    
    shiny::uiOutput(ns("division_buttons")),
    
    shiny::uiOutput(ns("view_buttons")),
    
    shiny::div(
      class = "d-flex justify-content-end mb-3",
      
      shiny::actionButton(
        ns("stats_explainer"),
        label = "How are these calculated?",
        icon = shiny::icon("circle-info"),
        class = "btn-sm btn-outline-info"
      )
    ),
    
    shiny::uiOutput(ns("standings")),
    
    shiny::div(
      class = "mt-4",
      shiny::uiOutput(ns("team_details"))
    )
  )
}


mod_teams_server <- function(id, pool) {
  
  shiny::moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # --------------------------------------------------------
    # Helpers
    # --------------------------------------------------------
    
    fmt_num <- function(x, digits = 1) {
      if (length(x) == 0 || is.na(x[1])) {
        return("N/A")
      }
      
      formatC(
        as.numeric(x[1]),
        format = "f",
        digits = digits
      )
    }
    
    fmt_int <- function(x) {
      if (length(x) == 0 || is.na(x[1])) {
        return("N/A")
      }
      
      as.character(as.integer(x[1]))
    }
    
    fmt_pct <- function(x) {
      if (length(x) == 0 || is.na(x[1])) {
        return("N/A")
      }
      
      paste0(fmt_num(100 * x), "%")
    }
    
    fmt_record <- function(w, l, t) {
      paste(
        fmt_int(w),
        fmt_int(l),
        fmt_int(t),
        sep = "-"
      )
    }
    
    ordinal <- function(n) {
      n <- as.integer(n)
      
      if (is.na(n)) return("N/A")
      
      suffix <- if (n %% 100 %in% 11:13) {
        "th"
      } else {
        switch(
          as.character(n %% 10),
          "1" = "st",
          "2" = "nd",
          "3" = "rd",
          "th"
        )
      }
      
      paste0(n, suffix)
    }
    
    # --------------------------------------------------------
    # Observer for modal
    # --------------------------------------------------------
    
    shiny::observeEvent(input$stats_explainer, {
      
      shiny::showModal(
        shiny::modalDialog(
          title = "Understanding Team Statistics",
          
          shiny::h5("Standings"),
          shiny::p(
            shiny::strong("W-L-T: "),
            "Wins, losses, and ties."
          ),
          shiny::p(
            shiny::strong("Win %: "),
            "The team's recorded win percentage."
          ),
          
          shiny::tags$hr(),
          
          shiny::h5("Offense"),
          shiny::p(
            shiny::strong("GF: "),
            "Total goals scored."
          ),
          shiny::p(
            shiny::strong("GF/G: "),
            "Average goals scored per game."
          ),
          shiny::p(
            shiny::strong("ETO: "),
            "Estimated Team Offense, a model-based rating ",
            "of offensive strength on a 0-100 scale. ",
            "Higher values indicate stronger offense."
          ),
          
          shiny::tags$hr(),
          
          shiny::h5("Defense"),
          shiny::p(
            shiny::strong("GA: "),
            "Total goals allowed."
          ),
          shiny::p(
            shiny::strong("GA/G: "),
            "Average goals allowed per game."
          ),
          shiny::p(
            shiny::strong("ETD: "),
            "Estimated Team Defense, a model-based rating ",
            "of defensive strength on a 0-100 scale. ",
            "Higher values indicate stronger defense."
          ),
          
          shiny::tags$hr(),
          
          shiny::h5("Division Rankings"),
          shiny::p(
            "Rankings compare teams within their division. ",
            "Teams with identical values share the same rank. ",
            "Lower goals-against statistics rank better; ",
            "higher values rank better for other measures, ",
            "except losses."
          ),
          
          shiny::h5("Historical Charts"),
          shiny::p(
            "The blue line represents the selected team. ",
            "The gray dashed line represents the division ",
            "average at each weekly snapshot."
          ),
          
          easyClose = TRUE,
          footer = shiny::modalButton("Close"),
          size = "m"
        )
      )
    })
    
    # --------------------------------------------------------
    # Statistical view definitions
    # --------------------------------------------------------
    
    views <- list(
      record = list(
        title = "Standings",
        metrics = c("wins", "losses", "win_pct"),
        labels = c("Wins", "Losses", "Win Percentage"),
        table_cols = c("record", "win_pct"),
        table_labels = c("W-L-T", "Win %"),
        sort = "win_pct"
      ),
      
      offense = list(
        title = "Offense",
        metrics = c("gf", "gf_pg", "eto_100"),
        labels = c(
          "Goals For",
          "Goals / Game",
          "Offensive Rating"
        ),
        table_cols = c("gf", "gf_pg", "eto_100"),
        table_labels = c("GF", "GF/G", "ETO"),
        sort = "eto_100"
      ),
      
      defense = list(
        title = "Defense",
        metrics = c("ga", "ga_pg", "etd_100"),
        labels = c(
          "Goals Against",
          "Goals Against / Game",
          "Defensive Rating"
        ),
        table_cols = c("ga", "ga_pg", "etd_100"),
        table_labels = c("GA", "GA/G", "ETD"),
        sort = "etd_100"
      )
    )
    
    lower_is_better <- c(
      "losses", "ga", "ga_pg"
    )
    
    # --------------------------------------------------------
    # Load current team data
    # --------------------------------------------------------
    
    team_stats <- get_team_stats(pool)
    
    # The latest snapshot should represent the same
    # season and Monday for every team.
    latest_date <- max(
      as.Date(team_stats$snapshot_date),
      na.rm = TRUE
    )
    
    team_stats <- team_stats[
      as.Date(team_stats$snapshot_date) == latest_date,
      ,
      drop = FALSE
    ]
    
    if (nrow(team_stats) == 0) {
      stop("No current team snapshots were found.")
    }
    
    selected_division <- shiny::reactiveVal(NULL)
    selected_view <- shiny::reactiveVal("record")
    selected_team <- shiny::reactiveVal(NULL)
    
    divisions <- unique(
      team_stats[
        ,
        c("division_id", "division_name"),
        drop = FALSE
      ]
    )
    
    divisions <- divisions[
      order(divisions$division_name),
      ,
      drop = FALSE
    ]
    
    selected_division(
      as.character(divisions$division_id[1])
    )
    
    # --------------------------------------------------------
    # Division buttons
    # --------------------------------------------------------
    
    for (i in seq_len(nrow(divisions))) {
      
      local({
        
        div_id <- as.character(divisions$division_id[i])
        button_id <- paste0("division_", div_id)
        
        shiny::observeEvent(
          input[[button_id]],
          {
            selected_division(div_id)
            selected_team(NULL)
          },
          ignoreInit = TRUE
        )
      })
    }
    
    output$division_buttons <- shiny::renderUI({
      
      active <- selected_division()
      
      buttons <- lapply(
        seq_len(nrow(divisions)),
        function(i) {
          
          div_id <- as.character(divisions$division_id[i])
          div_name <- divisions$division_name[i]
          
          is_active <- identical(active, div_id)
          
          shiny::actionButton(
            ns(paste0("division_", div_id)),
            label = div_name,
            class = if (is_active) {
              "btn-primary"
            } else {
              "btn-outline-secondary"
            }
          )
        }
      )
      
      shiny::div(
        class = "division-buttons",
        do.call(shiny::tagList, buttons)
      )
    })
    
    # --------------------------------------------------------
    # Record / Offense / Defense buttons
    # --------------------------------------------------------
    
    for (view_name in names(views)) {
      
      local({
        
        this_view <- view_name
        button_id <- paste0("view_", this_view)
        
        shiny::observeEvent(
          input[[button_id]],
          {
            selected_view(this_view)
          },
          ignoreInit = TRUE
        )
      })
    }
    
    output$view_buttons <- shiny::renderUI({
      
      active <- selected_view()
      
      buttons <- lapply(
        names(views),
        function(view_name) {
          
          shiny::actionButton(
            ns(paste0("view_", view_name)),
            label = views[[view_name]]$title,
            class = if (identical(active, view_name)) {
              "btn-primary"
            } else {
              "btn-outline-secondary"
            }
          )
        }
      )
      
      shiny::div(
        class = "view-buttons",
        do.call(shiny::tagList, buttons)
      )
    })
    
    # --------------------------------------------------------
    # Division data
    # --------------------------------------------------------
    
    division_stats <- shiny::reactive({
      
      shiny::req(selected_division())
      
      team_stats[
        as.character(team_stats$division_id) ==
          selected_division(),
        ,
        drop = FALSE
      ]
    })
    
    # --------------------------------------------------------
    # Sort current table
    # --------------------------------------------------------
    
    sorted_stats <- shiny::reactive({
      
      x <- division_stats()
      view <- views[[selected_view()]]
      
      shiny::req(nrow(x) > 0)
      
      metric <- view$sort
      
      ord <- order(
        -as.numeric(x[[metric]]),
        x$team_name,
        na.last = TRUE
      )
      
      x[ord, , drop = FALSE]
    })
    
    # --------------------------------------------------------
    # Team click observers
    # --------------------------------------------------------
    
    for (i in seq_len(nrow(team_stats))) {
      
      local({
        
        team_id <- as.character(team_stats$team_id[i])
        division_id <- as.character(
          team_stats$division_id[i]
        )
        
        button_id <- paste0(
          "team_",
          division_id,
          "_",
          team_id
        )
        
        shiny::observeEvent(
          input[[button_id]],
          {
            if (identical(
              selected_division(),
              division_id
            )) {
              selected_team(team_id)
            }
          },
          ignoreInit = TRUE
        )
      })
    }
    
    # --------------------------------------------------------
    # Table value formatting
    # --------------------------------------------------------
    
    table_value <- function(team, column) {
      
      if (column == "record") {
        return(fmt_record(
          team$wins,
          team$losses,
          team$ties
        ))
      }
      
      if (column == "win_pct") {
        return(fmt_pct(team$win_pct))
      }
      
      if (column %in% c("gf", "ga")) {
        return(fmt_int(team[[column]]))
      }
      
      fmt_num(team[[column]])
    }
    
    # --------------------------------------------------------
    # Compact standings table
    # --------------------------------------------------------
    
    output$standings <- shiny::renderUI({
      
      x <- sorted_stats()
      view <- views[[selected_view()]]
      
      headers <- c("Team", view$table_labels)
      
      header_row <- shiny::tags$tr(
        lapply(headers, shiny::tags$th)
      )
      
      rows <- lapply(seq_len(nrow(x)), function(i) {
        
        team <- x[i, , drop = FALSE]
        
        team_id <- as.character(team$team_id)
        div_id <- as.character(team$division_id)
        
        button_id <- paste0(
          "team_", div_id, "_", team_id
        )
        
        is_selected <- identical(
          selected_team(),
          team_id
        )
        
        values <- lapply(
          view$table_cols,
          function(column) {
            shiny::tags$td(
              table_value(team, column)
            )
          }
        )
        
        shiny::tags$tr(
          class = if (is_selected) {
            "team-selected"
          } else {
            NULL
          },
          
          shiny::tags$td(
            shiny::actionLink(
              ns(button_id),
              label = team$team_name,
              class = "team-link"
            )
          ),
          
          values
        )
      })
      
      bslib::card(
        bslib::card_header(
          shiny::div(
            class = "d-flex justify-content-between align-items-center w-100",
            
            shiny::strong(
              if (selected_view() == "record") {
                "Standings"
              } else {
                view$title
              }
            ),
            
            shiny::tags$small(
              class = "text-muted ms-auto",
              paste(nrow(x), "teams")
            )
          )
        ),
        
        bslib::card_body(
          shiny::div(
            class = "table-responsive",
            
            shiny::tags$table(
              class = paste(
                "table table-dark table-hover",
                "align-middle stats-table mb-0"
              ),
              
              shiny::tags$thead(header_row),
              
              shiny::tags$tbody(
                do.call(shiny::tagList, rows)
              )
            )
          ),
          
          shiny::p(
            class = "text-muted mt-3 mb-1",
            style = "font-size: 0.77rem;",
            "Select a team to compare its performance ",
            "with the division average."
          ),
          
          shiny::p(
            class = "text-muted mb-0",
            style = "font-size: 0.72rem;",
            paste(
              "Statistics through",
              format(latest_date, "%B %d, %Y")
            )
          )
        )
      )
    })
    
    # --------------------------------------------------------
    # Selected team data
    # --------------------------------------------------------
    
    selected_team_data <- shiny::reactive({
      
      shiny::req(selected_team())
      
      x <- division_stats()
      
      result <- x[
        as.character(x$team_id) ==
          selected_team(),
        ,
        drop = FALSE
      ]
      
      shiny::req(nrow(result) == 1)
      
      result
    })
    
    # --------------------------------------------------------
    # Rank calculations
    #
    # Competition ranks: 1, 2, 2, 4.
    # Missing values are not ranked.
    # --------------------------------------------------------
    
    get_rank <- function(data, team_id, metric) {
      
      values <- as.numeric(data[[metric]])
      
      if (metric %in% lower_is_better) {
        ranks <- rank(
          values,
          ties.method = "min",
          na.last = "keep"
        )
      } else {
        ranks <- rank(
          -values,
          ties.method = "min",
          na.last = "keep"
        )
      }
      
      idx <- which(
        as.character(data$team_id) ==
          as.character(team_id)
      )
      
      if (length(idx) != 1 || is.na(ranks[idx])) {
        return("Not ranked")
      }
      
      paste0(
        ordinal(ranks[idx]),
        " of ",
        sum(!is.na(values))
      )
    }
    
    # --------------------------------------------------------
    # Rank card
    # --------------------------------------------------------
    
    rank_card <- function(label, value, rank_text) {
      
      shiny::div(
        class = "rank-card",
        
        shiny::div(
          class = "rank-label",
          label
        ),
        
        shiny::div(
          class = "rank-value",
          value
        ),
        
        shiny::div(
          class = "rank-position",
          rank_text
        )
      )
    }
    
    # --------------------------------------------------------
    # Division history
    #
    # Query all teams in the division once per
    # selected division, not once per chart.
    # --------------------------------------------------------
    
    division_history <- shiny::reactive({
      
      x <- division_stats()
      
      shiny::req(nrow(x) > 0)
      
      get_division_rating_history(
        pool,
        season_id = x$season_id[1],
        division_id = x$division_id[1]
      )
    })
    
    # --------------------------------------------------------
    # Prepare historical comparison
    # --------------------------------------------------------
    
    comparison_data <- shiny::reactive({
      
      shiny::req(selected_team())
      
      history <- division_history()
      
      history <- history[
        as.Date(history$snapshot_date) <= latest_date,
        ,
        drop = FALSE
      ]
      
      team_id <- selected_team()
      
      metrics <- views[[selected_view()]]$metrics
      
      results <- lapply(metrics, function(metric) {
        
        dates <- sort(unique(
          as.Date(history$snapshot_date)
        ))
        
        team_values <- rep(NA_real_, length(dates))
        division_values <- rep(NA_real_, length(dates))
        
        for (i in seq_along(dates)) {
          
          week <- history[
            as.Date(history$snapshot_date) == dates[i],
            ,
            drop = FALSE
          ]
          
          # One observation per team at each date.
          week <- week[
            !duplicated(week$team_id),
            ,
            drop = FALSE
          ]
          
          values <- as.numeric(week[[metric]])
          
          if (any(!is.na(values))) {
            division_values[i] <- mean(
              values,
              na.rm = TRUE
            )
          }
          
          team_row <- which(
            as.character(week$team_id) ==
              team_id
          )
          
          if (length(team_row) == 1) {
            team_values[i] <- values[team_row]
          }
        }
        
        data.frame(
          snapshot_date = dates,
          team = team_values,
          division = division_values
        )
      })
      
      stats::setNames(results, metrics)
    })
    
    # --------------------------------------------------------
    # Historical comparison plots
    # --------------------------------------------------------
    
    draw_comparison <- function(metric_index) {
      
      view <- views[[selected_view()]]
      
      metric <- view$metrics[metric_index]
      label <- view$labels[metric_index]
      
      data <- comparison_data()[[metric]]
      
      shiny::validate(
        shiny::need(
          nrow(data) > 0 &&
            any(!is.na(data$team)),
          "Historical data unavailable."
        )
      )
      
      dates <- as.Date(data$snapshot_date)
      
      all_values <- c(data$team, data$division)
      all_values <- all_values[is.finite(all_values)]
      
      shiny::validate(
        shiny::need(
          length(all_values) > 0,
          "No values available."
        )
      )
      
      y_range <- range(all_values)
      
      # Include zero for cumulative counts and rates.
      if (metric %in% c(
        "wins", "losses", "gf", "ga",
        "gf_pg", "ga_pg"
      )) {
        y_range[1] <- min(0, y_range[1])
      }
      
      if (metric == "win_pct") {
        y_range <- c(0, 1)
      }
      
      if (metric %in% c("eto_100", "etd_100")) {
        y_range <- c(0, 100)
      }
      
      if (diff(y_range) == 0) {
        y_range <- y_range + c(-1, 1)
      }
      
      if (metric != "win_pct" &&
          !metric %in% c("eto_100", "etd_100")) {
        padding <- max(0.2, diff(y_range) * 0.10)
        y_range[2] <- y_range[2] + padding
      }
      
      x_range <- range(dates)
      
      if (x_range[1] == x_range[2]) {
        x_range <- x_range + c(-3, 3)
      }
      
      old_par <- graphics::par(no.readonly = TRUE)
      on.exit(graphics::par(old_par), add = TRUE)
      
      graphics::par(
        bg = "#222222",
        fg = "#e2e8f0",
        col.axis = "#aab4c4",
        col.lab = "#e2e8f0",
        mar = c(4, 4, 2, 1)
      )
      
      graphics::plot(
        dates,
        data$team,
        type = "n",
        xlim = x_range,
        ylim = y_range,
        xlab = "",
        ylab = label,
        xaxt = "n",
        yaxt = "n",
        bty = "n"
      )
      
      graphics::axis.Date(
        1,
        at = pretty(dates),
        format = "%b %d",
        col.axis = "#aab4c4",
        col = "#475569",
        cex.axis = 0.8
      )
      
      ticks <- pretty(y_range)
      
      if (metric == "win_pct") {
        ticks <- seq(0, 1, 0.25)
      }
      
      ticks <- ticks[
        ticks >= y_range[1] &
          ticks <= y_range[2]
      ]
      
      tick_labels <- if (metric == "win_pct") {
        paste0(round(ticks * 100), "%")
      } else {
        format(ticks, trim = TRUE)
      }
      
      graphics::axis(
        2,
        at = ticks,
        labels = tick_labels,
        las = 1,
        col.axis = "#aab4c4",
        col = "#475569",
        cex.axis = 0.8
      )
      
      graphics::abline(
        h = ticks,
        col = "#374151",
        lty = 3
      )
      
      # Division average: dashed gray.
      graphics::lines(
        dates,
        data$division,
        col = "#94a3b8",
        lwd = 2,
        lty = 2
      )
      
      # Selected team: solid blue.
      graphics::lines(
        dates,
        data$team,
        col = "#4DA3FF",
        lwd = 3,
        type = "o",
        pch = 16,
        cex = 0.75
      )
      
      # graphics::legend(
      #   "topleft",
      #   legend = c(
      #     "Selected Team",
      #     "Division Average"
      #   ),
      #   col = c("#4DA3FF", "#94a3b8"),
      #   lty = c(1, 2),
      #   lwd = c(3, 2),
      #   bty = "n",
      #   text.col = "#e2e8f0",
      #   cex = 0.75
      # )
    }
    
    output$comparison_1 <- shiny::renderPlot({
      draw_comparison(1)
    }, bg = "#222222", res = 110)
    
    output$comparison_2 <- shiny::renderPlot({
      draw_comparison(2)
    }, bg = "#222222", res = 110)
    
    output$comparison_3 <- shiny::renderPlot({
      draw_comparison(3)
    }, bg = "#222222", res = 110)
    
    # --------------------------------------------------------
    # Selected team details
    # --------------------------------------------------------
    
    output$team_details <- shiny::renderUI({
      
      if (is.null(selected_team())) {
        
        return(
          bslib::card(
            bslib::card_body(
              shiny::div(
                class = "text-center py-4",
                
                shiny::h5("Select a Team"),
                
                shiny::p(
                  class = "text-muted mb-0",
                  "Click a team name to view its ",
                  "division rankings and historical trends."
                )
              )
            )
          )
        )
      }
      
      team <- selected_team_data()
      division <- division_stats()
      view <- views[[selected_view()]]
      
      metrics <- view$metrics
      labels <- view$labels
      
      cards <- lapply(seq_along(metrics), function(i) {
        
        metric <- metrics[i]
        value <- team[[metric]][1]
        
        formatted <- if (metric == "win_pct") {
          fmt_pct(value)
        } else if (metric %in% c(
          "wins", "losses", "gf", "ga"
        )) {
          fmt_int(value)
        } else {
          fmt_num(value)
        }
        
        rank_text <- get_rank(
          division,
          team$team_id[1],
          metric
        )
        
        shiny::div(
          class = "col-4",
          
          rank_card(
            labels[i],
            formatted,
            rank_text
          )
        )
      })
      
      # chart_cards <- lapply(seq_along(metrics), function(i) {
      #   
      #   bslib::card(
      #     class = "chart-card",
      #     
      #     bslib::card_header(
      #       shiny::strong(labels[i])
      #     ),
      #     
      #     bslib::card_body(
      #       shiny::plotOutput(
      #         ns(paste0("comparison_", i)),
      #         height = "260px",
      #         width = "100%"
      #       )
      #     )
      #   )
      # })
      
      chart_cards <- lapply(seq_along(metrics), function(i) {
        
        bslib::card(
          class = "chart-card",
          
          bslib::card_header(
            shiny::strong(labels[i])
          ),
          
          bslib::card_body(
            
            shiny::plotOutput(
              ns(paste0("comparison_", i)),
              height = "230px",
              width = "100%"
            ),
            
            shiny::div(
              class = "d-flex flex-wrap justify-content-center gap-4 mt-2 mb-1",
              style = "font-size: 0.76rem; color: #cbd5e1;",
              
              shiny::span(
                shiny::span(
                  style = paste(
                    "display:inline-block;",
                    "width:22px;",
                    "border-top:3px solid #4DA3FF;",
                    "vertical-align:middle;",
                    "margin-right:7px;"
                  )
                ),
                "Selected Team"
              ),
              
              shiny::span(
                shiny::span(
                  style = paste(
                    "display:inline-block;",
                    "width:22px;",
                    "border-top:2px dashed #94a3b8;",
                    "vertical-align:middle;",
                    "margin-right:7px;"
                  )
                ),
                "Division Average"
              )
            )
          )
        )
      })
      
      bslib::card(
        
        bslib::card_header(
          shiny::div(
            class = paste(
              "d-flex justify-content-between",
              "align-items-center"
            ),
            
            shiny::div(
              shiny::h4(
                class = "mb-1",
                team$team_name
              ),
              
              shiny::tags$small(
                class = "text-muted",
                paste(
                  team$division_name,
                  view$title,
                  sep = " | "
                )
              )
            ),
            
            shiny::actionButton(
              ns("close_team"),
              label = NULL,
              icon = shiny::icon("times"),
              class = "btn-sm btn-outline-secondary"
            )
          )
        ),
        
        bslib::card_body(
          
          shiny::h5(
            class = "mb-3",
            "Division Rankings"
          ),
          
          shiny::div(
            class = "row g-2 mb-4",
            do.call(shiny::tagList, cards)
          ),
          
          shiny::h5(
            class = "mb-2",
            "Performance Over Time"
          ),
          
          # shiny::p(
          #   class = "text-muted",
          #   style = "font-size: 0.82rem;",
          #   "Blue: selected team. ",
          #   "Gray dashed: division average."
          # ),
          
          do.call(shiny::tagList, chart_cards),
          
          shiny::p(
            class = "text-muted mt-2 mb-0",
            style = "font-size: 0.75rem;",
            "Comparisons use the same weekly ",
            "snapshot dates for all teams. ",
            "Ranks reflect the latest available ",
            "division standings."
          )
        )
      )
    })
    
    # --------------------------------------------------------
    # Close selected team
    # --------------------------------------------------------
    
    shiny::observeEvent(
      input$close_team,
      {
        selected_team(NULL)
      }
    )
    
  })
}
