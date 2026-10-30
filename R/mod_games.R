
mod_games_ui <- function(id) {
  
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::div(
      class = "games-page",
      
      shiny::h2("Upcoming Games"),
      
      shiny::p(
        class = "games-subtitle",
        "This week's matchups, organized by division."
      ),
      
      shiny::actionButton(
        ns("about_stats"),
        label = "About Stats & Predictions",
        icon = shiny::icon("info-circle"),
        class = "btn-outline-secondary btn-sm mb-4"
      ),
      
      shiny::div(
        class = "division-tabs",
        
        shiny::radioButtons(
          inputId = ns("division"),
          label = NULL,
          choices = c("All Divisions" = "all"),
          selected = "all",
          inline = TRUE
        )
      ),
      
      shiny::uiOutput(ns("game_cards"))
    )
  )
}


mod_games_server <- function(id, pool) {
  
  shiny::moduleServer(id, function(input, output, session) {
    
    # --------------------------------------------------
    # Helpers
    # --------------------------------------------------
    
    fmt_rating <- function(value) {
      if (length(value) == 0 || is.na(value[1])) {
        return("N/A")
      }
      
      sprintf("%.1f", as.numeric(value[1]))
    }
    
    fmt_probability <- function(value) {
      if (length(value) == 0 || is.na(value[1])) {
        return("N/A")
      }
      
      sprintf("%.1f%%", 100 * as.numeric(value[1]))
    }
    
    # --------------------------------------------------
    # About Stats & Predictions modal
    # --------------------------------------------------
    
    shiny::observeEvent(input$about_stats, {
      
      shiny::showModal(
        shiny::modalDialog(
          title = "About Stats & Predictions",
          
          shiny::h5("Team Records"),
          shiny::p(
            "Wins, losses, and ties include official forfeit ",
            "decisions. Games where both teams are marked ",
            "as forfeiting are excluded."
          ),
          
          shiny::h5("Goals For / Goals Against"),
          shiny::p(
            "GF and GA reflect goals scored and allowed ",
            "in games actually played. Artificial goals ",
            "assigned for forfeits are excluded."
          ),
          
          shiny::h5("Head-to-Head Records"),
          shiny::p(
            "Head-to-head records include previous meetings ",
            "during the current season, including forfeit decisions."
          ),
          
          shiny::tags$hr(),
          
          shiny::h5("Offensive Rating (ETO-100)"),
          shiny::p(
            "A model-derived measure of a team's estimated ",
            "offensive strength, scaled from 0 to 100. ",
            "Higher values indicate stronger offensive performance."
          ),
          
          shiny::h5("Defensive Rating (ETD-100)"),
          shiny::p(
            "A model-derived measure of a team's estimated ",
            "defensive strength, scaled from 0 to 100. ",
            "Higher values indicate stronger defensive performance."
          ),
          
          shiny::p(
            shiny::tags$small(
              "These ratings describe estimated team quality. ",
              "They are not percentages, percentile rankings, ",
              "or probabilities of winning a game."
            )
          ),
          
          shiny::h5("Game Predictions"),
          shiny::p(
            "The prediction model estimates three possible ",
            "outcomes for each matchup: an away win, a tie, ",
            "or a home win. The three probabilities sum to 100%."
          ),
          
          shiny::p(
            "Predictions and team ratings are retrieved from ",
            "the latest available model snapshot for each game. ",
            "The ratings shown alongside a prediction come ",
            "from that same snapshot."
          ),
          
          shiny::p(
            shiny::tags$small(
              "Predictions are statistical estimates, ",
              "not guarantees of game outcomes."
            )
          ),
          
          easyClose = TRUE,
          footer = shiny::modalButton("Close"),
          size = "m"
        )
      )
    })
    
    # --------------------------------------------------
    # Retrieve and combine game data
    # --------------------------------------------------
    
    games <- shiny::reactive({
      
      schedule <- get_weekly_games(pool)
      stats <- get_matchup_stats(pool)
      predictions <- get_game_predictions(pool)
      
      # Separate home and away statistics
      home_stats <- stats[
        stats$side == "home",
        ,
        drop = FALSE
      ]
      
      away_stats <- stats[
        stats$side == "away",
        ,
        drop = FALSE
      ]
      
      # Prefix statistic names
      stat_columns <- setdiff(
        names(stats),
        c("game_id", "team_id", "side")
      )
      
      names(home_stats)[
        match(stat_columns, names(home_stats))
      ] <- paste0("home_", stat_columns)
      
      names(away_stats)[
        match(stat_columns, names(away_stats))
      ] <- paste0("away_", stat_columns)
      
      # Remove redundant identifiers
      home_stats <- home_stats[
        ,
        !names(home_stats) %in% c("team_id", "side"),
        drop = FALSE
      ]
      
      away_stats <- away_stats[
        ,
        !names(away_stats) %in% c("team_id", "side"),
        drop = FALSE
      ]
      
      # Preserve original schedule ordering
      schedule$.order <- seq_len(nrow(schedule))
      
      combined <- merge(
        schedule,
        home_stats,
        by = "game_id",
        all.x = TRUE
      )
      
      combined <- merge(
        combined,
        away_stats,
        by = "game_id",
        all.x = TRUE
      )
      
      # Merge latest predictions and ratings
      combined <- merge(
        combined,
        predictions,
        by = "game_id",
        all.x = TRUE
      )
      
      combined <- combined[
        order(combined$.order),
        ,
        drop = FALSE
      ]
      
      combined$.order <- NULL
      
      combined
    })
    
    # --------------------------------------------------
    # Division selection
    # --------------------------------------------------
    
    shiny::observe({
      
      x <- games()
      
      divisions <- unique(
        x[
          order(x$regular_weekday, x$division_name),
          c("division_id", "division_name")
        ]
      )
      
      choices <- stats::setNames(
        as.character(divisions$division_id),
        divisions$division_name
      )
      
      choices <- c("All Divisions" = "all", choices)
      
      selected <- shiny::isolate(input$division)
      
      if (is.null(selected) ||
          !selected %in% unname(choices)) {
        selected <- "all"
      }
      
      shiny::updateRadioButtons(
        session,
        "division",
        choices = choices,
        selected = selected,
        inline = TRUE
      )
    })
    
    filtered_games <- shiny::reactive({
      
      x <- games()
      
      if (!is.null(input$division) &&
          input$division != "all") {
        
        x <- x[
          as.character(x$division_id) == input$division,
          ,
          drop = FALSE
        ]
      }
      
      x
    })
    
    # --------------------------------------------------
    # Team rating display
    # --------------------------------------------------
    
    make_team_ratings <- function(game, side) {
      
      offense <- game[[paste0(side, "_eto_100")]]
      defense <- game[[paste0(side, "_etd_100")]]
      
      if (is.na(offense) && is.na(defense)) {
        return(NULL)
      }
      
      shiny::div(
        class = "team-ratings",
        style = paste(
          "margin-top: 12px;",
          "padding-top: 10px;",
          "border-top: 1px solid rgba(150,160,175,0.18);",
          "display: flex;",
          "flex-direction: column;",
          "gap: 5px;"
        ),
        
        shiny::div(
          style = paste(
            "display: flex;",
            "justify-content: space-between;",
            "align-items: center;",
            "gap: 8px;",
            "font-size: 0.78rem;"
          ),
          
          shiny::span(
            style = "color: #aab4c4;",
            "Offensive Rating"
          ),
          
          shiny::strong(
            style = paste(
              "font-size: 0.9rem;",
              "font-variant-numeric: tabular-nums;"
            ),
            fmt_rating(offense)
          )
        ),
        
        shiny::div(
          style = paste(
            "display: flex;",
            "justify-content: space-between;",
            "align-items: center;",
            "gap: 8px;",
            "font-size: 0.78rem;"
          ),
          
          shiny::span(
            style = "color: #aab4c4;",
            "Defensive Rating"
          ),
          
          shiny::strong(
            style = paste(
              "font-size: 0.9rem;",
              "font-variant-numeric: tabular-nums;"
            ),
            fmt_rating(defense)
          )
        )
      )
    }
    
    # --------------------------------------------------
    # Prediction probability bar
    # --------------------------------------------------
    
    make_prediction_bar <- function(game) {
      
      p_home <- game$p_home_win
      p_tie <- game$p_tie
      p_away <- game$p_away_win
      
      probabilities <- c(
        away = as.numeric(p_away),
        tie = as.numeric(p_tie),
        home = as.numeric(p_home)
      )
      
      # Keep the original card intact when no
      # prediction is available for a game.
      if (anyNA(probabilities) ||
          any(!is.finite(probabilities)) ||
          any(probabilities < 0) ||
          any(probabilities > 1) ||
          abs(sum(probabilities) - 1) > 0.001) {
        return(NULL)
      }
      
      # The bar follows the existing card layout:
      # away team on the left, home team on the right.
      colors <- c(
        away = "#3b82f6",
        tie = "#94a3b8",
        home = "#f59e0b"
      )
      
      # Use flex proportions to avoid rounded widths
      # causing the segments to exceed 100%.
      bar_segment <- function(outcome) {
        
        shiny::div(
          style = paste0(
            "flex: ", probabilities[[outcome]], " 1 0%;",
            "background-color: ", colors[[outcome]], ";",
            "height: 100%;",
            "min-width: 0;"
          )
        )
      }
      
      shiny::div(
        class = "matchup-prediction",
        style = paste(
          "margin-top: 15px;",
          "padding-top: 14px;",
          "border-top: 1px solid rgba(150,160,175,0.18);"
        ),
        
        shiny::div(
          style = paste(
            "text-align: center;",
            "font-size: 0.73rem;",
            "font-weight: 700;",
            "letter-spacing: 0.07em;",
            "color: #aab4c4;",
            "margin-bottom: 12px;"
          ),
          "PREDICTED GAME OUTCOME"
        ),
        
        # Probability labels
        shiny::div(
          style = paste(
            "display: grid;",
            "grid-template-columns: 1fr 1fr 1fr;",
            "gap: 8px;",
            "margin-bottom: 10px;"
          ),
          
          shiny::div(
            style = "text-align: left;",
            
            shiny::div(
              style = paste(
                "font-size: 1.15rem;",
                "font-weight: 700;",
                "color: #60a5fa;",
                "font-variant-numeric: tabular-nums;"
              ),
              fmt_probability(p_away)
            ),
            
            shiny::div(
              style = "font-size: 0.72rem; color: #aab4c4;",
              "Away Win"
            )
          ),
          
          shiny::div(
            style = "text-align: center;",
            
            shiny::div(
              style = paste(
                "font-size: 1.15rem;",
                "font-weight: 700;",
                "color: #cbd5e1;",
                "font-variant-numeric: tabular-nums;"
              ),
              fmt_probability(p_tie)
            ),
            
            shiny::div(
              style = "font-size: 0.72rem; color: #aab4c4;",
              "Tie"
            )
          ),
          
          shiny::div(
            style = "text-align: right;",
            
            shiny::div(
              style = paste(
                "font-size: 1.15rem;",
                "font-weight: 700;",
                "color: #fbbf24;",
                "font-variant-numeric: tabular-nums;"
              ),
              fmt_probability(p_home)
            ),
            
            shiny::div(
              style = "font-size: 0.72rem; color: #aab4c4;",
              "Home Win"
            )
          )
        ),
        
        # Stacked probability bar
        shiny::div(
          role = "img",
          `aria-label` = sprintf(
            paste0(
              "Predicted probabilities: away win %s, ",
              "tie %s, home win %s"
            ),
            fmt_probability(p_away),
            fmt_probability(p_tie),
            fmt_probability(p_home)
          ),
          
          style = paste(
            "display: flex;",
            "width: 100%;",
            "height: 15px;",
            "overflow: hidden;",
            "border-radius: 8px;",
            "background: #334155;"
          ),
          
          bar_segment("away"),
          bar_segment("tie"),
          bar_segment("home")
        ),
        
        # Model snapshot
        shiny::div(
          style = paste(
            "text-align: center;",
            "font-size: 0.69rem;",
            "color: #94a3b8;",
            "margin-top: 9px;"
          ),
          
          paste0(
            "Model snapshot: ",
            format(
              as.Date(game$snapshot_date),
              "%b %d, %Y"
            )
          )
        )
      )
    }
    
    # --------------------------------------------------
    # Construct one game card
    # --------------------------------------------------
    
    make_game_card <- function(game) {
      
      start_time <- as.POSIXct(
        paste(game$game_date, game$game_time),
        format = "%Y-%m-%d %H:%M:%S",
        tz = "America/Los_Angeles"
      )
      
      completed <- isTRUE(game$has_final_score)
      
      status <- if (completed) {
        "Final"
      } else if (!is.na(start_time) &&
                 Sys.time() >= start_time) {
        "Awaiting Result"
      } else {
        "Scheduled"
      }
      
      score <- if (completed) {
        paste(
          game$away_score,
          "-",
          game$home_score
        )
      } else {
        "VS"
      }
      
      game_time <- if (!is.na(start_time)) {
        format(start_time, "%a, %b %d · %I:%M %p")
      } else {
        as.character(game$game_date)
      }
      
      record <- function(side) {
        paste(
          game[[paste0(side, "_wins")]],
          game[[paste0(side, "_losses")]],
          game[[paste0(side, "_ties")]],
          sep = "-"
        )
      }
      
      goals <- function(side) {
        paste0(
          game[[paste0(side, "_goals_for")]],
          " / ",
          game[[paste0(side, "_goals_against")]]
        )
      }
      
      h2h <- function(side) {
        paste(
          game[[paste0(side, "_h2h_wins")]],
          game[[paste0(side, "_h2h_losses")]],
          game[[paste0(side, "_h2h_ties")]],
          sep = "-"
        )
      }
      
      shiny::div(
        class = "matchup-card",
        
        # Existing card header
        shiny::div(
          class = "matchup-header",
          
          shiny::span(game_time),
          
          shiny::span(
            class = "matchup-status",
            status
          )
        ),
        
        # Existing team layout with added ratings
        shiny::div(
          class = "matchup-teams",
          
          shiny::div(
            class = "matchup-team",
            
            shiny::strong(game$away_team),
            shiny::tags$small("Away"),
            
            shiny::div(
              class = "team-record",
              record("away")
            ),
            
            shiny::div(
              class = "team-goals",
              paste("GF/GA:", goals("away"))
            ),
            
            make_team_ratings(game, "away")
          ),
          
          shiny::div(
            class = "matchup-score",
            score
          ),
          
          shiny::div(
            class = "matchup-team",
            
            shiny::strong(game$home_team),
            shiny::tags$small("Home"),
            
            shiny::div(
              class = "team-record",
              record("home")
            ),
            
            shiny::div(
              class = "team-goals",
              paste("GF/GA:", goals("home"))
            ),
            
            make_team_ratings(game, "home")
          )
        ),
        
        # Existing head-to-head section
        shiny::div(
          class = "matchup-h2h",
          
          shiny::div(
            class = "h2h-title",
            "HEAD-TO-HEAD THIS SEASON"
          ),
          
          shiny::div(
            class = "h2h-record",
            h2h("away"),
            shiny::span("vs"),
            h2h("home")
          )
        ),
        
        # New prediction visualization
        make_prediction_bar(game),
        
        # Existing footer
        shiny::div(
          class = "matchup-footer",
          game$rink
        )
      )
    }
    
    # --------------------------------------------------
    # Render matchup cards
    # --------------------------------------------------
    
    output$game_cards <- shiny::renderUI({
      
      x <- filtered_games()
      
      if (nrow(x) == 0) {
        return(
          shiny::p("No games scheduled for this week.")
        )
      }
      
      regular <- x[
        !x$is_off_day,
        ,
        drop = FALSE
      ]
      
      off_day <- x[
        x$is_off_day,
        ,
        drop = FALSE
      ]
      
      # Group cards by division
      render_section <- function(data) {
        
        if (nrow(data) == 0) {
          return(NULL)
        }
        
        division_ids <- unique(data$division_id)
        
        shiny::tagList(
          lapply(division_ids, function(div_id) {
            
            div_games <- data[
              data$division_id == div_id,
              ,
              drop = FALSE
            ]
            
            shiny::div(
              class = "division-section",
              
              shiny::h4(div_games$division_name[1]),
              
              shiny::div(
                class = "matchup-grid",
                
                lapply(
                  seq_len(nrow(div_games)),
                  function(i) {
                    make_game_card(div_games[i, ])
                  }
                )
              )
            )
          })
        )
      }
      
      shiny::tagList(
        
        render_section(regular),
        
        if (nrow(off_day) > 0) {
          shiny::div(
            class = "offday-section",
            
            shiny::h3("Off-Day Games"),
            
            shiny::p(
              paste(
                "Games scheduled outside their",
                "division's regular playing day."
              )
            ),
            
            render_section(off_day)
          )
        }
      )
    })
  })
}
