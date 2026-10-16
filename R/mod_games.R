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
    
    # Show methodology explanation
    shiny::observeEvent(input$about_stats, {
      
      shiny::showModal(
        shiny::modalDialog(
          title = "About Stats & Predictions",
          
          shiny::h5("Team Records"),
          shiny::p(
            "Wins, losses, and ties include official forfeit decisions. ",
            "Games where both teams are marked as forfeiting are excluded."
          ),
          
          shiny::h5("Goals For / Goals Against"),
          shiny::p(
            "GF and GA reflect goals scored and allowed in games ",
            "actually played. Artificial goals assigned for forfeits ",
            "are excluded."
          ),
          
          shiny::h5("Head-to-Head Records"),
          shiny::p(
            "Head-to-head records include previous meetings ",
            "during the current season, including forfeit decisions."
          ),
          
          shiny::h5("Model Predictions"),
          shiny::p(
            "Win probabilities and details about the prediction ",
            "model will be provided when predictions are introduced."
          ),
          
          easyClose = TRUE,
          footer = shiny::modalButton("Close"),
          size = "m"
        )
      )
      
    })
    
    games <- shiny::reactive({
      
      schedule <- get_weekly_games(pool)
      stats <- get_matchup_stats(pool)
      
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
      
      # Preserve the original schedule ordering
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
      
      combined <- combined[
        order(combined$.order),
        ,
        drop = FALSE
      ]
      
      combined$.order <- NULL
      
      combined
    })
    
    # Build division tabs in regular weekday order.
    shiny::observe({
      
      x <- games()
      
      divisions <- unique(
        x[order(
          x$regular_weekday,
          x$division_name
        ), c("division_id", "division_name")]
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
    
    # Construct one game card.
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
        
        shiny::div(
          class = "matchup-header",
          
          shiny::span(game_time),
          
          shiny::span(
            class = "matchup-status",
            status
          )
        ),
        
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
            )
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
            )
          )
        ),
        
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
        
        shiny::div(
          class = "matchup-footer",
          game$rink
        )
      )
    }
    
    output$game_cards <- shiny::renderUI({
      
      x <- filtered_games()
      
      if (nrow(x) == 0) {
        return(
          shiny::p("No games scheduled for this week.")
        )
      }
      
      regular <- x[!x$is_off_day, , drop = FALSE]
      off_day <- x[x$is_off_day, , drop = FALSE]
      
      # Group cards by division.
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
                lapply(seq_len(nrow(div_games)), function(i) {
                  make_game_card(div_games[i, ])
                })
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
              "Games scheduled outside their division's regular playing day."
            ),
            
            render_section(off_day)
          )
        }
      )
    })
  })
}