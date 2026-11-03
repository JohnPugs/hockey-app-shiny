
# ============================================================
# Beer League IQ
# Player Statistics Module
# R/mod_players.R
# ============================================================

mod_players_ui <- function(id) {
  
  ns <- shiny::NS(id)
  
  shiny::div(
    class = "container-fluid py-4 player-stats-page",
    
    shiny::tags$style(shiny::HTML("
      .player-stats-page .player-view-buttons {
        display: grid;
        grid-template-columns: repeat(3, minmax(0, 1fr));
        gap: 8px;
        margin-bottom: 20px;
      }

      .player-stats-page .player-view-buttons .btn {
        width: 100%;
        border-radius: 9px;
        font-weight: 600;
        padding: 11px 6px;
      }

      .player-stats-page .player-choice-buttons {
        display: flex;
        flex-wrap: wrap;
        gap: 8px;
        margin: 10px 0 18px;
      }
      .player-stats-page .player-choice-buttons .btn {
        border-radius: 9px;
        font-size: 0.86rem;
        font-weight: 600;
        padding: 9px 13px;
        white-space: normal;
      }
      .player-stats-page .player-alpha {
        display: flex;
        flex-wrap: wrap;
        gap: 5px;
        margin: 15px 0 20px;
      }

      .player-stats-page .player-alpha .btn {
        min-width: 34px;
        border-radius: 7px;
        padding: 6px 9px;
        font-size: 0.82rem;
      }

      .player-stats-page .player-table {
        width: 100%;
        font-size: 0.88rem;
        font-variant-numeric: tabular-nums;
      }

      .player-stats-page .player-table th {
        color: #aab4c4;
        font-size: 0.76rem;
        white-space: nowrap;
      }

      .player-stats-page .player-table td {
        padding: 11px 8px;
        vertical-align: middle;
      }

      .player-stats-page .player-table th:not(:nth-child(2)),
      .player-stats-page .player-table td:not(:nth-child(2)) {
        text-align: right;
      }

      .player-stats-page .player-name {
        color: #4DA3FF;
        font-weight: 600;
        text-decoration: none;
        white-space: normal;
      }

      .player-stats-page .player-name:hover {
        text-decoration: underline;
      }

      .player-stats-page .player-sort {
        color: #aab4c4;
        text-decoration: none;
        font-weight: 600;
      }

      .player-stats-page .player-sort:hover {
        color: #4DA3FF;
      }

      .player-stats-page .player-pqr {
        color: #4DA3FF;
        font-weight: 700;
      }

      .player-stats-page .player-note {
        color: #aab4c4;
        font-size: 0.76rem;
      }

      .player-stats-page .player-badge {
        display: inline-block;
        border: 1px solid #64748b;
        border-radius: 5px;
        padding: 2px 5px;
        font-size: 0.68rem;
        color: #cbd5e1;
        margin-left: 5px;
      }

      .player-stats-page .player-table-wrap {
        overflow-x: auto;
      }

      .player-stats-page .player-summary {
        color: #aab4c4;
        font-size: 0.82rem;
      }

      .player-stats-page .player-table-container {
        position: relative;
        min-height: 240px;
      }
      .player-stats-page .player-loading {
        position: absolute;
        z-index: 5;
        inset: 0;
        max-width: none;
        margin: 0;
        padding: 40px 24px;
        text-align: center;
        background: #222;
      }
      .player-stats-page .player-loading-icon {
        font-size: 2.5rem;
        color: #4DA3FF;
        margin-bottom: 18px;
      }
      .player-stats-page .player-loading p {
        color: #aab4c0;
        font-size: 0.9rem;
        margin-bottom: 24px;
      }
      .player-stats-page .player-loading .progress {
        background-color: #2b3542;
        border-radius: 6px;
      }

      @media (max-width: 650px) {
        .player-stats-page .player-table {
          font-size: 0.8rem;
        }

        .player-stats-page .player-table td {
          padding: 9px 6px;
        }
      }
    ")),
    
    # Browser-side indicator: no dependency on a Shiny custom message.
    # The actual table is left visible to Shiny so its output is not suspended.
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
            const ready = !!wrapper.querySelector('.player-table, .player-empty-state');
            if (ready) overlay.style.display = 'none';
          }
          const observer = new MutationObserver(update);
          observer.observe(wrapper, {childList: true, subtree: true});
          update();
        }
        // Scripts in a Shiny nav panel may execute before its child nodes exist.
        // Retry initialization until the page elements are in the DOM.
        const initTimer = setInterval(function() {
          initialize();
          const overlay = document.getElementById(overlayId);
          if (overlay && overlay.dataset.initialized) clearInterval(initTimer);
        }, 100);
      })();
    ", ns("players_loading_overlay"), ns("players_table_wrapper")))),

    shiny::h2("Player Statistics", class = "mb-2"),
    
    shiny::p(
      class = "text-muted mb-4",
      "Explore current-season skater performance, ",
      "player quality ratings, and scoring leaders."
    ),
    
    shiny::uiOutput(ns("view_buttons")),
    
    shiny::uiOutput(ns("filters")),
    
    shiny::uiOutput(ns("alphabet")),
    
    shiny::div(
      class = "d-flex justify-content-end mb-3",
      
      shiny::actionButton(
        ns("explain_rankings"),
        label = "How is PQR calculated?",
        icon = shiny::icon("circle-info"),
        class = "btn-sm btn-outline-info"
      )
    ),
    
    bslib::card(
      bslib::card_header(
        shiny::div(
          class = "d-flex justify-content-between align-items-center",
          shiny::strong(
            shiny::textOutput(ns("table_title"), inline = TRUE)
          ),
          shiny::span(
            class = "player-summary",
            shiny::textOutput(ns("table_count"), inline = TRUE)
          )
        )
      ),
      
      bslib::card_body(
        shiny::div(
          class = "player-table-container",
          shiny::div(
            id = ns("players_loading_overlay"),
            class = "player-loading",
            shiny::div(
              class = "player-loading-icon",
              shiny::icon("users")
            ),
            shiny::h5("Loading player statistics"),
            shiny::p(
              "Retrieving 1,000+ player rankings ",
              "and performance statistics..."
            ),
            shiny::div(
              class = "progress",
              style = "height: 6px;",
              shiny::div(
                class = paste(
                  "progress-bar progress-bar-striped",
                  "progress-bar-animated bg-info"
                ),
                role = "progressbar",
                style = "width: 100%;"
              )
            )
          ),
          shiny::div(
            id = ns("players_table_wrapper"),
            shiny::uiOutput(ns("rankings_table"))
          )
        )
      )
    )
    # ,shiny::div(
    #   class = "mt-3 player-note",
    #   shiny::actionLink(
    #     ns("explain_rankings"),
    #     "How are players ranked?"
    #   )
    # )
  )
}


# ============================================================
# Server
# ============================================================

mod_players_server <- function(id, 
                               pool,
                               active = shiny::reactive(TRUE)) {
  
  shiny::moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # --------------------------------------------------------
    # State
    # --------------------------------------------------------
    
    current_view <- shiny::reactiveVal("league")
    current_letter <- shiny::reactiveVal("TOP")
    
    
    selected_division <- shiny::reactiveVal(NULL)
    selected_team <- shiny::reactiveVal(NULL)
    
    # --------------------------------------------------------
    # Load rankings - old
    # --------------------------------------------------------
    
    # rankings <- shiny::reactive({
    #   get_player_rankings(pool)
    # })
    
    # --------------------------------------------------------
    # Load rankings only when Players page is active
    # --------------------------------------------------------
    
    rankings_cache <- shiny::reactiveVal(NULL)
    
    shiny::observeEvent(active(), {
      
      if (!isTRUE(active()) ||
          !is.null(rankings_cache())) {
        return()
      }
      
      rankings_cache(
        get_player_rankings(pool)
      )

      
    }, ignoreInit = FALSE)
    
    
    rankings <- shiny::reactive({
      
      shiny::req(isTRUE(active()))
      shiny::req(rankings_cache())
      
      rankings_cache()
      
    })
    
    # --------------------------------------------------------
    # Helpers
    # --------------------------------------------------------
    
    safe_rate <- function(x, n) {
      ifelse(n > 0, x / n, NA_real_)
    }
    
    player_last_initial <- function(name) {
      name <- trimws(as.character(name))
      name <- gsub("\\s+", " ", name)
      last <- sub(".* ", "", name)
      toupper(substr(last, 1, 1))
    }
    
    # --------------------------------------------------------
    # View buttons
    # --------------------------------------------------------
    
    output$view_buttons <- shiny::renderUI({
      
      views <- c(
        league = "League",
        division = "Division",
        team = "Team"
      )
      
      buttons <- lapply(names(views), function(v) {
        
        shiny::actionButton(
          ns(paste0("view_", v)),
          views[[v]],
          class = if (identical(current_view(), v)) {
            "btn-primary"
          } else {
            "btn-outline-primary"
          }
        )
      })
      
      shiny::div(
        class = "player-view-buttons",
        do.call(shiny::tagList, buttons)
      )
    })
    
    shiny::observeEvent(input$view_league, {
      current_view("league")
      current_letter("TOP")
    })
    
    shiny::observeEvent(input$view_division, {
      current_view("division")
      current_letter("TOP")
    })
    
    shiny::observeEvent(input$view_team, {
      current_view("team")
      current_letter("TOP")
    })
    
    # --------------------------------------------------------
    # Division and team choices
    # --------------------------------------------------------
    
    division_choices <- shiny::reactive({
      
      d <- rankings()
      
      d <- unique(
        d[, c("division_id", "division_name")]
      )
      
      d <- d[order(d$division_name), , drop = FALSE]
      
      stats::setNames(
        as.character(d$division_id),
        d$division_name
      )
    })
    
    shiny::observe({
      
      choices <- division_choices()
      ids <- unname(choices)
      
      if (!length(ids)) return()
      
      if (is.null(selected_division()) ||
          !selected_division() %in% ids) {
        selected_division(ids[[1]])
      }
    })
    
    team_choices <- shiny::reactive({
      
      shiny::req(selected_division())
      
      d <- rankings()
      
      d <- d[
        as.character(d$division_id) ==
          selected_division(),
        ,
        drop = FALSE
      ]
      
      d <- unique(
        d[, c("team_id", "team_name")]
      )
      
      d <- d[order(d$team_name), , drop = FALSE]
      
      stats::setNames(
        as.character(d$team_id),
        d$team_name
      )
    })
    
    shiny::observe({
      
      choices <- team_choices()
      ids <- unname(choices)
      
      if (!length(ids)) {
        selected_team(NULL)
        return()
      }
      
      if (is.null(selected_team()) ||
          !selected_team() %in% ids) {
        selected_team(ids[[1]])
      }
    })
    
    # --------------------------------------------------------
    # Mobile-friendly division and team buttons
    # --------------------------------------------------------

    output$filters <- shiny::renderUI({
      if (current_view() == "league") return(NULL)

      divisions <- division_choices()
      division_buttons <- lapply(seq_along(divisions), function(i) {
        division_id <- unname(divisions[[i]])
        shiny::actionButton(
          ns(paste0("division_btn_", division_id)),
          names(divisions)[[i]],
          class = if (identical(selected_division(), division_id)) {
            "btn-primary"
          } else {
            "btn-outline-primary"
          }
        )
      })

      team_section <- NULL
      if (current_view() == "team" && !is.null(selected_division())) {
        teams <- team_choices()
        team_buttons <- lapply(seq_along(teams), function(i) {
          team_id <- unname(teams[[i]])
          shiny::actionButton(
            ns(paste0("team_btn_", team_id)),
            names(teams)[[i]],
            class = if (identical(selected_team(), team_id)) {
              "btn-primary"
            } else {
              "btn-outline-primary"
            }
          )
        })
        team_section <- shiny::tagList(
          shiny::tags$h5("Team"),
          shiny::div(
            class = "player-choice-buttons",
            do.call(shiny::tagList, team_buttons)
          )
        )
      }

      shiny::tagList(
        shiny::tags$h5("Division"),
        shiny::div(
          class = "player-choice-buttons",
          do.call(shiny::tagList, division_buttons)
        ),
        team_section
      )
    })

    # Register one observer per stable division ID.
    registered_divisions <- shiny::reactiveVal(character())
    shiny::observe({
      divisions <- division_choices()
      fresh <- setdiff(unname(divisions), registered_divisions())
      registered_divisions(union(registered_divisions(), fresh))
      lapply(fresh, function(division_id) {
        local({
          id <- division_id
          shiny::observeEvent(input[[paste0("division_btn_", id)]], {
            selected_division(id)
            current_letter("TOP")
          }, ignoreInit = TRUE)
        })
      })
    })

    # Team IDs are dynamic, so use the same button-input pattern.
    registered_teams <- shiny::reactiveVal(character())
    shiny::observe({
      teams <- team_choices()
      fresh <- setdiff(unname(teams), registered_teams())
      registered_teams(union(registered_teams(), fresh))
      lapply(fresh, function(team_id) {
        local({
          id <- team_id
          shiny::observeEvent(input[[paste0("team_btn_", id)]], {
            selected_team(id)
          }, ignoreInit = TRUE)
        })
      })
    })

    # --------------------------------------------------------
    # Alphabet navigation
    # --------------------------------------------------------
    
    output$alphabet <- shiny::renderUI({
      
      if (current_view() == "team") {
        return(NULL)
      }
      
      letters <- c("TOP", LETTERS)
      
      buttons <- lapply(letters, function(letter) {
        
        shiny::actionButton(
          ns(paste0("letter_", letter)),
          if (letter == "TOP") "Top 20" else letter,
          class = if (identical(current_letter(), letter)) {
            "btn-primary"
          } else {
            "btn-outline-secondary"
          }
        )
      })
      
      shiny::div(
        class = "player-alpha",
        do.call(shiny::tagList, buttons)
      )
    })
    
    for (letter in c("TOP", LETTERS)) {
      
      local({
        
        this_letter <- letter
        
        shiny::observeEvent(
          input[[paste0("letter_", this_letter)]],
          {
            current_letter(this_letter)
          }
        )
      })
    }
    
    # --------------------------------------------------------
    # Aggregate rankings by scope
    # --------------------------------------------------------
    
    aggregated <- shiny::reactive({
      
      d <- rankings()
      view <- current_view()
      
      if (view %in% c("division", "team")) {
        
        shiny::req(selected_division())
        
        d <- d[
          as.character(d$division_id) ==
            selected_division(),
          ,
          drop = FALSE
        ]
      }
      
      if (view == "team") {
        
        shiny::req(selected_team())
        
        d <- d[
          as.character(d$team_id) ==
            selected_team(),
          ,
          drop = FALSE
        ]
      }
      
      if (!nrow(d)) return(d)
      
      # Team view retains individual assignments.
      if (view == "team") {
        return(d)
      }
      
      # League: one record per player.
      # Division: one record per player within division.
      #
      # PQR is player-level, not team-level.
      # Rate statistics must be recalculated from totals.
      
      keys <- if (view == "league") {
        "player_id"
      } else {
        c("player_id", "division_id")
      }
      
      groups <- split(
        d,
        do.call(
          interaction,
          c(
            d[keys],
            list(
              drop = TRUE,
              lex.order = TRUE
            )
          )
        )
      )
      
      result <- lapply(groups, function(g) {
        
        first <- g[1, , drop = FALSE]
        
        first$gp <- sum(g$gp, na.rm = TRUE)
        first$goals <- sum(g$goals, na.rm = TRUE)
        first$assists <- sum(g$assists, na.rm = TRUE)
        first$points <- sum(g$points, na.rm = TRUE)
        
        first$gpg <- safe_rate(first$goals, first$gp)
        first$apg <- safe_rate(first$assists, first$gp)
        first$ppg <- safe_rate(first$points, first$gp)
        
        # PQR is the same across assignments.
        vals <- g$pqr[!is.na(g$pqr)]
        first$pqr <- if (length(vals)) vals[[1]] else NA_real_
        
        # Projected PPG is division-specific.
        # Do not average across different divisions.
        if (view == "league") {
          first$projected_ppg <- NA_real_
        } else {
          vals <- g$projected_ppg[
            !is.na(g$projected_ppg)
          ]
          
          first$projected_ppg <- if (length(vals)) {
            vals[[1]]
          } else {
            NA_real_
          }
        }
        
        # Combined attendance uses rostered assignments only.
        # Guest appearances have no regular-roster denominator.
        rostered <- g[g$roster_status == "rostered", , drop = FALSE]
        eligible <- sum(rostered$completed_team_games, na.rm = TRUE)
        attended <- sum(rostered$gp, na.rm = TRUE)
        first$attendance_pct <- if (eligible > 0) {
          round(100 * attended / eligible, 1)
        } else {
          NA_real_
        }
        
        first$roster_status <- if (
          any(g$roster_status == "rostered")
        ) "rostered" else "guest"
        
        first
      })
      
      do.call(rbind, result)
    })
    
    # --------------------------------------------------------
    # Filter and rank exclusively by PQR
    # --------------------------------------------------------
    
    displayed <- shiny::reactive({
      
      d <- aggregated()
      
      if (!nrow(d)) return(d)
      
      
      if (current_view() != "team") {
        
        # Only observed skaters enter performance rankings.
        d <- d[d$gp > 0, , drop = FALSE]
        
        if (!nrow(d)) return(d)
        
        # PQR is the exclusive ranking metric.
        score <- d$pqr
        
        d <- d[
          order(
            -score,
            -d$points,
            d$player_name,
            na.last = TRUE
          ),
          ,
          drop = FALSE
        ]
        
        d$rank <- seq_len(nrow(d))
        
        if (current_letter() == "TOP") {
          
          d <- head(d, 20)
          
        } else {
          
          initials <- player_last_initial(d$player_name)
          
          d <- d[
            initials == current_letter(),
            ,
            drop = FALSE
          ]
        }
        
      } else {
        
        # Team view includes zero-GP rostered players.
        d <- d[
          order(
            -d$pqr,
            d$player_name,
            na.last = TRUE
          ),
          ,
          drop = FALSE
        ]
        
        d$rank <- seq_len(nrow(d))
      }
      
      if (!nrow(d)) return(d)
      
      d
    })
    
    # --------------------------------------------------------
    # Compact table columns
    # --------------------------------------------------------
    
    columns <- c(
      player_name = "Player",
      gpg = "G/G",
      apg = "A/G",
      attendance_pct = "ATT%",
      pqr = "PQR"
    )
    
    # --------------------------------------------------------
    # Formatting
    # --------------------------------------------------------
    
    format_number <- function(x, digits = 0) {
      
      if (length(x) == 0 || is.na(x)) {
        return("—")
      }
      
      formatC(
        x,
        format = "f",
        digits = digits
      )
    }
    
    # --------------------------------------------------------
    # Table
    # --------------------------------------------------------
    
    output$rankings_table <- shiny::renderUI({
      
      d <- displayed()
      
      if (!nrow(d)) {
        
        return(
          shiny::div(
            class = "player-empty-state text-muted py-4 text-center",
            "No players found for this selection."
          )
        )
      }
      
      visible_columns <- names(columns)
      
      header <- shiny::tags$tr(
        
        shiny::tags$th("#"),
        
        lapply(visible_columns, function(column) {
          
          shiny::tags$th(
            columns[[column]]
          )
        })
      )
      
      rows <- lapply(seq_len(nrow(d)), function(i) {
        
        x <- d[i, , drop = FALSE]
        
        player_link <- shiny::tags$a(
          href = "#",
          class = "player-name",
          x$player_name,
          onclick = sprintf(
            paste0(
              "Shiny.setInputValue('%s', %s, ",
              "{priority: 'event'}); return false;"
            ),
            ns("selected_player"),
            as.integer(x$player_id)
          )
        )
        
        badges <- list()
        
        if (isTRUE(x$provisional)) {
          
          badges <- append(
            badges,
            list(
              shiny::span(
                class = "player-badge",
                "Provisional"
              )
            )
          )
        }
        
        if (identical(x$roster_status, "guest")) {
          
          badges <- append(
            badges,
            list(
              shiny::span(
                class = "player-badge",
                "Guest"
              )
            )
          )
        }
        
        values <- list(
          player_name = shiny::tagList(
            player_link,
            badges
          ),
          
          pqr = shiny::span(
            class = "player-pqr",
            if (is.na(x$pqr)) {
              "Unrated"
            } else {
              sprintf("%.0f", x$pqr)
            }
          ),
          
          gp = format_number(x$gp),
          goals = format_number(x$goals),
          assists = format_number(x$assists),
          points = format_number(x$points),
          
          gpg = format_number(x$gpg, 2),
          apg = format_number(x$apg, 2),
          ppg = format_number(x$ppg, 2),
          
          attendance_pct = if (
            is.na(x$attendance_pct)
          ) {
            "N/A"
          } else {
            paste0(
              format_number(x$attendance_pct, 1),
              "%"
            )
          }
        )
        
        shiny::tags$tr(
          shiny::tags$td(x$rank),
          
          lapply(visible_columns, function(column) {
            shiny::tags$td(values[[column]])
          })
        )
      })
      
      shiny::div(
        class = "player-table-wrap",
        
        shiny::tags$table(
          class = paste(
            "table table-dark table-hover",
            "player-table"
          ),
          
          shiny::tags$thead(header),
          
          shiny::tags$tbody(
            do.call(shiny::tagList, rows)
          )
        )
      )
    })
    
    # --------------------------------------------------------
    # Dynamic titles
    # --------------------------------------------------------
    
    output$table_title <- shiny::renderText({
      
      view <- current_view()
      
      if (view == "league") {
        "League Player Rankings"
      } else if (view == "division") {
        "Division Player Rankings"
      } else {
        "Team Player Statistics"
      }
    })
    
    output$table_count <- shiny::renderText({
      
      n <- nrow(displayed())
      
      paste(
        format(n, big.mark = ","),
        if (n == 1) "player" else "players"
      )
    })
    
    # --------------------------------------------------------
    # Player detail modal: historical seasons and linemates
    # --------------------------------------------------------

    player_detail_id <- shiny::reactiveVal(NULL)

    # Format a database result without assuming every season has
    # the same set of fields. Only actual returned fields are shown.
    detail_table <- function(data, fields, labels, digits = character()) {
      keep <- intersect(fields, names(data))
      if (!nrow(data) || !length(keep)) {
        return(shiny::p(class = "text-muted", "No records available."))
      }
      x <- data[, keep, drop = FALSE]
      names(x) <- unname(labels[keep])
      for (nm in names(x)) {
        if (is.numeric(x[[nm]])) {
          dec <- if (nm %in% digits) 2 else 0
          x[[nm]] <- ifelse(
            is.na(x[[nm]]), "N/A",
            formatC(x[[nm]], format = "f", digits = dec,
                    big.mark = ",")
          )
        } else {
          x[[nm]] <- ifelse(is.na(x[[nm]]), "N/A", as.character(x[[nm]]))
        }
      }
      shiny::div(
        class = "table-responsive",
        shiny::tags$table(
          class = "table table-dark table-striped table-sm align-middle",
          shiny::tags$thead(shiny::tags$tr(
            lapply(names(x), shiny::tags$th)
          )),
          shiny::tags$tbody(lapply(seq_len(nrow(x)), function(i) {
            shiny::tags$tr(lapply(x[i, , drop = TRUE], shiny::tags$td))
          }))
        )
      )
    }

    shiny::observeEvent(input$selected_player, {
      pid <- suppressWarnings(as.numeric(input$selected_player))
      if (length(pid) != 1L || is.na(pid)) return()
      d <- rankings()
      player <- d[d$player_id == pid, , drop = FALSE]
      if (!nrow(player)) return()

      player_detail_id(pid)
      name <- as.character(player$player_name[[1]])
      pqr <- player$pqr[[1]]
      rating <- if (is.na(pqr)) "Unrated" else sprintf("%.0f", pqr)
      career_gp <- if ("career_gp" %in% names(player)) {
        player$career_gp[[1]]
      } else NA_real_

      shiny::showModal(shiny::modalDialog(
        title = shiny::tagList(
          shiny::icon("user"), " ", name
        ),
        shiny::div(
          class = "d-flex flex-wrap gap-3 mb-3",
          shiny::tags$span(class = "badge bg-primary", paste("PQR", rating)),
          shiny::tags$span(
            class = "badge bg-secondary",
            paste("Career GP:", if (is.na(career_gp)) "N/A" else career_gp)
          ),
          if (isTRUE(player$provisional[[1]])) {
            shiny::tags$span(class = "badge bg-warning text-dark", "Provisional")
          }
        ),
        shiny::tags$style(shiny::HTML("
          .player-modal-loading {
            position: relative;
            min-height: 64px;
          }
          .player-modal-loading-indicator {
            position: absolute;
            inset: 0 0 auto 0;
            min-height: 58px;
            pointer-events: none;
          }
          .player-modal-loading-track {
            height: 5px;
            width: 100%;
            overflow: hidden;
            border-radius: 5px;
            background: #2b3542;
          }
          .player-modal-loading-track::before {
            content: '';
            display: block;
            height: 100%;
            width: 38%;
            border-radius: inherit;
            background: linear-gradient(90deg, #2263a4, #4DA3FF, #a3d5ff);
            animation: player-modal-slide 1.25s ease-in-out infinite;
          }
          .player-modal-loading-caption {
            display: block;
            padding-top: 12px;
            color: #aab4c4;
            font-size: .8rem;
          }
          /* Hide the loader as soon as Shiny supplies finished content. */
          .player-modal-loading:has(> .shiny-html-output:not(:empty):not(.recalculating))
            > .player-modal-loading-indicator {
            display: none;
          }
          .player-modal-loading > .shiny-html-output.recalculating {
            opacity: .35;
          }
          @keyframes player-modal-slide {
            0%   { transform: translateX(-110%); }
            100% { transform: translateX(275%); }
          }
          @media (prefers-reduced-motion: reduce) {
            .player-modal-loading-track::before { animation: none; width: 100%; }
          }
        ")),
        shiny::h5("Current season"),
        shiny::div(class = "player-modal-loading",
                   shiny::div(class = "player-modal-loading-indicator",
                     shiny::div(class = "player-modal-loading-track"),
                     shiny::span(class = "player-modal-loading-caption", "Loading player data...")),
                   shiny::uiOutput(ns("player_current_detail"))),
        shiny::hr(),
        shiny::h5("Career scoring trends"),
        shiny::div(class = "player-modal-loading",
                   shiny::div(class = "player-modal-loading-indicator",
                     shiny::div(class = "player-modal-loading-track"),
                     shiny::span(class = "player-modal-loading-caption", "Loading player data...")),
                   shiny::uiOutput(ns("player_history_detail"))),
        shiny::hr(),
        shiny::h5("Linemate chemistry"),
        shiny::p(
          class = "small text-muted",
          "Directional connections based on recorded game and scoring data. " ,
          "These are descriptive statistics, not causal estimates of chemistry."
        ),
        shiny::div(class = "player-modal-loading",
                   shiny::div(class = "player-modal-loading-indicator",
                     shiny::div(class = "player-modal-loading-track"),
                     shiny::span(class = "player-modal-loading-caption", "Loading player data...")),
                   shiny::uiOutput(ns("player_chemistry_detail"))),
        size = "l",
        easyClose = TRUE,
        footer = shiny::modalButton("Close")
      ))
    })

    # Current-season player assignments: compact, modal-specific table.
    output$player_current_detail <- shiny::renderUI({
      pid <- shiny::req(player_detail_id())
      d <- rankings()
      d <- d[as.character(d$player_id) == as.character(pid), , drop = FALSE]

      if (!nrow(d)) {
        return(shiny::p(class = "text-muted", "No current season statistics."))
      }

      # RPostgres returns PostgreSQL bigint fields as bit64::integer64.
      # Convert only values used in this display, without altering rankings().
      numeric_fields <- c("gp", "goals", "assists", "points",
                          "gpg", "apg", "ppg", "attendance_pct")
      for (field in intersect(numeric_fields, names(d))) {
        d[[field]] <- as.numeric(d[[field]])
      }

      fmt <- function(value, digits = 0L, suffix = "") {
        if (length(value) == 0L || is.na(value)) return("N/A")
        paste0(formatC(value, format = "f", digits = digits,
                       big.mark = ","), suffix)
      }

      headers <- c("Division", "Team", "GP", "G", "A", "PTS",
                   "G/G", "A/G", "P/G", "ATT%")

      rows <- lapply(seq_len(nrow(d)), function(i) {
        x <- d[i, , drop = FALSE]
        vals <- list(
          as.character(x$division_name[[1]]),
          as.character(x$team_name[[1]]),
          fmt(x$gp[[1]]),
          fmt(x$goals[[1]]),
          fmt(x$assists[[1]]),
          fmt(x$points[[1]]),
          fmt(x$gpg[[1]], 2L),
          fmt(x$apg[[1]], 2L),
          fmt(x$ppg[[1]], 2L),
          fmt(x$attendance_pct[[1]], 1L, "%")
        )
        shiny::tags$tr(lapply(seq_along(vals), function(j) {
          shiny::tags$td(
            class = if (j == 6L) "player-current-points" else NULL,
            vals[[j]]
          )
        }))
      })

      shiny::tagList(
        shiny::tags$style(shiny::HTML("
          .player-current-wrap { overflow-x: auto; border: 1px solid
            var(--bs-border-color); border-radius: 10px; }
          .player-current-table { width: 100%; margin: 0;
            font-size: .84rem; font-variant-numeric: tabular-nums; }
          .player-current-table thead th { color: #aab4c4;
            font-size: .72rem; font-weight: 600; letter-spacing: .035em;
            border-bottom: 1px solid var(--bs-border-color);
            background: rgba(255,255,255,.035); padding: 11px 10px;
            white-space: nowrap; }
          .player-current-table tbody td { padding: 12px 10px;
            vertical-align: middle; border-bottom-color:
            var(--bs-border-color); }
          .player-current-table tbody tr:last-child td { border-bottom: 0; }
          .player-current-table th:nth-child(n+3),
          .player-current-table td:nth-child(n+3) { text-align: right; }
          .player-current-table th:nth-child(-n+2),
          .player-current-table td:nth-child(-n+2) { text-align: left; }
          .player-current-table td:nth-child(-n+2) { font-weight: 500; }
          .player-current-table .player-current-points { color: #4DA3FF;
            font-weight: 700; }
          .player-current-table tbody tr:hover { background:
            rgba(77,163,255,.055); }
        ")),
        shiny::div(
          class = "player-current-wrap",
          shiny::tags$table(
            class = "table table-dark player-current-table",
            shiny::tags$thead(
              shiny::tags$tr(lapply(headers, shiny::tags$th))
            ),
            shiny::tags$tbody(do.call(shiny::tagList, rows))
          )
        )
      )
    })

    # Fetch on demand, only when a player is opened. Cache within session.
    # Cache within session.
    player_history <- shiny::reactive({
      
      pid <- shiny::req(player_detail_id())
      
      tryCatch({
        
        history <- DBI::dbGetQuery(
          pool,
          "
          SELECT
            v.*,
            s.season_name
          FROM public.vw_player_season_stats v
          LEFT JOIN public.seasons s
            ON s.id = v.season_id
          WHERE v.player_id = $1
          ORDER BY v.season_id DESC
          ",
          params = list(pid)
        )
        
        # Convert PostgreSQL bigint (integer64) columns
        # to standard R numeric values.
        i64_cols <- vapply(
          history,
          inherits,
          logical(1),
          "integer64"
        )
        
        history[i64_cols] <- lapply(
          history[i64_cols],
          as.numeric
        )
        
        history
        
      }, error = function(e) {
        
        warning(
          "Player history query failed: ",
          conditionMessage(e)
        )
        
        NULL
        
      })
      
    })

    # Three division-specific per-game charts with full-career reference lines.
    # Uses base R graphics, so no additional charting dependency is required.
    career_chart_data <- shiny::reactive({
      d <- player_history()
      if (is.null(d) || !nrow(d)) return(NULL)

      needed <- c("season_id", "division_name", "gp",
                  "goals", "assists", "points")
      if (!all(needed %in% names(d))) return(NULL)

      for (nm in c("season_id", "gp", "goals", "assists", "points")) {
        d[[nm]] <- as.numeric(d[[nm]])
      }
      d <- d[!is.na(d$season_id) & !is.na(d$division_name) &
               is.finite(d$gp) & d$gp > 0, , drop = FALSE]
      if (!nrow(d)) return(NULL)

      # Combine duplicate season/division rows by summing counts, not rates.
      key <- interaction(d$season_id, d$division_name, drop = TRUE)
      groups <- split(d, key)
      d <- do.call(rbind, lapply(groups, function(g) {
        data.frame(
          season_id = g$season_id[[1]],
          division_name = as.character(g$division_name[[1]]),
          gp = sum(g$gp, na.rm = TRUE),
          goals = sum(g$goals, na.rm = TRUE),
          assists = sum(g$assists, na.rm = TRUE),
          points = sum(g$points, na.rm = TRUE),
          stringsAsFactors = FALSE
        )
      }))
      d <- d[order(d$season_id, d$division_name), , drop = FALSE]

      original <- player_history()
      season_labels <- vapply(sort(unique(d$season_id)), function(id) {
        idx <- which(as.numeric(original$season_id) == id)
        nm <- if ("season_name" %in% names(original) && length(idx)) {
          as.character(original$season_name[idx[[1]]])
        } else NA_character_
        if (!length(nm) || is.na(nm) || !nzchar(nm)) {
          paste("Season", id)
        } else nm
      }, character(1))

      # Career averages use ALL available seasons. Plot only the latest three.
      all_seasons <- sort(unique(d$season_id))
      recent_seasons <- tail(all_seasons, 3L)
      recent_data <- d[d$season_id %in% recent_seasons, , drop = FALSE]

      list(data = recent_data, seasons = recent_seasons,
           labels = tail(season_labels, length(recent_seasons)),
           divisions = sort(unique(recent_data$division_name)),
           career = c(
             goals = sum(d$goals, na.rm = TRUE) / sum(d$gp),
             assists = sum(d$assists, na.rm = TRUE) / sum(d$gp),
             points = sum(d$points, na.rm = TRUE) / sum(d$gp)
           ))
    })

    output$player_history_detail <- shiny::renderUI({
      info <- career_chart_data()
      if (is.null(info)) {
        return(shiny::p(class = "text-muted",
                        "No historical scoring data available."))
      }

      shiny::tagList(
        shiny::tags$style(shiny::HTML("
          .player-career-charts { display: grid; gap: 14px; }
          .player-career-chart { border: 1px solid var(--bs-border-color);
            border-radius: 10px; padding: 12px 12px 4px;
            background: rgba(255,255,255,.02); }
          .player-career-chart h6 { margin: 0 0 2px;
            font-weight: 650; }
          .player-career-chart .small { color: #aab4c4; }
          .player-career-legend { display: flex; flex-wrap: wrap;
            gap: 7px 15px; font-size: .76rem; margin-bottom: 12px; }
          .player-career-legend span { display: inline-flex;
            align-items: center; gap: 6px; }
          .player-career-swatch { display: inline-block;
            width: 18px; height: 3px; border-radius: 2px; }
        ")),
        shiny::div(
          class = "small text-muted mb-2",
          "Colored lines show the three most recent seasons by division. ",
          "The dashed line is the player's games-weighted full-career average."
        ),
        shiny::div(
          class = "player-career-legend",
          lapply(seq_along(info$divisions), function(i) {
            colors <- grDevices::hcl.colors(length(info$divisions),
                                             palette = "Dark 3")
            shiny::tags$span(
              shiny::tags$i(class = "player-career-swatch",
                            style = paste0("background:", colors[[i]], ";")),
              info$divisions[[i]]
            )
          }),
          shiny::tags$span(
            shiny::tags$i(class = "player-career-swatch",
                          style = "background:#b6c1cf;border-top:1px dashed #222;"),
            "Career average"
          )
        ),
        shiny::div(
          class = "player-career-charts",
          lapply(list(
            c("goals", "Goals per game"),
            c("assists", "Assists per game"),
            c("points", "Points per game")
          ), function(spec) {
            shiny::div(
              class = "player-career-chart",
              shiny::tags$h6(spec[[2]]),
              shiny::plotOutput(
                ns(paste0("career_chart_", spec[[1]])),
                height = "185px", width = "100%"
              )
            )
          })
        )
      )
    })

    plot_career_metric <- function(metric) {
      info <- career_chart_data()
      shiny::req(info)
      d <- info$data
      seasons <- info$seasons
      divs <- info$divisions
      cols <- grDevices::hcl.colors(length(divs), palette = "Dark 3")
      x <- seq_along(seasons)
      vals <- d[[metric]] / d$gp
      avg <- unname(info$career[[metric]])
      upper <- max(c(vals, avg), na.rm = TRUE)
      if (!is.finite(upper) || upper <= 0) upper <- 1

      old <- graphics::par(c("mar", "bg", "fg", "col.axis", "col.lab"))
      on.exit(graphics::par(old), add = TRUE)
      graphics::par(mar = c(4.1, 3.7, 0.7, 0.8),
                    bg = "#262626", fg = "#cbd5e1",
                    col.axis = "#aab4c4", col.lab = "#aab4c4")
      graphics::plot(NA, xlim = c(0.7, length(x) + 0.3),
                     ylim = c(0, upper * 1.13), xlab = "", ylab = "",
                     axes = FALSE, type = "n")
      graphics::axis(2, las = 1, cex.axis = .8)
      # Show the original season names for the three recent seasons.
      ticks <- seq_along(seasons)
      graphics::axis(1, at = ticks, labels = as.character(info$labels[ticks]),
                     cex.axis = .60, las = 2, tick = FALSE)
      graphics::abline(h = graphics::axTicks(2), col = "#39424d", lty = 3)
      graphics::abline(h = avg, col = "#b6c1cf", lwd = 2, lty = 2)
      for (i in seq_along(divs)) {
        y <- rep(NA_real_, length(seasons))
        subset <- d[d$division_name == divs[[i]], , drop = FALSE]
        pos <- match(subset$season_id, seasons)
        y[pos] <- subset[[metric]] / subset$gp
        graphics::lines(x, y, col = cols[[i]], lwd = 2)
        graphics::points(x[!is.na(y)], y[!is.na(y)],
                         col = cols[[i]], pch = 16, cex = .7)
      }
      graphics::box(col = "#46505d")
    }

    output$career_chart_goals <- shiny::renderPlot({
      plot_career_metric("goals")
    }, res = 110)
    output$career_chart_assists <- shiny::renderPlot({
      plot_career_metric("assists")
    }, res = 110)
    output$career_chart_points <- shiny::renderPlot({
      plot_career_metric("points")
    }, res = 110)

    player_chemistry <- shiny::reactive({
      pid <- shiny::req(player_detail_id())
      tryCatch(
        DBI::dbGetQuery(
          pool,
          "SELECT * FROM public.get_player_linemate_chemistry($1, $2)",
          params = list(pid, "career")
        ),
        error = function(e) {
          warning("Linemate chemistry query failed: ", conditionMessage(e))
          NULL
        }
      )
    })

    output$player_chemistry_detail <- shiny::renderUI({
      d <- player_chemistry()
      if (is.null(d)) return(shiny::p(
        class = "text-warning", "Linemate statistics could not be loaded."
      ))
      if (!nrow(d)) return(shiny::p(
        class = "text-muted", "No recorded linemate connections yet."
      ))

      # Use the function's established directional schema. Convert bigint
      # columns before sorting and formatting to avoid integer64 surprises.
      count_fields <- c("connections", "primary_connections",
                        "secondary_connections", "opportunity_goals",
                        "chemistry_share_pct")
      for (field in intersect(count_fields, names(d))) {
        d[[field]] <- as.numeric(d[[field]])
      }

      fmt_count <- function(x) {
        if (length(x) == 0L || is.na(x)) return("0")
        formatC(x, format = "f", digits = 0, big.mark = ",")
      }
      fmt_share <- function(x) {
        if (length(x) == 0L || is.na(x)) return("N/A")
        paste0(formatC(x, format = "f", digits = 1), "%")
      }

      panel <- function(direction, title, subtitle, accent) {
        subset <- d[!is.na(d$direction) & d$direction == direction,
                    , drop = FALSE]
        if (nrow(subset)) {
          subset <- subset[order(-subset$connections,
                                 -subset$primary_connections,
                                 as.character(subset$partner_name),
                                 na.last = TRUE), , drop = FALSE]
          subset <- utils::head(subset, 3L)
        }

        items <- if (!nrow(subset)) {
          list(shiny::div(class = "linemate-empty",
                          "No recorded connections."))
        } else {
          lapply(seq_len(nrow(subset)), function(i) {
            x <- subset[i, , drop = FALSE]
            shiny::div(
              class = "linemate-item",
              shiny::div(
                class = "linemate-topline",
                shiny::span(class = "linemate-rank", paste0("#", i)),
                shiny::span(class = "linemate-name",
                            as.character(x$partner_name[[1]])),
                shiny::span(class = "linemate-total",
                            paste(fmt_count(x$connections[[1]]), "connections"))
              ),
              shiny::div(
                class = "linemate-metrics",
                shiny::span("Primary: ",
                            shiny::strong(fmt_count(x$primary_connections[[1]]))),
                shiny::span("Secondary: ",
                            shiny::strong(fmt_count(x$secondary_connections[[1]]))),
                shiny::span("Share: ",
                            shiny::strong(fmt_share(x$chemistry_share_pct[[1]])))
              )
            )
          })
        }
        shiny::div(
          class = "linemate-panel",
          style = paste0("--linemate-accent:", accent, ";"),
          shiny::div(class = "linemate-panel-title", title),
          shiny::div(class = "linemate-panel-subtitle", subtitle),
          do.call(shiny::tagList, items)
        )
      }

      shiny::tagList(
        shiny::tags$style(shiny::HTML("
          .linemate-grid { display:grid; grid-template-columns:repeat(2,minmax(0,1fr));
                           gap:14px; margin:12px 0 18px; }
          .linemate-panel { background:#24282d; border:1px solid #414850;
                            border-top:3px solid var(--linemate-accent);
                            border-radius:10px; padding:15px; min-width:0; }
          .linemate-panel-title { font-size:1rem; font-weight:700; color:#f1f3f5; }
          .linemate-panel-subtitle { font-size:.78rem; color:#aeb7c2;
                                     margin:3px 0 12px; }
          .linemate-item { border-top:1px solid #3d444c; padding:11px 0; }
          .linemate-topline { display:flex; align-items:center; gap:8px; flex-wrap:wrap; }
          .linemate-rank { color:var(--linemate-accent); font-size:.8rem;
                           font-weight:700; }
          .linemate-name { font-weight:600; color:#f1f3f5; }
          .linemate-total { margin-left:auto; font-size:.8rem; color:#cbd3db; }
          .linemate-metrics { display:flex; gap:13px; flex-wrap:wrap;
                              font-size:.77rem; color:#aeb7c2; margin-top:5px; }
          .linemate-metrics strong { color:#e8edf2; font-weight:600; }
          .linemate-empty { color:#aeb7c2; padding:12px 0; }
          @media(max-width:650px) {
            .linemate-grid { grid-template-columns:1fr; }
          }
        ")),
        shiny::div(
          class = "linemate-grid",
          panel("provides", "Assists to", "Top 3 players receiving scoring connections",
                "#4DA3FF"),
          panel("receives", "Assisted by", "Top 3 players providing scoring connections",
                "#60c9a9")
        )
      )
    })

    # --------------------------------------------------------
    # PQR explanation
    # --------------------------------------------------------
    
    shiny::observeEvent(input$explain_rankings, {
      
      shiny::showModal(
        shiny::modalDialog(
          
          title = "Understanding Player Quality Rating (PQR)",
          
          shiny::h5("What does PQR measure?"),
          
          shiny::p(
            "Player Quality Rating (PQR) estimates a skater's ",
            "historical scoring ability relative to other ",
            "players in the league. It accounts for differences ",
            "in division strength and gives greater weight ",
            "to recent performance."
          ),
          
          shiny::h5("How is PQR calculated?"),
          
          shiny::tags$ol(
            shiny::tags$li(
              shiny::strong("Adjust for division strength. "),
              "Each game's scoring production is converted ",
              "to a common Silver B-equivalent scale using ",
              "estimated differences in scoring rates ",
              "between competitive tiers."
            ),
            
            shiny::tags$li(
              shiny::strong("Weight recent games more heavily. "),
              "An exponential weighting function gives ",
              "recent games greater influence. The model ",
              "uses a 50-game half-life, meaning a game ",
              "50 appearances earlier receives half the ",
              "weight of a recent game."
            ),
            
            shiny::tags$li(
              shiny::strong("Calculate adjusted scoring rate. "),
              "Weighted, division-adjusted points are ",
              "divided by weighted games played to obtain ",
              "an adjusted points-per-game estimate."
            ),
            
            shiny::tags$li(
              shiny::strong("Convert to a percentile. "),
              "The adjusted scoring rate is compared ",
              "with current-season rostered players who ",
              "have at least 10 career games. Tied values ",
              "receive a midrank percentile."
            )
          ),
          
          shiny::h5("How should I interpret my PQR?"),
          
          shiny::p(
            "A PQR of 85 means a player's adjusted ",
            "historical scoring rate is approximately ",
            "at the 85th percentile of the established ",
            "player reference group."
          ),
          
          shiny::p(
            "PQR is measured on the same scale across ",
            "all divisions. A player retains the same ",
            "PQR whether viewed in League, Division, ",
            "or Team rankings."
          ),
          
          shiny::h5("Provisional and unrated players"),
          
          shiny::p(
            shiny::strong("Provisional: "),
            "Players with fewer than 10 career games ",
            "may receive a PQR, but their estimates ",
            "are based on limited observations."
          ),
          
          shiny::p(
            shiny::strong("Unrated: "),
            "Players without historical skater-game ",
            "data do not yet have a PQR."
          ),
          
          shiny::h5("What PQR does not measure"),
          
          shiny::p(
            "PQR measures scoring production, not overall ",
            "hockey ability. It does not directly account ",
            "for defensive play, faceoffs, penalties, ",
            "or other contributions that may affect ",
            "a player's value to a team."
          ),
          
          shiny::p(
            class = "text-muted small",
            "PQR differs from projected PPG. PQR uses ",
            "the historical adjusted scoring rate, ",
            "while projected PPG applies additional ",
            "statistical shrinkage and adjusts for ",
            "the player's current competitive tier."
          ),
          
          size = "l",
          easyClose = TRUE,
          footer = shiny::modalButton("Close")
        )
      )
    })
    
  })
}
