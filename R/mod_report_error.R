
# ============================================================
# Beer League IQ: Report an Error
# R/mod_report_error.R
# ============================================================

mod_report_error_ui <- function(id) {
  
  ns <- shiny::NS(id)
  
  time_values <- seq(0, 23 * 60 + 45, by = 15)
  
  time_labels <- vapply(time_values, function(x) {
    h <- x %/% 60
    m <- x %% 60
    sprintf(
      "%d:%02d %s",
      ifelse(h %% 12 == 0, 12, h %% 12),
      m,
      ifelse(h < 12, "AM", "PM")
    )
  }, character(1))
  
  time_choices <- c(
    "Select game time (optional)" = "",
    stats::setNames(
      sprintf(
        "%02d:%02d",
        time_values %/% 60,
        time_values %% 60
      ),
      time_labels
    )
  )
  
  #bslib::page_fillable(
  div(
    class = "report-error-page",
    padding = 20,
    
    bslib::card(
      full_screen = FALSE,
      
      bslib::card_header(
        tags$h3(
          bsicons::bs_icon("flag"),
          " Report an Error",
          style = "margin: 0;"
        )
      ),
      
      bslib::card_body(
        
        p(
          paste(
            "Found something that doesn't look right?",
            "Help us keep Beer League IQ accurate",
            "by reporting the issue below."
          )
        ),
        
        fluidRow(
          column(
            6,
            selectInput(
              ns("report_page"),
              "Where did you find the error?",
              choices = c(
                "Players",
                "Teams",
                "Upcoming Games",
                "Make Your Picks",
                "Leaderboards",
                "Penalties",
                "Home",
                "Other"
              )
            )
          ),
          
          column(
            6,
            selectInput(
              ns("error_type"),
              "Nature of the error",
              choices = c(
                "Incorrect statistics",
                "Missing statistics",
                "Incorrect game result",
                "Incorrect player or team",
                "Website or display issue",
                "Other"
              )
            )
          )
        ),
        
        tags$hr(),
        
        tags$h4("Game and Player Details"),
        
        p(
          class = "text-muted",
          paste(
            "Provide as much information as possible.",
            "Leave fields blank if they do not apply."
          )
        ),
        
        fluidRow(
          column(
            4,
            textInput(
              ns("game_id"),
              "Game ID (if known)",
              placeholder = "e.g., 35977"
            )
          ),
          
          column(
            4,
            dateInput(
              ns("game_date"),
              "Game Date",
              value = NULL,
              format = "mm/dd/yyyy"
            )
          ),
          
          column(
            4,
            selectInput(
              ns("game_time"),
              "Game Time",
              choices = time_choices
            )
          )
        ),
        
        fluidRow(
          column(
            6,
            textInput(
              ns("team_name"),
              "Team Name",
              placeholder = "e.g., Foothill Flyers"
            )
          ),
          
          column(
            6,
            textInput(
              ns("player_name"),
              "Player Name (if applicable)",
              placeholder = "e.g., Kirk Van Rooyan"
            )
          )
        ),
        
        textAreaInput(
          ns("description"),
          "Describe the error",
          placeholder = paste(
            "What does the app currently show?",
            "What should it show instead?"
          ),
          rows = 5,
          width = "100%"
        ),
        
        textInput(
          ns("contact_email"),
          "Your Email (optional)",
          placeholder = "For follow-up questions"
        ),
        
        div(
          style = "text-align: right;",
          actionButton(
            ns("submit"),
            "Submit Report",
            icon = shiny::icon("paper-plane"),
            class = "btn-primary"
          )
        )
      )
    )
  )
}


# ------------------------------------------------------------
# Server
# ------------------------------------------------------------

mod_report_error_server <- function(id) {
  
  shiny::moduleServer(id, function(input, output, session) {
    
    submitting <- shiny::reactiveVal(FALSE)
    
    observeEvent(input$submit, {
      
      if (isTRUE(submitting())) return()
      
      description <- trimws(input$description)
      
      if (nchar(description) < 10) {
        showNotification(
          "Please provide a more detailed description.",
          type = "warning"
        )
        return()
      }
      
      game_id <- trimws(input$game_id)
      
      if (nzchar(game_id) &&
          !grepl("^[0-9]+$", game_id)) {
        showNotification(
          "Game ID must contain only numbers.",
          type = "warning"
        )
        return()
      }
      
      contact_email <- trimws(input$contact_email)
      
      if (nzchar(contact_email) &&
          !grepl(
            "^[^[:space:]@]+@[^[:space:]@]+\\.[^[:space:]@]+$",
            contact_email
          )) {
        showNotification(
          "Please enter a valid contact email.",
          type = "warning"
        )
        return()
      }
      
      report <- list(
        page = input$report_page,
        error_type = input$error_type,
        game_id = game_id,
        game_date = if (
          is.null(input$game_date) ||
          is.na(input$game_date)
        ) "" else as.character(input$game_date),
        game_time = input$game_time,
        team_name = trimws(input$team_name),
        player_name = trimws(input$player_name),
        description = description,
        contact_email = contact_email,
        submitted_at = format(
          Sys.time(),
          "%Y-%m-%d %H:%M:%S %Z"
        )
      )
      
      submitting(TRUE)
      on.exit(submitting(FALSE), add = TRUE)
      
      result <- tryCatch(
        send_stat_error_report(report),
        error = function(e) {
          message(
            "Stat report delivery error: ",
            conditionMessage(e)
          )
          FALSE
        }
      )
      
      if (isTRUE(result)) {
        
        showNotification(
          "Thank you! Your report has been sent.",
          type = "message",
          duration = 8
        )
        
        # Navigate to the Home page after successful submission.
        bslib::nav_select(
          id = "page",
          selected = "home",
          session = session$rootScope()
        )
        
        # Clear the form for the next report.
        updateTextAreaInput(
          session, "description", value = ""
        )
        
        updateTextInput(
          session, "game_id", value = ""
        )
        
      } else {
        
        showNotification(
          paste(
            "Your report could not be sent.",
            "Please try again later."
          ),
          type = "error",
          duration = 10
        )
      }
    })
  })
}
