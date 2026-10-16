mod_home_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::div(
      class = "home-hero",
      shiny::h1("Think you know hockey?"),
      shiny::p(
        "Beating the model is one thing.",
        shiny::tags$br(),
        "But can you beat other players?"
      )
    ),
    
    shiny::uiOutput(ns("navigation_cards"))
  )
}

mod_home_server <- function(id, signed_in, navigate) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    cards <- data.frame(
      page = c(
        "games", "picks", "leaderboards",
        "players", "teams", "referees"
      ),
      title = c(
        "Upcoming Games", "Make Your Picks", "Leaderboards",
        "Player Statistics", "Team Statistics",
        "Referee Statistics"
      ),
      icon = c(
        "calendar-event", "bullseye", "trophy",
        "people", "shield", "clipboard-data"
      ),
      description = c(
        "Schedules & model predictions",
        "Challenge the model",
        "Compete with the league",
        "Ratings & performance",
        "Standings & history",
        "Penalties & trends"
      )
    )
    
    output$navigation_cards <- shiny::renderUI({
      shiny::div(
        class = "home-grid",
        lapply(seq_len(nrow(cards)), function(i) {
          card <- cards[i, ]
          locked <- card$page == "picks" && !signed_in()
          
          shiny::actionButton(
            inputId = ns(paste0("card_", card$page)),
            label = shiny::tagList(
              bsicons::bs_icon(card$icon),
              shiny::tags$strong(card$title),
              shiny::tags$span(
                if (locked) "Sign in to play"
                else card$description
              )
            ),
            class = paste(
              "home-card",
              if (locked) "home-card-locked"
            )
          )
        })
      )
    })
    
    for (page in cards$page) {
      local({
        target <- page
        
        shiny::observeEvent(
          input[[paste0("card_", target)]],
          {
            if (target == "picks" && !signed_in()) {
              navigate("login")
            } else {
              navigate(target)
            }
          }
        )
      })
    }
  })
}