library(shiny)
library(bslib)
library(bsicons)

source("R/db.R")
source("R/mod_home.R")
source("R/mod_games.R")

ui <- page_navbar(
  title = "Beer League IQ",
  id = "page",
  selected = "home",
  
  theme = bs_theme(
    version = 5,
    bootswatch = "darkly",
    primary = "#4DA3FF"
  ),
  
  header = tags$head(
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "styles.css"
    )
  ),
  
  nav_panel(
    "Home",
    value = "home",
    mod_home_ui("home")
  ),
  
  nav_panel(
    "Upcoming Games",
    value = "games",
    mod_games_ui("games")
  ),
  
  nav_panel(
    "Leaderboards",
    value = "leaderboards",
    h2("Leaderboards")
  ),
  
  nav_panel(
    "Player Statistics",
    value = "players",
    h2("Player Statistics")
  ),
  
  nav_panel(
    "Team Statistics",
    value = "teams",
    h2("Team Statistics")
  ),
  
  nav_panel(
    "Referee Statistics",
    value = "referees",
    h2("Referee Statistics")
  ),
  
  nav_spacer(),
  
  nav_item(
    actionButton(
      "auth_toggle",
      "Sign In (Demo)",
      class = "btn-outline-light btn-sm"
    )
  )
)

db_pool <- get_db_pool()

onStop(function() {
  pool::poolClose(db_pool)
})

server <- function(input, output, session) {
  
  signed_in <- reactiveVal(FALSE)
  
  navigate <- function(page) {
    
    if (page == "login") {
      
      showModal(modalDialog(
        title = "Sign in required",
        "Sign in to participate in weekly predictions.",
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
      
    } else if (page == "picks") {
      
      if (signed_in()) {
        showModal(modalDialog(
          title = "Make Your Picks",
          "Weekly game picks will appear here.",
          easyClose = TRUE
        ))
      }
      
    } else {
      
      bslib::nav_select(
        id = "page",
        selected = page,
        session = session
      )
      
    }
  }
  
  mod_home_server(
    "home",
    signed_in = signed_in,
    navigate = navigate
  )
  
  mod_games_server(
    "games",
    pool = db_pool
  )
  
  observeEvent(input$auth_toggle, {
    signed_in(!signed_in())
    
    updateActionButton(
      session,
      "auth_toggle",
      label = if (signed_in()) "Sign Out (Demo)"
      else "Sign In (Demo)"
    )
  })
  
 
}

shinyApp(ui, server)