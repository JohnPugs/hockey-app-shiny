library(shiny)
library(bslib)
library(bsicons)

source("R/db.R")
source("R/mod_home.R")
source("R/mod_games.R")
source("R/mod_teams.R")
source("R/mod_players.R")
source("R/mod_penalties.R")
source("R/mod_leaderboard.R")

ui <- page_navbar(
  title = "Beer League IQ",
  id = "page",
  selected = "home",
  
  theme = bs_theme(
    version = 5,
    bootswatch = "darkly",
    primary = "#4DA3FF"
  ),
  
  # header = tags$head(
  #   tags$link(
  #     rel = "stylesheet",
  #     type = "text/css",
  #     href = "styles.css"
  #   )
  # ),
  
  header = tags$head(
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "styles.css"
    ),
    tags$script(HTML("
    document.addEventListener('DOMContentLoaded', function() {
  
      const navbar = document.querySelector(
        'nav.navbar.navbar-static-top'
      );
  
      if (!navbar) return;
      
      const brand = navbar.querySelector('.navbar-brand');

      if (brand) {
        brand.classList.add('brand-home-link');
        brand.style.cursor = 'pointer';
        brand.setAttribute('role', 'button');
        brand.setAttribute('tabindex', '0');
        brand.setAttribute('aria-label', 'Return to Beer League IQ home');
      
        function goHome() {
          Shiny.setInputValue('brand_home', Date.now(), {
            priority: 'event'
          });
        }
      
        brand.addEventListener('click', goHome);
      
        brand.addEventListener('keydown', function(event) {
          if (event.key === 'Enter' || event.key === ' ') {
            event.preventDefault();
            goHome();
          }
        });
      }
  
      navbar.classList.add('fixed-top');
  
      // Create a spacer immediately after the navbar.
      let spacer = document.getElementById('navbar-spacer');
  
      if (!spacer) {
        spacer = document.createElement('div');
        spacer.id = 'navbar-spacer';
        navbar.insertAdjacentElement('afterend', spacer);
      }
  
      function updateNavbarSpacing() {
        spacer.style.height =
          Math.ceil(navbar.getBoundingClientRect().height) + 'px';
      }
  
      updateNavbarSpacing();
  
      const observer = new ResizeObserver(updateNavbarSpacing);
      observer.observe(navbar);
  
      window.addEventListener('resize', updateNavbarSpacing);
    });
  "))
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
    value = "leaderboard",
    mod_leaderboard_ui("leaderboard")
  ),
  
  nav_panel(
    "Players",
    value = "players",
    mod_players_ui("players")
  ),
  
  nav_panel(
    "Teams",
    value = "teams",
    mod_teams_ui("teams")
  ),
  
  nav_panel(
    "Penalties",
    value = "penalties",
    mod_penalties_ui("penalties")
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
  
  observeEvent(input$brand_home, {
    bslib::nav_select(
      id = "page",
      selected = "home",
      session = session
    )
  })
  
  mod_home_server(
    "home",
    signed_in = signed_in,
    navigate = navigate
  )
  
  mod_games_server(
    "games",
    pool = db_pool
  )
  
  mod_teams_server(
    "teams",
    pool = db_pool
  )
  
  # mod_players_server(
  #   "players",
  #   pool = db_pool
  # )
  
  mod_players_server(
    "players",
    pool = db_pool,
    active = shiny::reactive(input$page == "players")
  )
  
  mod_penalties_server(
    "penalties",
    pool = db_pool,
    active = shiny::reactive(input$page == "penalties")
  )
  
  mod_leaderboard_server(
    "leaderboard",
    active = shiny::reactive(input$page == "leaderboard")
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