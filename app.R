library(shiny)
library(bslib)
library(bsicons)

source("R/db.R")
source("R/auth.R")
source("R/mod_home.R")
source("R/mod_games.R")
source("R/mod_picks.R")
source("R/mod_teams.R")
source("R/mod_players.R")
source("R/mod_penalties.R")
source("R/mod_leaderboard.R")
source("R/mod_report_error.R")
source("R/report_email.R")

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
  ")),
    
    tags$script(HTML("
    (function () {
    
      const hash = window.location.hash;
      const params = new URLSearchParams(
        hash.startsWith('#') ? hash.substring(1) : hash
      );
    
      if (params.get('type') !== 'recovery') {
        return;
      }
    
      const token = params.get('access_token');
    
      if (!token) {
        console.warn(
          'Password recovery: access token missing.'
        );
        return;
      }
    
      console.info(
        'Password recovery: callback detected.'
      );
    
      let delivered = false;
    
      function sendRecovery() {
    
        if (delivered) return;
    
        const connected = Boolean(
          window.Shiny &&
          Shiny.shinyapp &&
          Shiny.shinyapp.isConnected()
        );
    
        if (!connected) return;
    
        delivered = true;
    
        Shiny.setInputValue(
          'password_recovery_callback',
          { access_token: token },
          { priority: 'event' }
        );
    
        console.info(
          'Password recovery: callback sent to Shiny.'
        );
    
        // Remove credentials from the address bar.
        window.history.replaceState(
          null,
          '',
          window.location.pathname + window.location.search
        );
      }
    
      // Shiny may already be connected.
      sendRecovery();
    
      // Otherwise wait for its connection event.
      document.addEventListener(
        'shiny:connected',
        sendRecovery
      );
    
      // Handle initialization timing differences.
      let attempts = 0;
      const check = window.setInterval(function () {
        attempts++;
        sendRecovery();
    
        if (delivered || attempts >= 50) {
          window.clearInterval(check);
        }
      }, 100);
    
    })();
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
    "Make Your Picks",
    value = "picks",
    mod_picks_ui("picks")
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
  
  nav_panel(
    title = "Report Error",
    value = "report_error",
    icon = bsicons::bs_icon("flag"),
    mod_report_error_ui("report_error")
  ),
  
  nav_item(
    actionButton(
      "auth_toggle",
      "Sign In",
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
  
  
  # ============================================================
  # Beer League IQ: Authentication interface
  # ============================================================
  
  # Authentication state is private to each Shiny session.
  auth_session <- reactiveVal(NULL)
  
  # ============================================================
  # Automatic Supabase session refresh
  # Retry transient failures until token expiration
  # ============================================================
  
  AUTH_REFRESH_TEST <- FALSE # Should be FALSE for deployment.
  
  refresh_lead <- if (AUTH_REFRESH_TEST) 3500 else 120
  
  refresh_failures <- reactiveVal(0L)
  next_refresh_attempt <- reactiveVal(NULL)
  
  # Backoff intervals in seconds:
  # 10, 20, 40, 60, 60, ...
  refresh_delay <- function(attempt) {
    min(60, 10 * 2^min(attempt - 1L, 3L))
  }
  
  end_auth_session <- function(message_text) {
    
    auth_session(NULL)
    signed_in(FALSE)
    refresh_failures(0L)
    next_refresh_attempt(NULL)
    
    updateActionButton(
      session,
      "auth_toggle",
      label = "Sign In"
    )
    
    showNotification(
      message_text,
      type = "warning",
      duration = 10
    )
  }
  
  # # TEMPORARY TEST: Four failures, then success.
  # real_auth_refresh <- auth_refresh
  # simulated_failures <- 0L
  # 
  # auth_refresh <- function(refresh_token) {
  #   
  #   if (simulated_failures < 4L) {
  #     
  #     simulated_failures <<- simulated_failures + 1L
  #     
  #     return(list(
  #       success = FALSE,
  #       status = 503,
  #       error = "Simulated temporary outage"
  #     ))
  #   }
  #   
  #   real_auth_refresh(refresh_token)
  # }
  
  observe({
    
    shiny::invalidateLater(5000, session)
    
    current <- isolate(auth_session())
    
    if (is.null(current)) return()
    
    now <- Sys.time()
    
    seconds_left <- as.numeric(difftime(
      current$expires_at,
      now,
      units = "secs"
    ))
    
    if (!is.finite(seconds_left)) {
      end_auth_session(
        "Invalid session expiration. Please sign in again."
      )
      return()
    }
    
    # Never retain an expired access token.
    if (seconds_left <= 0) {
      end_auth_session(
        "Your session has expired. Please sign in again."
      )
      return()
    }
    
    # Wait until the refresh window.
    if (seconds_left > refresh_lead) return()
    
    # Respect the retry schedule.
    retry_at <- isolate(next_refresh_attempt())
    
    if (!is.null(retry_at) && now < retry_at) return()
    
    attempt <- isolate(refresh_failures()) + 1L
    
    message("AUTH: Refresh attempt ", attempt)
    
    result <- auth_refresh(current$refresh_token)
    
    valid_response <- isTRUE(result$success) &&
      is.list(result$data) &&
      is.character(result$data$access_token) &&
      length(result$data$access_token) == 1L &&
      nzchar(result$data$access_token) &&
      is.character(result$data$refresh_token) &&
      length(result$data$refresh_token) == 1L &&
      nzchar(result$data$refresh_token) &&
      is.numeric(result$data$expires_in) &&
      length(result$data$expires_in) == 1L &&
      is.finite(result$data$expires_in) &&
      result$data$expires_in > 0
    
    if (valid_response) {
      
      current$access_token <- result$data$access_token
      current$refresh_token <- result$data$refresh_token
      current$expires_at <- Sys.time() +
        result$data$expires_in
      
      auth_session(current)
      
      refresh_failures(0L)
      next_refresh_attempt(NULL)
      
      message(
        "Session refreshed successfully. ",
        "Next expiration: ",
        format(current$expires_at)
      )
      
      return()
    }
    
    # HTTP 429, HTTP 5xx, or connection failure.
    status <- result$status
    
    transient_failure <- !isTRUE(result$success) &&
      (
        is.null(status) ||
          (
            is.numeric(status) &&
              length(status) == 1L &&
              (status == 429 || status >= 500)
          )
      )
    
    if (!transient_failure) {
      
      message("Authentication failure")
      
      end_auth_session(
        "Your session expired. Please sign in again."
      )
      
      return()
    }
    
    refresh_failures(attempt)
    
    delay <- refresh_delay(attempt)
    
    # Do not schedule a retry after the token expires.
    delay <- min(delay, max(1, seconds_left - 5))
    
    next_refresh_attempt(Sys.time() + delay)
    
    message(
      "AUTH: Temporary failure. ",
      "Next attempt in approximately ",
      round(delay),
      " seconds."
    )
    
  })
  
  # Display sign-in, registration, or recovery form.
  show_auth_modal <- function(mode = "login") {
    
    if (mode == "register") {
      
      title <- "Create Your Account"
      
      fields <- tagList(
        textInput(
          "auth_display_name",
          "Display Name",
          placeholder = "e.g., I_am_a_Cone"
        ),
        helpText(
          "3 to 24 characters. Letters, numbers,",
          "and underscores only.",
          "This name will appear on public leaderboards."
        ),
        textInput("auth_email", "Email address"),
        passwordInput("auth_password", "Password"),
        passwordInput(
          "auth_password_confirm",
          "Confirm password"
        )
      )
      
      submit_id <- "auth_register_submit"
      submit_label <- "Create Account"
      
      links <- tagList(
        actionLink("auth_show_login", "Already have an account?")
      )
      
    } else if (mode == "forgot") {
      
      title <- "Reset Your Password"
      
      fields <- tagList(
        p("Enter your email address to receive a password-reset link."),
        textInput("auth_email", "Email address")
      )
      
      submit_id <- "auth_forgot_submit"
      submit_label <- "Send Reset Email"
      
      links <- tagList(
        actionLink("auth_show_login", "Back to Sign In")
      )
      
    } else {
      
      title <- "Sign In to Beer League IQ"
      
      fields <- tagList(
        textInput("auth_email", "Email address"),
        passwordInput("auth_password", "Password")
      )
      
      submit_id <- "auth_login_submit"
      submit_label <- "Sign In"
      
      links <- tagList(
        actionLink("auth_show_register", "Create an account"),
        tags$span(" | "),
        actionLink("auth_show_forgot", "Forgot password?")
      )
    }
    
    showModal(
      modalDialog(
        title = title,
        fields,
        tags$div(
          style = "margin-top:16px;",
          links
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            submit_id,
            submit_label,
            class = "btn-primary"
          )
        ),
        easyClose = TRUE
      )
    )
  }
  
  show_password_reset_modal <- function() {
    showModal(modalDialog(
      title = "Set a New Password",
      
      passwordInput(
        "reset_new_password",
        "New Password"
      ),
      
      helpText(
        "Choose a new password that differs from your current password."
      ),
      
      passwordInput(
        "reset_confirm_password",
        "Confirm New Password"
      ),
      
      footer = tagList(
        actionButton(
          "reset_password_submit",
          "Update Password",
          class = "btn-primary"
        )
      ),
      
      easyClose = FALSE
    ))
  }
  
  # ------------------------------------------------------------
  # Switch between authentication forms
  # ------------------------------------------------------------
  
  observeEvent(input$auth_show_register, {
    show_auth_modal("register")
  })
  
  observeEvent(input$auth_show_login, {
    show_auth_modal("login")
  })
  
  observeEvent(input$auth_show_forgot, {
    show_auth_modal("forgot")
  })
  
  # ------------------------------------------------------------
  # Register a new player
  # ------------------------------------------------------------
  
  observeEvent(input$auth_register_submit, {
    
    email <- trimws(input$auth_email)
    password <- input$auth_password
    confirmation <- input$auth_password_confirm
    
    if (!nzchar(email) || !nzchar(password)) {
      showNotification(
        "Email and password are required.",
        type = "error"
      )
      return()
    }
    
    if (nchar(password) < 8) {
      showNotification(
        "Use a password of at least 8 characters.",
        type = "error"
      )
      return()
    }
    
    if (!identical(password, confirmation)) {
      showNotification(
        "Passwords do not match.",
        type = "error"
      )
      return()
    }
    
    # Check display name before creating the account.
    name_check <- auth_display_name_available(
      input$auth_display_name
    )
    
    if (!name_check$success) {
      showNotification(
        name_check$error,
        type = "error",
        duration = 8
      )
      return()
    }
    
    if (!name_check$available) {
      showNotification(
        "That display name is already taken. Choose another.",
        type = "error",
        duration = 8
      )
      return()
    }
    
    result <- auth_register(
      email = email,
      password = password,
      display_name = input$auth_display_name
    )
    
    if (!result$success) {
      showNotification(
        result$error,
        type = "error",
        duration = 8
      )
      return()
    }
    
    removeModal()
    
    showNotification(
      paste(
        "Registration submitted.",
        "Check your email to confirm your account."
      ),
      type = "message",
      duration = 10
    )
  })
  
  # ------------------------------------------------------------
  # Sign in
  # ------------------------------------------------------------
  
  observeEvent(input$auth_login_submit, {
    
    email <- trimws(input$auth_email)
    password <- input$auth_password
    
    if (!nzchar(email) || !nzchar(password)) {
      showNotification(
        "Enter your email and password.",
        type = "error"
      )
      return()
    }
    
    result <- auth_login(email, password)
    
    if (!result$success) {
      showNotification(
        "Sign in failed. Check your credentials and email confirmation.",
        type = "error",
        duration = 8
      )
      return()
    }
    
    token <- result$data$access_token
    refresh <- result$data$refresh_token
    
    if (is.null(token) || is.null(refresh)) {
      showNotification(
        "No authenticated session was returned.",
        type = "error"
      )
      return()
    }
    
    # Verify the user with Supabase before enabling picks.
    verified <- auth_get_user(token)
    
    if (!verified$success ||
        is.null(verified$data$id)) {
      showNotification(
        "Unable to verify your account.",
        type = "error"
      )
      return()
    }
    
    auth_session(list(
      access_token = token,
      refresh_token = refresh,
      user_id = verified$data$id,
      email = verified$data$email,
      expires_at = Sys.time() +
        as.numeric(result$data$expires_in)
      
      # Temoporary, remove before deployment #
      # expires_at = Sys.time() +
      #   if (AUTH_REFRESH_TEST) 45 else as.numeric(result$data$expires_in)
    ))
    
    
    signed_in(TRUE)
    
    updateActionButton(
      session,
      "auth_toggle",
      label = "Sign Out"
    )
    
    removeModal()
    
    showNotification(
      "Welcome to Beer League IQ!",
      type = "message"
    )
  })
  
  # ------------------------------------------------------------
  # Request password reset
  # ------------------------------------------------------------
  
  observeEvent(input$auth_forgot_submit, {
    
    email <- trimws(input$auth_email)
    
    if (!nzchar(email)) {
      showNotification(
        "Enter your email address.",
        type = "error"
      )
      return()
    }
    
    result <- auth_forgot_password(email)
    
    removeModal()
    
    # Do not reveal whether an email is registered.
    showNotification(
      paste(
        "If an account exists for that address,",
        "a password-reset email will be sent."
      ),
      type = "message",
      duration = 10
    )
  })
  
  # Recovery credentials belong to this Shiny session only.
  recovery_token <- reactiveVal(NULL)
  
  observeEvent(input$password_recovery_callback, {
    #message("PASSWORD RECOVERY CALLBACK RECEIVED")
    token <- input$password_recovery_callback$access_token
    
    if (is.null(token) ||
        !is.character(token) ||
        length(token) != 1 ||
        !nzchar(token)) {
      showNotification(
        "Invalid password-reset link.",
        type = "error"
      )
      return()
    }
    
    # Verify the recovery session with Supabase.
    verified <- auth_get_user(token)
    
    if (!isTRUE(verified$success) ||
        is.null(verified$data$id)) {
      showNotification(
        "Password-reset link is invalid or expired.",
        type = "error",
        duration = 10
      )
      return()
    }
    
    recovery_token(token)
    show_password_reset_modal()
  })
  
  observeEvent(input$reset_password_submit, {
    
    token <- recovery_token()
    req(token)
    
    password <- input$reset_new_password
    confirm <- input$reset_confirm_password
    
    if (is.null(password) ||
        nchar(password) < 8) {
      showNotification(
        "Password must be at least 8 characters.",
        type = "error"
      )
      return()
    }
    
    if (!identical(password, confirm)) {
      showNotification(
        "Passwords do not match.",
        type = "error"
      )
      return()
    }
    
    result <- auth_update_password(
      access_token = token,
      new_password = password
    )
    
    
    if (!isTRUE(result$success)) {
      
      error_code <- tolower(
        as.character(result$data$code %||auth% "")
      )
      
      error_message <- tolower(
        as.character(result$error %||auth% "")
      )
      
      message_to_user <- if (
        error_code == "same_password" ||
        grepl(
          "same password|different from.*old|different from.*current",
          error_message
        )
      ) {
        "Your new password must be different from your current password."
      } else if (
        grepl("weak|short|length|password", error_code) ||
        grepl("too short|weak password", error_message)
      ) {
        "Please choose a stronger password."
      } else if (
        grepl("expired|invalid.*token|jwt", error_message)
      ) {
        "Your reset link has expired. Please request a new one."
      } else {
        "Unable to update your password. Please try again."
      }
      
      showNotification(
        message_to_user,
        type = "error",
        duration = 10
      )
      
      return()
    }
    
    
    # Clear recovery state after success.
    recovery_token(NULL)
    
    removeModal()
    
    showNotification(
      "Password updated. Please sign in.",
      type = "message",
      duration = 8
    )
  })
  
  
  
  navigate <- function(page) {
    
    if (page == "login") {
      show_auth_modal("login")
      
    } else if (page == "picks") {
      if (signed_in()) {
        bslib::nav_select(id = "page", selected = "picks", session = session)
      } else {
        show_auth_modal("login")
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
  
  mod_picks_server(
    "picks",
    pool = db_pool,
    auth_session = auth_session,
    active = shiny::reactive(input$page == "picks")
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
  
  mod_report_error_server("report_error")
  
  observeEvent(input$auth_toggle, {
    
    if (!signed_in()) {
      show_auth_modal("login")
      return()
    }
    
    current <- auth_session()
    
    if (!is.null(current)) {
      auth_logout(current$access_token)
    }
    
    auth_session(NULL)
    signed_in(FALSE)
    
    updateActionButton(
      session,
      "auth_toggle",
      label = "Sign In"
    )
    
    showNotification(
      "You have signed out.",
      type = "message"
    )
  })
  
}

shinyApp(ui, server)