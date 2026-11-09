
# ============================================================
# Beer League IQ: Supabase Authentication
# R/auth.R
# ============================================================

library(httr2)
library(jsonlite)

AUTH_URL <- "https://pmpauqoybrdujkmvcswm.supabase.co"

AUTH_KEY <- Sys.getenv("SUPABASE_PUBLISHABLE_KEY")

APP_URL <- paste0(
  "https://jpugs-bearleagueiq.share.connect.posit.cloud/"
)

if (!nzchar(AUTH_KEY)) {
  stop("SUPABASE_PUBLISHABLE_KEY is missing.")
}

# ------------------------------------------------------------
# Shared API request helper
# ------------------------------------------------------------

auth_request <- function(
    endpoint,
    method = "POST",
    body = NULL,
    access_token = NULL
) {
  
  req <- request(
    paste0(AUTH_URL, "/auth/v1/", endpoint)
  ) |>
    req_headers(apikey = AUTH_KEY) |>
    req_timeout(20) |>
    req_error(is_error = function(resp) FALSE)
  
  if (!is.null(access_token)) {
    req <- req |>
      req_auth_bearer_token(access_token)
  }
  
  if (method == "POST") {
    req <- req |> req_method("POST")
  } else if (method == "PUT") {
    req <- req |> req_method("PUT")
  } else if (method == "GET") {
    req <- req |> req_method("GET")
  } else {
    stop("Unsupported HTTP method.")
  }
  
  if (!is.null(body)) {
    req <- req |> req_body_json(body)
  }
  
  tryCatch({
    
    resp <- req_perform(req)
    status <- resp_status(resp)
    
    result <- tryCatch(
      resp_body_json(resp, simplifyVector = FALSE),
      error = function(e) list()
    )
    
    if (status >= 400) {
      return(list(
        success = FALSE,
        status = status,
        error = result$msg %||auth%
          result$message %||auth%
          result$error_description %||auth%
          result$error %||auth%
          "Authentication request failed."
      ))
    }
    
    list(
      success = TRUE,
      status = status,
      data = result
    )
    
  }, error = function(e) {
    list(
      success = FALSE,
      error = "Unable to connect to authentication service."
    )
  })
}

# Base R helper: first non-NULL value
`%||auth%` <- function(x, y) {
  if (is.null(x)) y else x
}

# ------------------------------------------------------------
# Register player
# ------------------------------------------------------------

    auth_register <- function(email, password, display_name) {
      
      display_name <- trimws(display_name)
      
      if (!grepl(
        "^[A-Za-z0-9_]{3,24}$",
        display_name
      )) {
        return(list(
          success = FALSE,
          error = paste(
            "Display name must contain 3 to 24",
            "letters, numbers, or underscores."
          )
        ))
      }
      
      redirect_url <- Sys.getenv(
        "BEERLEAGUEIQ_AUTH_REDIRECT",
        unset = APP_URL
      )
      
      endpoint <- paste0(
        "signup?redirect_to=",
        utils::URLencode(redirect_url, reserved = TRUE)
      )
      
      auth_request(
        endpoint,
        body = list(
          email = trimws(email),
          password = password,
          data = list(display_name = display_name)
        )
      )
    }


# ------------------------------------------------------------
# Sign in
# ------------------------------------------------------------

auth_login <- function(email, password) {
  
  auth_request(
    "token?grant_type=password",
    body = list(
      email = trimws(email),
      password = password
    )
  )
}

# ------------------------------------------------------------
# Request password recovery
# ------------------------------------------------------------

# auth_forgot_password <- function(email) {
#   
#   auth_request(
#     paste0(
#       "recover?redirect_to=",
#       utils::URLencode(APP_URL, reserved = TRUE)
#     ),
#     body = list(
#       email = trimws(email)
#     )
#   )
# }


auth_forgot_password <- function(email) {
  
  redirect_url <- Sys.getenv(
    "BEERLEAGUEIQ_AUTH_REDIRECT",
    unset = APP_URL
  )
  
  endpoint <- paste0(
    "recover?redirect_to=",
    utils::URLencode(redirect_url, reserved = TRUE)
  )
  
  auth_request(
    endpoint,
    body = list(
      email = trimws(email)
    )
  )
}


# ------------------------------------------------------------
# Set or change password
# Requires a valid authenticated access token.
# ------------------------------------------------------------

auth_update_password <- function(
    access_token,
    new_password
) {
  
  auth_request(
    "user",
    method = "PUT",
    body = list(password = new_password),
    access_token = access_token
  )
}

# ------------------------------------------------------------
# Retrieve authenticated user
# ------------------------------------------------------------

auth_get_user <- function(access_token) {
  
  auth_request(
    "user",
    method = "GET",
    access_token = access_token
  )
}

# ------------------------------------------------------------
# Refresh an authenticated session
# ------------------------------------------------------------

auth_refresh <- function(refresh_token) {
  
  auth_request(
    "token?grant_type=refresh_token",
    body = list(refresh_token = refresh_token)
  )
}

# ------------------------------------------------------------
# Sign out
# ------------------------------------------------------------

auth_logout <- function(access_token) {
  
  auth_request(
    "logout",
    access_token = access_token
  )
}


auth_display_name_available <- function(display_name) {
  
  display_name <- trimws(display_name)
  
  if (!grepl(
    "^[A-Za-z0-9_]{3,24}$",
    display_name
  )) {
    return(list(
      success = FALSE,
      error = paste(
        "Display name must be 3 to 24 characters:",
        "letters, numbers, or underscores."
      )
    ))
  }
  
  response <- tryCatch({
    
    httr2::request(paste0(
      Sys.getenv("SUPABASE_URL"),
      "/rest/v1/profiles"
    )) |>
      httr2::req_url_query(
        select = "user_id",
        display_name = paste0(
          "ilike.", display_name
        ),
        limit = 1
      ) |>
      httr2::req_headers(
        apikey = Sys.getenv(
          "SUPABASE_PUBLISHABLE_KEY"
        )
      ) |>
      httr2::req_perform() |>
      httr2::resp_body_json()
    
  }, error = function(e) NULL)
  
  if (is.null(response)) {
    return(list(
      success = FALSE,
      error = "Could not check display name availability."
    ))
  }
  
  list(
    success = TRUE,
    available = length(response) == 0
  )
}


