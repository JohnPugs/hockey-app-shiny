

# ============================================================
# Beer League IQ: Brevo error report email
# R/report_email.R
# ============================================================

send_stat_error_report <- function(report) {
  
  api_key <- Sys.getenv("BREVO_API_KEY")
  sender <- Sys.getenv("BREVO_SENDER_EMAIL")
  recipient <- Sys.getenv("ERROR_REPORT_EMAIL")
  
  if (!nzchar(api_key) ||
      !nzchar(sender) ||
      !nzchar(recipient)) {
    warning("Brevo email configuration is incomplete.")
    return(FALSE)
  }
  
  field <- function(name) {
    value <- report[[name]]
    if (is.null(value) || length(value) == 0 ||
        is.na(value[1]) || !nzchar(trimws(value[1]))) {
      return("Not provided")
    }
    as.character(value[1])
  }
  
  body <- paste(
    "BEER LEAGUE IQ - STAT ERROR REPORT",
    "",
    paste("Page:", field("page")),
    paste("Error type:", field("error_type")),
    "",
    "GAME DETAILS",
    paste("Game ID:", field("game_id")),
    paste("Game date:", field("game_date")),
    paste("Game time:", field("game_time")),
    paste("Team:", field("team_name")),
    paste("Player:", field("player_name")),
    "",
    "DESCRIPTION",
    field("description"),
    "",
    paste("Contact email:", field("contact_email")),
    paste("Submitted:", field("submitted_at")),
    sep = "\n"
  )
  
  subject <- paste0(
    "BeerLeagueIQ Error Report: ",
    field("error_type"),
    " | Game ",
    field("game_id")
  )
  
  payload <- list(
    sender = list(
      name = "BeerLeagueIQ",
      email = sender
    ),
    to = list(
      list(email = recipient)
    ),
    subject = subject,
    textContent = body,
    tags = list("stat-error-report")
  )
  
  contact <- trimws(field("contact_email"))
  
  if (contact != "Not provided") {
    payload$replyTo <- list(email = contact)
  }
  
  tryCatch({
    
    response <- httr2::request(
      "https://api.brevo.com/v3/smtp/email"
    ) |>
      httr2::req_headers(
        "api-key" = api_key,
        "accept" = "application/json"
      ) |>
      httr2::req_body_json(payload) |>
      httr2::req_timeout(20) |>
      httr2::req_perform()
    
    httr2::resp_status(response) == 201L
    
  }, error = function(e) {
    warning(
      "Brevo error report request failed: ",
      conditionMessage(e)
    )
    FALSE
  })
}
