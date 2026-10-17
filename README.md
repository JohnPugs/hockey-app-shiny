# Hockey Stats & Predictions

An interactive hockey statistics and prediction dashboard built with **R Shiny**, using historical and current game data from a recreational hockey league.

The application brings together team performance, upcoming matchups, and eventually statistical game predictions in a single, accessible interface.

## Features

### Currently Available

- **Upcoming Games:** View the current week's schedule, organized by division.
- **Team Records:** Compare wins, losses, and ties for upcoming matchups.
- **Team Performance:** View goals for and goals against.
- **Head-to-Head Records:** Compare previous meetings between teams during the current season.
- **Division Navigation:** Browse matchups by division, including games scheduled outside a division's usual playing day.
- **Methodology Notes:** Learn how game outcomes and statistics are calculated.

### Planned Features

- Individual player statistics and team scoring leaders
- Statistical game predictions and win probabilities
- User prediction contests and leaderboards
- Historical team and player performance
- Referee statistics and analysis

## Technology

- **R Shiny:** Interactive web application
- **bslib:** Responsive interface and styling
- **Supabase / PostgreSQL:** Hockey statistics database
- **Posit Connect Cloud:** Application hosting and deployment
- **GitHub:** Version control and application publishing

## Data and Methodology

Game and player information originates from publicly available recreational hockey league statistics.

Team records include official forfeit decisions. Goals for and goals against exclude artificial goals awarded for forfeits, so these measures reflect on-ice performance.

Head-to-head records reflect previous meetings within the current season, including single-team forfeit decisions.

Statistical predictions are under development and are not yet available in the public application.

## Repository Structure

```text
hockey-app-shiny/
├── app.R              # Application entry point
├── R/
│   ├── db.R           # Database queries and connections
│   ├── mod_home.R     # Home dashboard module
│   └── mod_games.R    # Upcoming games module
├── www/
│   └── styles.css     # Custom styling
├── renv.lock          # R package dependencies
└── README.md
```

## Running Locally

The application requires R and access to the configured PostgreSQL database.

1. Clone this repository.
2. Restore R package dependencies using `renv`.
3. Configure the following environment variables in a local `.Renviron` file:

   - `SUPABASE_DB_HOST`
   - `SUPABASE_DB_PORT`
   - `SUPABASE_DB_NAME`
   - `SUPABASE_DB_USER`
   - `SUPABASE_DB_PASSWORD`

4. Launch the application:

   ```r
   shiny::runApp(".")
   ```

**Security:** Never commit database credentials, `.Renviron`, or other secrets to this repository.

## Deployment

The application is hosted on Posit Connect Cloud and deployed from the `main` branch of this repository.

Database credentials are configured separately as deployment environment variables.

## Project Status

Active development. Additional statistics, prediction models, and interactive features are planned.

## Data Attribution

Hockey statistics are sourced from the publicly accessible [Time to Score league statistics website](https://stats.rv.timetoscore.com/).

This project is independently developed and is not an official league application.