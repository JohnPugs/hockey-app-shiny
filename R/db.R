library(DBI)
library(RPostgres)
library(pool)

get_db_pool <- function() {
  
  pool::dbPool(
    drv = RPostgres::Postgres(),
    host = Sys.getenv("SUPABASE_DB_HOST"),
    port = as.integer(Sys.getenv("SUPABASE_DB_PORT", "5432")),
    dbname = Sys.getenv("SUPABASE_DB_NAME", "postgres"),
    user = Sys.getenv("SUPABASE_DB_USER"),
    password = Sys.getenv("SUPABASE_DB_PASSWORD"),
    sslmode = "require"
  )
}

get_weekly_games <- function(pool) {
  
  DBI::dbGetQuery(
    pool,
    "
    WITH current_season AS (
      SELECT id
      FROM public.seasons
      WHERE season_name = 'Current'
    ),

    regular_days AS (
      SELECT DISTINCT ON (division_id)
        division_id,
        EXTRACT(ISODOW FROM game_date)::integer
          AS regular_weekday
      FROM public.games
      WHERE season_id IN (
        SELECT id FROM current_season
      )
        AND game_date IS NOT NULL
      GROUP BY
        division_id,
        EXTRACT(ISODOW FROM game_date)
      ORDER BY
        division_id,
        COUNT(*) DESC,
        EXTRACT(ISODOW FROM game_date)
    ),

    week_bounds AS (
      SELECT
        date_trunc(
          'week',
          now() AT TIME ZONE 'America/Los_Angeles'
        )::date AS week_start
    )

    SELECT
      g.id AS game_id,
      g.season_id,
      g.division_id,
      g.home_team_id,
      g.away_team_id,
      g.game_date,
      g.game_time::text AS game_time,
      g.rink,
      d.division_name,
      rd.regular_weekday,

      EXTRACT(ISODOW FROM g.game_date)::integer
        AS game_weekday,

      (
        EXTRACT(ISODOW FROM g.game_date)::integer
        <> rd.regular_weekday
      ) AS is_off_day,

      ht.team_name AS home_team,
      at.team_name AS away_team,

      g.home_score,
      g.away_score,
      g.has_final_score

    FROM public.games g

    JOIN current_season s
      ON g.season_id = s.id

    JOIN public.divisions d
      ON g.division_id = d.id

    JOIN regular_days rd
      ON g.division_id = rd.division_id

    JOIN public.teams ht
      ON g.home_team_id = ht.id

    JOIN public.teams at
      ON g.away_team_id = at.id

    CROSS JOIN week_bounds w

    WHERE g.game_date >= w.week_start
      AND g.game_date < w.week_start + 7

    ORDER BY
      rd.regular_weekday,
      d.division_name,
      is_off_day,
      g.game_date,
      g.game_time,
      g.id
    "
  )
}

get_matchup_stats <- function(pool) {
  
  DBI::dbGetQuery(
    pool,
    "
    WITH current_season AS (
      SELECT id
      FROM public.seasons
      WHERE season_name = 'Current'
    ),

    weekly_games AS (
      SELECT
        g.id AS game_id,
        g.season_id,
        g.division_id,
        g.home_team_id,
        g.away_team_id,
        g.game_date,
        g.game_time
      FROM public.games g
      JOIN current_season s
        ON g.season_id = s.id
      WHERE g.game_date >=
        date_trunc(
          'week',
          now() AT TIME ZONE 'America/Los_Angeles'
        )::date
        AND g.game_date <
        date_trunc(
          'week',
          now() AT TIME ZONE 'America/Los_Angeles'
        )::date + 7
        AND g.home_team_id IS NOT NULL
        AND g.away_team_id IS NOT NULL
    ),

    completed_games AS (
      SELECT
        g.id,
        g.season_id,
        g.division_id,
        g.home_team_id,
        g.away_team_id,
        g.home_score,
        g.away_score,
        g.game_date,
        g.game_time,
    
        CASE
          WHEN UPPER(TRIM(g.home_score_raw)) = 'F'
           AND UPPER(TRIM(g.away_score_raw)) <> 'F'
          THEN 'away_forfeit_win'
    
          WHEN UPPER(TRIM(g.away_score_raw)) = 'F'
           AND UPPER(TRIM(g.home_score_raw)) <> 'F'
          THEN 'home_forfeit_win'
    
          WHEN UPPER(TRIM(g.home_score_raw)) = 'F'
           AND UPPER(TRIM(g.away_score_raw)) = 'F'
          THEN 'double_forfeit'
    
          ELSE 'normal'
        END AS result_type
    
      FROM public.games g
      JOIN current_season s
        ON g.season_id = s.id
    
      WHERE (
        (
          g.has_final_score = TRUE
          AND g.home_score IS NOT NULL
          AND g.away_score IS NOT NULL
        )
        OR UPPER(TRIM(g.home_score_raw)) = 'F'
        OR UPPER(TRIM(g.away_score_raw)) = 'F'
      )
    ),

    team_results AS (

      SELECT
        id AS completed_game_id,
        season_id,
        division_id,
        game_date,
        game_time,
        home_team_id AS team_id,
        away_team_id AS opponent_id,
    
        CASE
          WHEN result_type = 'normal'
          THEN home_score
          ELSE NULL
        END AS goals_for,
    
        CASE
          WHEN result_type = 'normal'
          THEN away_score
          ELSE NULL
        END AS goals_against,
    
        CASE
          WHEN result_type = 'home_forfeit_win' THEN 'win'
          WHEN result_type = 'away_forfeit_win' THEN 'loss'
          WHEN result_type = 'double_forfeit' THEN 'exclude'
          WHEN home_score > away_score THEN 'win'
          WHEN home_score < away_score THEN 'loss'
          ELSE 'tie'
        END AS result
    
      FROM completed_games
    
      UNION ALL
    
      SELECT
        id AS completed_game_id,
        season_id,
        division_id,
        game_date,
        game_time,
        away_team_id AS team_id,
        home_team_id AS opponent_id,
    
        CASE
          WHEN result_type = 'normal'
          THEN away_score
          ELSE NULL
        END AS goals_for,
    
        CASE
          WHEN result_type = 'normal'
          THEN home_score
          ELSE NULL
        END AS goals_against,
    
        CASE
          WHEN result_type = 'away_forfeit_win' THEN 'win'
          WHEN result_type = 'home_forfeit_win' THEN 'loss'
          WHEN result_type = 'double_forfeit' THEN 'exclude'
          WHEN away_score > home_score THEN 'win'
          WHEN away_score < home_score THEN 'loss'
          ELSE 'tie'
        END AS result
    
      FROM completed_games
    ),

    matchup_teams AS (
      SELECT
        game_id,
        season_id,
        division_id,
        game_date,
        game_time,
        home_team_id AS team_id,
        away_team_id AS opponent_id,
        'home' AS side
      FROM weekly_games

      UNION ALL

      SELECT
        game_id,
        season_id,
        division_id,
        game_date,
        game_time,
        away_team_id AS team_id,
        home_team_id AS opponent_id,
        'away' AS side
      FROM weekly_games
    )

    SELECT
      mt.game_id,
      mt.team_id,
      mt.side,

      COUNT(*) FILTER (
        WHERE tr.result IN ('win', 'loss', 'tie')
      ) AS games_played,

      COUNT(*) FILTER (
        WHERE tr.result = 'win'
      ) AS wins,
      
      COUNT(*) FILTER (
        WHERE tr.result = 'loss'
      ) AS losses,
      
      COUNT(*) FILTER (
        WHERE tr.result = 'tie'
      ) AS ties,
      
      COALESCE(SUM(tr.goals_for), 0) AS goals_for,
      COALESCE(SUM(tr.goals_against), 0) AS goals_against,
      
      COUNT(*) FILTER (
        WHERE tr.opponent_id = mt.opponent_id
          AND tr.result = 'win'
      ) AS h2h_wins,
      
      COUNT(*) FILTER (
        WHERE tr.opponent_id = mt.opponent_id
          AND tr.result = 'loss'
      ) AS h2h_losses,
      
      COUNT(*) FILTER (
        WHERE tr.opponent_id = mt.opponent_id
          AND tr.result = 'tie'
      ) AS h2h_ties

    FROM matchup_teams mt

    LEFT JOIN team_results tr
      ON tr.season_id = mt.season_id
      AND tr.division_id = mt.division_id
      AND tr.team_id = mt.team_id
      AND (
        tr.game_date < mt.game_date
        OR (
          tr.game_date = mt.game_date
          AND tr.game_time < mt.game_time
        )
      )

    GROUP BY
      mt.game_id,
      mt.team_id,
      mt.side

    ORDER BY
      mt.game_id,
      mt.side
    "
  )
}