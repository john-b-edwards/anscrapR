#' parse_odds_url
#'
#' @description A helper function to generically parse data returned from the Action Network odds API.
#' @param url A URL reference to the odds endpoint of the Action Network API.
#'
#' @return The odds data for the game, returned in rectangular format.
parse_odds_url <- function(url) {
  odds_json <- httr::RETRY("GET", url = url, httr::add_headers(
    .headers = c(`user-agent` = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36 Edg/124.0.0.0')
  )) |>
    httr::content(as = 'text', encoding = 'utf-8') |>
    jsonlite::parse_json(simplifyVector = T)

  if(length(odds_json$games)) {
    cleaned <- odds_json |>
      purrr::pluck("games") |>
      tidyr::unnest("markets", names_sep = "_")
    # right now, this just grabs "markets_15" whatever that means. needs to be expanded for all markets I think?
    if(any(stringr::str_detect(colnames(cleaned),"markets"))) {
      cleaned <- cleaned |>
        tidyr::unnest(dplyr::starts_with("markets_"), names_sep = "_") |>
        tidyr::unnest(dplyr::starts_with("markets_"), names_sep = "_")

      game_data <- cleaned |>
        dplyr::rename(game_id = id) |>
        tidyr::unnest(teams) |>
        dplyr::mutate(home_away = dplyr::case_when(away_team_id == id ~ "away", home_team_id == id ~
                                                     "home", T ~ NA)) |>
        dplyr::filter(!is.na(home_away)) |>
        dplyr::select(
          game_id,
          league_id,
          league_name,
          game_type = type,
          season,
          week,
          num_bets,
          status = status_display,
          start_time,
          away_team_id,
          home_team_id,
          home_away,
          winning_team_id,
          home_away,
          full_name
        ) |>
        tidyr::pivot_wider(values_from = c(full_name), names_from = home_away)

      totals <- game_data |>
        dplyr::inner_join(
          cleaned |>
            dplyr::select(id, ends_with("_event_total")) |>
            tidyr::pivot_longer(ends_with("_event_total")) |>
            tidyr::unnest(value, names_sep = "_") |>
            dplyr::select(-c(name)) |>
            tidyr::unnest(value_bet_info,names_sep="_") |>
            tidyr::unnest(c(value_bet_info_tickets, value_bet_info_money),names_sep="_"),
          by = dplyr::join_by("game_id" == "id"),
          relationship = "many-to-many"
        )

      moneylines <- game_data |>
        dplyr::inner_join(
          cleaned |>
            dplyr::select(id, ends_with("_event_moneyline")) |>
            tidyr::pivot_longer(ends_with("_event_moneyline")) |>
            tidyr::unnest(value, names_sep = "_") |>
            dplyr::select(-c(name)) |>
            tidyr::unnest(value_bet_info,names_sep="_") |>
            tidyr::unnest(c(value_bet_info_tickets, value_bet_info_money),names_sep="_"),
          by = dplyr::join_by("game_id" == "id"),
          relationship = "many-to-many"
        )

      spreads <- game_data |>
        dplyr::inner_join(
          cleaned |>
            dplyr::select(id, ends_with("_event_spread")) |>
            tidyr::pivot_longer(ends_with("_event_spread")) |>
            tidyr::unnest(value, names_sep = "_") |>
            dplyr::select(-c(name)) |>
            tidyr::unnest(value_bet_info,names_sep="_") |>
            tidyr::unnest(c(value_bet_info_tickets, value_bet_info_money),names_sep="_"),
          by = dplyr::join_by("game_id" == "id"),
          relationship = "many-to-many"
        )

      games <- dplyr::bind_rows(totals, moneylines, spreads) |>
        dplyr::rename_with(\(x) gsub("value_","",x)) |>
        dplyr::arrange(game_id, start_time, book_id)

      if ("bet_info" %in% colnames(games)) {
        games <- games |>
          tidyr::unnest("bet_info") |>
          tidyr::unnest("money", names_sep = "_") |>
          tidyr::unnest("tickets", names_sep = "_")
      }
    } else {
      cli::cli_alert_warning("No betting information found for this query! Returning game info without betting info...")
      games <- cleaned |>
        dplyr::rename(game_id = id) |>
        tidyr::unnest(teams) |>
        dplyr::mutate(home_away = dplyr::case_when(away_team_id == id ~ "away", home_team_id == id ~
                                                     "home", T ~ NA)) |>
        dplyr::filter(!is.na(home_away)) |>
        dplyr::select(
          game_id,
          league_id,
          league_name,
          game_type = type,
          season,
          week,
          num_bets,
          status = status_display,
          start_time,
          away_team_id,
          home_team_id,
          home_away,
          winning_team_id,
          home_away,
          full_name
        ) |>
        tidyr::pivot_wider(values_from = c(full_name), names_from = home_away)
    }
  } else {
    cli::cli_alert_warning("No games found for this query! Returning empty dataframe...")
    games <- data.frame()
  }

  return(games)
}


#' @title scrape_cfb_game_odds
#'
#' @description Obtains historical and future CFB odds from the Action Network
#'
#' @param week The day to obtain data from. Ranges from 0-4 for the preseason,
#' from 0-15 for the regular season (including conference championships), and
#' just 1 for the postseason.
#' @param season_type The type of season to obtain data for. Must be one of
#' "reg", or "post".
#' @param season The season to obtain data from.
#' @param division One of "FBS","FCS", or "D2".
#' @param period One of "event", "firsthalf", "secondhalf", "firstquarter", "secondquarter", "thirdquarter", or "fourthquarter". Defaults to "event".
#' @return a dataframe of odds from the Action Network.
scrape_cfb_game_odds <-
  function(week, season_type, season, division = "FBS", period = "event") {
    stopifnot(season >= 2017)
    games <- glue::glue(
      "https://api.actionnetwork.com/web/v2/scoreboard/ncaaf?season={season}&division={division}&week={week}&seasonType={season_type}&periods={period}"
    ) |>
      parse_odds_url()

    return(games)
  }

#' @title scrape_nfl_game_odds
#'
#' @description Obtains historical and future NFL odds from the Action Network
#'
#' @param week The week to obtain data from. Ranges from 0-4 for the preseason,
#' from 0-18 for the regular season, and from 0-5 for the postseason.
#' @param season_type The type of season to obtain data for. Must be one of
#' "pre", "reg", or "post".
#' @param season The season to obtain data from.
#' @param period One of "event", "firsthalf", "secondhalf", "firstquarter", "secondquarter", "thirdquarter", or "fourthquarter". Defaults to "event".
#' @return a dataframe of odds from the Action Network.
scrape_nfl_game_odds <- function(week, season_type, season,
                                 period = "event") {
  stopifnot(season >= 2017)

  games <- glue::glue(
    "https://api.actionnetwork.com/web/v2/scoreboard/nfl?season={season}&week={week}&seasonType={season_type}&periods={period}"
  ) |>
    parse_odds_url()

  return(games)

}

#' scrape_ncaab_game_odds
#'
#' @description
#' Return the odds for a given period for all games on a specified date for NCAA men's basketball games from the Action Network.
#'
#' @param date The date to obtain odds for. Defaults to the current day.
#' @param period One of "event", "firsthalf", "secondhalf". Defaults to "event".
#'
#' @return A dataframe of odds for men's NCAA basketball games on the specified date.
#' @export
scrape_ncaab_game_odds <- function(date = Sys.Date(),
                                   period = "event") {
  games <- glue::glue(
    "https://api.actionnetwork.com/web/v2/scoreboard/ncaab?division=D1&date={gsub('-','',date)}&tournament=0&periods={period}"
  ) |>
    parse_odds_url()

  return(games)

}

#' scrape_nhl_game_odds
#'
#' @description
#' Return the odds for a given period for all games on a specified date for NHL games from the Action Network.
#'
#' @param date The date to obtain odds for. Defaults to the current day.
#' @param period One of "event", "firstperiod", "secondperiod", "thirdperiod". Defaults to "event".
#'
#' @return A dataframe of odds for NHL games on the specified date.
#' @export
scrape_nhl_game_odds <- function(date = Sys.Date(),
                                 period = "event") {
  games <- glue::glue(
    "https://api.actionnetwork.com/web/v2/scoreboard/nhl?date={gsub('-','',date)}&periods={period}"
  ) |>
    parse_odds_url()

  return(games)

}

#' scrape_nba_game_odds
#'
#' @description
#' Return the odds for a given period for all games on a specified date for NBA games from the Action Network.
#'
#' @param date The date to obtain odds for. Defaults to the current day.
#' @param period One of "event", "firsthalf", "secondhalf", "firstquarter", "secondquarter", "thirdquarter", "fourthquarter". Defaults to "event".
#'
#' @return A dataframe of odds for NBA games on the specified date.
#' @export
scrape_nba_game_odds <- function(date = Sys.Date(),
                                 period = "event") {
  games <- glue::glue(
    "https://api.actionnetwork.com/web/v2/scoreboard/nba?date={gsub('-','',date)}&periods={period}"
  ) |>
    parse_odds_url()

  return(games)

}

#' scrape_wnba_game_odds
#'
#' @description
#' Return the odds for a given period for all games on a specified date for WNBA games from the Action Network.
#'
#' @param date The date to obtain odds for. Defaults to the current day.
#' @param period One of "event", "firsthalf", "secondhalf", "firstquarter", "secondquarter", "thirdquarter", "fourthquarter". Defaults to "event".
#'
#' @return A dataframe of odds for WNBA games on the specified date.
#' @export
scrape_wnba_game_odds <- function(date = Sys.Date(),
                                  period = "event") {
  games <- glue::glue(
    "https://api.actionnetwork.com/web/v2/scoreboard/wnba?date={gsub('-','',date)}&periods={period}"
  ) |>
    parse_odds_url()

  return(games)

}

#' scrape_wncaab_game_odds
#'
#' @description
#' Return the odds for a given period for all games on a specified date for NCAAB women's games from the Action Network.
#'
#' @param date The date to obtain odds for. Defaults to the current day.
#' @param period One of "event", "firsthalf", "secondhalf", "firstquarter", "secondquarter", "thirdquarter", "fourthquarter". Defaults to "event".
#'
#' @return A dataframe of odds for NCAAB women's games on the specified date.
#' @export
scrape_wncaab_game_odds <- function(date = Sys.Date(),
                                    period = "event") {
  games <- glue::glue(
    "https://api.actionnetwork.com/web/v2/scoreboard/ncaaw?division=D1&date={gsub('-','',date)}&tournament=0&periods={event}"
  ) |>
    parse_odds_url()

  return(games)

}

#' scrape_mlb_game_odds
#'
#' @description
#' Return the odds for a given period for all games on a specified date for MLB games from the Action Network.
#'
#' @param date The date to obtain odds for. Defaults to the current day.
#' @param period One of "event", "firstinning", "firstfiveinnings". Defaults to "event".
#'
#' @return A dataframe of odds for MLB games on the specified date.
#' @export
scrape_mlb_game_odds <- function(date = Sys.Date(),
                                 period = "event") {
  games <- glue::glue(
    "https://api.actionnetwork.com/web/v2/scoreboard/mlb?date={gsub('-','',date)}&periods={period}"
  ) |>
    parse_odds_url()

  return(games)

}

#' scrape_soccer_game_odds
#'
#' @description
#' Return the odds for a given period for all games on a specified date for soccer games from the Action Network.
#'
#' @param date The date to obtain odds for. Defaults to the current day.
#' @param period One of "event", "firsthalf", "secondhalf". Defaults to "event".
#'
#' @return A dataframe of odds for soccer games on the specified date.
#' @export
scrape_soccer_game_odds <- function(date = Sys.Date(),
                                    period = "event") {
  games <- glue::glue(
    "https://api.actionnetwork.com/web/v2/scoreboard/soccer?date={gsub('-','',date)}&periods={period}"
  ) |>
    parse_odds_url()

  return(games)

}
