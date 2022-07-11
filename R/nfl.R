#' @title scrape_nfl_game_odds
#'
#' @description Obtains historical and future NFL odds from the Action Network
#'
#' @param week The week to obtain data from. Ranges from 0-4 for the preseason,
#' from 0-18 for the regular season, and from 0-5 for the postseason.
#' @param season_type The type of season to obtain data for. Must be one of
#' "pre", "reg", or "post".
#' @param season The season to obtain data from.
#' @return a dataframe of odds from the Action Network.

scrape_nfl_game_odds <- function(week, season_type, season) {
  stopifnot(season >= 2017)

  odds_json <- jsonlite::fromJSON(
    glue::glue(
      "https://api.actionnetwork.com/web/v1/scoreboard/nfl?week={week}&seasonType={season_type}&season={season}"
    )
  )

  odds <- lapply(odds_json[["games"]][["id"]], \(x) {
    if (length(odds_json[["games"]][["odds"]][[which(x == odds_json[["games"]][["id"]])]])) {
      odds_json[["games"]][["odds"]][[which(x == odds_json[["games"]][["id"]])]] |>
        dplyr::left_join(
          anscrapR::books |>
            dplyr::select(book_id = id,
                          book = source_name),
          by = c("book_id")
        ) |>
        dplyr::select(-c(meta, line_status)) |>
        tidyr::pivot_longer(-c(type, inserted, book, book_id),
                            names_to = "bet_type",
                            values_to = "value") |>
        dplyr::mutate(updated = lubridate::ymd_hms(inserted),
                      game_id = x) |>
        dplyr::select(game_id,
                      book,
                      book_id,
                      scope = type,
                      updated,
                      bet_type,
                      value)
    } else {
      data.frame()
    }
  }) |>
    dplyr::bind_rows()

  games <- odds_json[["games"]] |>
    tibble::as_tibble() |>
    dplyr::select(
      game_id = id,
      status,
      start_time,
      away_team_id,
      home_team_id,
      league_name,
      type,
      season,
    ) |>
    dplyr::left_join(
      anscrapR::nfl_teams |>
        dplyr::select(away_team_id = id,
                      away_team_name = full_name, ),
      by = c('away_team_id')
    ) |>
    dplyr::left_join(
      anscrapR::nfl_teams |>
        dplyr::select(home_team_id = id,
                      home_team_name = full_name, ),
      by = c('home_team_id')
    ) |>
    dplyr::inner_join(odds, by = c("game_id"))

  return(games)

}
