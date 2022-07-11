setwd(here::here())

# Build books dataframe ----

books <-
  jsonlite::fromJSON("https://api.actionnetwork.com/web/v1/books")

books <- books |>
  purrr::pluck("books") |>
  dplyr::select(-c("meta")) |>
  cbind(books |>
          purrr::pluck("books") |>
          purrr::pluck("meta") |>
          purrr::pluck("logos")) |>
  dplyr::mutate(
    states = books |>
      purrr::pluck("books") |>
      purrr::pluck("meta") |>
      purrr::pluck("states") |>
      lapply(paste0, collapse = ";") |>
      unlist()
  ) |>
  cbind(books |>
          purrr::pluck("books") |>
          purrr::pluck("meta") |>
          dplyr::select(-c("logos", "states", "deeplink")))

usethis::use_data(books, overwrite = T)

# Build NFL teams dataframe ----

nfl_teams <-
  jsonlite::fromJSON("https://api.actionnetwork.com/web/v1/leagues/1/teams") |>
  purrr::pluck("teams") |>
  dplyr::select(
    id,
    full_name,
    display_name,
    short_name,
    location,
    abbr,
    logo,
    primary_color,
    secondary_color,
    conference_type,
    division_type,
    since
  )

usethis::use_data(nfl_teams, overwrite = T)

# Build CFB teams dataframe ----

cfb_teams <-
  jsonlite::fromJSON("https://api.actionnetwork.com/web/v1/leagues/2/teams") |>
  purrr::pluck("teams") |>
  dplyr::select(
    id,
    full_name,
    display_name,
    short_name,
    location,
    abbr,
    logo,
    primary_color,
    secondary_color,
    conference_type,
    division_type,
    since
  )

usethis::use_data(cfb_teams, overwrite = T)

# build NBA teams dataframe ----

nba_teams <-
  jsonlite::fromJSON("https://api.actionnetwork.com/web/v1/leagues/4/teams") |>
  purrr::pluck("teams") |>
  dplyr::select(
    id,
    full_name,
    display_name,
    short_name,
    location,
    abbr,
    logo,
    primary_color,
    secondary_color,
    conference_type,
    division_type,
    since
  )

usethis::use_data(nba_teams, overwrite = T)

# build NCAAB teams dataframe ----

ncaab_teams <-
  jsonlite::fromJSON("https://api.actionnetwork.com/web/v1/leagues/6/teams") |>
  purrr::pluck("teams") |>
  dplyr::select(
    id,
    full_name,
    display_name,
    short_name,
    location,
    abbr,
    logo,
    primary_color,
    secondary_color,
    conference_type,
    division_type,
    since
  )

usethis::use_data(ncaab_teams, overwrite = T)

# todo: NHL, WNBA, MLB, Foreign soccer leagues, World Cup, XFL?
