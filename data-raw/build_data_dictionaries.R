setwd(here::here())

pull_teams <- function(league_id) {
  glue::glue("https://api.actionnetwork.com/web/v1/leagues/{league_id}/teams") |>
  jsonlite::fromJSON() |>
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
}

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

nfl_teams <- pull_teams(1)

usethis::use_data(nfl_teams, overwrite = T)

# Build CFB teams dataframe ----

cfb_teams <- pull_teams(2)

usethis::use_data(cfb_teams, overwrite = T)

# build NHL teams dataframe ----

nhl_teams <- pull_teams(3)

usethis::use_data(nhl_teams, overwrite = T)

# build NBA teams dataframe ----

nba_teams <- pull_teams(4)

usethis::use_data(nba_teams, overwrite = T)

# build WNBA teams dataframe ----

wnba_teams <- pull_teams(5)

usethis::use_data(wnba_teams, overwrite = T)

# build NCAAB teams dataframe ----

ncaab_teams <- pull_teams(6)

usethis::use_data(ncaab_teams, overwrite = T)

# build WNCAAB teams dataframe ----

wncaab_teams <- pull_teams(7)

usethis::use_data(wncaab_teams, overwrite = T)

# build MLB teams dataframe -----

mlb_teams <- pull_teams(8)

usethis::use_data(mlb_teams, overwrite = T)

# build Premiere League teams dataframe ----

pl_teams <- pull_teams(9)

usethis::use_data(pl_teams, overwrite = T)

# build Bundesliga teams dataframe ----

bundesliga_teams <- pull_teams(10)

usethis::use_data(bundesliga_teams, overwrite = T)

# build Ligue teams dataframe ----

ligue_teams <- pull_teams(11)

usethis::use_data(ligue_teams, overwrite = T)

# build La Liga teams dataframe ----

la_liga_teams <- pull_teams(12)

usethis::use_data(la_liga_teams, overwrite = T)

# build Serie teams dataframe ----

serie_teams <- pull_teams(13)

usethis::use_data(serie_teams, overwrite = T)

# build MLS teams dataframe ----

mls_teams <- pull_teams(14)

usethis::use_data(mls_teams, overwrite = T)

# build world cup dataframe -----
wc_teams <- pull_teams(20)

usethis::use_data(wc_teams, overwrite = T)

# build women's world cup dataframe -----
wwc_teams <- pull_teams(34)

usethis::use_data(wwc_teams, overwrite = T)

# build aaf dataframe -----
aaf_teams <- pull_teams(21)

usethis::use_data(aaf_teams, overwrite = T)

# build usfl dataframe -------
aaf_teams <- pull_teams(24)

usethis::use_data(aaf_teams, overwrite = T)
