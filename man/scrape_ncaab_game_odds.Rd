% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ncaab.R
\name{scrape_ncaab_game_odds}
\alias{scrape_ncaab_game_odds}
\title{scrape_ncaab_game_odds}
\usage{
scrape_ncaab_game_odds(date = Sys.Date())
}
\arguments{
\item{date}{the day to scrape odds from. Defaults to today}
}
\value{
a dataframe of odds from the Action Network.
}
\description{
Obtains historical and future NCAA MBB odds from the Action Network
}