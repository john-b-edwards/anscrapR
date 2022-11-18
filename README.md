
# anscrapR

<!-- badges: start -->
<!-- badges: end -->

anscrapR (or Action Network Scraper) is a package designed to interface with the Action Network API to pull down sports betting information. The Action Network has betting information on a variety of games and sports, dating back to 2017. Action Network also provides access to a variety of different books, allowing you to see which books provided which lines.

The information made available from anscrapR is designed to be purely informational.

## Installation

You can install the development version of anscrapR from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("john-b-edwards/anscrapR")
```

## Example

You can scrape a variety of sports with the existing functionality of anscrapR.

``` r
anscrapR::scrape_nfl_game_odds(week = 1, season_type = "reg", season = 2018)
#> # A tibble: 6,003 x 16
#>    game_id status  start_time away_team_id home_team_id league_name type  season
#>      <int> <chr>   <chr>             <int>        <int> <chr>       <chr>  <int>
#>  1   44367 comple~ 2018-09-0~          151          142 nfl         reg     2018
#>  2   44367 comple~ 2018-09-0~          151          142 nfl         reg     2018
#>  3   44367 comple~ 2018-09-0~          151          142 nfl         reg     2018
#>  4   44367 comple~ 2018-09-0~          151          142 nfl         reg     2018
#>  5   44367 comple~ 2018-09-0~          151          142 nfl         reg     2018
#>  6   44367 comple~ 2018-09-0~          151          142 nfl         reg     2018
#>  7   44367 comple~ 2018-09-0~          151          142 nfl         reg     2018
#>  8   44367 comple~ 2018-09-0~          151          142 nfl         reg     2018
#>  9   44367 comple~ 2018-09-0~          151          142 nfl         reg     2018
#> 10   44367 comple~ 2018-09-0~          151          142 nfl         reg     2018
#> # ... with 5,993 more rows, and 8 more variables: away_team_name <chr>,
#> #   home_team_name <chr>, book <chr>, book_id <int>, scope <chr>,
#> #   updated <dttm>, bet_type <chr>, value <dbl>
```

## Todo

Limited functionality is built out for NFL, NBA, CFB, and NCAAMB games. Additional work can be done to build out the ability to pull lines for the following sports:

* MLB
* NHL
* Boxing
* Motor Sports
* Tennis
* Golf
* UFC
* Horse Racing
* Soccer
* WNBA
* Others

Additionally, there are likely other aspects of the Action Network API that have yet to be explored beyond the functionality seen here. Further exploration could prove useful in finding additional endpoints that could be scraped by functions included in anscrapR.
