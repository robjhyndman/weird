# Cricket batting data for international test players

A dataset containing career batting statistics for all international
test players (men and women) up to 6 October 2025.

## Usage

``` r
cricket_batting
```

## Format

A data frame with 3968 rows and 15 variables:

- Player:

  Player name in form of "initials surname"

- Country:

  Country played for

- Start:

  First year of test playing career

- End:

  Last year of test playing career

- Matches:

  Number of matches played

- Innings:

  Number of innings batted

- NotOuts:

  Number of times not out

- Runs:

  Total runs scored

- HighScore:

  Highest score in an innings

- HighScoreNotOut:

  Was highest score not out?

- Average:

  Batting average at end of career

- Hundreds:

  Total number of 100s scored

- Fifties:

  Total number of 50s scored

- Ducks:

  Total number of 0s scored

- Gender:

  "Men" or "Women"

## Source

<https://www.espncricinfo.com>

## Value

Data frame

## Examples

``` r
cricket_batting |>
  filter(Innings > 20) |>
  select(Player, Country, Matches, Runs, Average, Hundreds, Fifties, Ducks) |>
  arrange(desc(Average))
#> # A tibble: 1,217 × 8
#>    Player        Country      Matches  Runs Average Hundreds Fifties Ducks
#>    <chr>         <chr>          <int> <int>   <dbl>    <int>   <int> <int>
#>  1 DG Bradman    Australia         52  6996    99.9       29      13     7
#>  2 PHKD Mendis   Sri Lanka         14  1316    62.7        5       5     0
#>  3 AC Voges      Australia         20  1485    61.9        5       4     2
#>  4 RG Pollock    South Africa      23  2256    61.0        7      11     1
#>  5 GA Headley    West Indies       22  2190    60.8       10       5     2
#>  6 H Sutcliffe   England           54  4555    60.7       16      23     2
#>  7 E Bakewell    England           12  1078    59.9        4       7     0
#>  8 E Paynter     England           20  1540    59.2        4       7     3
#>  9 KF Barrington England           82  6806    58.7       20      35     5
#> 10 ED Weekes     West Indies       48  4455    58.6       15      19     6
#> # ℹ 1,207 more rows
```
