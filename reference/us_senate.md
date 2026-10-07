# US Senate co-voting networks

Co-voting networks for the US Senate from the 40th to the 113th Congress
(1867 to 2015). Each Congress gives an undirected network in which each
node is a senator, and an edge connects two senators if they voted the
same way (both yea or both nay) on at least 80% of the bills for which
they were both present. The data are taken from Lee, Li and Wilson
(2020). Although they state that Independent senators were excluded, a
few Independent and minor-party senators are included, coded as Democrat
or Republican.

## Usage

``` r
us_senate_edges

us_senate_members
```

## Format

`us_senate_edges` is a data frame with 73802 rows and 3 columns:

- congress:

  Congress number

- from:

  Node number of one senator

- to:

  Node number of the other senator

`us_senate_members` is a data frame with 7272 rows and 6 columns:

- congress:

  Congress number

- node:

  Node number of the senator

- name:

  Name of the senator (surname first)

- state:

  Two-letter state abbreviation ("USA" for the President)

- party:

  "Democrat" or "Republican"

- icpsr:

  ICPSR identifier of the senator, as used by Voteview

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with
7272 rows and 6 columns.

## Source

Lee, J., Li, G., & Wilson, J. D. (2020). Varying-coefficient models for
dynamic networks. *Computational Statistics & Data Analysis*, 152,
107052.
[doi:10.1016/j.csda.2020.107052](https://doi.org/10.1016/j.csda.2020.107052)
. Data available from <https://github.com/jihuilee/VCERGM>.

Names, states and ICPSR identifiers from Lewis, J. B., Poole, K.,
Rosenthal, H., Boche, A., Rudkin, A., & Sonnet, L. Voteview:
Congressional roll-call votes database. <https://voteview.com/>

## Value

Data frame

## Details

`us_senate_edges` contains the edges of every network, and
`us_senate_members` contains the name, state and party of each senator.
Senators are numbered from 1 within each Congress, so the same number
refers to different senators in different Congresses; use `icpsr` to
follow a senator across Congresses. From the 84th Congress onwards, node
1 is the President, whose announced positions on bills are treated as
votes.

## References

Hyndman, R J (2026) "That's weird: Anomaly detection using R", Chapter
12, <https://OTexts.com/weird/>.

## Examples

``` r
# Number of co-voting edges in each Congress
us_senate_edges |>
  count(congress)
#> Error in order(y): unimplemented type 'list' in 'orderVector1'
# Party composition of the 100th Congress
us_senate_members |>
  filter(congress == 100) |>
  count(party)
#> Error in order(y): unimplemented type 'list' in 'orderVector1'
# Congresses in which Patrick Leahy served
us_senate_members |>
  filter(icpsr == 14307)
#> # A tibble: 20 × 6
#>    congress  node name                  state party    icpsr
#>       <int> <int> <chr>                 <chr> <chr>    <int>
#>  1       94    92 LEAHY, Patrick Joseph VT    Democrat 14307
#>  2       95    95 LEAHY, Patrick Joseph VT    Democrat 14307
#>  3       96    91 LEAHY, Patrick Joseph VT    Democrat 14307
#>  4       97    92 LEAHY, Patrick Joseph VT    Democrat 14307
#>  5       98    91 LEAHY, Patrick Joseph VT    Democrat 14307
#>  6       99    92 LEAHY, Patrick Joseph VT    Democrat 14307
#>  7      100    92 LEAHY, Patrick Joseph VT    Democrat 14307
#>  8      101    92 LEAHY, Patrick Joseph VT    Democrat 14307
#>  9      102    93 LEAHY, Patrick Joseph VT    Democrat 14307
#> 10      103    93 LEAHY, Patrick Joseph VT    Democrat 14307
#> 11      104    94 LEAHY, Patrick Joseph VT    Democrat 14307
#> 12      105    91 LEAHY, Patrick Joseph VT    Democrat 14307
#> 13      106    93 LEAHY, Patrick Joseph VT    Democrat 14307
#> 14      107    93 LEAHY, Patrick Joseph VT    Democrat 14307
#> 15      108    91 LEAHY, Patrick Joseph VT    Democrat 14307
#> 16      109    92 LEAHY, Patrick Joseph VT    Democrat 14307
#> 17      110    92 LEAHY, Patrick Joseph VT    Democrat 14307
#> 18      111   100 LEAHY, Patrick Joseph VT    Democrat 14307
#> 19      112    93 LEAHY, Patrick Joseph VT    Democrat 14307
#> 20      113    96 LEAHY, Patrick Joseph VT    Democrat 14307
```
