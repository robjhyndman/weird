# Conflicts between weird packages and other packages

This function lists all the conflicts between packages in the weird
collection and other packages that you have loaded.

## Usage

``` r
weird_conflicts()
```

## Value

A list object of class `weird_conflicts`.

## Details

Some conflicts are deliberately ignored: `intersect`, `union`,
`setequal`, and `setdiff` from dplyr; and `intersect`, `union`,
`setdiff`, and `as.difftime` from lubridate. These functions make the
base equivalents generic, so shouldn't negatively affect any existing
code.

## Examples

``` r
weird_conflicts()
#> ── Conflicts ──────────────────────────────────────────────── weird_conflicts ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
```
