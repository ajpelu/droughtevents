# SPEI time series for Granada

Standardised Precipitation-Evapotranspiration Index (SPEI) time series
for Granada, Spain (coordinates -3.75, 37.25), at four different time
scales (6, 12, 24, and 48 months). SPEI is a multi-scalar drought index
that combines precipitation and evapotranspiration data; negative values
indicate drier-than-average conditions, positive values indicate
wetter-than-average conditions.

Note that the longer time scales (`spei12`, `spei24`, `spei48`) require
that many months of prior data to be computed, so they contain `NaN`
values at the start of the series.

## Usage

``` r
spei_granada
```

## Format

A data frame (tibble) with 1,483 rows and 5 columns:

- date:

  Date of the observation (class `Date`), monthly from 1901-06-16 to
  2024-12-16.

- spei6:

  SPEI at the 6-month time scale.

- spei12:

  SPEI at the 12-month time scale. `NaN` for the first months of the
  series.

- spei24:

  SPEI at the 24-month time scale. `NaN` for the first months of the
  series.

- spei48:

  SPEI at the 48-month time scale. `NaN` for the first months of the
  series.

## Source

<https://spei.csic.es/spei_database/>

## Value

A `tibble` with 1,483 rows and 5 columns, as described in `@format`.

## References

Vicente-Serrano, S.M., Beguería, S., López-Moreno, J.I. (2010). A
Multi-scalar Drought Index Sensitive to Global Warming: The Standardized
Precipitation Evapotranspiration Index. *Journal of Climate*, 23(7),
1696-1718.
[doi:10.1175/2009JCLI2909.1](https://doi.org/10.1175/2009JCLI2909.1)

## Examples

``` r
data(spei_granada)
tail(spei_granada)
#> # A tibble: 6 × 5
#>   date       spei6 spei12 spei24 spei48
#>   <date>     <dbl>  <dbl>  <dbl>  <dbl>
#> 1 2024-07-16  -1.1   -1.5   -2     -2.1
#> 2 2024-08-16  -1.3   -1.5   -2     -2.1
#> 3 2024-09-16  -2.2   -1.7   -2     -2.1
#> 4 2024-10-16  -1     -1.3   -1.6   -2  
#> 5 2024-11-16  -0.5   -1.1   -1.6   -2  
#> 6 2024-12-16  -0.9   -1.2   -2     -2  

# Detect drought events using the 12-month SPEI
droughts(spei_granada, vname = "spei12", threshold = -1.28)
#> Drought Assessment Summary:
#> # A tibble: 17 × 9
#>    index_events d_duration d_intensity d_severity lowest_spei month_peak minyear
#>           <int>      <dbl>       <dbl>      <dbl>       <dbl>      <dbl>   <dbl>
#>  1            3         11       -1.93       21.2        -2.3         10    1945
#>  2            5          4       -1.45        5.8        -1.6          5    1949
#>  3            7          2       -1.45        2.9        -1.5         12    1950
#>  4           11          2       -1.35        2.7        -1.4          6    1965
#>  5           21         14       -1.83       25.6        -2.2         10    1994
#>  6           23          9       -1.63       14.7        -1.9          6    1998
#>  7           27          7       -1.64       11.5        -1.7          7    2005
#>  8           29          9       -1.92       17.3        -2.2          8    2012
#>  9           31          5       -1.38        6.9        -1.5          5    2014
#> 10           33          5       -1.64        8.2        -1.9          3    2015
#> 11           35          3       -1.5         4.5        -1.7         10    2016
#> 12           37         11       -1.6        17.6        -2.1         12    2017
#> 13           39         11       -1.7        18.7        -2           10    2019
#> 14           41          2       -1.5         3          -1.7         12    2020
#> 15           43         21       -1.58       33.2        -2.1          1    2021
#> 16           45         12       -1.91       22.9        -2.4          1    2023
#> 17           47          5       -1.54        7.7        -1.7          9    2024
#> # ℹ 2 more variables: maxyear <dbl>, rangeDate <chr>
```
