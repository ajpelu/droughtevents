# Detect and summarize drought events from SPEI droughts index time series

Identifies drought events in a time series when a given index (e.g.,
SPEI) falls below a specified threshold for at least two consecutive
months. Returns the processed dataset, the detected drought events, and
a summary of their characteristics.

## Usage

``` r
droughts(df, vname, threshold, min_duration = 2)
```

## Arguments

- df:

  A `data.frame` or `tibble` containing the time series data. Must
  include either:

  - a `date` column of class `Date` or `POSIXct`, or

  - separate `year` and `month` columns.

  - a numeric column specified by `vname` that contains the drought
    index values.

- vname:

  A string indicating the name of the numeric column to be evaluated
  (e.g., `"spei12"`).

- threshold:

  A numeric threshold below which values are considered part of a
  drought event.

- min_duration:

  Minimum number of consecutive months below `threshold` required for an
  event to be considered a drought. Default is 2.

## Value

A named list with three elements:

- `data`: the original data frame with drought flags and durations.

- `drought_events`: only the rows that are part of drought events
  (duration ≥ 2).

- `drought_assessment`: summary of each event, including duration,
  intensity, severity, and timing.

## Examples

``` r
data(spei_granada)
# Detect droughts in the SPEI-12 time series with a threshold of -1.28
droughts_result <- droughts(spei_granada, vname = "spei12", threshold = -1.28)
```
