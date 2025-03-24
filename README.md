# TractorTsbox

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/TractorTsbox)](https://CRAN.R-project.org/package=TractorTsbox)
[![GH Pages built](https://github.com/TractorTom/TractorTsbox/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/TractorTom/TractorTsbox/actions/workflows/pkgdown.yaml)
[![R-CMD-check](https://github.com/TractorTom/TractorTsbox/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/TractorTom/TractorTsbox/actions/workflows/R-CMD-check.yaml)

[![Codecov test coverage](https://codecov.io/gh/TractorTom/TractorTsbox/graph/badge.svg)](https://app.codecov.io/gh/TractorTom/TractorTsbox)
[![CodeFactor](https://www.codefactor.io/repository/github/TractorTom/TractorTsbox/badge)](https://www.codefactor.io/repository/github/TractorTom/TractorTsbox)
[![lint](https://github.com/TractorTom/TractorTsbox/actions/workflows/lint.yaml/badge.svg)](https://github.com/TractorTom/TractorTsbox/actions/workflows/lint.yaml)
<!-- badges: end -->

TractorTsbox est une boite à outils pour la manipulation des objets `ts` en R.

La motivation pour la création de ce package est le fait que pour créer un objet `ts` en R, il faut préciser la date sous le format `c(AAAA, PP)` (avec `PP` le numéro de la période).

Par exemple, pour désigner le mois de septembre 2024, on utilise `c(2024, 9)` et pour désigner le mois de janvier 2025 on peut écrire `c(2025, 1)` ou `2025`.

Mais on peut aussi utiliser le time-units ($AAAA + PP / f$ avec $f$ la fréquence).

L'idée est d'uniformiser les dates avec un ensemble de fonction de conversion, de formattage mais aussi de modification des `ts`.


## Installation

You can install the development version of TractorTsbox from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("TractorTom/TractorTsbox")
```

## Usage

### Converting Dates

- Convert a date from TimeUnits format to `date_ts` format:

```r
as_yyyytt(2019.75)# 4th quarter 2019
as_yyyytt(2020) # 1st quarter 2020
as_yyyytt(2022 + 1 / 4)# 2nd quarter 2022
```

- Convert a monthly date to a quarterly date:

```r
trim2mens(c(2019L, 4L))# 4th quarter 2019 -> October 2019
mens2trim(c(2020L, 11L)) # November 2020 -> 4th quarter 2020
```

### Manipulating Dates

- Get the previous date:

```r
previous_date_ts(c(2020L, 4L), frequency_ts = 4L, lag = 2L)
```

- Get the next date:

```r
next_date_ts(c(2020L, 4L), frequency_ts = 4L, lag = 2L)
```

- Find the first non-NA date in a time series:

```r
ts1 <- ts(c(NA, NA, NA, 1:10, NA), start = 2000, frequency = 12)
first_date(ts1)
```

### Data Retrieval and Modification

- Retrieve values from a time series:

```r
ts1 <- ts(1:100, start = 2012L, frequency = 12L)
get_value_ts(series = ts1, date_from = c(2015L, 7L), date_to = c(2018L, 6L))
```

- Set values in a time series:

```r
set_value_ts(series = ev_pib, date_ts = c(2021L, 2L), replacement = c(1, 2, 3))
```

- Combine two time series:

```r
trim_1 <- stats::ts(rep(1, 4), start = 2021, frequency = 4)
combine2ts(ev_pib, trim_1)
```

- Extend a time series with new values:

```r
ts1 <- ts(data = c(rep(NA_integer_, 3L), 1L:10L, rep(NA_integer_, 3L)), start = 2020, frequency = 12)
x <- rep(3L, 2L)
extend_ts(series = ts1, replacement = x)
```

### Formatting and Labels

- Normalize a date:

```r
normalize_date_ts(c(2020L, 0L), frequency_ts = 4L)# 4th quarter of 2019
```

- Generate labels for a period:

```r
libelles(date_ts = c(2019L, 10L), frequency_ts = 12L, n = 9L)
```

### Data Information

- Evolution of French GDP until Q1 2022:

```r
ev_pib
```

