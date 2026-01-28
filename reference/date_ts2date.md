# Conversion d'une date du format TS au format date

Conversion d'une date du format TS au format date

## Usage

``` r
date_ts2date(date_ts, frequency_ts)
```

## Arguments

- date_ts:

  un vecteur numérique, de préférence `integer`, au format `AAAA`,
  `c(AAAA, MM)` ou `c(AAAA, TT)`

- frequency_ts:

  un entier qui vaut `4L` (ou `4.0`) pour les séries trimestrielles et
  `12L` (ou `12.0`) pour les séries mensuelles.

## Value

En sortie, la fonction retourne un objet de type Date (atomic) de
longueur 1 qui correspond à l'objet `date_ts`.

## Examples

``` r
date_ts2date(date_ts = c(2020L, 11L), frequency_ts = 12L)
#> [1] "2020-11-01"
date_ts2date(date_ts = c(1995L, 2L), frequency_ts = 4L)
#> [1] "1995-04-01"
```
