# Conversion d'une date au format TS

La fonction `date2date_ts` prend en argument une date au format date
(integer avec une class Date) et la convertit au format `date_ts` :
`c(AAAA, MM)` ou `c(AAAA, TT)` avec le mois ou trimestre en cours.

## Usage

``` r
date2date_ts(date, frequency_ts = 12L)
```

## Arguments

- date:

  un objet de type Date

- frequency_ts:

  un entier qui vaut `4L` (ou `4.0`) pour les séries trimestrielles et
  `12L` (ou `12.0`) pour les séries mensuelles.

## Value

En sortie, la fonction retourne la date au format `date_ts`
(`c(AAAA, MM)` ou `c(AAAA, TT)`) avec le mois ou trimestre en cours
selon l'argument `frequency_ts`.

## Examples

``` r
date2date_ts(as.Date("2000-01-01"))
#> [1] 2000    1
date2date_ts(as.Date("2000-01-01"), frequency_ts = 12L)
#> [1] 2000    1

date2date_ts(as.Date("2021-10-01"), frequency_ts = 12L)
#> [1] 2021   10
date2date_ts(as.Date("2021-10-01"), frequency_ts = 4L)
#> [1] 2021    4
```
