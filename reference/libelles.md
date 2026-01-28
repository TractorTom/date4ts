# Libelés pour une période

La fonction `libelles` créé un vecteur de chaines de caractère contenant
les libelés de toutes les dates sur une période

## Usage

``` r
libelles(date_ts, frequency_ts, n = 1L, warn = TRUE)
```

## Arguments

- date_ts:

  un vecteur numérique, de préférence `integer`, au format `AAAA`,
  `c(AAAA, MM)` ou `c(AAAA, TT)`

- frequency_ts:

  un entier qui vaut `4L` (ou `4.0`) pour les séries trimestrielles et
  `12L` (ou `12.0`) pour les séries mensuelles.

- n:

  un entier

- warn:

  un booleen

## Value

En sortie, la fonction retourne un vecteur de chaine de caractère de
longueur `n` avec les libellés de la période (de la date `date_ts` à la
date `date_ts + n périodes`.

## Details

Pour choisir la période, il faut spécifier une date de début `date_ts`,
une fréquence `frequency_ts` pour le pas entre 2 dates (trimestrielle ou
mensuelle) et un nombre de valeur `n` (nombre de période).

Si l'argument `warn` est `FALSE`, alors la fonction ne retournera pas de
warning lors de l'évaluation.

## Examples

``` r
libelles(date_ts = c(2019L, 10L), frequency_ts = 12L, n = 9L)
#> [1] "Oct 2019" "Nov 2019" "Dec 2019" "Jan 2020" "Feb 2020" "Mar 2020" "Apr 2020"
#> [8] "May 2020" "Jun 2020"
libelles(date_ts = c(2019L, 4L), frequency_ts = 4L, n = 3L)
#> [1] "Q4 2019" "Q1 2020" "Q2 2020"
```
