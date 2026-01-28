# Obtenir la date suivante

Obtenir la date suivante

## Usage

``` r
next_date_ts(date_ts, frequency_ts, lag = 1L)
```

## Arguments

- date_ts:

  un vecteur numérique, de préférence `integer`, au format `AAAA`,
  `c(AAAA, MM)` ou `c(AAAA, TT)`

- frequency_ts:

  un entier qui vaut `4L` (ou `4.0`) pour les séries trimestrielles et
  `12L` (ou `12.0`) pour les séries mensuelles.

- lag:

  un entier

## Value

En sortie, la fonction retourne un vecteur d'entier qui représente la
date à la période future au format `date_ts`.

## Details

Lorsqu'on parle de date suivante, on parle de date future. L'argument
`lag` est entier et désigne le nombre de décalage que l'on affecte à
notre date. Par exemple pour des lag positif (1L, 2L, 10L) on désigne le
décalage de la période suivante, celle d'après et celle dans 10
périodes. Cependant, lorsque l'argument `lag` vaut zéro, la fonction
retourne la `date` inchangée. Aussi lorsque l'argument `lag` est
négatif, la fonction se comporte comme la fonction `previous_date_ts` et
retourne les périodes passées et non futures.

## See also

`previous_date_ts`

## Examples

``` r
next_date_ts(c(2020L, 4L), frequency_ts = 4L, lag = 2L)
#> [1] 2021    2
next_date_ts(c(2021L, 1L), frequency_ts = 4L, lag = -2L)
#> [1] 2020    3

next_date_ts(c(2020L, 4L), frequency_ts = 12L, lag = 2L)
#> [1] 2020    6
next_date_ts(c(2022L, 6L), frequency_ts = 12L, lag = 12L)
#> [1] 2023    6
```
