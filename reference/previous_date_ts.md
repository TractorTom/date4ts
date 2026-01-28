# Obtenir la date précédente

Obtenir la date précédente

## Usage

``` r
previous_date_ts(date_ts, frequency_ts, lag = 1L)
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
date à la période passée au format `date_ts`.

## Details

Lorsqu'on parle de date précédente, on parle de date passée. L'argument
`lag` est entier et désigne le nombre de décalage que l'on affecte à
notre date. Par exemple pour des lag positif (1L, 2L, 10L) on désigne le
décalage de la période précédente, celle d'avant et celle d'il y a 10
périodes. Cependant, lorsque l'argument `lag` vaut zéro, la fonction
retourne la `date` inchangée. Aussi lorsque l'argument `lag` est
négatif, la fonction se comporte comme la fonction `next_date_ts` et
retourne les périodes futures et non passées.

## See also

`next_date_ts`

## Examples

``` r
previous_date_ts(c(2020L, 4L), frequency_ts = 4L, lag = 2L)
#> [1] 2020    2
previous_date_ts(c(2021L, 1L), frequency_ts = 4L, lag = -2L)
#> [1] 2021    3

previous_date_ts(c(2020L, 4L), frequency_ts = 12L, lag = 2L)
#> [1] 2020    2
previous_date_ts(c(2022L, 6L), frequency_ts = 12L, lag = 12L)
#> [1] 2021    6
```
