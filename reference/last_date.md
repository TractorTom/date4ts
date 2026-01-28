# Dernière date non NA

Cette fonction calcule la dernière date pour laquelle l'objet `series`
ne vaut pas NA.

## Usage

``` r
last_date(series)
```

## Arguments

- series:

  un objet ts unidimensionnel conforme aux règles de assert_ts

## Value

En sortie, la fonction retourne un objet au format `date_ts` (`AAAA`,
`c(AAAA, MM)` ou `c(AAAA, TT)`)

## Details

La date retournée en output est au format `date_ts`. Si l'objet `series`
ne contient que des NAs, la fonction retourne une erreur.

## See also

`first_date`

## Examples

``` r
ts1 <- ts(c(NA, NA, NA, 1:10, NA), start = 2000, frequency = 12L)
ts2 <- ts(c(1:10), start = 2020, frequency = 4L)

stats::end(ts1)
#> [1] 2001    2
last_date(ts1)
#> [1] 2001    1

stats::end(ts2)
#> [1] 2022    2
last_date(ts2)
#> [1] 2022    2
```
