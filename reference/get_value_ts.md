# Récupère des valeurs d'un ts

La fonction `get_value_ts` permet de récupérer des valeurs.

## Usage

``` r
get_value_ts(series, date_from, date_to, n)
```

## Arguments

- series:

  un objet ts unidimensionnel conforme aux règles de assert_ts

- date_from:

  un vecteur numérique, de préférence `integer` au format `AAAA`,
  `c(AAAA, MM)` ou `c(AAAA, TT)`

- date_to:

  un vecteur numérique, de préférence `integer` au format `AAAA`,
  `c(AAAA, MM)` ou `c(AAAA, TT)`

- n:

  un entier

## Value

En sortie, la fonction retourne un vecteur (atomic) de même type que
`series` avec les valeurs extraites.

## Details

Il faut qu'exactement 2 arguments parmi `date_to`, `date_to` et `n`
soient renseignés. L'argument `n` combiné avec `date_to` ou `date_from`
permet de déterminer combien de période seront retourné à partir de ou
jusqu'à la date renseignée.

Il faudrait parler d'extraction car contrairement à la fonction
`window`, ici on retourne un vecteur devaleur et plus un objet ts.

## Examples

``` r
ts1 <- ts(1:100, start = 2012L, frequency = 12L)
ts2 <- ts(letters, start = 2014L, frequency = 4L)
ts3 <- ts(exp(-(1:50)), start = 2015L, frequency = 12L)

get_value_ts(series = ts1, date_from = c(2015L, 7L), date_to = c(2018L, 6L))
#>  [1] 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67
#> [26] 68 69 70 71 72 73 74 75 76 77 78
get_value_ts(series = ts2, date_from = c(2018L, 4L), n = 4L)
#> [1] "t" "u" "v" "w"
get_value_ts(series = ts3, date_to = c(2018L, 4L), n = 14L)
#>  [1] 1.879529e-12 6.914400e-13 2.543666e-13 9.357623e-14 3.442477e-14
#>  [6] 1.266417e-14 4.658886e-15 1.713908e-15 6.305117e-16 2.319523e-16
#> [11] 8.533048e-17 3.139133e-17 1.154822e-17 4.248354e-18
```
