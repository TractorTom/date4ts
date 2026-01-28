# Conversion entre date mensuelle et trimestrielle

Les fonctions `trim2mens` et `mens2trim` convertissent une `date_ts` du
format mensuelle `c(AAAA, MM)` au format trimestrielle `c(AAAA, TT)`.

## Usage

``` r
trim2mens(date_ts)

mens2trim(date_ts)
```

## Arguments

- date_ts:

  un vecteur numérique, de préférence `integer`, au format `AAAA`,
  `c(AAAA, MM)` ou `c(AAAA, TT)`

## Value

En sortie, la fonction retourne la date toujours au format `date_ts`.

## Examples

``` r
trim2mens(c(2019L, 4L)) # 4ème trimestre 2019 --> Octobre 2019
#> [1] 2019   10
trim2mens(c(2020L, 1L)) # 1er trimestre 2020 --> Janvier 2020
#> [1] 2020    1

mens2trim(c(2019L, 4L)) # Avril 2019 --> 2ème trimestre 2019
#> [1] 2019    2
mens2trim(c(2020L, 11L)) # Novembre 2020 --> 4ème trimestre 2020
#> [1] 2020    4
```
