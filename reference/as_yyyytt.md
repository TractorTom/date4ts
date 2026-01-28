# Conversion au format date_ts

Les fonctions `as_yyyytt` et `as_yyyymm` convertissent une date du
format TimeUnits au format `date_ts`.

## Usage

``` r
as_yyyytt(timeunits)

as_yyyymm(timeunits)
```

## Arguments

- timeunits:

  une date en année (Par exemple 2015.25 pour le 2ème trimestre 2015 ou
  `2021.83333333333` pour novembre 2021)

## Value

En sortie, ces fonctions retournent la date au format `date_ts`
(c'est-à-dire un vecteur d'entiers de la forme `AAAA`, `c(AAAA, MM)` ou
`c(AAAA, TT)`)

## Details

La fonction `as_yyyytt` retourne la date par trimestre et la fonction
`as_yyyymm` retourne la date par mois.

## Examples

``` r
as_yyyytt(2019.75) # 4ème trimestre 2019
#> [1] 2019    4
as_yyyytt(2020) # 1er trimestre 2020
#> [1] 2020    1
as_yyyytt(2022 + 1 / 4) # 2ème trimestre 2022
#> [1] 2022    2

as_yyyymm(2019.75) # Octobre 2019
#> [1] 2019   10
as_yyyymm(2020) # Janvier 2020
#> [1] 2020    1
as_yyyymm(2020 + 1 / 12) # Février 2020
#> [1] 2020    2
as_yyyymm(2020 + 12 / 12) # Janvier 2021
#> [1] 2021    1
```
