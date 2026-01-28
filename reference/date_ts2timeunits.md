# Conversion d'une date du format date_ts au format TimeUnits

Conversion d'une date du format date_ts au format TimeUnits

## Usage

``` r
date_ts2timeunits(date_ts, frequency_ts)
```

## Arguments

- date_ts:

  un vecteur numérique, de préférence `integer`, au format `AAAA`,
  `c(AAAA, MM)` ou `c(AAAA, TT)`

- frequency_ts:

  un entier qui vaut `4L` (ou `4.0`) pour les séries trimestrielles et
  `12L` (ou `12.0`) pour les séries mensuelles.

## Value

En sortie, la fonction retourne la date au format `AAAA + TT/4` ou
`AAAA + MM/12` (un numeric de longueur 1).

## Details

`AAAA` signifie que l'année est au format numérique avec 4 chiffres
(Exemple : l'année deux mille vingt-deux s'écrit 2022 et non 22) MM
signifie que le mois est au format numérique (Exemple : le mois de mai
s'écrit 5, le moi de décembre s'écrit 12) TT signifie que le trimestre
est au format numérique (Exemple : le troisième trimestre s'écrit 3)

## Examples

``` r
# Avril 2020
date_ts2timeunits(date_ts = c(2020L, 4L), frequency_ts = 12L)
#> [1] 2020.25
# Novembre 2020
date_ts2timeunits(date_ts = c(2022L, 11L), frequency_ts = 12L)
#> [1] 2022.833

# 4ème trimestre de 2022
date_ts2timeunits(date_ts = c(2022, 4L), frequency_ts = 4L)
#> Warning: Assertion on 'date_ts' failed: Must be of type 'integer', not 'double'.
#> [1] 2022.75
# 2ème trimestre de 1995
date_ts2timeunits(date_ts = c(1995L, 2L), frequency_ts = 4L)
#> [1] 1995.25
```
