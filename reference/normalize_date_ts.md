# Ajuste un objet date_ts dans un format conforme.

Ajuste un objet date_ts dans un format conforme.

## Usage

``` r
normalize_date_ts(date_ts, frequency_ts, test = TRUE)
```

## Arguments

- date_ts:

  un vecteur numérique, de préférence `integer`, au format `AAAA`,
  `c(AAAA, MM)` ou `c(AAAA, TT)`

- frequency_ts:

  un entier qui vaut `4L` (ou `4.0`) pour les séries trimestrielles et
  `12L` (ou `12.0`) pour les séries mensuelles.

- test:

  un booléen (Default is TRUE)

## Value

En sortie, la fonction retourne une date au même format que l'objet
`date_ts` avec la période inclus entre 1 et la fréquence.

## Details

Ici le formattage correspond à une réécriture de la date sans en changer
la valeur. Alors que l'objet c(2020L, 12L) désigne le mois de décembre
2020 et c(2021L, 1L) le mois de janvier 2021, on peut imaginer que la
date_ts c(2021L, 0L) peut aussi représenter le mois de décembre 2020. Si
l'argument `test` est mis à FALSE, alors aucun test ne sera effectué sur
les données en entrée.

## Examples

``` r
# Formattage inchangée
normalize_date_ts(c(2020L, 1L), frequency_ts = 4L) # 1er trimestre de 2020
#> [1] 2020    1
normalize_date_ts(c(2020L, 8L), frequency_ts = 12L) # Aout 2020
#> [1] 2020    8

# Retour dans le passé
normalize_date_ts(c(2020L, 0L), frequency_ts = 4L) # 4ème trimestre de 2019
#> Warning: Assertion on 'period' failed: Element 1 is not >= 1.
#> [1] 2019    4
normalize_date_ts(c(2020L, -10L), frequency_ts = 12L) # février 2019
#> Warning: Assertion on 'period' failed: Element 1 is not >= 1.
#> [1] 2019    2

# Avancée dans le futur
normalize_date_ts(c(2020L, 7L), frequency_ts = 4L) # 3ème trimestre de 2021
#> Warning: Assertion on 'period' failed: Element 1 is not <= 4.
#> [1] 2021    3
normalize_date_ts(c(2020L, 13L), frequency_ts = 4L) # janvier 2021
#> Warning: Assertion on 'period' failed: Element 1 is not <= 4.
#> [1] 2023    1
```
