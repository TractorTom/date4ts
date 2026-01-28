# Intervalle entre 2 dates

Intervalle entre 2 dates

## Usage

``` r
diff_periode(a, b, frequency_ts)
```

## Arguments

- a:

  un objet date_ts, c'est-à-dire un vecteur numérique, de préférence
  `integer` au format `AAAA`, `c(AAAA, MM)` ou `c(AAAA, TT)`

- b:

  un objet date_ts, c'est-à-dire un vecteur numérique, de préférence
  `integer` au format `AAAA`, `c(AAAA, MM)` ou `c(AAAA, TT)`

- frequency_ts:

  un entier qui vaut `4L` (ou `4.0`) pour les séries trimestrielles et
  `12L` (ou `12.0`) pour les séries mensuelles.

## Value

En sortie, la fonction retourne un entier qui désigne le nombre de
période (mois ou trimestres) qui sépare les 2 dates `a` et `b`.

## Details

On travaille ici avec des dates au format date_ts, c'est-à-dire qui
passe le test de la fonction `assert_date_ts`. Lorsqu'on parle
d'intervalle et de nombre de période entre `a` et `b`, les bornes sont
incluses. Ainsi `diff_periode(2020L, 2020L, 12L)` retourne bien 1L et
non 2L ou 0L.

## Examples

``` r
# Une seule période
diff_periode(a = 2020L, b = 2020L, frequency_ts = 4L)
#> [1] 1

diff_periode(a = c(2000L, 1L), b = c(2020L, 4L), frequency_ts = 4L)
#> [1] 84

# Ordre chronologique respecté
diff_periode(a = c(2021L, 5L), b = c(2023L, 8L), frequency_ts = 12L)
#> [1] 28

# Date inversées
diff_periode(a = c(2023L, 8L), b = c(2021L, 5L), frequency_ts = 12L)
#> [1] 28
```
