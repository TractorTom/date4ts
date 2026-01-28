# Comparaison de 2 date_ts

Comparaison de 2 date_ts

## Usage

``` r
is_before(a, b, frequency_ts, strict = FALSE)
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

- strict:

  un booleen (default `FALSE`)

## Value

En sortie, la fonction retourne un booleen (de longueur 1) qui indique
si la date `a` est antérieure à la date `b`.

## Details

Les dates `a` et `b` sont au format date_ts. L'argument frequency_ts est
nécessaire pour interpréter les dates. Ainsi, si je souhaite comparer la
date `a = c(2023L, 4L)` et la date `b = c(2023L, -2L)`. Dans le cas
d'une fréquence mensuelle, la date `a` est antérieure à la date `b`.
Dans le cas d'une fréquence mensuelle, c'est l'inverse. Si `strict` vaut
`TRUE`, la fonction compare strictement les dates `a` et `b` (`<`).

## Examples

``` r
is_before(a = c(2020L, 3L), b = c(2022L, 4L), frequency_ts = 12L)
#> [1] TRUE
is_before(a = c(2022L, 3L), b = c(2010L, 1L), frequency_ts = 4L)
#> [1] FALSE

is_before(a = c(2022L, 4L), b = c(2022L, 4L), frequency_ts = 12L)
#> [1] TRUE
is_before(a = c(2022L, 4L), b = c(2022L, 4L),
    frequency_ts = 12L, strict = TRUE)
#> [1] FALSE

# Importance de la fréquence
is_before(a = c(2022L, -3L), b = c(2021L, 8L), frequency_ts = 12L)
#> Warning: Assertion on 'period' failed: Element 1 is not >= 1.
#> [1] FALSE
is_before(a = c(2022L, -3L), b = c(2021L, 8L), frequency_ts = 4L)
#> Warning: Assertion on 'period' failed: Element 1 is not >= 1.
#> Warning: Assertion on 'period' failed: Element 1 is not <= 4.
#> [1] TRUE
```
