# Vérifie le format de date

La fonction `assert_date_ts` vérifie qu'un objet est de type `AAAA`,
`c(AAAA, MM)` ou `c(AAAA, TT)`

## Usage

``` r
check_date_ts(x, frequency_ts, .var.name = checkmate::vname(x), warn = TRUE)

assert_date_ts(
  x,
  frequency_ts,
  add = NULL,
  .var.name = checkmate::vname(x),
  warn = TRUE
)
```

## Arguments

- x:

  un vecteur numérique, de préférence `integer` au format `AAAA`,
  `c(AAAA, MM)` ou `c(AAAA, TT)`

- frequency_ts:

  un entier qui vaut `4L` (ou `4.0`) pour les séries trimestrielles et
  `12L` (ou `12.0`) pour les séries mensuelles.

- .var.name:

  Nom de l'objet à vérifier pour afficher dans les messages

- warn:

  un booleen

- add:

  Collection pour stocker les messages d'erreurs (Default is NULL)

## Value

En sortie la fonction retourne l'objet `x` de manière invisible ou une
erreur.

## Details

Les fonctions du package date4ts sont faites pour fonctionner avec des
times-series de fréquence mensuelle ou trimestrielle et basés sur le
système des mois, trimestres et années classiques. On cherche donc à
favoriser l'utilisation de vecteur `c(AAAA, MM)` pour désigner la date
choisie. Lorsque l'objet `x` en entrée est au mauvais format, il est
corrigé pendant la checks et l'objet en sortie est au bon format. Si
l'argument `warn` est `FALSE`, alors la fonction ne retournera pas de
warning lors de l'évaluation.

Ici, l'argument `frequency_ts` est nécessaire car une date sous la forme
c(AAAA, PP), avec PP le nombre de période, ne désigne pas une date
absolue. Par exemple, c(2020L 5L) désigne mai 2020 pour une fréquence
mensuelle et le 1er trimestre 2021 pour une fréquence trimestrielle.

Selon le préfixe de la fonction :

- si le check réussi :

  - la fonction `assert_date_ts` retourne l'objet `x` de manière
    invisible;

  - la fonction `check_date_ts` retourne le booléen `TRUE`.

- si le check échoue :

  - la fonction `assert_date_ts` retourne un message d'erreur;

  - la fonction `check_date_ts` retourne une chaîne de caractère
    signalant le problème.

## Examples

``` r
# De bons formats de date
assert_date_ts(c(2020L, 8L), frequency_ts = 12L)
assert_date_ts(c(2020L, 2L), frequency_ts = 4L)
check_date_ts(2022L, frequency_ts = 12L)
#> [1] TRUE

# Format double --> génération d'un warning
assert_date_ts(c(2020., 4.0), frequency_ts = 4L)
#> Warning: Assertion on 'c(2020, 4)' failed: Must be of type 'integer', not 'double'.
assert_date_ts(2022., frequency_ts = 12L)
#> Warning: Assertion on '2022' failed: Must be of type 'integer', not 'double'.
check_date_ts(2022., frequency_ts = 12L)
#> Warning: Assertion on '2022' failed: Must be of type 'integer', not 'double'.
#> [1] TRUE

# Fréquence au format double --> génération d'un warning
assert_date_ts(c(2020L, 6L), frequency_ts = 4.0)
#> Warning: Assertion on 'frequency_ts' failed: Must be of type 'integer', not 'double'.
#> Warning: Assertion on 'period' failed: Element 1 is not <= 4.
assert_date_ts(c(2020L, 42L), frequency_ts = 12.0)
#> Warning: Assertion on 'frequency_ts' failed: Must be of type 'integer', not 'double'.
#> Warning: Assertion on 'period' failed: Element 1 is not <= 12.

# Dépassement la fréquence --> génération d'un warning
assert_date_ts(c(2020L, 6L), frequency_ts = 4L)
#> Warning: Assertion on 'period' failed: Element 1 is not <= 4.
assert_date_ts(c(2020L, 42L), frequency_ts = 12L)
#> Warning: Assertion on 'period' failed: Element 1 is not <= 12.
assert_date_ts(c(2020L, -4L), frequency_ts = 12L)
#> Warning: Assertion on 'period' failed: Element 1 is not >= 1.

# Avec des erreurs
check_date_ts(1:10, frequency_ts = 12L)
#> [1] "* 1:10 Must have length <= 2, but has length 10"
```
