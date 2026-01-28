# Vérifie la conformité d'un entier naturel

Le but de cett fonction est de tester si une variable x est un nombre
naturel strictement positif.

## Usage

``` r
check_scalar_natural(x, warn = TRUE)

assert_scalar_natural(
  x,
  add = NULL,
  .var.name = checkmate::vname(x),
  warn = TRUE
)
```

## Arguments

- x:

  un entier naturel strictement positif

- warn:

  un booleen

- add:

  Collection pour stocker les messages d'erreurs (Default is NULL)

- .var.name:

  Nom de l'objet à vérifier pour afficher dans les messages

## Value

En sortie la fonction retourne l'objet `x` de manière invisible ou une
erreur.

## Details

Cette fonction s'appuie essentiellement sur la fonction
[`checkmate::assert_count`](https://mllg.github.io/checkmate/reference/checkCount.html).
Il y a néanmoins une petite subtilité : on vérifie si l'objet `x` est de
type double ou integer. Dans le premier cas, on affichera un warning et
on corrigera l'objet au format integer pour les traitements ultérieurs.
En sortie, `x` est retourné de manière invisible. Si l'argument `warn`
est `FALSE`, alors la fonction ne retournera pas de warning lors de
l'évaluation.

Selon le préfixe de la fonction :

- si le check réussi :

  - la fonction `assert_scalar_natural` retourne l'objet `x` de manière
    invisible;

  - la fonction `check_scalar_natural` retourne le booléen `TRUE`.

- si le check échoue :

  - la fonction `assert_scalar_natural` retourne un message d'erreur;

  - la fonction `check_scalar_natural` retourne une chaîne de caractère
    signalant le problème.

## See also

[`check_scalar_integer()`](https://tractortom.github.io/date4ts/reference/check_scalar_integer.md),
[`assert_scalar_integer()`](https://tractortom.github.io/date4ts/reference/check_scalar_integer.md)

## Examples

``` r
# Avec des entier integer
assert_scalar_natural(1L)
assert_scalar_natural(100L)

# Avec des entiers double
assert_scalar_natural(2.)
#> Warning: Assertion on '2' failed: Must be of type 'integer', not 'double'.
assert_scalar_natural(457)
#> Warning: Assertion on '457' failed: Must be of type 'integer', not 'double'.
```
