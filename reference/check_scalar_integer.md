# Vérifie la conformité d'un entier scalaire

Vérifie la conformité d'un entier scalaire

## Usage

``` r
check_scalar_integer(x, warn = TRUE)

assert_scalar_integer(
  x,
  add = NULL,
  .var.name = checkmate::vname(x),
  warn = TRUE
)
```

## Arguments

- x:

  un entier relatif (positif, négatif ou nul)

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

On vérifie que l'objet `x` en entrée est bien un entier. Cette fonction
s'appuie essentiellement sur la fonction
[`checkmate::assert_int`](https://mllg.github.io/checkmate/reference/checkInt.html).
Il y a néanmoins une petite subtilité : on vérifie si l'objet `x` est de
type double ou integer. Si l'objet est de type double (et non integer),
la fonction retournera aussi un warning. Dans le premier cas, on
affichera un warning et on corrigera l'objet au format integer pour les
traitements ultérieurs. En sortie, `x` est retourné de manière
invisible. Si l'argument `warn` vaut `FALSE`, alors la fonction ne
retournera pas de warning lors de l'évaluation.

Selon le préfixe de la fonction :

- si le check réussi :

  - la fonction `assert_scalar_integer` retourne l'objet `x` de manière
    invisible;

  - la fonction `check_scalar_integer` retourne le booléen `TRUE`.

- si le check échoue :

  - la fonction `assert_scalar_integer` retourne un message d'erreur;

  - la fonction `check_scalar_integer` retourne une chaîne de caractère
    signalant le problème.

## See also

[`check_scalar_natural()`](https://tractortom.github.io/date4ts/reference/check_scalar_natural.md),
[`assert_scalar_natural()`](https://tractortom.github.io/date4ts/reference/check_scalar_natural.md)

## Examples

``` r
assert_scalar_integer(1L)
assert_scalar_integer(100L)
assert_scalar_integer(-4L)
assert_scalar_integer(0L)

check_scalar_integer(1L)
#> [1] TRUE
check_scalar_integer(100L)
#> [1] TRUE
check_scalar_integer(-4L)
#> [1] TRUE
check_scalar_integer(0L)
#> [1] TRUE

# Avec des erreurs,

check_scalar_integer(Inf)
#> [1] "* x Must be of type 'single integerish value', not 'double'"
check_scalar_integer(1:10)
#> [1] "* x Must have length 1"
check_scalar_integer(pi)
#> [1] "* x Must be of type 'single integerish value', not 'double'"
check_scalar_integer(2.)
#> Warning: Assertion on 'x' failed: Must be of type 'integer', not 'double'.
#> [1] TRUE
```
