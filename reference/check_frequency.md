# Vérifie la conformité d'une fréquence

Vérifie la conformité d'une fréquence

## Usage

``` r
check_frequency(x, .var.name = checkmate::vname(x), warn = TRUE)

assert_frequency(x, add = NULL, .var.name = checkmate::vname(x), warn = TRUE)
```

## Arguments

- x:

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

La fréquence d'une série temporelle est soit mensuelle (`12L` ou `12.0`)
soit trimestrielle (`4L` ou `4.0`). Les autres fréquences ne sont pas
acceptées. Cette fonction s'appuie essentiellement sur les fonctions
[`checkmate::check_numeric`](https://mllg.github.io/checkmate/reference/checkNumeric.html),
[`checkmate::check_int`](https://mllg.github.io/checkmate/reference/checkInt.html)
et
[`checkmate::check_choice`](https://mllg.github.io/checkmate/reference/checkChoice.html).
Il y a néanmoins une petite subtilité : on vérifie si l'objet `x` est de
type double ou integer. Dans le premier cas, on affichera un warning et
on corrigera l'objet au format integer pour les traitements ultérieurs.
En sortie, `x` est retourné de manière invisible. Si l'argument `warn`
est `FALSE`, alors la fonction ne retournera pas de warning lors de
l'évaluation.

Selon le préfixe de la fonction :

- si le check réussi :

  - la fonction `assert_frequency` retourne l'objet `x` de manière
    invisible;

  - la fonction `check_frequency` retourne le booléen `TRUE`.

- si le check échoue :

  - la fonction `assert_frequency` retourne un message d'erreur;

  - la fonction `check_frequency` retourne une chaîne de caractère
    signalant le problème.

## Examples

``` r
assert_frequency(4L)
assert_frequency(12L)

check_frequency(4L)
#> [1] TRUE
check_frequency(12L)
#> [1] TRUE

# Avec des erreurs,

check_frequency(Inf, warn = FALSE)
#> [1] "* Inf Must be finite"
check_frequency(1:10)
#> [1] "* 1:10 Must have length 1 \n * 1:10 Must be element of set {'4','12'}, but is not atomic scalar"
check_frequency(1L)
#> [1] "* 1L Must be element of set {'4','12'}, but is '1'"
```
