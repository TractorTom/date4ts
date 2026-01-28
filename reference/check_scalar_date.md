# Vérifie la conformité d'une date scalaire

Vérifie la conformité d'une date scalaire

## Usage

``` r
check_scalar_date(x)

assert_scalar_date(x, add = NULL, .var.name = checkmate::vname(x))
```

## Arguments

- x:

  un objet de type `Date`.

- add:

  Collection pour stocker les messages d'erreurs (Default is NULL)

- .var.name:

  Nom de l'objet à vérifier pour afficher dans les messages

## Value

En sortie la fonction retourne l'objet `x` de manière invisible ou une
erreur.

## Details

On vérifie que l'objet `x` en entrée est bien au format `Date` et qu'il
s'agit d'un scalaire (vecteur de taille 1). Cette fonction s'appuie
essentiellement sur la fonction
[`checkmate::assert_date`](https://mllg.github.io/checkmate/reference/checkDate.html).

Selon le préfixe de la fonction :

- si le check réussi :

  - la fonction `assert_scalar_date` retourne l'objet `x` de manière
    invisible;

  - la fonction `check_scalar_date` retourne le booléen `TRUE`.

- si le check échoue :

  - la fonction `assert_scalar_date` retourne un message d'erreur;

  - la fonction `check_scalar_date` retourne la chaîne de caractère
    correspondante à l'erreur du check.

## Examples

``` r
assert_scalar_date(as.Date("2018-01-24"))
assert_scalar_date(as.Date("2000-02-29"))
assert_scalar_date(Sys.Date())

check_scalar_date(as.Date("2018-01-24"))
#> [1] "TRUE"
check_scalar_date(as.Date("2000-02-29"))
#> [1] "TRUE"
check_scalar_date(Sys.Date())
#> [1] "TRUE"

# Avec des erreurs

check_scalar_date(2L)
#> [1] "Must be of class 'Date', not 'integer'"
check_scalar_date(seq(from = as.Date("2000-01-01"), to = Sys.Date(), by =
"year"))
#> [1] "Must have length 1"
```
