# Vérifie la conformité d'un objet ts

Les fonctions `assert_ts` et `check_ts` vérifient qu'un objet ts est
bien conforme.

## Usage

``` r
check_ts(x, .var.name = checkmate::vname(x), allow_mts = FALSE)

assert_ts(x, add = NULL, .var.name = checkmate::vname(x), allow_mts = FALSE)
```

## Arguments

- x:

  Un objet ts unidimensionnel

- .var.name:

  Nom de l'objet à vérifier pour afficher dans les messages

- allow_mts:

  Booleen. Est ce que les objects `mts` sont acceptés ?

- add:

  Collection pour stocker les messages d'erreurs (Default is NULL)

## Value

En sortie la fonction retourne l'objet `x` de manière invisible ou une
erreur.

## Details

Les fonctions du package date4ts sont faites pour fonctionner avec des
times-series de fréquence mensuelle ou trimestrielle et basées sur le
système des mois, trimestres et années classiques. On travaille avec des
données numériques (integer, double ou logical) mais les autres types
atomic sont acceptés également. On cherche donc à favoriser
l'utilisation de séries temporelles classiques utilisants des types
atomiques. Lorsque l'objet `x` en entrée est au mauvais format, une
erreur est généré.

Selon le préfixe de la fonction :

- si le check réussi :

  - la fonction `assert_ts` retourne l'objet `x` de manière invisible;

  - la fonction `check_ts` retourne le booléen `TRUE`.

- si le check échoue :

  - la fonction `assert_ts` retourne un message d'erreur;

  - la fonction `check_ts` retourne une chaîne de caractère signalant le
    problème.

## Examples

``` r
ts1 <- ts(1:100, start = 2010L, frequency = 12L)
ts2 <- ts(1:10, start = c(2020L, 4L), frequency = 4L)

assert_ts(ts1)
assert_ts(ts2)

check_ts(ts1)
#> [1] TRUE
check_ts(ts2)
#> [1] TRUE

# Exemples avec des erreurs

check_ts(1)
#> [1] "* 1 Must inherit from class 'ts', but has class 'numeric'"
check_ts(ts(1:10, start = 2010L, frequency = 2L))
#> [1] "* frequency_ts Must be element of set {'4','12'}, but is '2'"
check_ts(1:10)
#> [1] "* 1:10 Must inherit from class 'ts', but has class 'integer'"
```
