# Vérifie la conformité d'un objet TimeUnits

La fonction `assert_timeunits` vérifie qu'un objet est un TimeUnits.

## Usage

``` r
check_timeunits(x, frequency_ts, .var.name = checkmate::vname(x))

assert_timeunits(x, frequency_ts, add = NULL, .var.name = checkmate::vname(x))
```

## Arguments

- x:

  un numérique qui représente le time units de

- frequency_ts:

  un entier qui vaut `4L` (ou `4.0`) pour les séries trimestrielles et
  `12L` (ou `12.0`) pour les séries mensuelles.

- .var.name:

  Nom de l'objet à vérifier pour afficher dans les messages

- add:

  Collection pour stocker les messages d'erreurs (Default is NULL)

## Value

En sortie la fonction retourne l'objet `x` de manière invisible ou une
erreur.

## Details

Un objet de type TimeUnits est un numérique qui désigne l'année et la
période en cours avec ses décimales. Ainsi pour une série temporelle
mensuelle, `2020.5` représente la moitié de l'année donc juillet 2020 et
s'écrit `c(2020L, 7L)` au format date_ts.

Selon le préfixe de la fonction :

- si le check réussi :

  - la fonction `assert_timeunits` retourne l'objet `x` de manière
    invisible;

  - la fonction `check_timeunits` retourne le booléen `TRUE`.

- si le check échoue :

  - la fonction `assert_timeunits` retourne un message d'erreur;

  - la fonction `check_timeunits` retourne une chaîne de caractère
    signalant le problème.

## Examples

``` r
assert_timeunits(2020.5, frequency_ts = 12L)
assert_timeunits(2020.5, frequency_ts = 4L)
assert_timeunits(2023., frequency_ts = 12L)

assert_timeunits(2000. + 5. / 12.0, frequency_ts = 12L)
assert_timeunits(2015. + 3. / 4.0, frequency_ts = 4L)

check_timeunits(2020.5, frequency_ts = 12L)
#> [1] TRUE
check_timeunits(2015. + 3. / 4.0, frequency_ts = 4L)
#> [1] TRUE

# Avec erreur

check_timeunits(list(1.), frequency_ts = 12L)
#> [1] "* list(1) Must be of type 'number', not 'list'"
check_timeunits(2000., frequency_ts = 1L)
#> [1] "* frequency_ts Must be element of set {'4','12'}, but is '1'"
```
