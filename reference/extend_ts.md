# Ajoute de nouvelles valeurs à un ts

La fonction `extend_ts` ajoute de nouvelles valeurs à un ts.

## Usage

``` r
extend_ts(
  series,
  replacement,
  date_ts_to = NULL,
  replace_na = TRUE,
  times = 1L,
  each = 1L
)
```

## Arguments

- series:

  un objet ts unidimensionnel conforme aux règles de assert_ts

- replacement:

  un vecteur de même type que le ts `series`

- date_ts_to:

  un vecteur numérique, de préférence `integer`, au format date_ts,
  c'est-à-dire `AAAA`, `c(AAAA, MM)` ou `c(AAAA, TT)`.

- replace_na:

  un booléen.

- times:

  un entier qui précise le nombre de fois où `replacement` doit être
  répété, le vecteur entier.

- each:

  un entier qui précise le nombre de fois où `replacement` doit être
  répété mais élément par élément.

## Value

En sortie, la fonction retourne une copie de l'objet `series` complété
avec le vecteur `replacement`.

## Details

`date_ts_to` désigne la date jusqu'à laquelle le remplacement
s'effectue. Par défault, cette valeur vaut `NULL`.

Si `replace_na` vaut `TRUE` alors le remplacement commence dès que
l'objet ne contient que des NA. Dans le cas contraire, le ts est étendu,
qu'il contienne des NA ou non à la fin. Si le vecteur `replacement` est
de taille un sous-multiple de la différence de période entre la date de
fin de `series` et `date_ts_to`, le vecteur `replacement` est répété
jusqu'à la date `date_ts_to`. Sinon une erreur est générée.

Les arguments `times` et `each` en sont utilisé que si `date_ts` est
manquant (non fourni par l'utilisateur). Si tel est le cas, ils se
comporte comme si `replacement` devenait
`rep(replacement, times = times, each = each)`.

## Examples

``` r
ts1 <- ts(
    data = c(rep(NA_integer_, 3L), 1L:10L, rep(NA_integer_, 3L)),
    start = 2020,
    frequency = 12
)
x <- rep(3L, 2L)

extend_ts(series = ts1, replacement = x)
#>      Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec
#> 2020  NA  NA  NA   1   2   3   4   5   6   7   8   9
#> 2021  10   3   3  NA                                
extend_ts(series = ts1, replacement = x, replace_na = FALSE)
#>      Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec
#> 2020  NA  NA  NA   1   2   3   4   5   6   7   8   9
#> 2021  10   3   3  NA                                
extend_ts(series = ts1, replacement = x,
          date_ts_to = c(2021L, 7L), replace_na = TRUE)
#> Warning: extending time series when replacing values
#>      Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec
#> 2020  NA  NA  NA   1   2   3   4   5   6   7   8   9
#> 2021  10   3   3   3   3   3   3                    
```
