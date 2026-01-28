# Supprime les NA aux bords

La fonction `na_trim` supprime les NA en début et en fin de période.

## Usage

``` r
na_trim(series, sides = c("both", "left", "right"))
```

## Arguments

- series:

  un objet ts unidimensionnel conforme aux règles de assert_ts

- sides:

  une chaine de caractere qui spécifie quelle NA doivent être retirés
  (au début et à la fin ("both"), juste au début ("left") ou juste à la
  fin ("right"))

## Value

En sortie, la fonction retourne une copie de l'objet `series` corrigée
des NA et début et fin de série.

## Details

L'objet retourné commence et finis par des valeurs non manquantes.

## Examples

``` r
ts1 <- ts(c(rep(NA, 3L), 1:10, rep(NA, 3L)), start = 2020, frequency = 12L)
ts2 <- ts(c(1:10, rep(NA, 3L)), start = c(2023, 2), frequency = 4L)
ts3 <- ts(c(rep(NA, 3L), 1:10), start = 2000, frequency = 12L)

na_trim(ts1)
#>      Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec
#> 2020               1   2   3   4   5   6   7   8   9
#> 2021  10                                            
na_trim(ts2)
#>      Qtr1 Qtr2 Qtr3 Qtr4
#> 2023         1    2    3
#> 2024    4    5    6    7
#> 2025    8    9   10     
na_trim(ts3)
#>      Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec
#> 2000               1   2   3   4   5   6   7   8   9
#> 2001  10                                            
```
