# Retire une année à une date

La fonction `substr_year` retire `n` annnée(s) à une date.

## Usage

``` r
substr_year(date, n = 1L)
```

## Arguments

- date:

  un objet de type Date

- n:

  un entier

## Value

En sortie, la fonction retourne un objet de type Date (atomic) de
longueur 1.

## Examples

``` r
substr_year(as.Date("2000-02-29"), n = 1L)
#> [1] "1999-02-28"
substr_year(as.Date("2000-02-29"), n = 3L)
#> [1] "1997-02-28"
substr_year(as.Date("2000-02-29"), n = 4L)
#> [1] "1996-02-29"
substr_year(as.Date("2000-02-29"), n = 16L)
#> [1] "1984-02-29"

substr_year(as.Date("2023-01-25"), n = 10L)
#> [1] "2013-01-25"
substr_year(as.Date("2022-11-01"), n = 3L)
#> [1] "2019-11-01"
```
