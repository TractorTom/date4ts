# Vérifie la conformité d'une expression

Vérifie la conformité d'une expression

## Usage

``` r
check_expression(expr)

assert_expression(expr)
```

## Arguments

- expr:

  une expression à évaluer

## Value

En sortie la fonction retourne l'objet `x` (le résultat de l'évaluation
de l'expression `expr`) de manière invisible ou une erreur.

## Details

La fonction évalue l'expression `expr`. Le check vérifie si la fonction
génère une erreur ou un warning. Si elle ne génère aucun message
particulier, on retourne alors l'objet `x` (le résultat de l'évaluation
de l'expression `expr`), sans erreur.

Selon le préfixe de la fonction :

- si le check réussi :

  - la fonction `assert_expression` retourne l'objet `x` de manière
    invisible;

  - la fonction `check_expression` retourne le booléen `TRUE`.

- si le check échoue :

  - la fonction `assert_expression` retourne un message d'erreur;

  - la fonction `check_expression` retourne la chaîne de caractère
    "Invalid expression".

## Examples

``` r
assert_expression(expr = {2 + 2})
assert_expression(expr = {is.integer(1L)})
try(assert_expression(expr = {log("a")}), silent = TRUE)

check_expression(expr = {2 + 2})
#> [1] TRUE
check_expression(expr = {is.integer(1L)})
#> [1] TRUE
check_expression(expr = {log("a")})
#> [1] "Invalid expression : {"              "Invalid expression :     log(\"a\")"
#> [3] "Invalid expression : }"             
```
