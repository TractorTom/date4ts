
# Why ?

Because…

Un petit document qui expliquent certains choix de développement faits.

## Pourquoi ne pas utiliser `checkmate::makeAssertionFunction()`

J’aurai aimé juste construire les fonctions `check` et créer les
`assert` avec. Seulement, le nom des objets est mal pris en compte pour
les `check`s.

Un petit exemple illustratif :

``` r
library("checkmate")
```

    ## Warning: package 'checkmate' was built under R version 4.3.1

``` r
# Un check un peu random fait à la main
check_inf_2 <- function(x, .var.name = checkmate::vname(x)) {
    cond1 <- check_integerish(x, lower = 1, upper = 2)
    return(ifelse(isTRUE(cond1), TRUE, paste("*", .var.name, ":", cond1)))
}

assert_inf_2 <- makeAssertionFunction(check_inf_2)

print(check_inf_2(1, .var.name = "Nom bizarre"))
```

    ## [1] TRUE

``` r
print(check_inf_2(3, .var.name = "Nom bizarre"))
```

    ## [1] "* Nom bizarre : Element 1 is not <= 2"

``` r
assert_inf_2(1, .var.name = "Nom bizarre")
```

    ## Error in assert_inf_2(1, .var.name = "Nom bizarre"): argument 2 matches multiple formal arguments

``` r
assert_inf_2(3, .var.name = "Nom bizarre")
```

    ## Error in assert_inf_2(3, .var.name = "Nom bizarre"): argument 2 matches multiple formal arguments

On remarque directement qu’on ne peut pas créer de `check` avec
`.var.name` puis créer une `assert` à partir de ça.

Un autre possibilité serait de ne pas utiliser de `.var.name` pour les
`check`s mais :

``` r
# Un check sans .var.name (donc "normal")
check_inf_2 <- function(x) {
    return(check_integerish(x, lower = 1, upper = 2))
}

check_objet <- function(x) {
    cond1 <- check_inf_2(x)
    cond2 <- check_inf_2(length(x))
    
    output <- c()
    verif <- TRUE
    
    if (!isTRUE(cond1)) {
        output <- c(output, cond1)
        verif <- FALSE
    }
    
    if (!isTRUE(cond2)) {
        output <- c(output, cond2)
        verif <- FALSE
    }
    
    output <- paste("*", output, collapse = "\n")
    
    return(ifelse(verif, verif, output))
}

assert_objet <- makeAssertionFunction(check_objet)
```

``` r
check_objet(4)
```

    ## [1] "* Element 1 is not <= 2"

``` r
check_objet(c(1, 1, 2))
```

    ## [1] "* Element 1 is not <= 2"

Remarque : La fonction de `check` marche bien mais uniquement parce
qu’on l’a doté de `.var.name`.

``` r
assert_objet(4)
```

    ## Error in eval(expr, envir, enclos): Assertion on '4' failed: * Element 1 is not <= 2.

``` r
assert_objet(c(1, 1, 2))
```

    ## Error in eval(expr, envir, enclos): Assertion on 'c(1, 1, 2)' failed: * Element 1 is not <= 2.

Mais on remarque alors que le message d’erreur n’est pas très clair…

Pire :

``` r
assert_objet(c(4, 1, 2))
```

    ## Error in eval(expr, envir, enclos): Assertion on 'c(4, 1, 2)' failed: * Element 1 is not <= 2
    ## * Element 1 is not <= 2.

Dernière solution : faire des fonctions mixtes :

``` r
# Un check avec .var.name
check_inf_2 <- function(x, .var.name = checkmate::vname(x)) {
    cond1 <- check_integerish(x, lower = 1, upper = 2)
    return(ifelse(isTRUE(cond1), TRUE, paste("*", .var.name, ":", cond1)))
}

# Un check sns
check_objet <- function(x) {
    cond1 <- check_inf_2(x, .var.name = "x")
    cond2 <- check_inf_2(length(x), .var.name = "length(x)")
    
    output <- c()
    verif <- TRUE
    
    if (!isTRUE(cond1)) {
        output <- c(output, cond1)
        verif <- FALSE
    }
    
    if (!isTRUE(cond2)) {
        output <- c(output, cond2)
        verif <- FALSE
    }
    
    output <- paste(output, collapse = "\n")
    
    return(ifelse(verif, verif, output))
}

assert_objet <- makeAssertionFunction(check_objet)
```

``` r
print(check_objet(c(1, 1, 2)))
```

    ## [1] "* length(x) : Element 1 is not <= 2"

``` r
assert_objet(c(1, 1, 2))
```

    ## Error in eval(expr, envir, enclos): Assertion on 'c(1, 1, 2)' failed: * length(x) : Element 1 is not <= 2.

``` r
assert_objet(c(4, 1, 2))
```

    ## Error in eval(expr, envir, enclos): Assertion on 'c(4, 1, 2)' failed: * x : Element 1 is not <= 2
    ## * length(x) : Element 1 is not <= 2.

Mais cette solution n’est pas viable à long terme car on aimerait quand
même pouvoir transformer en `assert` nos `check` (pour `check_inf_2`) et
peut être que `check_objet` sera aussi appelé par une autre fonction et
ne fournira alors à son tour aucune info sur les objets scannés.
