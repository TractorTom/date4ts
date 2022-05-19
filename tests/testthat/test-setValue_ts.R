
set.seed(2023L)

create_random_type <- function(type, len){
    if (type == "char") return(strsplit(intToUtf8(sample(c(1L:55295L, 57344L:1114111L), size = len, replace = TRUE)), "")[[1]])
    if (type == "int") return(sample(-20000000L:20000000L, size = len, replace = TRUE))
    if (type == "num") return(runif(n = len, min = -10000L, max = 10000L))
    if (type == "logical") return(sample(x = c(TRUE, FALSE), size = len, replace = TRUE))
    if (type == "complex") return(complex(real = runif(n = len, min = -10000L, max = 10000),
                                          imaginary = runif(n = len, min = -10000L, max = 10000L)))
    if (type == "raw") return(sample(x = as.raw(0L:255L), size = len, replace = TRUE))
    stop("Le type n'est pas reconnu.")
}

liste_type <- c("int")#, "char", "num", "logical", "complex", "raw")
liste_len <- c(1L, 4L:6L)#, 10L, 10000L)
liste_lag <- c(-1, 0, 1)#c(-10000L, -5L, -1L, 0L, 1L, 5L, 10000L)
liste_frequence <- c(4, 12)
weird_frequency <- c(1, 2, 7, 0.1, 1/3, 3.5, 365.25, pi)
liste_start <- list(c(2020, -1), c(2020, 0), c(2020, 4), c(2020, 5), c(2020, 12), c(2020, 13))

# Tests de résultat avec start vecteur d'entiers -------------------------------

for (typeA in liste_type){
    for (frequenceA in liste_frequence){
        for (startA in liste_start){
            for (lenA in liste_len){
                A_content <- create_random_type(type = typeA, len = lenA)
                ts_A <-  ts(A_content, start = startA, frequency = frequenceA)
                for (lagB in liste_lag){
                    for (lenB in liste_len){

                        test_name <- paste("expected result with",
                                           "type", typeA,
                                           "frequency", frequenceA,
                                           "start", deparse(startA),
                                           "lenA", lenA,
                                           "lagB", lagB,
                                           "lenB", lenB)

                        testthat::test_that(test_name, {

                            valueB <- create_random_type(type = typeA, len = lenB)
                            startB <- ts4conj::getTimeUnits(startA, frequency = frequenceA) + lagB / frequenceA
                            ts_B <- ts(valueB, start = startB, frequency = frequenceA)

                            #Cas où lagB >= 0 (start = startA)
                            if (lagB > 0){
                                if (lagB > lenA) { #Cas 4

                                    ts_ResAB1 <- ts(c(A_content, rep(NA, lagB - lenA), valueB), start = startA, frequency = frequenceA)
                                    testthat::expect_warning({resAB <- setValue_ts(ts_A, date = startB, value = valueB)},
                                                             regexp = "extending time series when replacing values")
                                    testthat::expect_warning({ts_ResAB2 <- combine2ts(ts_A, ts_B)},
                                                             regexp = "extending time series when replacing values")


                                } else if (lagB + lenB < lenA) { #Cas 6
                                    ts_ResAB1 <- ts(c(A_content[1:lagB], valueB, A_content[(lagB + lenB + 1):lenA]), start = startA, frequency = frequenceA)
                                    ts_ResAB2 <- combine2ts(ts_A, ts_B)
                                    resAB <- setValue_ts(ts_A, date = startB, value = valueB)
                                } else if (lagB + lenB == lenA) { #Autres cas
                                    ts_ResAB1 <- ts(c(A_content[1:lagB], valueB), start = startA, frequency = frequenceA)
                                    ts_ResAB2 <- combine2ts(ts_A, ts_B)
                                    resAB <- setValue_ts(ts_A, date = startB, value = valueB)
                                } else { #Cas1

                                    ts_ResAB1 <- ts(c(A_content[1:lagB], valueB), start = startA, frequency = frequenceA)
                                    testthat::expect_warning({ts_ResAB2 <- combine2ts(ts_A, ts_B)},
                                                             regexp = "extending time series when replacing values")
                                    testthat::expect_warning({resAB <- setValue_ts(ts_A, date = startB, value = valueB)},
                                                             regexp = "extending time series when replacing values")

                                }

                            } else if(lagB < 0) { #Cas où start = strartB < startA

                                if (lagB + lenB < 0) { #Cas 3
                                    ts_ResAB1 <- ts(c(valueB, rep(NA, -lagB - lenB), A_content), start = startB, frequency = frequenceA)
                                } else if (lagB + lenB >= lenA) { #Cas 5
                                    ts_ResAB1 <- ts(valueB, start = startB, frequency = frequenceA)
                                } else { #Cas 2
                                    ts_ResAB1 <- ts(c(valueB, A_content[(lagB + lenB + 1):lenA]), start = startB, frequency = frequenceA)
                                }

                                testthat::expect_warning({ts_ResAB2 <- combine2ts(ts_A, ts_B)},
                                                         regexp = "extending time series when replacing values")
                                testthat::expect_warning({resAB <- setValue_ts(ts_A, date = startB, value = valueB)},
                                                         regexp = "extending time series when replacing values")

                            } else { #Cas où lag = 0
                                if (lenB < lenA){
                                    ts_ResAB1 <- ts(c(valueB, A_content[(lenB + 1):lenA]), start = startA, frequency = frequenceA)
                                    ts_ResAB2 <- combine2ts(ts_A, ts_B)
                                    resAB <- setValue_ts(ts_A, date = startB, value = valueB)
                                } else if (lenB == lenA){
                                    ts_ResAB1 <- ts(valueB, start = startA, frequency = frequenceA)
                                    ts_ResAB2 <- combine2ts(ts_A, ts_B)
                                    resAB <- setValue_ts(ts_A, date = startB, value = valueB)
                                } else {
                                    ts_ResAB1 <- ts(valueB, start = startA, frequency = frequenceA)
                                    testthat::expect_warning({ts_ResAB2 <- combine2ts(ts_A, ts_B)},
                                                             regexp = "extending time series when replacing values")
                                    testthat::expect_warning({resAB <- setValue_ts(ts_A, date = startB, value = valueB)},
                                                             regexp = "extending time series when replacing values")

                                }
                            }

                            testthat::expect_equal(resAB,ts_ResAB1)
                            testthat::expect_equal(resAB,ts_ResAB2)
                        })

                    }
                }
            }
        }
    }
}


# Tests sur les erreurs de mts --------------------------------------------

testthat::test_that("Several dimensions are not allowed", {
    A_content <- create_random_type(type = "int", len = 100L)
    B_content <- sapply(1L:5L, function(i) create_random_type(type = "int", len = 100L))
    ts_A <- ts(A_content,  start = 2015L, frequency = 12L)
    mts_B <- ts(B_content, start = 2001L, frequency = 12L)

    testthat::expect_error(combine2ts(ts_A, mts_B), regexp = "Les objets a et b doivent être des ts unidimensionels.")
    testthat::expect_error(combine2ts(mts_B, ts_A), regexp = "Les objets a et b doivent être des ts unidimensionels.")
})

# Tests sur les erreurs d'input --------------------------------------------

object_bank_R <- fuzzr::test_all()

testthat::test_that("miscellaneous input are not allowed", {
    A_content <- create_random_type(type = "int", len = 100L)
    ts_A <- ts(A_content,  start = 2010L, frequency = 4L)

    for (objA in object_bank_R){
        testthat::expect_error(combine2ts(ts_A, objA), regexp = "Les objets a et b doivent être des ts unidimensionels.")
        testthat::expect_error(combine2ts(objA, ts_A), regexp = "Les objets a et b doivent être des ts unidimensionels.")
        for (objB in object_bank_R){
            testthat::expect_error(combine2ts(objA, objB), regexp = "Les objets a et b doivent être des ts unidimensionels.")
        }
    }
})

# Tests sur les erreurs de type d'objets --------------------------------------------

ts_type <- list(
    ts_integer =   create_random_type(type = "int",     len = 100L) |> ts(start = 2001L, frequency = 12L),
    ts_numeric =   create_random_type(type = "num",     len = 101L) |> ts(start = 2002L, frequency = 12L),
    ts_complex =   create_random_type(type = "complex", len = 102L) |> ts(start = 2003L, frequency = 12L),
    ts_character = create_random_type(type = "char",    len = 103L) |> ts(start = 2004L, frequency = 12L),
    ts_logical =   create_random_type(type = "logical", len = 104L) |> ts(start = 2005L, frequency = 12L),
    ts_raw =       create_random_type(type = "raw",     len = 105L) |> ts(start = 2006L, frequency = 12L)
)

testthat::test_that("different input type are not allowed", {
    for (index_A in seq_along(ts_type)){
        objA <- ts_type[[index_A]]
        for (index_B in seq_along(ts_type)){
            objB <- ts_type[[index_B]]
            if (index_A != index_B) testthat::expect_error(combine2ts(objA, objB), regexp = "Les objets a et b doivent être de même type.")
        }
    }
})

# Tests sur les erreurs de temporalité --------------------------------------------

testthat::test_that("arguments have same frequency", {
    for (freq_A in liste_frequence){
        objA <- ts(create_random_type("num", len = 50L), start = 2001L, frequency = freq_A)
        for (freq_B in liste_frequence){
            objB <- ts(create_random_type("num", len = 50L), start = 2001L, frequency = freq_B)
            if (freq_A != freq_B) testthat::expect_error(combine2ts(objA, objB), regexp = "Les objets a et b doivent avoir la même fréquence.")
        }
    }
})

testthat::test_that("arguments are temporally consistent", {

    ts_A <- create_random_type(type = "num", len = 100L) |> ts(start = 2015L, frequency = 12L)
    ts_B <- create_random_type(type = "num", len = 100L) |> ts(start = 2004 + 1/7, frequency = 12L)
    testthat::expect_error(combine2ts(ts_A, ts_B), regexp = "Les objets a et b doivent être cohérents temporellement.")
    testthat::expect_error(combine2ts(ts_B, ts_A), regexp = "Les objets a et b doivent être cohérents temporellement.")

    ts_A <- create_random_type(type = "num", len = 20L) |> ts(start = 2015L, frequency = 4)
    ts_B <- create_random_type(type = "num", len = 20L) |> ts(start = 2016 + 5/12, frequency = 4)
    testthat::expect_error(combine2ts(ts_A, ts_B), regexp = "Les objets a et b doivent être cohérents temporellement.")
    testthat::expect_error(combine2ts(ts_B, ts_A), regexp = "Les objets a et b doivent être cohérents temporellement.")

})

