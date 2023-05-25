
# Initialisation ---------------------------------------------------------------

set.seed(2022L)

create_random_type <- function(type, len = NULL) {
    if (is.null(len)) len <- sample(1L:1000L, size = 1)
    if (type == "character") return(strsplit(intToUtf8(sample(c(1L:55295L, 57344L:1114111L), size = len, replace = TRUE)), "")[[1]])
    if (type == "integer") return(sample(-20000000L:20000000L, size = len, replace = TRUE))
    if (type == "double") return(runif(n = len, min = -10000L, max = 10000L))
    if (type == "logical") return(sample(x = c(TRUE, FALSE), size = len, replace = TRUE))
    if (type == "complex") return(complex(real = runif(n = len, min = -10000L, max = 10000),
                                          imaginary = runif(n = len, min = -10000L, max = 10000L)))
    if (type == "raw") return(sample(x = as.raw(0L:255L), size = len, replace = TRUE))
    if (type == "Date") return(sample(x = seq(as.Date('1950/01/01'), as.Date('2022/01/01'), by = "day"), size = len, replace = T))
    stop("Le type n'est pas reconnu.")
}

create_random_date <- function() {
    if (runif(1, 0, 1) > .5) return(sample(1950L:2022L, size = 1L))
    return(c(sample(1950L:2022L, size = 1L),
             sample(-20L:20L, size = 1L)))
}

create_random_ts <- function(type, len = NULL, start = NULL, frequency = NULL) {
    if (is.null(len)) len <- sample(1L:1000L, size = 1)
    if (is.null(frequency)) frequency <- sample(c(4L, 12L), size = 1)
    if (is.null(start)) start <- create_random_date()

    content <- create_random_type(type, len)

    return(ts(content, start = start, frequency = frequency))
}

liste_type <- c("integer", "character", "double", "logical", "complex", "raw", "Date")
liste_len <- c(0L:5L, 10000L)
liste_frequence <- c(4L, 12L)
weird_frequency <- list(1L, 2, 7, .1, 1/3, 3.5, 365.25, pi)
liste_start <- list(c(2020L, -1L), c(2020L, 0L), c(2020L, 4L), c(2020L, 5L), c(2020L, 12L), c(2020L, 13L), 2019L)
object_bank_R <- fuzzr::test_all()


# Tests de résultat ------------------------------------------------------------

for (typeA in liste_type) {
    for (frequenceA in liste_frequence) {
        for (startA in liste_start) {
            for (lenA in liste_len[-1L]) {
                A_content <- create_random_type(type = typeA, len = lenA)
                ts_A <-  ts(A_content, start = startA, frequency = frequenceA)
                for (param1 in liste_len) {
                    for (param2 in liste_len) {

                        test_name <- paste0(
                            "expected result with ",
                            "\ntypeA = '", typeA,
                            "'\nfrequenceA = ", frequenceA,
                            "\nstartA = ", deparse(startA),
                            "\nlenA = ", lenA,
                            "\nparam1 = ", param1,
                            "\nparam2 = ", param2)


                        testthat::test_that(desc = test_name, {

                            #Cas 1
                            if (param1 < lenA & param1 + param2 > 0L) {
                                B1_contents <- create_random_type(type = typeA, len = param1 + param2)
                                ts_B1 <- ts(B1_contents,  start = ts4conj::getTimeUnits(end(ts_A) |> as.integer(), frequency = frequenceA) - (param1 - 1L) / frequenceA, frequency = frequenceA)

                                ts_ResAB1 <- ts(c(A_content[1L:(lenA - param1)], B1_contents),                  start = startA, frequency = frequenceA)
                                if (param2 == 0L) {
                                    ts_ResB1A <- ts_A
                                } else {
                                    ts_ResB1A <- ts(c(A_content, B1_contents[(param1 + 1L):(param1 + param2)]), start = startA, frequency = frequenceA)
                                }

                                if (param2 > 0L) {
                                    testthat::expect_warning({ resAB1 <- combine2ts(ts_A,ts_B1)},
                                                             regexp = "extending time series when replacing values")
                                } else {
                                    resAB1 <- combine2ts(ts_A,ts_B1)
                                }
                                testthat::expect_warning({ resB1A <- combine2ts(ts_B1,ts_A)},
                                                         regexp = "extending time series when replacing values")
                                if (typeA != "Date") {
                                    testthat::expect_type(resAB1, typeA)
                                    testthat::expect_type(resB1A, typeA)
                                }

                                testthat::expect_equal(resAB1,ts_ResAB1)
                                testthat::expect_equal(resB1A,ts_ResB1A)
                            }


                            #Cas 2
                            if (param2 < lenA & param1 + param2 > 0L) {
                                B2_content <- create_random_type(type = typeA, len = param1 + param2)
                                ts_B2 <- ts(B2_content,  start = ts4conj::getTimeUnits(startA, frequency = frequenceA) - param1 / frequenceA, frequency = frequenceA)

                                ts_ResAB2 <- ts(c(B2_content, A_content[(param2 + 1L):lenA]), start = start(ts_B2), frequency = frequenceA)
                                if (param1 == 0L) {
                                    ts_ResB2A <- ts_A
                                } else {
                                    ts_ResB2A <- ts(c(B2_content[1:param1], A_content),       start = start(ts_B2), frequency = frequenceA)
                                }

                                if (param1 > 0L) {
                                    testthat::expect_warning({ resAB2 <- combine2ts(ts_A,ts_B2)},
                                                             regexp = "extending time series when replacing values")
                                } else {
                                    resAB2 <- combine2ts(ts_A,ts_B2)
                                }
                                testthat::expect_warning({ resB2A <- combine2ts(ts_B2,ts_A)},
                                                         regexp = "extending time series when replacing values")

                                if (typeA != "Date") {
                                    testthat::expect_type(resAB2, typeA)
                                    testthat::expect_type(resB2A, typeA)
                                }

                                testthat::expect_equal(resAB2,ts_ResAB2)
                                testthat::expect_equal(resB2A,ts_ResB2A)
                            }


                            #Cas 3
                            if (param1 > 0L) {
                                B3_content <- create_random_type(type = typeA, len = param1)
                                ts_B3 <- ts(B3_content,  start = ts4conj::getTimeUnits(startA, frequency = frequenceA) - (param1 + param2) / frequenceA, frequency = frequenceA)

                                if (typeA == "raw") {
                                    ts_ResAB3 <- ts(c(B3_content, rep(as.raw(0L), param2), A_content), start = start(ts_B3), frequency = frequenceA)
                                    ts_ResB3A <- ts(c(B3_content, rep(as.raw(0L), param2), A_content), start = start(ts_B3), frequency = frequenceA)
                                    if (param2 > 0L) {
                                        testthat::expect_warning({
                                            testthat::expect_warning({ resAB3 <- combine2ts(ts_A,ts_B3)},
                                                                     regexp = "extending time series when replacing values")},
                                            regexp = "out-of-range values treated as 0 in coercion to raw")
                                        testthat::expect_warning({
                                            testthat::expect_warning({ resB3A <- combine2ts(ts_B3,ts_A)},
                                                                     regexp = "extending time series when replacing values")},
                                            regexp = "out-of-range values treated as 0 in coercion to raw")
                                    } else {
                                        testthat::expect_warning({ resAB3 <- combine2ts(ts_A,ts_B3)},
                                                                 regexp = "extending time series when replacing values")
                                        testthat::expect_warning({ resB3A <- combine2ts(ts_B3,ts_A)},
                                                                 regexp = "extending time series when replacing values")
                                    }
                                } else {
                                    ts_ResAB3 <- ts(c(B3_content, rep(NA, param2), A_content), start = start(ts_B3), frequency = frequenceA)
                                    ts_ResB3A <- ts(c(B3_content, rep(NA, param2), A_content), start = start(ts_B3), frequency = frequenceA)
                                    testthat::expect_warning({ resAB3 <- combine2ts(ts_A,ts_B3)},
                                                             regexp = "extending time series when replacing values")
                                    testthat::expect_warning({ resB3A <- combine2ts(ts_B3,ts_A)},
                                                             regexp = "extending time series when replacing values")
                                }

                                if (typeA != "Date") {
                                    testthat::expect_type(resAB3, typeA)
                                    testthat::expect_type(resB3A, typeA)
                                }

                                testthat::expect_equal(resAB3,ts_ResAB3)
                                testthat::expect_equal(resB3A,ts_ResB3A)
                            }


                            #Cas 4
                            if (param2 > 0L) {
                                B4_content <- create_random_type(type = typeA, len = param2)
                                ts_B4 <- ts(B4_content,  start = ts4conj::getTimeUnits(end(ts_A) |> as.integer(), frequency = frequenceA) + (param1 + 1L) / frequenceA, frequency = frequenceA)

                                if (typeA == "raw") {
                                    ts_ResAB4 <- ts(c(A_content, rep(as.raw(0L), param1), B4_content), start = startA, frequency = frequenceA)
                                    ts_ResB4A <- ts(c(A_content, rep(as.raw(0L), param1), B4_content), start = startA, frequency = frequenceA)
                                    if (param1 > 0L) {
                                        testthat::expect_warning({
                                            testthat::expect_warning({ resAB4 <- combine2ts(ts_A,ts_B4)},
                                                                     regexp = "extending time series when replacing values")},
                                            regexp = "out-of-range values treated as 0 in coercion to raw")
                                        testthat::expect_warning({
                                            testthat::expect_warning({ resB4A <- combine2ts(ts_B4,ts_A)},
                                                                     regexp = "extending time series when replacing values")},
                                            regexp = "out-of-range values treated as 0 in coercion to raw")
                                    } else {
                                        testthat::expect_warning({ resAB4 <- combine2ts(ts_A,ts_B4)},
                                                                 regexp = "extending time series when replacing values")
                                        testthat::expect_warning({ resB4A <- combine2ts(ts_B4,ts_A)},
                                                                 regexp = "extending time series when replacing values")
                                    }
                                } else {
                                    ts_ResAB4 <- ts(c(A_content, rep(NA, param1), B4_content), start = startA, frequency = frequenceA)
                                    ts_ResB4A <- ts(c(A_content, rep(NA, param1), B4_content), start = startA, frequency = frequenceA)
                                    testthat::expect_warning({ resAB4 <- combine2ts(ts_A,ts_B4)},
                                                             regexp = "extending time series when replacing values")
                                    testthat::expect_warning({ resB4A <- combine2ts(ts_B4,ts_A)},
                                                             regexp = "extending time series when replacing values")
                                }

                                if (typeA != "Date") {
                                    testthat::expect_type(resAB4, typeA)
                                    testthat::expect_type(resB4A, typeA)
                                }

                                testthat::expect_equal(resAB4,ts_ResAB4)
                                testthat::expect_equal(resB4A,ts_ResB4A)
                            }


                            #Cas 5
                            B5_content <- create_random_type(type = typeA, len = param1 + param2 + lenA)
                            ts_B5 <- ts(B5_content,  start = ts4conj::getTimeUnits(startA, frequency = frequenceA) - param1 / frequenceA, frequency = frequenceA)
                            ts_ResAB5 <- ts_B5

                            if (param1 == 0L & param2 == 0L) {
                                ts_ResB5A <- ts_A
                            } else if (param1 == 0L) {
                                ts_ResB5A <- ts(c(A_content, B5_content[(param1 + lenA + 1):(param1 + param2 + lenA)]), start = start(ts_B5), frequency = frequenceA)
                            } else if (param2 == 0L) {
                                ts_ResB5A <- ts(c(B5_content[1:param1], A_content), start = start(ts_B5), frequency = frequenceA)
                            } else {
                                ts_ResB5A <- ts(c(B5_content[1:param1], A_content, B5_content[(param1 + lenA + 1):(param1 + param2 + lenA)]), start = start(ts_B5), frequency = frequenceA)
                            }

                            if (param1 + param2 > 0L) {
                                testthat::expect_warning({ resAB5 <- combine2ts(ts_A,ts_B5)},
                                                         regexp = "extending time series when replacing values")
                            } else {
                                resAB5 <- combine2ts(ts_A,ts_B5)
                            }
                            resB5A <- combine2ts(ts_B5,ts_A)

                            if (typeA != "Date") {
                                testthat::expect_type(resAB5, typeA)
                                testthat::expect_type(resB5A, typeA)
                            }

                            testthat::expect_equal(resAB5,ts_ResAB5)
                            testthat::expect_equal(resB5A,ts_ResB5A)


                            #Cas 6
                            if (param1 + param2 < lenA & param2 > 0L) {
                                B6_content <- create_random_type(type = typeA, len = param2)
                                ts_B6 <- ts(B6_content,  start = ts4conj::getTimeUnits(startA, frequency = frequenceA) + param1 / frequenceA, frequency = frequenceA)
                                if (param1 == 0L) {
                                    ts_ResAB6 <- ts(c(B6_content, A_content[(param1 + param2 + 1L):lenA]), start = startA, frequency = frequenceA)
                                } else {
                                    ts_ResAB6 <- ts(c(A_content[1L:param1], B6_content, A_content[(param1 + param2 + 1L):lenA]), start = startA, frequency = frequenceA)
                                }
                                ts_ResB6A <- ts_A

                                resAB6 <- combine2ts(ts_A,ts_B6)
                                testthat::expect_warning({resB6A <- combine2ts(ts_B6,ts_A)},
                                                         regexp = "extending time series when replacing values")

                                if (typeA != "Date") {
                                    testthat::expect_type(resAB6, typeA)
                                    testthat::expect_type(resB6A, typeA)
                                }

                                testthat::expect_equal(resAB6,ts_ResAB6)
                                testthat::expect_equal(resB6A,ts_ResB6A)
                            }

                        })
                    }
                }
            }
        }
    }
}


# Tests sur les erreurs de mts --------------------------------------------

stop("Ici il faut faire une boucle avec des ts valide de tous les types/longueur/start/freq... et sur la taille du mts")

testthat::test_that("Several dimensions are not allowed", {
    for (typeA in liste_type) {
        for (frequenceA in liste_frequence) {
            for (startA in liste_start) {
                for (lenA in liste_len[-1L]) {

                    ts_A <-  create_random_ts(type = typeA, start = startA, frequency = frequenceA, len = lenA)
                    B_content <- as.data.frame(lapply(1L:5L, function(i) create_random_type(type = typeA, len = 100L)))
                    mts_B <- ts(B_content, start = create_random_date(), frequency = stats::frequency(ts_A))

                    testthat::expect_error(combine2ts(ts_A, mts_B), regexp = "Les objets a et b doivent être des ts unidimensionnels.")
                    testthat::expect_error(combine2ts(mts_B, ts_A), regexp = "Les objets a et b doivent être des ts unidimensionnels.")

                }
            }
        }
    }
})

# Tests sur les erreurs d'input --------------------------------------------

testthat::test_that("miscellaneous input are not allowed", {
    for (typeA in liste_type) {
        ts_A <- create_random_ts(type = typeA)

        for (objA in object_bank_R) {
            testthat::expect_error(combine2ts(ts_A, objA), regexp = "Les objets a et b doivent être des ts unidimensionnels.")
            testthat::expect_error(combine2ts(objA, ts_A), regexp = "Les objets a et b doivent être des ts unidimensionnels.")
            for (objB in object_bank_R) {
                testthat::expect_error(combine2ts(objA, objB), regexp = "Les objets a et b doivent être des ts unidimensionnels.")
            }
        }
    }
})

# Tests sur les erreurs de type d'objets --------------------------------------------

testthat::test_that("different input type are not allowed", {
    for (typeA in liste_type[-7L]) {
        objA <- create_random_ts(type = typeA, frequency = 12L)
        for (typeB in liste_type[-7L]) {
            objB <- create_random_ts(type = typeB, frequency = 12L)
            if (typeA != typeB) testthat::expect_error(combine2ts(objA, objB), regexp = "Les objets a et b doivent être de même type.")
        }
    }
})

# Tests sur les erreurs de temporalité --------------------------------------------

testthat::test_that("arguments have same frequency", {
    for (typeA in liste_type) {
        objA <- create_random_ts(type = typeA, frequency = 12L)
        objB <- create_random_ts(type = typeA, frequency = 4L)
        testthat::expect_error(combine2ts(objA, objB), regexp = "Les objets a et b doivent avoir la même fréquence.")
        testthat::expect_error(combine2ts(objB, objA), regexp = "Les objets a et b doivent avoir la même fréquence.")
    }
})

testthat::test_that("arguments are monthly or quarterly", {
    for (typeA in liste_type) {
        for (freq_A in c(weird_frequency)) {
            for (freq_B in c(weird_frequency, liste_frequence)) {
                objA <- create_random_ts(type = typeA, frequency = freq_A)
                objB <- create_random_ts(type = typeA, frequency = freq_B)
                testthat::expect_error(combine2ts(objA, objB), regexp = "Les objets a et b doivent être trimestriels ou mensuels.")
                testthat::expect_error(combine2ts(objB, objA), regexp = "Les objets a et b doivent être trimestriels ou mensuels.")
            }
        }
    }
})

testthat::test_that("arguments are temporally consistent", {
    for (typeA in liste_type) {
        ts_A <- create_random_ts(type = typeA, start = 2015L, frequency = 12L)
        ts_B <- create_random_ts(type = typeA, start = 2004 + 1/7, frequency = 12L)
        testthat::expect_error(combine2ts(ts_A, ts_B), regexp = "Les objets a et b doivent être cohérents temporellement.")
        testthat::expect_error(combine2ts(ts_B, ts_A), regexp = "Les objets a et b doivent être cohérents temporellement.")

        ts_A <- create_random_ts(type = typeA, start = 2015L, frequency = 4L)
        ts_B <- create_random_ts(type = typeA, start = 2016 + 1/12, frequency = 4L)
        testthat::expect_error(combine2ts(ts_A, ts_B), regexp = "Les objets a et b doivent être cohérents temporellement.")
        testthat::expect_error(combine2ts(ts_B, ts_A), regexp = "Les objets a et b doivent être cohérents temporellement.")
    }
})

