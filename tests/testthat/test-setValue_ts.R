
# Initialisation ---------------------------------------------------------------

set.seed(2023L)

create_random_type <- function(type, len = NULL){
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

create_random_date <- function(){
    if (runif(1, 0, 1) > 0.5) return(sample(1950L:2022L, size = 1L))
    return(c(sample(1950L:2022L, size = 1L),
             sample(-20L:20L, size = 1L)))
}

create_random_ts <- function(type, len = NULL, start = NULL, frequency = NULL){
    if (is.null(len)) len <- sample(1L:1000L, size = 1)
    if (is.null(frequency)) frequency <- sample(c(4L, 12L), size = 1)
    if (is.null(start)) start <- create_random_date()

    content <- create_random_type(type, len)

    return(ts(content, start = start, frequency = frequency))
}

liste_type <- c("integer", "character", "double", "logical", "complex", "raw", "Date")
object_bank_R <- fuzzr::test_all()
weird_frequency <- c(1, 2, 7, 0.1, 1/3, 3.5, 365.25, pi)
wrong_dates <- c(
    fuzzr::test_all()[-10],
    list(list(2020L, 5L), list(2L, "a", 3.5), list(NULL), list(2005), list(c(2022L, 8L)), list(c(2022L, 8.))),
    lapply(liste_type[-c(1L, 3L)], create_random_type, len = 2),
    lapply(liste_type[-c(1L, 3L)], create_random_type, len = 3),
    list(2019.5, 2020 + 1/12, pi / 4, c(2020, 2.5), c(2010.25, 3), c(2002, 3, 1), c("2002", "3")),
    list(c(2020L, NA_integer_), c(NA_integer_, 5L), c(NA_integer_, NA_integer_), c(2020, NA_real_), c(NA_real_, 5), c(NA_real_, NA_real_)),
    list(2L:4L, c(2020.0, 7, 1), c(2020L, 0L, NA_integer_), numeric(0), integer(0))
)

liste_len <- c(1L, 4L:6L, 10L, 10000L)
liste_lag <- c(-10000L, -5L, -1L, 0L, 1L, 5L, 10000L)
liste_frequence <- c(4L, 12L)
liste_start <- list(c(2020L, -1L), c(2020L, 0L), c(2020L, 4L), c(2020L, 5L), c(2020L, 12L), c(2020L, 13L))

# Tests de résultat avec start vecteur d'entiers -------------------------------

for (typeA in liste_type){
    for (frequenceA in liste_frequence){
        for (startA in liste_start){
            for (lenA in liste_len){
                A_content <- create_random_type(type = typeA, len = lenA)
                ts_A <-  ts(A_content, start = startA, frequency = frequenceA)
                for (lagB in liste_lag){
                    for (lenB in liste_len){

                        test_name <- paste("expected result with ",
                                           "\ntypeA = '", typeA,
                                           "'\nfrequenceA = ", frequenceA,
                                           "\nstartA = ", deparse(startA),
                                           "\nlenA = ", lenA,
                                           "\nlagB = ", lagB,
                                           "\nlenB = ", lenB, sep = "")

                        testthat::test_that(test_name, {

                            valueB <- create_random_type(type = typeA, len = lenB)
                            startB <- c(startA[1], startA[2] + lagB)
                            ts_B <- ts(valueB, start = startB, frequency = frequenceA)


                            #Cas où lagB >= 0 (start = startA)
                            if (lagB > 0){
                                if (lagB > lenA) { #Cas 4

                                    if (typeA == "raw"){
                                        ts_ResAB1 <- ts(c(A_content, rep(as.raw(0L), lagB - lenA), valueB), start = startA, frequency = frequenceA)

                                        testthat::expect_warning({
                                            testthat::expect_warning({resAB <- setValue_ts(ts_A, date = startB, value = valueB)},
                                                                     regexp = "extending time series when replacing values")},
                                            regexp = "out-of-range values treated as 0 in coercion to raw")
                                        testthat::expect_warning({
                                            testthat::expect_warning({ts_ResAB2 <- combine2ts(ts_A, ts_B)},
                                                                     regexp = "extending time series when replacing values")},
                                            regexp = "out-of-range values treated as 0 in coercion to raw")

                                    } else {
                                        ts_ResAB1 <- ts(c(A_content, rep(NA, lagB - lenA), valueB), start = startA, frequency = frequenceA)

                                        testthat::expect_warning({resAB <- setValue_ts(ts_A, date = startB, value = valueB)},
                                                                 regexp = "extending time series when replacing values")
                                        testthat::expect_warning({ts_ResAB2 <- combine2ts(ts_A, ts_B)},
                                                                 regexp = "extending time series when replacing values")
                                    }


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

                                    if (typeA == "raw"){
                                        ts_ResAB1 <- ts(c(valueB, rep(as.raw(0L), -lagB - lenB), A_content), start = startB, frequency = frequenceA)

                                        testthat::expect_warning({
                                            testthat::expect_warning({resAB <- setValue_ts(ts_A, date = startB, value = valueB)},
                                                                     regexp = "extending time series when replacing values")},
                                            regexp = "out-of-range values treated as 0 in coercion to raw")
                                        testthat::expect_warning({
                                            testthat::expect_warning({ts_ResAB2 <- combine2ts(ts_A, ts_B)},
                                                                     regexp = "extending time series when replacing values")},
                                            regexp = "out-of-range values treated as 0 in coercion to raw")

                                    } else {
                                        ts_ResAB1 <- ts(c(valueB, rep(NA, -lagB - lenB), A_content), start = startB, frequency = frequenceA)
                                        testthat::expect_warning({ts_ResAB2 <- combine2ts(ts_A, ts_B)},
                                                                 regexp = "extending time series when replacing values")
                                        testthat::expect_warning({resAB <- setValue_ts(ts_A, date = startB, value = valueB)},
                                                                 regexp = "extending time series when replacing values")
                                    }

                                } else if (lagB + lenB >= lenA) { #Cas 5
                                    ts_ResAB1 <- ts(valueB, start = startB, frequency = frequenceA)
                                    testthat::expect_warning({ts_ResAB2 <- combine2ts(ts_A, ts_B)},
                                                             regexp = "extending time series when replacing values")
                                    testthat::expect_warning({resAB <- setValue_ts(ts_A, date = startB, value = valueB)},
                                                             regexp = "extending time series when replacing values")
                                } else { #Cas 2
                                    ts_ResAB1 <- ts(c(valueB, A_content[(lagB + lenB + 1):lenA]), start = startB, frequency = frequenceA)
                                    testthat::expect_warning({ts_ResAB2 <- combine2ts(ts_A, ts_B)},
                                                             regexp = "extending time series when replacing values")
                                    testthat::expect_warning({resAB <- setValue_ts(ts_A, date = startB, value = valueB)},
                                                             regexp = "extending time series when replacing values")
                                }

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

                            if (typeA != "Date"){
                                testthat::expect_type(resAB, typeA)
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

stop("Ici il faut faire une boucle avec des ts valide de tous les types/longueur/start/freq... et sur la taille du mts")

testthat::test_that("Several dimensions are not allowed", {
    for (typeA in liste_type){
        B_content <- as.data.frame(lapply(1L:5L, function(i) create_random_type(type = typeA, len = 100L)))
        mts_B <- ts(B_content, start = create_random_date(), frequency = 12L)

        testthat::expect_error(setValue_ts(dataTS = mts_B,
                                           date = create_random_date(),
                                           value = create_random_type(type = typeA)),
                               regexp = "L'objets dataTS doit être un ts unidimensionnel.")
    }
})

# Tests sur les erreurs d'input ------------------------------------------------

## Test sur le ts --------------------------------------------------------------

testthat::test_that("miscellaneous dataTS are not allowed", {
    for (typeA in liste_type){
        for (obj in object_bank_R){
            testthat::expect_error(setValue_ts(dataTS = obj,
                                               date = create_random_date(),
                                               value = create_random_type(type = typeA)),
                                   regexp = "L'objets dataTS doit être un ts unidimensionnel.")
        }
    }
})

## Test sur la date ------------------------------------------------------------

testthat::test_that("miscellaneous date are not allowed", {
    for (typeA in liste_type){
        for (wrong_date in wrong_dates){
            testthat::expect_error(setValue_ts(dataTS = create_random_ts(type = typeA),
                                               date = wrong_date,
                                               value = create_random_type(type = typeA)),
                                   regexp = "La date est au mauvais format.")
        }
    }
})

## Test sur le vecteur value ---------------------------------------------------

testthat::test_that("miscellaneous value input are not allowed", {
    liste_wrong_value <- c(fuzzr::test_df()[-4], NULL, character(0L), numeric(0L), logical(0L), integer(0L), complex(0L))
    for (typeA in liste_type){
        for (value in liste_wrong_value){
            testthat::expect_error(setValue_ts(dataTS = create_random_ts(type = typeA),
                                               date = create_random_date(),
                                               value = value),
                                   regexp = "L'argument value doit être unidimensionnel.")
        }
    }
})

testthat::test_that("value should have same type as dataTS", {
    for (typeA in liste_type[-7]){
        for (typeB in liste_type[-7]){
            if (typeA != typeB){
                testthat::expect_error(setValue_ts(dataTS = create_random_ts(type = typeA),
                                                   date = create_random_date(),
                                                   value = create_random_type(typeB)),
                                       regexp = "Les objets dataTS et value doivent être de même type.")
            }
        }
    }
})

testthat::test_that("NA values generate warning", {
    for (typeA in liste_type[-6L]){

        ts_A <- create_random_ts(type = typeA, start = 2000L, len = 80L, frequency = 4L)
        v1 <- sample(c(create_random_type(typeA, len = 10L), get(paste0("as.", typeA))(rep(NA, 5L))), replace = TRUE)
        v2 <- get(paste0("as.", typeA))(rep(NA, 5L))

        testthat::expect_warning(setValue_ts(dataTS = ts_A, date = 2010L, value = v1),
                                 regexp = "L'argument value contient des NAs.")
        testthat::expect_warning(setValue_ts(dataTS = ts_A, date = 2010L, value = v2),
                                 regexp = "L'argument value contient des NAs.")
    }
})

# Tests sur les erreurs de temporalité --------------------------------------------

testthat::test_that("dataTS and date are temporally consistent", {
    for (typeA in liste_type){
        testthat::expect_error(setValue_ts(dataTS = create_random_ts(type = typeA, start = 2010 + 1/7, frequency = 12L),
                                           date = create_random_date(),
                                           value = create_random_type(type = typeA)),
                               regexp = "Les objets a et b doivent être cohérents temporellement.")
    }

    for (typeA in liste_type){
        testthat::expect_error(setValue_ts(dataTS = create_random_ts(type = typeA, start = 2022 + 1/5, frequency = 4L),
                                           date = create_random_date(),
                                           value = create_random_type(type = typeA)),
                               regexp = "Les objets a et b doivent être cohérents temporellement.")
    }
})

