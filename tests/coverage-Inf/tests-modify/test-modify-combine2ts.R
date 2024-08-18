# Initialisation ---------------------------------------------------------------

set.seed(2022L)


# Tests sur les erreurs de mts -------------------------------------------------

testthat::test_that("Several dimensions are not allowed", {
    for (typeA in list_type) {
        for (frequenceA in list_frequence) {
            for (startA in list_start) {
                for (lenA in list_len[-1L]) {
                    ts_A <- create_random_ts(type = typeA, start = startA, frequency = frequenceA, len = lenA)
                    B_content <- as.data.frame(lapply(1L:5L, function(i) create_random_type(type = typeA, len = 100L)))
                    startB <- create_random_date_ts()

                    if (typeA == "complex") {
                        mts_B <- do.call(
                            what = cbind,
                            args = lapply(
                                X = B_content,
                                FUN = ts,
                                start = startB,
                                frequency = frequenceA
                            )
                        )
                    } else {
                        mts_B <- ts(B_content, start = startB, frequency = frequenceA)
                    }

                    testthat::expect_error(
                        object = combine2ts(a = ts_A, b = mts_B),
                        regexp = "Variable 'b': Must be of type 'atomic vector'"
                    )
                    testthat::expect_error(
                        object = combine2ts(a = mts_B, b = ts_A),
                        regexp = "Variable 'a': Must be of type 'atomic vector'"
                    )
                }
            }
        }
    }
})

# Tests sur les erreurs d'input ------------------------------------------------

testthat::test_that("miscellaneous input are not allowed", {
    for (typeA in list_type) {
        ts_A <- create_random_ts(type = typeA)

        for (objA in object_bank_R) {
            testthat::expect_error(
                combine2ts(ts_A, objA)
            )
            testthat::expect_error(
                combine2ts(objA, ts_A)
            )
            for (objB in object_bank_R) {
                testthat::expect_error(
                    combine2ts(objA, objB)
                )
            }
        }
    }
})

# Tests sur les erreurs de type d'objets ---------------------------------------

testthat::test_that("different input type are not allowed", {
    for (typeA in list_type[-7L]) {
        objA <- create_random_ts(type = typeA, frequency = 12L)
        for (typeB in list_type[-7L]) {
            objB <- create_random_ts(type = typeB, frequency = 12L)
            if (typeA != typeB) testthat::expect_error(combine2ts(objA, objB), regexp = "Les objets `a` et `b` doivent \u00eatre de m\u00eame type.")
        }
    }
})

# Tests sur les erreurs de temporalité -----------------------------------------

testthat::test_that("arguments have same frequency", {
    for (typeA in list_type) {
        objA <- create_random_ts(type = typeA, frequency = 12L)
        objB <- create_random_ts(type = typeA, frequency = 4L)
        testthat::expect_error(combine2ts(objA, objB), regexp = "Les objets `a` et `b` doivent avoir la m\u00eame fr\u00e9quence.")
        testthat::expect_error(combine2ts(objB, objA), regexp = "Les objets `a` et `b` doivent avoir la m\u00eame fr\u00e9quence.")
    }
})

testthat::test_that("arguments are monthly or quarterly", {
    for (typeA in list_type) {
        for (freq_A in c(weird_frequency)) {
            for (freq_B in c(weird_frequency, list_frequence)) {
                objA <- create_random_ts(type = typeA, frequency = freq_A)
                objB <- create_random_ts(type = typeA, frequency = freq_B)
                testthat::expect_error(combine2ts(objA, objB))
                testthat::expect_error(combine2ts(objB, objA))
            }
        }
    }
})

testthat::test_that("arguments are temporally consistent", {
    for (typeA in list_type) {
        ts_A <- create_random_ts(type = typeA, start = 2015L, frequency = 12L)
        ts_B <- create_random_ts(type = typeA, start = 2004 + 1 / 7, frequency = 12L)
        testthat::expect_error(combine2ts(ts_A, ts_B))
        testthat::expect_error(combine2ts(ts_B, ts_A))

        ts_A <- create_random_ts(type = typeA, start = 2015L, frequency = 4L)
        ts_B <- create_random_ts(type = typeA, start = 2016 + 1 / 12, frequency = 4L)
        testthat::expect_error(combine2ts(ts_A, ts_B))
        testthat::expect_error(combine2ts(ts_B, ts_A))
    }
})
