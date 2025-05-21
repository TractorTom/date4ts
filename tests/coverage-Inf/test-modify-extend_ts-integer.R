# Initialisation ---------------------------------------------------------------

set.seed(2044L)


# Tests de résultat avec start vecteur d'entiers -------------------------------

typeA <- "integer"

for (frequenceA in list_frequence) {
    for (startA in list_start) {
        for (lenA in list_len[-1L]) {
            A_content <- create_random_type(type = typeA, len = lenA)
            for (lenB in list_len[-1L]) {
                B_content <- create_random_type(type = typeA, len = lenB)

                test_name <- paste0(
                    "expected result with ",
                    "\ntypeA = ",
                    deparse(typeA),
                    "\nfrequenceA = ",
                    deparse(frequenceA),
                    "\nstartA = ",
                    deparse(startA),
                    "\nlenA = ",
                    deparse(lenA),
                    "\nlenB = ",
                    deparse(lenB)
                )

                testthat::test_that(test_name, {
                    # Cas 1 : simple
                    ts_A <- ts(
                        A_content,
                        start = startA,
                        frequency = frequenceA
                    )
                    res_theo <- ts(
                        c(A_content, B_content),
                        start = startA,
                        frequency = frequenceA
                    )
                    testthat::expect_warning(
                        {
                            res <- extend_ts(
                                series = ts_A,
                                replacement = B_content,
                                date_ts = NULL,
                                replace_na = FALSE
                            )
                        },
                        regexp = warning_extend
                    )
                    testthat::expect_equal(expected = res_theo, object = res)

                    # Cas 1.5 : simple (by default)
                    ts_A <- ts(
                        A_content,
                        start = startA,
                        frequency = frequenceA
                    )
                    res_theo <- ts(
                        c(A_content, B_content),
                        start = startA,
                        frequency = frequenceA
                    )
                    testthat::expect_warning(
                        {
                            res <- extend_ts(
                                series = ts_A,
                                replacement = B_content
                            )
                        },
                        regexp = warning_extend
                    )
                    testthat::expect_equal(expected = res_theo, object = res)

                    # Cas 2 : with with NA
                    for (param1 in list_len) {
                        ts_A <- ts(
                            c(
                                A_content,
                                create_NA_type(type = typeA, len = param1)
                            ),
                            start = startA,
                            frequency = frequenceA
                        )
                        res_theo <- ts(
                            c(
                                A_content,
                                create_NA_type(type = typeA, len = param1),
                                B_content
                            ),
                            start = startA,
                            frequency = frequenceA
                        )
                        testthat::expect_warning(
                            {
                                res <- extend_ts(
                                    series = ts_A,
                                    replacement = B_content,
                                    date_ts = NULL,
                                    replace_na = FALSE
                                )
                            },
                            regexp = warning_extend
                        )
                        testthat::expect_equal(
                            expected = res_theo,
                            object = res
                        )
                    }

                    # Cas 2.5 : with with NA (by default)
                    for (param1 in list_len) {
                        ts_A <- ts(
                            c(
                                A_content,
                                create_NA_type(type = typeA, len = param1)
                            ),
                            start = startA,
                            frequency = frequenceA
                        )
                        res_theo <- ts(
                            c(
                                A_content,
                                create_NA_type(type = typeA, len = param1),
                                B_content
                            ),
                            start = startA,
                            frequency = frequenceA
                        )
                        testthat::expect_warning(
                            {
                                res <- extend_ts(
                                    series = ts_A,
                                    replacement = B_content,
                                    replace_na = FALSE
                                )
                            },
                            regexp = warning_extend
                        )
                        testthat::expect_equal(
                            expected = res_theo,
                            object = res
                        )
                    }

                    # Cas 3 : with without NA
                    for (param1 in list_len) {
                        ts_A <- ts(
                            c(
                                A_content,
                                create_NA_type(type = typeA, len = param1)
                            ),
                            start = startA,
                            frequency = frequenceA
                        )
                        res_theo <- ts(
                            c(A_content, B_content),
                            start = startA,
                            frequency = frequenceA
                        )

                        testthat::expect_warning(
                            {
                                res <- extend_ts(
                                    series = ts_A,
                                    replacement = B_content,
                                    date_ts = NULL,
                                    replace_na = TRUE
                                )
                            },
                            regexp = warning_extend
                        )

                        testthat::expect_equal(
                            expected = res_theo,
                            object = res
                        )
                    }

                    # Cas 3.5 : with without NA (by default)
                    for (param1 in list_len) {
                        ts_A <- ts(
                            c(
                                A_content,
                                create_NA_type(type = typeA, len = param1)
                            ),
                            start = startA,
                            frequency = frequenceA
                        )
                        res_theo <- ts(
                            c(A_content, B_content),
                            start = startA,
                            frequency = frequenceA
                        )

                        testthat::expect_warning(
                            {
                                res <- extend_ts(
                                    series = ts_A,
                                    replacement = B_content
                                )
                            },
                            regexp = warning_extend
                        )

                        testthat::expect_equal(
                            expected = res_theo,
                            object = res
                        )
                    }

                    # Cas 4 : date_ts
                    ts_A <- ts(
                        A_content,
                        start = startA,
                        frequency = frequenceA
                    )

                    for (param1 in list_len[-1L]) {
                        date_end_replacement <- as.integer(end(ts_A))
                        date_end_replacement[2L] <- date_end_replacement[2L] +
                            lenB * param1

                        date_end_replacement[1L] <- date_end_replacement[1L] +
                            (date_end_replacement[2L] - 1L) %/% frequenceA
                        date_end_replacement[2L] <- (date_end_replacement[2L] -
                            1L) %%
                            frequenceA +
                            1L

                        res_theo <- ts(
                            c(A_content, rep(B_content, param1)),
                            start = startA,
                            frequency = frequenceA
                        )

                        testthat::expect_warning(
                            {
                                res <- extend_ts(
                                    series = ts_A,
                                    replacement = B_content,
                                    date_ts = date_end_replacement,
                                    replace_na = FALSE
                                )
                            },
                            regexp = warning_extend
                        )
                        testthat::expect_equal(
                            expected = res_theo,
                            object = res
                        )
                    }

                    # Cas 5 : date_ts with with NA
                    for (param1 in list_len) {
                        ts_A <- ts(
                            c(
                                A_content,
                                create_NA_type(type = typeA, len = param1)
                            ),
                            start = startA,
                            frequency = frequenceA
                        )

                        for (param2 in list_len[-1L]) {
                            date_end_replacement <- as.integer(end(ts_A))
                            date_end_replacement[2L] <- date_end_replacement[
                                2L
                            ] +
                                lenB * param2

                            date_end_replacement[1L] <- date_end_replacement[
                                1L
                            ] +
                                (date_end_replacement[2L] - 1L) %/% frequenceA
                            date_end_replacement[2L] <- (date_end_replacement[
                                2L
                            ] -
                                1L) %%
                                frequenceA +
                                1L

                            res_theo <- ts(
                                c(
                                    A_content,
                                    create_NA_type(type = typeA, len = param1),
                                    rep(B_content, param2)
                                ),
                                start = startA,
                                frequency = frequenceA
                            )
                            testthat::expect_warning(
                                {
                                    res <- extend_ts(
                                        series = ts_A,
                                        replacement = B_content,
                                        date_ts = date_end_replacement,
                                        replace_na = FALSE
                                    )
                                },
                                regexp = warning_extend
                            )
                            testthat::expect_equal(
                                expected = res_theo,
                                object = res
                            )
                        }
                    }

                    # Cas 6 : date_ts with without NA
                    for (param1 in list_len) {
                        ts_A <- ts(
                            c(
                                A_content,
                                create_NA_type(type = typeA, len = param1)
                            ),
                            start = startA,
                            frequency = frequenceA
                        )

                        for (param2 in list_len[-1L]) {
                            date_end_replacement <- as.integer(end(ts_A))
                            date_end_replacement[2L] <- date_end_replacement[
                                2L
                            ] +
                                lenB * param2 -
                                param1

                            date_end_replacement[1L] <- date_end_replacement[
                                1L
                            ] +
                                (date_end_replacement[2L] - 1L) %/% frequenceA
                            date_end_replacement[2L] <- (date_end_replacement[
                                2L
                            ] -
                                1L) %%
                                frequenceA +
                                1L

                            res_theo <- ts(
                                c(A_content, rep(B_content, param2)),
                                start = startA,
                                frequency = frequenceA
                            )
                            testthat::expect_warning(
                                {
                                    res <- extend_ts(
                                        series = ts_A,
                                        replacement = B_content,
                                        date_ts = date_end_replacement,
                                        replace_na = TRUE
                                    )
                                },
                                regexp = warning_extend
                            )
                            testthat::expect_equal(
                                expected = res_theo,
                                object = res
                            )
                        }
                    }

                    # Cas 6.5 : date_ts with without NA (by default)
                    for (param1 in list_len) {
                        ts_A <- ts(
                            c(
                                A_content,
                                create_NA_type(type = typeA, len = param1)
                            ),
                            start = startA,
                            frequency = frequenceA
                        )

                        for (param2 in list_len[-1L]) {
                            date_end_replacement <- as.integer(end(ts_A))
                            date_end_replacement[2L] <- date_end_replacement[
                                2L
                            ] +
                                lenB * param2 -
                                param1

                            date_end_replacement[1L] <- date_end_replacement[
                                1L
                            ] +
                                (date_end_replacement[2L] - 1L) %/% frequenceA
                            date_end_replacement[2L] <- (date_end_replacement[
                                2L
                            ] -
                                1L) %%
                                frequenceA +
                                1L

                            res_theo <- ts(
                                c(A_content, rep(B_content, param2)),
                                start = startA,
                                frequency = frequenceA
                            )
                            testthat::expect_warning(
                                {
                                    res <- extend_ts(
                                        series = ts_A,
                                        replacement = B_content,
                                        date_ts = date_end_replacement
                                    )
                                },
                                regexp = warning_extend
                            )
                            testthat::expect_equal(
                                expected = res_theo,
                                object = res
                            )
                        }
                    }

                    # stop("A compléter avec des appels de extend_ts avec des arguments par défault (non précisés)")
                })
            }
        }
    }
}
