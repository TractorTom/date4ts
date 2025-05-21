# Initialisation ---------------------------------------------------------------

set.seed(2051L)


# Tests de résultat ------------------------------------------------------------

typeA <- "complex"

for (frequenceA in list_frequence) {
    for (startA in list_start) {
        for (lenA in list_len[-1L]) {
            A_content <- create_random_type(type = typeA, len = lenA)
            ts_A <- ts(A_content, start = startA, frequency = frequenceA)
            for (param1 in list_len) {
                for (param2 in list_len) {
                    test_name <- paste0(
                        "expected result with ",
                        "\ntypeA = '",
                        typeA,
                        "'\nfrequenceA = ",
                        frequenceA,
                        "\nstartA = ",
                        deparse(startA),
                        "\nlenA = ",
                        lenA,
                        "\nparam1 = ",
                        param1,
                        "\nparam2 = ",
                        param2
                    )

                    testthat::test_that(desc = test_name, {
                        # Cas 1
                        if (param1 <= lenA & param1 > 0L) {
                            date_from_1 <- end(ts_A) |> as.integer()
                            if (length(date_from_1) == 1L)
                                date_from_1 <- c(date_from_1, 1L)
                            date_from_1[2L] <- date_from_1[2L] - (param1 - 1L)
                            date_from_1 <- c(
                                date_from_1[1L] +
                                    (date_from_1[2L] - 1L) %/% frequenceA,
                                (date_from_1[2L] - 1L) %% frequenceA + 1L
                            )

                            date_to_1 <- end(ts_A) |> as.integer()
                            if (length(date_to_1) == 1L)
                                date_to_1 <- c(date_to_1, 1L)
                            date_to_1[2L] <- date_to_1[2L] + param2
                            date_to_1 <- c(
                                date_to_1[1L] +
                                    (date_to_1[2L] - 1L) %/% frequenceA,
                                (date_to_1[2L] - 1L) %% frequenceA + 1L
                            )

                            res_theo_1 <- c(
                                A_content[(lenA - param1 + 1L):lenA],
                                create_NA_type(type = typeA, len = param2)
                            )

                            vect_res1 <- get_value_ts(
                                series = ts_A,
                                date_from = date_from_1,
                                date_to = date_to_1
                            )
                            vect_res2 <- get_value_ts(
                                series = ts_A,
                                date_from = date_from_1,
                                n = param1 + param2
                            )
                            vect_res3 <- get_value_ts(
                                series = ts_A,
                                date_to = date_to_1,
                                n = param1 + param2
                            )

                            testthat::expect_identical(res_theo_1, vect_res1)
                            testthat::expect_identical(res_theo_1, vect_res2)
                            testthat::expect_identical(res_theo_1, vect_res3)
                        }

                        # Cas 2
                        if (param2 <= lenA & param2 > 0L) {
                            date_from_2 <- start(ts_A) |> as.integer()
                            if (length(date_from_2) == 1L)
                                date_from_2 <- c(date_from_2, 1L)
                            date_from_2[2L] <- date_from_2[2L] - param1
                            date_from_2 <- c(
                                date_from_2[1L] +
                                    (date_from_2[2L] - 1L) %/% frequenceA,
                                (date_from_2[2L] - 1L) %% frequenceA + 1L
                            )

                            date_to_2 <- start(ts_A) |> as.integer()
                            if (length(date_to_2) == 1L)
                                date_to_2 <- c(date_to_2, 1L)
                            date_to_2[2L] <- date_to_2[2L] + param2 - 1L
                            date_to_2 <- c(
                                date_to_2[1L] +
                                    (date_to_2[2L] - 1L) %/% frequenceA,
                                (date_to_2[2L] - 1L) %% frequenceA + 1L
                            )

                            res_theo_2 <- c(
                                create_NA_type(type = typeA, len = param1),
                                A_content[1:param2]
                            )

                            vect_res1 <- get_value_ts(
                                ts_A,
                                date_from = date_from_2,
                                date_to = date_to_2
                            )
                            vect_res2 <- get_value_ts(
                                ts_A,
                                date_from = date_from_2,
                                n = param1 + param2
                            )
                            vect_res3 <- get_value_ts(
                                ts_A,
                                date_to = date_to_2,
                                n = param1 + param2
                            )

                            testthat::expect_identical(res_theo_2, vect_res1)
                            testthat::expect_identical(res_theo_2, vect_res2)
                            testthat::expect_identical(res_theo_2, vect_res3)
                        }

                        # Cas 3
                        if (param1 > 0L) {
                            date_from_3 <- start(ts_A) |> as.integer()
                            if (length(date_from_3) == 1L)
                                date_from_3 <- c(date_from_3, 1L)
                            date_from_3[2L] <- date_from_3[2L] - param1 - param2
                            date_from_3 <- c(
                                date_from_3[1L] +
                                    (date_from_3[2L] - 1L) %/% frequenceA,
                                (date_from_3[2L] - 1L) %% frequenceA + 1L
                            )

                            date_to_3 <- start(ts_A) |> as.integer()
                            if (length(date_to_3) == 1L)
                                date_to_3 <- c(date_to_3, 1L)
                            date_to_3[2L] <- date_to_3[2L] - param2 - 1L
                            date_to_3 <- c(
                                date_to_3[1L] +
                                    (date_to_3[2L] - 1L) %/% frequenceA,
                                (date_to_3[2L] - 1L) %% frequenceA + 1L
                            )

                            res_theo_3 <- create_NA_type(
                                type = typeA,
                                len = param1
                            )

                            vect_res1 <- get_value_ts(
                                ts_A,
                                date_from = date_from_3,
                                date_to = date_to_3
                            )
                            vect_res2 <- get_value_ts(
                                ts_A,
                                date_from = date_from_3,
                                n = param1
                            )
                            vect_res3 <- get_value_ts(
                                ts_A,
                                date_to = date_to_3,
                                n = param1
                            )

                            testthat::expect_identical(res_theo_3, vect_res1)
                            testthat::expect_identical(res_theo_3, vect_res2)
                            testthat::expect_identical(res_theo_3, vect_res3)
                        }

                        # Cas 4
                        if (param2 > 0L) {
                            date_from_4 <- end(ts_A) |> as.integer()
                            if (length(date_from_4) == 1L)
                                date_from_4 <- c(date_from_4, 1L)
                            date_from_4[2L] <- date_from_4[2L] + param1 + 1L
                            date_from_4 <- c(
                                date_from_4[1L] +
                                    (date_from_4[2L] - 1L) %/% frequenceA,
                                (date_from_4[2L] - 1L) %% frequenceA + 1L
                            )

                            date_to_4 <- end(ts_A) |> as.integer()
                            if (length(date_to_4) == 1L)
                                date_to_4 <- c(date_to_4, 1L)
                            date_to_4[2L] <- date_to_4[2L] + param1 + param2
                            date_to_4 <- c(
                                date_to_4[1L] +
                                    (date_to_4[2L] - 1L) %/% frequenceA,
                                (date_to_4[2L] - 1L) %% frequenceA + 1L
                            )

                            res_theo_4 <- create_NA_type(
                                type = typeA,
                                len = param2
                            )

                            vect_res1 <- get_value_ts(
                                ts_A,
                                date_from = date_from_4,
                                date_to = date_to_4
                            )
                            vect_res2 <- get_value_ts(
                                ts_A,
                                date_from = date_from_4,
                                n = param2
                            )
                            vect_res3 <- get_value_ts(
                                ts_A,
                                date_to = date_to_4,
                                n = param2
                            )

                            testthat::expect_identical(res_theo_4, vect_res1)
                            testthat::expect_identical(res_theo_4, vect_res2)
                            testthat::expect_identical(res_theo_4, vect_res3)
                        }

                        # Cas 5
                        date_from_5 <- start(ts_A) |> as.integer()
                        if (length(date_from_5) == 1L)
                            date_from_5 <- c(date_from_5, 1L)
                        date_from_5[2L] <- date_from_5[2L] - param1
                        date_from_5 <- c(
                            date_from_5[1L] +
                                (date_from_5[2L] - 1L) %/% frequenceA,
                            (date_from_5[2L] - 1L) %% frequenceA + 1L
                        )

                        date_to_5 <- end(ts_A) |> as.integer()
                        if (length(date_to_5) == 1L)
                            date_to_5 <- c(date_to_5, 1L)
                        date_to_5[2L] <- date_to_5[2L] + param2
                        date_to_5 <- c(
                            date_to_5[1L] + (date_to_5[2L] - 1L) %/% frequenceA,
                            (date_to_5[2L] - 1L) %% frequenceA + 1L
                        )

                        res_theo_5 <- c(
                            create_NA_type(type = typeA, len = param1),
                            A_content,
                            create_NA_type(type = typeA, len = param2)
                        )

                        vect_res1 <- get_value_ts(
                            ts_A,
                            date_from = date_from_5,
                            date_to = date_to_5
                        )
                        vect_res2 <- get_value_ts(
                            ts_A,
                            date_from = date_from_5,
                            n = param2 + lenA + param1
                        )
                        vect_res3 <- get_value_ts(
                            ts_A,
                            date_to = date_to_5,
                            n = param2 + lenA + param1
                        )

                        testthat::expect_identical(res_theo_5, vect_res1)
                        testthat::expect_identical(res_theo_5, vect_res2)
                        testthat::expect_identical(res_theo_5, vect_res3)

                        # Cas 6
                        if (param1 + param2 <= lenA && param2 > 0) {
                            date_from_6 <- start(ts_A) |> as.integer()
                            if (length(date_from_6) == 1L)
                                date_from_6 <- c(date_from_6, 1L)
                            date_from_6[2L] <- date_from_6[2L] + param1
                            date_from_6 <- c(
                                date_from_6[1L] +
                                    (date_from_6[2L] - 1L) %/% frequenceA,
                                (date_from_6[2L] - 1L) %% frequenceA + 1L
                            )

                            date_to_6 <- start(ts_A) |> as.integer()
                            if (length(date_to_6) == 1L)
                                date_to_6 <- c(date_to_6, 1L)
                            date_to_6[2L] <- date_to_6[2L] +
                                param1 +
                                param2 -
                                1L
                            date_to_6 <- c(
                                date_to_6[1L] +
                                    (date_to_6[2L] - 1L) %/% frequenceA,
                                (date_to_6[2L] - 1L) %% frequenceA + 1L
                            )

                            res_theo_6 <- A_content[
                                (param1 + 1L):(param1 + param2)
                            ]

                            vect_res1 <- get_value_ts(
                                ts_A,
                                date_from = date_from_6,
                                date_to = date_to_6
                            )
                            vect_res2 <- get_value_ts(
                                ts_A,
                                date_from = date_from_6,
                                n = param2
                            )
                            vect_res3 <- get_value_ts(
                                ts_A,
                                date_to = date_to_6,
                                n = param2
                            )

                            testthat::expect_identical(res_theo_6, vect_res1)
                            testthat::expect_identical(res_theo_6, vect_res2)
                            testthat::expect_identical(res_theo_6, vect_res3)
                        }
                    })
                }
            }
        }
    }
}
