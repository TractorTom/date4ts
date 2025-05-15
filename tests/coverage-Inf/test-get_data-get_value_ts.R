# Initialisation ---------------------------------------------------------------

set.seed(2040L)


# Tests sur les erreurs de mts -------------------------------------------------

testthat::test_that("Several dimensions are not allowed", {
    for (typeA in list_type) {
        for (frequenceA in list_frequence) {
            for (startA in list_start) {
                for (lenA in list_len[-1L]) {

                    A_content <- as.data.frame(lapply(1L:5L, function(i) create_random_type(type = typeA, len = 100L)))
                    startA <- create_random_date_ts()

                    if (typeA == "complex") {
                        mts_A <- do.call(
                            what = cbind,
                            args = lapply(
                                X = A_content,
                                FUN = ts,
                                start = startA,
                                frequency = frequenceA
                            )
                        )
                    } else {
                        mts_A <- ts(A_content, start = startA, frequency = frequenceA)
                    }

                    testthat::expect_error(
                        object = get_value_ts(series = mts_A, date_from = startA, date_to = end(mts_A) |> as.integer()),
                        regexp = "Variable 'series': Must be of type 'atomic vector'"
                    )
                    testthat::expect_error(
                        object = get_value_ts(series = mts_A, date_from = startA, n = lenA),
                        regexp = "Variable 'series': Must be of type 'atomic vector'"
                    )
                    testthat::expect_error(
                        object = get_value_ts(series = mts_A, date_to = end(mts_A) |> as.integer(), n = lenA),
                        regexp = "Variable 'series': Must be of type 'atomic vector'"
                    )
                }
            }
        }
    }
})

# Tests sur les erreurs d'input ------------------------------------------------

testthat::test_that("miscellaneous input are not allowed", {

    date_1 <- 2000L
    date_2 <- 2001L
    len <- 15L

    for (objA in object_bank_R) {
        testthat::expect_error(
            object = get_value_ts(series = objA, date_from = date_1, date_to = date_2)
        )
        testthat::expect_error(
            object = get_value_ts(series = objA, date_from = date_1, n = len)
        )
        testthat::expect_error(
            object = get_value_ts(series = objA, date_to = date_2, n = len)
        )

    }
})

testthat::test_that("3 argyuments input are not allowed", {
    for (typeA in list_type[-7]){
        for (frequenceA in list_frequence) {
            ts_A <- create_random_ts(type = typeA, frequency = frequenceA)
            testthat::expect_error(
                object = get_value_ts(
                    series = ts_A,
                    date_from = create_random_date_ts(),
                    date_to = create_random_date_ts(),
                    n = create_random_type(type = "integer", len = 1L)
                )
            )
        }
    }
})

# Tests sur les erreurs de temporalité -----------------------------------------

testthat::test_that("date_from must be before date_to", {

    for (year in good_years) {
        for (len in list_len[-1L]){

            ts_A <- create_random_ts(frequency = 12L)

            date_1 <- year
            date_2 <- year + len
            testthat::expect_error(get_value_ts(ts_A, date_from = date_2, date_to = date_1))

            for (month in good_months) {

                date_1 <- c(year, month)
                date_2 <- date_1
                date_2[1L] <- date_2[1L] + len
                date_2 <- c(date_2[1L] + (date_2[2L] - 1L) %/% 12L,
                            (date_2[2L] - 1L) %% 12L + 1L)
                testthat::expect_error(get_value_ts(ts_A, date_from = date_2, date_to = date_1))

            }

            ts_A <- create_random_ts(frequency = 4L)

            for (quarter in good_quarters) {

                date_1 <- c(year, quarter)
                date_2 <- date_1
                date_2[1L] <- date_2[1L] + len
                date_2 <- c(date_2[1L] + (date_2[2L] - 1L) %/% 4L,
                            (date_2[2L] - 1L) %% 4L + 1L)
                testthat::expect_error(get_value_ts(ts_A, date_from = date_2, date_to = date_1))

            }
        }
    }

})

testthat::test_that("n is non positive", {
    for (len in list_len){
        ts_A <- create_random_ts()
        testthat::expect_error(
            object = get_value_ts(
                series = ts_A,
                date_from = create_random_date_ts(),
                n = -abs(len)
            )
        )
        testthat::expect_error(
            object = get_value_ts(
                series = ts_A,
                date_to = create_random_date_ts(),
                n = -abs(len)
            )
        )
    }
})

testthat::test_that("miscelanous date are not accepted", {
    for (wrong_date_ts in list_wrong_date_ts) {

        ts_A <- create_random_ts()

        testthat::expect_error(
            object = get_value_ts(
                series = ts_A, date_to = wrong_date_ts,
                n = 10L
            )
        )

        testthat::expect_error(
            object = get_value_ts(
                series = ts_A, date_from = wrong_date_ts,
                n = 10L
            )
        )

        testthat::expect_error(
            object = get_value_ts(
                series = ts_A, date_from = wrong_date_ts,
                date_to = create_random_date_ts(frequency_ts = frequency(ts_A))
            )
        )

        testthat::expect_error(
            object = get_value_ts(
                series = ts_A, date_from = create_random_date_ts(frequency_ts = frequency(ts_A)),
                date_to = wrong_date_ts
            )
        )
    }
})

testthat::test_that("miscelanous n are not accepted", {
    for (wrong_n in c(list(0., 0L),
                      list_wrong_date_ts,
                      object_bank_R[-10L],
                      rnorm(10L),
                      as.double(-abs(c(list_lag, list_len, create_random_type("integer", len = 10L)))),
                      -abs(c(list_len, list_lag, create_random_type("integer", len = 10L))))) {

        ts_A <- create_random_ts()

        testthat::expect_error(
            object = get_value_ts(
                series = ts_A, date_to = create_random_date_ts(frequency_ts = frequency(ts_A)),
                n = wrong_n
            )
        )

        testthat::expect_error(
            object = get_value_ts(
                series = ts_A, date_from = create_random_date_ts(frequency_ts = frequency(ts_A)),
                n = wrong_n
            )
        )

    }
})
