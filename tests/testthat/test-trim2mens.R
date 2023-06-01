
# Initialisation ---------------------------------------------------------------

set.seed(2029L)

# Tests de résultats positifs --------------------------------------------------

testthat::test_that("good result for integer date", {
    for (good_year in good_years) {
        for (quarter in good_quarters) {
            real_quarter <- (quarter - 1L) %% 4L + 1L
            year_real <- good_year + (quarter - 1L) %/% 4L
            date_expected <- as.integer(c(year_real, conversion_quarter_month[real_quarter, "month"]))
            res <- trim2mens(c(good_year, quarter))


            testthat::expect_identical(res, date_expected)
            testthat::expect_type(res, "integer")
        }
    }
})


# Tests de résultats négatifs --------------------------------------------------

testthat::test_that("detection of wrong dates", {
    for (wrong_date in wrong_dates) testthat::expect_error(trim2mens(wrong_date), regexp = "La date est au mauvais format.")
})

# Tests positifs avec warning --------------------------------------------------

testthat::test_that("warning for integer date", {
    for (good_year in good_years) {
        for (warning_quarter in warning_integer_quarters) {

            testthat::expect_warning({resMonthly <- trim2mens(c(good_year, warning_quarter))}, regexp = "Le nombre de période est négatif ou nul ou dépasse la fréquence. La date va être reformattée.")

            real_quarter <- (warning_quarter - 1L) %% 4L + 1L
            year_real <- good_year + (warning_quarter - 1L) %/% 4L
            date_expected <- as.integer(c(year_real, conversion_quarter_month[real_quarter, "month"]))

            testthat::expect_identical(resMonthly, date_expected)
            testthat::expect_type(resMonthly, "integer")
        }
    }
})

testthat::test_that("warning for double date", {
    for (warning_year in double_years) {
        for (good_quarter in good_quarters) {

            testthat::expect_warning({resMonthly <- trim2mens(c(warning_year, good_quarter))}, regexp = "La date est de type double. Il faut privilégier le format integer.")

            real_quarter <- (good_quarter - 1L) %% 4L + 1L
            year_real <- warning_year + (good_quarter - 1L) %/% 4L
            date_expected <- as.integer(c(year_real, conversion_quarter_month[real_quarter, "month"]))

            testthat::expect_identical(resMonthly, date_expected)
            testthat::expect_type(resMonthly, "integer")
        }
    }

    for (good_year in good_years) {
        for (warning_quarter in double_quarters) {

            testthat::expect_warning({resMonthly <- trim2mens(c(good_year, warning_quarter))}, regexp = "La date est de type double. Il faut privilégier le format integer.")

            real_quarter <- (warning_quarter - 1L) %% 4L + 1L
            year_real <- good_year + (warning_quarter - 1L) %/% 4L
            date_expected <- as.integer(c(year_real, conversion_quarter_month[real_quarter, "month"]))

            testthat::expect_identical(resMonthly, date_expected)
            testthat::expect_type(resMonthly, "integer")
        }
    }

    for (warning_year in double_years) {
        for (warning_quarter in double_quarters) {

            testthat::expect_warning({resMonthly <- trim2mens(c(good_year, warning_quarter))}, regexp = "La date est de type double. Il faut privilégier le format integer.")

            real_quarter <- (warning_quarter - 1L) %% 4L + 1L
            year_real <- good_year + (warning_quarter - 1L) %/% 4L
            date_expected <- as.integer(c(year_real, conversion_quarter_month[real_quarter, "month"]))

            testthat::expect_identical(resMonthly, date_expected)
            testthat::expect_type(resMonthly, "integer")
        }
    }
})

testthat::test_that("several warning", {
    for (warning_year in double_years) {
        for (warning_quarter in warning_integer_quarters) {

            w <- testthat::capture_warnings({resMonthly <- trim2mens(c(warning_year, warning_quarter))})
            testthat::expect_match(object = w, regexp = "La date est de type double. Il faut privilégier le format integer.", all = FALSE)
            testthat::expect_match(object = w, regexp = "Le nombre de période est négatif ou nul ou dépasse la fréquence. La date va être reformattée.", all = FALSE)

            real_quarter <- (warning_quarter - 1L) %% 4L + 1L
            year_real <- warning_year + (warning_quarter - 1L) %/% 4L
            date_expected <- as.integer(c(year_real, conversion_quarter_month[real_quarter, "month"]))

            testthat::expect_identical(resMonthly, date_expected)
            testthat::expect_type(resMonthly, "integer")
        }
    }

    for (warning_year in double_years) {
        for (warning_quarter in warning_double_quarters) {

            w <- testthat::capture_warnings({resMonthly <- trim2mens(c(warning_year, warning_quarter))})
            testthat::expect_match(object = w, regexp = "La date est de type double. Il faut privilégier le format integer.", all = FALSE)
            testthat::expect_match(object = w, regexp = "Le nombre de période est négatif ou nul ou dépasse la fréquence. La date va être reformattée.", all = FALSE)

            real_quarter <- (warning_quarter - 1L) %% 4L + 1L
            year_real <- warning_year + (warning_quarter - 1L) %/% 4L
            date_expected <- as.integer(c(year_real, conversion_quarter_month[real_quarter, "month"]))

            testthat::expect_identical(resMonthly, date_expected)
            testthat::expect_type(resMonthly, "integer")
        }
    }

    for (good_year in good_years) {
        for (warning_quarter in warning_double_quarters) {

            w <- testthat::capture_warnings({resMonthly <- trim2mens(c(good_year, warning_quarter))})
            testthat::expect_match(object = w, regexp = "La date est de type double. Il faut privilégier le format integer.", all = FALSE)
            testthat::expect_match(object = w, regexp = "Le nombre de période est négatif ou nul ou dépasse la fréquence. La date va être reformattée.", all = FALSE)

            real_quarter <- (warning_quarter - 1L) %% 4L + 1L
            year_real <- good_year + (warning_quarter - 1L) %/% 4L
            date_expected <- as.integer(c(year_real, conversion_quarter_month[real_quarter, "month"]))

            testthat::expect_identical(resMonthly, date_expected)
            testthat::expect_type(resMonthly, "integer")
        }
    }
})

