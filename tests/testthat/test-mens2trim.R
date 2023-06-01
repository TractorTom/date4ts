
# Initialisation ---------------------------------------------------------------

set.seed(2030L)


# Tests de résultats positifs --------------------------------------------------

testthat::test_that("good result for integer date", {
    for (good_year in good_years) {
        for (mens in good_months) {

            real_month <- (mens - 1L) %% 12L + 1L
            year_real <- good_year + (mens - 1L) %/% 12L
            date_expected <- as.integer(c(year_real, conversion_month_quarter[real_month, "quarter"]))
            res <- mens2trim(c(good_year, mens))

            testthat::expect_identical(res, date_expected)
            testthat::expect_type(res, "integer")
        }
    }
})


# Tests de résultats négatifs --------------------------------------------------

testthat::test_that("detection of wrong dates", {
    for (wrong_date in wrong_dates) testthat::expect_error(mens2trim(wrong_date), regexp = "La date est au mauvais format.")
})


# Tests positifs avec warning --------------------------------------------------

testthat::test_that("warning for integer date", {
    for (good_year in good_years) {
        for (warning_month in warning_integer_months) {

            testthat::expect_warning({resQuarterly <- mens2trim(c(good_year, warning_month))}, regexp = "Le nombre de p\u00e9riode est n\u00e9gatif ou nul ou d\u00e9passe la fr\u00e9quence. La date va \u00eatre reformatt\u00e9e.")

            real_month <- (warning_month - 1L) %% 12L + 1L
            year_real <- good_year + (warning_month - 1L) %/% 12L
            date_expected <- as.integer(c(year_real, conversion_month_quarter[real_month, "quarter"]))

            testthat::expect_identical(resQuarterly, date_expected)
            testthat::expect_type(resQuarterly, "integer")
        }
    }
})

testthat::test_that("warning for double date", {
    for (warning_year in double_years) {
        for (good_month in good_months) {

            testthat::expect_warning({resQuarterly <- mens2trim(c(warning_year, good_month))}, regexp = "La date est de type double. Il faut privil\u00e9gier le format integer.")

            real_month <- (good_month - 1L) %% 12L + 1L
            year_real <- warning_year + (good_month - 1L) %/% 12L
            date_expected <- as.integer(c(year_real, conversion_month_quarter[real_month, "quarter"]))

            testthat::expect_identical(resQuarterly, date_expected)
            testthat::expect_type(resQuarterly, "integer")
        }
    }

    for (good_year in good_years) {
        for (warning_month in double_months) {

            testthat::expect_warning({resQuarterly <- mens2trim(c(good_year, warning_month))}, regexp = "La date est de type double. Il faut privil\u00e9gier le format integer.")

            real_month <- (warning_month - 1L) %% 12L + 1L
            year_real <- good_year + (warning_month - 1L) %/% 12L
            date_expected <- as.integer(c(year_real, conversion_month_quarter[real_month, "quarter"]))

            testthat::expect_identical(resQuarterly, date_expected)
            testthat::expect_type(resQuarterly, "integer")
        }
    }

    for (warning_year in double_years) {
        for (warning_month in double_months) {

            testthat::expect_warning({resQuarterly <- mens2trim(c(good_year, warning_month))}, regexp = "La date est de type double. Il faut privil\u00e9gier le format integer.")

            real_month <- (warning_month - 1L) %% 12L + 1L
            year_real <- good_year + (warning_month - 1L) %/% 12L
            date_expected <- as.integer(c(year_real, conversion_month_quarter[real_month, "quarter"]))

            testthat::expect_identical(resQuarterly, date_expected)
            testthat::expect_type(resQuarterly, "integer")
        }
    }
})

testthat::test_that("several warning", {
    for (warning_year in double_years) {
        for (warning_month in warning_integer_months) {

            w <- testthat::capture_warnings({resQuarterly <- mens2trim(c(warning_year, warning_month))})
            testthat::expect_match(object = w, regexp = "La date est de type double. Il faut privil\u00e9gier le format integer.", all = FALSE)
            testthat::expect_match(object = w, regexp = "Le nombre de p\u00e9riode est n\u00e9gatif ou nul ou d\u00e9passe la fr\u00e9quence. La date va \u00eatre reformatt\u00e9e.", all = FALSE)

            real_month <- (warning_month - 1L) %% 12L + 1L
            year_real <- warning_year + (warning_month - 1L) %/% 12L
            date_expected <- as.integer(c(year_real, conversion_month_quarter[real_month, "quarter"]))

            testthat::expect_identical(resQuarterly, date_expected)
            testthat::expect_type(resQuarterly, "integer")
        }
    }

    for (warning_year in double_years) {
        for (warning_month in warning_double_months) {

            w <- testthat::capture_warnings({resQuarterly <- mens2trim(c(warning_year, warning_month))})
            testthat::expect_match(object = w, regexp = "La date est de type double. Il faut privil\u00e9gier le format integer.", all = FALSE)
            testthat::expect_match(object = w, regexp = "Le nombre de p\u00e9riode est n\u00e9gatif ou nul ou d\u00e9passe la fr\u00e9quence. La date va \u00eatre reformatt\u00e9e.", all = FALSE)

            real_month <- (warning_month - 1L) %% 12L + 1L
            year_real <- warning_year + (warning_month - 1L) %/% 12L
            date_expected <- as.integer(c(year_real, conversion_month_quarter[real_month, "quarter"]))

            testthat::expect_identical(resQuarterly, date_expected)
            testthat::expect_type(resQuarterly, "integer")
        }
    }

    for (good_year in good_years) {
        for (warning_month in warning_double_months) {

            w <- testthat::capture_warnings({resQuarterly <- mens2trim(c(good_year, warning_month))})
            testthat::expect_match(object = w, regexp = "La date est de type double. Il faut privil\u00e9gier le format integer.", all = FALSE)
            testthat::expect_match(object = w, regexp = "Le nombre de p\u00e9riode est n\u00e9gatif ou nul ou d\u00e9passe la fr\u00e9quence. La date va \u00eatre reformatt\u00e9e.", all = FALSE)

            real_month <- (warning_month - 1L) %% 12L + 1L
            year_real <- good_year + (warning_month - 1L) %/% 12L
            date_expected <- as.integer(c(year_real, conversion_month_quarter[real_month, "quarter"]))

            testthat::expect_identical(resQuarterly, date_expected)
            testthat::expect_type(resQuarterly, "integer")
        }
    }
})
