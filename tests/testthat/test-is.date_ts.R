
# Initialisation ---------------------------------------------------------------

# stop("Il faut compléter avec l'utilisation de l'argumetn frequency pour donner de nouveaux cas d'erreur et dupliquer les tests avec ou sans l'argument.")
# stop("Faire les vérifs avec frequency")

set.seed(2024L)


# Tests de résultats positifs --------------------------------------------------

testthat::test_that("good result for integer date month", {
    for (year in good_years) {
        testthat::expect_true(is_date_ts(year, frequency = 12L))
        for (month in good_months) {
            testthat::expect_true(is_date_ts(c(year, month), frequency = 12L))
        }
    }
})

testthat::test_that("good result for integer date quarter", {
    for (year in good_years) {
        testthat::expect_true(is_date_ts(year, frequency = 4L))
        for (quarter in list_good_quarters) {
            testthat::expect_true(is_date_ts(c(year, quarter), frequency = 4L))
        }
    }
})


# Tests positifs avec warning --------------------------------------------------

## Mensuel -----------------------------------------------------------------

testthat::test_that("warning for integer date", {
    for (good_year in good_years) {
        for (warning_month in warning_integer_months) {
            warning_date <- c(good_year, warning_month)
            testthat::expect_warning({boolRes <- is_date_ts(warning_date)}, regexp = "Le nombre de p\u00e9riode est n\u00e9gatif ou nul ou d\u00e9passe la fr\u00e9quence. La date va \u00eatre reformatt\u00e9e.")
            testthat::expect_true(boolRes)
        }
    }
})

testthat::test_that("warning for double date", {
    for (warning_year in double_years) {

        warning_date <- warning_year
        testthat::expect_warning({boolRes <- is_date_ts(warning_date)}, regexp = "La date est de type double. Il faut privil\u00e9gier le format integer.")
        testthat::expect_true(boolRes)

        for (good_month in good_months) {

            warning_date <- c(warning_year, good_month)
            testthat::expect_warning({boolRes <- is_date_ts(warning_date)}, regexp = "La date est de type double. Il faut privil\u00e9gier le format integer.")
            testthat::expect_true(boolRes)

        }
    }

    for (good_year in good_years) {
        for (warning_month in double_months) {

            warning_date <- c(good_year, warning_month)
            testthat::expect_warning({boolRes <- is_date_ts(warning_date)}, regexp = "La date est de type double. Il faut privil\u00e9gier le format integer.")
            testthat::expect_true(boolRes)

        }
    }

    for (warning_year in double_years) {
        for (warning_month in double_months) {

            warning_date <- c(warning_year, warning_month)
            testthat::expect_warning({boolRes <- is_date_ts(warning_date)}, regexp = "La date est de type double. Il faut privil\u00e9gier le format integer.")
            testthat::expect_true(boolRes)

        }
    }
})

testthat::test_that("several warning", {
    for (warning_year in double_years) {
        for (warning_month in warning_integer_months) {

            warning_date <- c(warning_year, warning_month)
            w <- testthat::capture_warnings({boolRes <- is_date_ts(warning_date)})
            testthat::expect_match(object = w, regexp = "La date est de type double. Il faut privil\u00e9gier le format integer.", all = FALSE)
            testthat::expect_match(object = w, regexp = "Le nombre de p\u00e9riode est n\u00e9gatif ou nul ou d\u00e9passe la fr\u00e9quence. La date va \u00eatre reformatt\u00e9e.", all = FALSE)

            testthat::expect_true(boolRes)
        }
    }

    for (warning_year in double_years) {
        for (warning_month in warning_double_months) {

            warning_date <- c(warning_year, warning_month)
            w <- testthat::capture_warnings({boolRes <- is_date_ts(warning_date)})
            testthat::expect_match(object = w, regexp = "La date est de type double. Il faut privil\u00e9gier le format integer.", all = FALSE)
            testthat::expect_match(object = w, regexp = "Le nombre de p\u00e9riode est n\u00e9gatif ou nul ou d\u00e9passe la fr\u00e9quence. La date va \u00eatre reformatt\u00e9e.", all = FALSE)

            testthat::expect_true(boolRes)
        }
    }

    for (good_year in good_years) {
        for (warning_month in warning_double_months) {

            warning_date <- c(warning_year, warning_month)
            w <- testthat::capture_warnings({boolRes <- is_date_ts(warning_date)})
            testthat::expect_match(object = w, regexp = "La date est de type double. Il faut privil\u00e9gier le format integer.", all = FALSE)
            testthat::expect_match(object = w, regexp = "Le nombre de p\u00e9riode est n\u00e9gatif ou nul ou d\u00e9passe la fr\u00e9quence. La date va \u00eatre reformatt\u00e9e.", all = FALSE)

            testthat::expect_true(boolRes)
        }
    }
})


## Trimestriel -----------------------------------------------------------------

testthat::test_that("warning for integer date", {
    for (good_year in good_years) {
        for (warning_quarter in warning_integer_quarters) {
            warning_date <- c(good_year, warning_quarter)
            testthat::expect_warning({boolRes <- is_date_ts(warning_date, frequency = 4L)}, regexp = "Le nombre de p\u00e9riode est n\u00e9gatif ou nul ou d\u00e9passe la fr\u00e9quence. La date va \u00eatre reformatt\u00e9e.")
            testthat::expect_true(boolRes)
        }
    }
})

testthat::test_that("warning for double date", {
    for (warning_year in double_years) {

        warning_date <- warning_year
        testthat::expect_warning({boolRes <- is_date_ts(warning_date, frequency = 4L)}, regexp = "La date est de type double. Il faut privil\u00e9gier le format integer.")
        testthat::expect_true(boolRes)

        for (good_quarter in list_good_quarters) {

            warning_date <- c(warning_year, good_quarter)
            testthat::expect_warning({boolRes <- is_date_ts(warning_date, frequency = 4L)}, regexp = "La date est de type double. Il faut privil\u00e9gier le format integer.")
            testthat::expect_true(boolRes)

        }
    }

    for (good_year in good_years) {
        for (warning_quarter in double_quarters) {

            warning_date <- c(good_year, warning_quarter)
            testthat::expect_warning({boolRes <- is_date_ts(warning_date, frequency = 4L)}, regexp = "La date est de type double. Il faut privil\u00e9gier le format integer.")
            testthat::expect_true(boolRes)

        }
    }

    for (warning_year in double_years) {
        for (warning_quarter in double_quarters) {

            warning_date <- c(warning_year, warning_quarter)
            testthat::expect_warning({boolRes <- is_date_ts(warning_date, frequency = 4L)}, regexp = "La date est de type double. Il faut privil\u00e9gier le format integer.")
            testthat::expect_true(boolRes)

        }
    }
})

testthat::test_that("several warning", {
    for (warning_year in double_years) {
        for (warning_quarter in warning_integer_quarters) {

            warning_date <- c(warning_year, warning_quarter)
            w <- testthat::capture_warnings({boolRes <- is_date_ts(warning_date, frequency = 4L)})
            testthat::expect_match(object = w, regexp = "La date est de type double. Il faut privil\u00e9gier le format integer.", all = FALSE)
            testthat::expect_match(object = w, regexp = "Le nombre de p\u00e9riode est n\u00e9gatif ou nul ou d\u00e9passe la fr\u00e9quence. La date va \u00eatre reformatt\u00e9e.", all = FALSE)

            testthat::expect_true(boolRes)
        }
    }

    for (warning_year in double_years) {
        for (warning_quarter in warning_double_quarters) {

            warning_date <- c(warning_year, warning_quarter)
            w <- testthat::capture_warnings({boolRes <- is_date_ts(warning_date, frequency = 4L)})
            testthat::expect_match(object = w, regexp = "La date est de type double. Il faut privil\u00e9gier le format integer.", all = FALSE)
            testthat::expect_match(object = w, regexp = "Le nombre de p\u00e9riode est n\u00e9gatif ou nul ou d\u00e9passe la fr\u00e9quence. La date va \u00eatre reformatt\u00e9e.", all = FALSE)

            testthat::expect_true(boolRes)
        }
    }

    for (good_year in good_years) {
        for (warning_quarter in warning_double_quarters) {

            warning_date <- c(warning_year, warning_quarter)
            w <- testthat::capture_warnings({boolRes <- is_date_ts(warning_date, frequency = 4L)})
            testthat::expect_match(object = w, regexp = "La date est de type double. Il faut privil\u00e9gier le format integer.", all = FALSE)
            testthat::expect_match(object = w, regexp = "Le nombre de p\u00e9riode est n\u00e9gatif ou nul ou d\u00e9passe la fr\u00e9quence. La date va \u00eatre reformatt\u00e9e.", all = FALSE)

            testthat::expect_true(boolRes)
        }
    }
})


# Tests de résultats négatifs --------------------------------------------------


testthat::test_that("detection of wrong dates", {
    for (wrong_date in wrong_dates) testthat::expect_false(is_date_ts(wrong_date))
})

testthat::test_that("miscellaneous frequency are not allowed", {
    for (wrong_frequency in c(object_bank_R, weird_frequency)) {
        testthat::expect_error(is_date_ts(date_ts = create_random_date(), frequency = wrong_frequency),
                               regexp = "La fr\u00e9quence doit \u00eatre trimestrielle ou mensuelle.")
    }
})
