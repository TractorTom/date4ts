
# Initialisation ---------------------------------------------------------------

set.seed(2024L)

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

liste_type <- c("integer", "character", "double", "logical", "complex", "raw", "Date")


# Tests de résultats positifs --------------------------------------------------

good_year <- c(-200L, -1L, 0L, 1L, 2L, 1950L, 2000L, 2022L, 3000L)
good_mois <- c(-200L, -5L, -1L, 0L, 1L, 3L, 5L, 12L, 13L, 46L)
good_dates <- do.call(c, lapply(X = good_year, FUN = \(annee) lapply(X = good_mois, FUN = c, annee)))

testthat::test_that("good result for integer date", {
    for (good_date in good_dates) testthat::expect_true(isTSdate(good_date))
})


# Tests de résultats négatifs --------------------------------------------------

wrong_dates <- c(
    fuzzr::test_all()[-10],
    list(list(2020L, 5L), list(2L, "a", 3.5), list(NULL), list(2005), list(c(2022L, 8L)), list(c(2022L, 8.))),
    lapply(liste_type[-c(1L, 3L)], create_random_type, len = 2),
    lapply(liste_type[-c(1L, 3L)], create_random_type, len = 3),
    list(2019.5, 2020 + 1/12, pi / 4, c(2020, 2.5), c(2010.25, 3), c(2002, 3, 1), c("2002", "3")),
    list(c(2020L, NA_integer_), c(NA_integer_, 5L), c(NA_integer_, NA_integer_), c(2020, NA_real_), c(NA_real_, 5), c(NA_real_, NA_real_)),
    list(2L:4L, c(2020.0, 7, 1), c(2020L, 0L, NA_integer_), numeric(0), integer(0))
)

testthat::test_that("detection of wrong dates", {
    for (wrong_date in wrong_dates) testthat::expect_false(isTSdate(wrong_date))
})

# Tests positifs avec warning --------------------------------------------------

warning_year <- c(-200, -1, 0, 1, 2, 1950, 2000, 2022, 3000)
warning_mois <- c(-200, -5, -1, 0, 1, 3, 5, 12, 13, 46)
warning_dates <- do.call(c, lapply(X = warning_year, FUN = \(annee) lapply(X = warning_mois, FUN = c, annee)))

testthat::test_that("good result for integer date", {
    for (warning_date in warning_dates) {
        testthat::expect_warning({boolRes <- isTSdate(warning_date)}, regexp = "La date est de type double. Il faut privilégier le format integer.")
        testthat::expect_true(boolRes)
    }
})




