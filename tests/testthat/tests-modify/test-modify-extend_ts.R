# Initialisation ---------------------------------------------------------------

set.seed(2023L)


# Tests de résultat avec start vecteur d'entiers -------------------------------

for (typeA in list_type) {
    for (frequenceA in list_frequence) {
        for (startA in list_start) {
            for (lenA in list_len[-1L]) {
                A_content <- create_random_type(type = typeA, len = lenA)
                for (lenB in list_len[-1L]) {
                    B_content <- create_random_type(type = typeA, len = lenB)

                    test_name <- paste("expected result with ",
                                       "typeA = '", deparse(typeA),
                                       "frequenceA = ", deparse(frequenceA),
                                       "startA = ", deparse(startA),
                                       "lenA = ", deparse(lenA),
                                       "lenB = ", deparse(lenB),
                                       sep = "\n"
                    )

                    testthat::test_that(test_name, {

                        # Cas 1 : simple
                        ts_A <- ts(A_content, start = startA, frequency = frequenceA)
                        res_theo <- ts(c(A_content, B_content), start = startA, frequency = frequenceA)
                        testthat::expect_warning(
                            {
                                res <- extend_ts(
                                    series = ts_A,
                                    replacement = B_content,
                                    date_ts = NULL,
                                    replace_na = FALSE)
                            },
                            regexp = warning_extend
                        )
                        testthat::expect_identical(expected = res_theo, object = res)

                        # Cas 2 : with with NA
                        for (param1 in list_len) {
                            ts_A <- ts(
                                c(A_content, create_NA_type(type = typeA, len = param1)),
                                start = startA, frequency = frequenceA
                            )
                            res_theo <- ts(
                                c(A_content, create_NA_type(type = typeA, len = param1), B_content),
                                start = startA, frequency = frequenceA
                            )
                            testthat::expect_warning(
                                {
                                    res <- extend_ts(series = ts_A, replacement = B_content, date_ts = NULL, replace_na = FALSE)
                                },
                                regexp = warning_extend
                            )
                            testthat::expect_identical(expected = res_theo, object = res)
                        }

                        # Cas 3 : with without NA
                        for (param1 in list_len) {
                            ts_A <- ts(
                                c(A_content, create_NA_type(type = typeA, len = param1)),
                                start = startA, frequency = frequenceA
                            )
                            res_theo <- ts(
                                c(A_content, B_content),
                                start = startA, frequency = frequenceA
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

                            testthat::expect_identical(expected = res_theo, object = res)
                        }

                        # Cas 4 : date_ts
                        # Cas 5 : date_ts with with NA
                        # Cas 6 : date_ts with without NA

                        stop("A compléter")


                    })

                }

            }
        }
    }
}


# # Tests sur les erreurs de mts --------------------------------------------
#
# testthat::test_that("Several dimensions are not allowed", {
#     for (typeA in list_type) {
#         for (frequenceA in list_frequence) {
#             for (startA in list_start) {
#                 for (lenA in list_len[-1L]) {
#                     B_content <- as.data.frame(lapply(
#                         X = 1L:5L,
#                         FUN = function(i) create_random_type(type = typeA, len = lenA)
#                     ))
#
#                     if (typeA == "complex") {
#                         mts_B <- do.call(
#                             what = cbind,
#                             args = lapply(
#                                 X = B_content,
#                                 FUN = ts,
#                                 start = startA,
#                                 frequency = frequenceA
#                             )
#                         )
#                     } else {
#                         mts_B <- ts(B_content, start = startA, frequency = frequenceA)
#                     }
#
#                     testthat::expect_error(
#                         extend_ts(
#                             series = mts_B,
#                             date = create_random_date_ts(),
#                             replacement = create_random_type(type = typeA)
#                         ),
#                         regexp = "Variable 'series': Must be of type 'atomic vector'"
#                     )
#                 }
#             }
#         }
#     }
# })
#
# # Tests sur les erreurs d'input ------------------------------------------------
#
# ## Test sur le ts --------------------------------------------------------------
#
# testthat::test_that("miscellaneous series are not allowed", {
#     for (typeA in list_type) {
#         for (obj in object_bank_R) {
#             testthat::expect_error(extend_ts(
#                 series = obj,
#                 date = create_random_date_ts(),
#                 replacement = create_random_type(type = typeA)
#             ))
#         }
#     }
# })
#
# ## Test sur la date ------------------------------------------------------------
#
# testthat::test_that("miscellaneous date are not allowed", {
#     for (typeA in list_type) {
#         for (wrong_date in list_wrong_date_ts) {
#             testthat::expect_error(extend_ts(
#                 series = create_random_ts(type = typeA),
#                 date = wrong_date,
#                 replacement = create_random_type(type = typeA)
#             ))
#         }
#     }
# })
#
# ## Test sur le vecteur value ---------------------------------------------------
#
# testthat::test_that("miscellaneous value input are not allowed", {
#     list_wrong_value <- c(fuzzr::test_df()[-4L], NULL, character(0L), numeric(0L), logical(0L), integer(0L), complex(0L))
#     for (typeA in list_type) {
#         for (value in list_wrong_value) {
#             testthat::expect_error(extend_ts(
#                 series = create_random_ts(type = typeA),
#                 date = create_random_date_ts(),
#                 replacement = value
#             ))
#         }
#     }
# })
#
# testthat::test_that("value should have same type as series", {
#     for (typeA in list_type[-7L]) {
#         for (typeB in list_type[-7L]) {
#             if (typeA != typeB) {
#                 testthat::expect_error(
#                     extend_ts(
#                         series = create_random_ts(type = typeA),
#                         date = create_random_date_ts(),
#                         replacement = create_random_type(typeB)
#                     ),
#                     regexp = "Les objets `series` et `replacement` doivent \u00eatre de m\u00eame type."
#                 )
#             }
#         }
#     }
# })
#
# testthat::test_that("NA values generate warning", {
#     for (typeA in list_type[-6L]) {
#         ts_A <- create_random_ts(type = typeA, start = 2000L, len = 80L, frequency = 4L)
#         v1 <- sample(c(create_random_type(typeA, len = 10L), get(paste0("as.", typeA))(rep(NA, 5L))), replace = TRUE)
#         v2 <- get(paste0("as.", typeA))(rep(NA, 5L))
#
#         testthat::expect_warning(extend_ts(series = ts_A, date = 2010L, replacement = v1),
#                                  regexp = "Contains missing values"
#         )
#         testthat::expect_warning(extend_ts(series = ts_A, date = 2010L, replacement = v2),
#                                  regexp = "Contains missing values"
#         )
#     }
# })
#
# # Tests sur les erreurs de temporalité --------------------------------------------
#
# testthat::test_that("series and date are temporally consistent", {
#     for (typeA in list_type) {
#         testthat::expect_error(extend_ts(
#             series = create_random_ts(type = typeA, start = 2010 + 1 / 7, frequency = 12L),
#             date = create_random_date_ts(),
#             replacement = create_random_type(type = typeA)
#         ))
#     }
#
#     for (typeA in list_type) {
#         testthat::expect_error(extend_ts(
#             series = create_random_ts(type = typeA, start = 2022 + 1 / 5, frequency = 4L),
#             date = create_random_date_ts(),
#             replacement = create_random_type(type = typeA)
#         ))
#     }
# })
