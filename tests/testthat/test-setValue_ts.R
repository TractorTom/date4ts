#
# # Initialisation ---------------------------------------------------------------
#
# set.seed(2023L)
#
#
# # Tests de résultat avec start vecteur d'entiers -------------------------------
#
# for (typeA in list_type) {
#     for (frequenceA in list_frequence) {
#         for (startA in list_start) {
#             for (lenA in list_len) {
#                 A_content <- create_random_type(type = typeA, len = lenA)
#                 ts_A <-  ts(A_content, start = startA, frequency = frequenceA)
#                 for (lagB in list_lag) {
#                     for (lenB in list_len) {
#
#                         test_name <- paste("expected result with ",
#                                            "\ntypeA = '", typeA,
#                                            "'\nfrequenceA = ", frequenceA,
#                                            "\nstartA = ", deparse(startA),
#                                            "\nlenA = ", lenA,
#                                            "\nlagB = ", lagB,
#                                            "\nlenB = ", lenB, sep = "")
#
#                         testthat::test_that(test_name, {
#
#                             valueB <- create_random_type(type = typeA, len = lenB)
#                             startB <- format_date_ts(
#                                 date_ts = c(startA[1L], startA[2L] + lagB),
#                                 frequency = frequenceA)
#                             ts_B <- ts(valueB, start = startB, frequency = frequenceA)
#
#                             #Cas où lagB >= 0 (start = startA)
#                             if (lagB > 0L) {
#                                 if (lagB > lenA) { #Cas 4
#
#                                     if (typeA == "raw") {
#                                         ts_ResAB1 <- ts(c(A_content, rep(as.raw(0L), lagB - lenA), valueB), start = startA, frequency = frequenceA)
#
#                                         testthat::expect_warning({
#                                             testthat::expect_warning({resAB <- setValue_ts(ts_A, date = startB, value = valueB)},
#                                                                      regexp = "extending time series when replacing values")},
#                                             regexp = "out-of-range values treated as 0 in coercion to raw")
#                                         testthat::expect_warning({
#                                             testthat::expect_warning({ts_ResAB2 <- combine2ts(ts_A, ts_B)},
#                                                                      regexp = "extending time series when replacing values")},
#                                             regexp = "out-of-range values treated as 0 in coercion to raw")
#
#                                     } else {
#                                         ts_ResAB1 <- ts(c(A_content, rep(NA, lagB - lenA), valueB), start = startA, frequency = frequenceA)
#
#                                         testthat::expect_warning({resAB <- setValue_ts(ts_A, date = startB, value = valueB)},
#                                                                  regexp = "extending time series when replacing values")
#                                         testthat::expect_warning({ts_ResAB2 <- combine2ts(ts_A, ts_B)},
#                                                                  regexp = "extending time series when replacing values")
#                                     }
#
#
#                                 } else if (lagB + lenB < lenA) { #Cas 6
#                                     ts_ResAB1 <- ts(c(A_content[1L:lagB], valueB, A_content[(lagB + lenB + 1):lenA]), start = startA, frequency = frequenceA)
#                                     ts_ResAB2 <- combine2ts(ts_A, ts_B)
#                                     resAB <- setValue_ts(ts_A, date = startB, value = valueB)
#                                 } else if (lagB + lenB == lenA) { #Autres cas
#                                     ts_ResAB1 <- ts(c(A_content[1L:lagB], valueB), start = startA, frequency = frequenceA)
#                                     ts_ResAB2 <- combine2ts(ts_A, ts_B)
#                                     resAB <- setValue_ts(ts_A, date = startB, value = valueB)
#                                 } else { #Cas1
#
#                                     ts_ResAB1 <- ts(c(A_content[1L:lagB], valueB), start = startA, frequency = frequenceA)
#                                     testthat::expect_warning({ts_ResAB2 <- combine2ts(ts_A, ts_B)},
#                                                              regexp = "extending time series when replacing values")
#                                     testthat::expect_warning({resAB <- setValue_ts(ts_A, date = startB, value = valueB)},
#                                                              regexp = "extending time series when replacing values")
#
#                                 }
#
#                             } else if (lagB < 0L) { #Cas où start = strartB < startA
#
#                                 if (lagB + lenB < 0L) { #Cas 3
#
#                                     if (typeA == "raw") {
#                                         ts_ResAB1 <- ts(c(valueB, rep(as.raw(0L), -lagB - lenB), A_content), start = startB, frequency = frequenceA)
#
#                                         testthat::expect_warning({
#                                             testthat::expect_warning({resAB <- setValue_ts(ts_A, date = startB, value = valueB)},
#                                                                      regexp = "extending time series when replacing values")},
#                                             regexp = "out-of-range values treated as 0 in coercion to raw")
#                                         testthat::expect_warning({
#                                             testthat::expect_warning({ts_ResAB2 <- combine2ts(ts_A, ts_B)},
#                                                                      regexp = "extending time series when replacing values")},
#                                             regexp = "out-of-range values treated as 0 in coercion to raw")
#
#                                     } else {
#                                         ts_ResAB1 <- ts(c(valueB, rep(NA, -lagB - lenB), A_content), start = startB, frequency = frequenceA)
#                                         testthat::expect_warning({ts_ResAB2 <- combine2ts(ts_A, ts_B)},
#                                                                  regexp = "extending time series when replacing values")
#                                         testthat::expect_warning({resAB <- setValue_ts(ts_A, date = startB, value = valueB)},
#                                                                  regexp = "extending time series when replacing values")
#                                     }
#
#                                 } else if (lagB + lenB >= lenA) { #Cas 5
#                                     ts_ResAB1 <- ts(valueB, start = startB, frequency = frequenceA)
#                                     testthat::expect_warning({ts_ResAB2 <- combine2ts(ts_A, ts_B)},
#                                                              regexp = "extending time series when replacing values")
#                                     testthat::expect_warning({resAB <- setValue_ts(ts_A, date = startB, value = valueB)},
#                                                              regexp = "extending time series when replacing values")
#                                 } else { #Cas 2
#                                     ts_ResAB1 <- ts(c(valueB, A_content[(lagB + lenB + 1):lenA]), start = startB, frequency = frequenceA)
#                                     testthat::expect_warning({ts_ResAB2 <- combine2ts(ts_A, ts_B)},
#                                                              regexp = "extending time series when replacing values")
#                                     testthat::expect_warning({resAB <- setValue_ts(ts_A, date = startB, value = valueB)},
#                                                              regexp = "extending time series when replacing values")
#                                 }
#
#                             } else { #Cas où lag = 0
#                                 if (lenB < lenA) {
#                                     ts_ResAB1 <- ts(c(valueB, A_content[(lenB + 1):lenA]), start = startA, frequency = frequenceA)
#                                     ts_ResAB2 <- combine2ts(ts_A, ts_B)
#                                     resAB <- setValue_ts(ts_A, date = startB, value = valueB)
#                                 } else if (lenB == lenA) {
#                                     ts_ResAB1 <- ts(valueB, start = startA, frequency = frequenceA)
#                                     ts_ResAB2 <- combine2ts(ts_A, ts_B)
#                                     resAB <- setValue_ts(ts_A, date = startB, value = valueB)
#                                 } else {
#                                     ts_ResAB1 <- ts(valueB, start = startA, frequency = frequenceA)
#                                     testthat::expect_warning({ts_ResAB2 <- combine2ts(ts_A, ts_B)},
#                                                              regexp = "extending time series when replacing values")
#                                     testthat::expect_warning({resAB <- setValue_ts(ts_A, date = startB, value = valueB)},
#                                                              regexp = "extending time series when replacing values")
#
#                                 }
#                             }
#
#                             if (typeA != "Date") {
#                                 testthat::expect_type(resAB, typeA)
#                             }
#
#                             testthat::expect_equal(resAB,ts_ResAB1)
#                             testthat::expect_equal(resAB,ts_ResAB2)
#
#                         })
#
#                     }
#                 }
#             }
#         }
#     }
# }
#
#
# # Tests sur les erreurs de mts --------------------------------------------
#
# testthat::test_that("Several dimensions are not allowed", {
#     for (typeA in list_type) {
#         for (frequenceA in list_frequence) {
#             for (startA in list_start) {
#                 for (lenA in list_len) {
#
#                     B_content <- as.data.frame(lapply(1L:5L, function(i) create_random_type(type = typeA, len = lenA)))
#
#                     if (typeA == "complex") {
#                         mts_B <- do.call(
#                             what = cbind,
#                             args = lapply(X = B_content,
#                                           FUN = ts,
#                                           start = startA,
#                                           frequency = frequenceA)
#                         )
#                     } else {
#                         mts_B <- ts(B_content, start = startA, frequency = frequenceA)
#                     }
#
#                     testthat::expect_error(setValue_ts(dataTS = mts_B,
#                                                        date = create_random_date(),
#                                                        value = create_random_type(type = typeA)),
#                                            regexp = "L'objet `dataTS` doit \u00eatre un ts unidimensionnel de fr\u00e9quence mensuelle ou trimestrielle.")
#
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
# testthat::test_that("miscellaneous dataTS are not allowed", {
#     for (typeA in list_type) {
#         for (obj in object_bank_R) {
#             testthat::expect_error(setValue_ts(dataTS = obj,
#                                                date = create_random_date(),
#                                                value = create_random_type(type = typeA)),
#                                    regexp = "L'objet `dataTS` doit \u00eatre un ts unidimensionnel de fr\u00e9quence mensuelle ou trimestrielle.")
#         }
#     }
# })
#
# ## Test sur la date ------------------------------------------------------------
#
# testthat::test_that("miscellaneous date are not allowed", {
#     for (typeA in list_type) {
#         for (wrong_date in wrong_dates) {
#             testthat::expect_error(setValue_ts(dataTS = create_random_ts(type = typeA),
#                                                date = wrong_date,
#                                                value = create_random_type(type = typeA)),
#                                    regexp = "La date est au mauvais format.")
#         }
#     }
# })
#
# ## Test sur le vecteur value ---------------------------------------------------
#
# testthat::test_that("miscellaneous value input are not allowed", {
#     list_wrong_value <- c(fuzzr::test_df()[-4], NULL, character(0L), numeric(0L), logical(0L), integer(0L), complex(0L))
#     for (typeA in list_type) {
#         for (value in list_wrong_value) {
#             testthat::expect_error(setValue_ts(dataTS = create_random_ts(type = typeA),
#                                                date = create_random_date(),
#                                                value = value),
#                                    regexp = "L'argument value doit \u00eatre unidimensionnel.")
#         }
#     }
# })
#
# testthat::test_that("value should have same type as dataTS", {
#     for (typeA in list_type[-7]) {
#         for (typeB in list_type[-7]) {
#             if (typeA != typeB) {
#                 testthat::expect_error(setValue_ts(dataTS = create_random_ts(type = typeA),
#                                                    date = create_random_date(),
#                                                    value = create_random_type(typeB)),
#                                        regexp = "Les objets dataTS et value doivent \u00eatre de m\u00eame type.")
#             }
#         }
#     }
# })
#
# testthat::test_that("NA values generate warning", {
#     for (typeA in list_type[-6L]) {
#
#         ts_A <- create_random_ts(type = typeA, start = 2000L, len = 80L, frequency = 4L)
#         v1 <- sample(c(create_random_type(typeA, len = 10L), get(paste0("as.", typeA))(rep(NA, 5L))), replace = TRUE)
#         v2 <- get(paste0("as.", typeA))(rep(NA, 5L))
#
#         testthat::expect_warning(setValue_ts(dataTS = ts_A, date = 2010L, value = v1),
#                                  regexp = "L'argument value contient des NAs.")
#         testthat::expect_warning(setValue_ts(dataTS = ts_A, date = 2010L, value = v2),
#                                  regexp = "L'argument value contient des NAs.")
#     }
# })
#
# # Tests sur les erreurs de temporalité --------------------------------------------
#
# testthat::test_that("dataTS and date are temporally consistent", {
#     for (typeA in list_type) {
#         testthat::expect_error(setValue_ts(dataTS = create_random_ts(type = typeA, start = 2010 + 1/7, frequency = 12L),
#                                            date = create_random_date(),
#                                            value = create_random_type(type = typeA)),
#                                regexp = "L'objet `dataTS` doit \u00eatre un ts unidimensionnel de fr\u00e9quence mensuelle ou trimestrielle.")
#     }
#
#     for (typeA in list_type) {
#         testthat::expect_error(setValue_ts(dataTS = create_random_ts(type = typeA, start = 2022 + 1/5, frequency = 4L),
#                                            date = create_random_date(),
#                                            value = create_random_type(type = typeA)),
#                                regexp = "L'objet `dataTS` doit \u00eatre un ts unidimensionnel de fr\u00e9quence mensuelle ou trimestrielle.")
#     }
# })
#
