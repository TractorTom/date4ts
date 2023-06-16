#
# # Initialisation ---------------------------------------------------------------
#
# set.seed(2032L)
#
#
# # Tests de résultats positifs = TRUE -------------------------------------------
#
# testthat::test_that("Result TRUE expected with good TS", {
#     for (typeA in list_type) {
#         for (frequenceA in list_frequence) {
#             for (startA in list_start) {
#                 for (lenA in list_len) {
#                     ts_A <- create_random_ts(type = typeA, len = lenA, frequency = frequenceA,
#                                              start = startA)
#                     testthat::expect_true(assert_ts(ts_A, warn = TRUE))
#                     testthat::expect_true(assert_ts(ts_A, warn = FALSE))
#                 }
#             }
#         }
#     }
# })
#
#
# # Tests de résultats n\u00e9gatif = FALSE -------------------------------------------
#
# ## Tests avec warn = TRUE -----------------------------------------------
#
# ### Mauvais objet R ------------------------------------------------------------
#
# testthat::test_that("Result FALSE expected with wrong object R", {
#     for (wrong_ts in object_bank_R) {
#         testthat::expect_warning({res_logical <- assert_ts(wrong_ts, warn = TRUE)}, regexp = "L'objet dataTS doit \u00eatre un ts unidimensionnel.")
#         testthat::expect_false(res_logical)
#     }
# })
#
# ### MTS ------------------------------------------------------------------------
#
# testthat::test_that("Result FALSE expected with mts", {
#     for (typeA in list_type) {
#         for (lenA in list_len) {
#             for (frequenceA in list_frequence) {
#                 for (len2 in list_len[-c(1L, 5L:6L)]) {
#
#                     A_content <- as.data.frame(lapply(1L:len2, function(i) create_random_type(type = typeA, len = lenA)))
#                     startA <- create_random_date()
#                     if (typeA == "complex") {
#                         mts_A <- do.call(
#                             what = cbind,
#                             args = lapply(X = A_content,
#                                           FUN = ts,
#                                           start = startA,
#                                           frequency = frequenceA)
#                         )
#                     } else {
#                         mts_A <- ts(A_content, start = startA, frequency = frequenceA)
#                     }
#
#                     testthat::expect_warning({res_logical <- assert_ts(mts_A, warn = TRUE)}, regexp = "L'objet dataTS doit \u00eatre un ts unidimensionnel.")
#                     testthat::expect_false(res_logical)
#                 }
#             }
#         }
#     }
# })
#
# ### Mauvaise fréquence ---------------------------------------------------------
#
# testthat::test_that("Result FALSE expected with wrong frequency", {
#     for (wrong_freq in weird_frequency) {
#         for (typeA in list_type) {
#             for (startA in list_start) {
#                 for (lenA in list_len) {
#                     ts_A <- create_random_ts(type = typeA, len = lenA,
#                                              frequency = wrong_freq,
#                                              start = startA)
#                     testthat::expect_warning({res_logical <- assert_ts(ts_A, warn = TRUE)}, regexp = "L'objet dataTS doit \u00eatre de fr\u00e9quence mensuelle ou trimestrielle.")
#                     testthat::expect_false(res_logical)
#                 }
#             }
#         }
#     }
# })
#
# ### Pb de cohérence temporelle -------------------------------------------------
#
# testthat::test_that("Result FALSE expected with wrong frequency", {
#     for (wrong_start in wrong_dates) {
#         for (typeA in list_type) {
#             for (frequenceA in list_frequence) {
#                 for (lenA in list_len) {
#                     ts_A <- create_random_ts(type = typeA, len = lenA, frequency = frequenceA,
#                                              start = wrong_start)
#                     testthat::expect_warning({res_logical <- assert_ts(ts_A, warn = TRUE)}, regexp = "L'objet dataTS doit \u00eatre coh\u00e9rent avec la temporalit\u00e9 classique.")
#                     testthat::expect_false(res_logical)
#                 }
#             }
#         }
#     }
# })
#
# ### Mauvais type ---------------------------------------------------------------
#
# testthat::test_that("Result FALSE expected with wrong type of ts", {
#     for (wrong_ts in wrong_type_ts) {
#         testthat::expect_warning({res_logical <- assert_ts(wrong_ts, warn = TRUE)}, regexp = "L'objet dataTS doit \u00eatre d'un type atomic.")
#         testthat::expect_false(res_logical)
#     }
# })
#
# ## Tests avec warn = FALSE ----------------------------------------------
#
# ### Mauvais objet R ------------------------------------------------------------
#
# testthat::test_that("Result FALSE expected with wrong object R", {
#     for (wrong_ts in object_bank_R) {
#         testthat::expect_false(assert_ts(wrong_ts, warn = FALSE))
#     }
# })
#
# ### mts ------------------------------------------------------------------------
#
# testthat::test_that("Result FALSE expected with mts", {
#     for (typeA in list_type) {
#         for (lenA in list_len) {
#             for (frequenceA in list_frequence) {
#                 for (len2 in list_len[-c(1L, 5L:6L)]) {
#
#                     if (typeA != "complex") {
#                         A_content <- as.data.frame(lapply(1L:len2, function(i) create_random_type(type = typeA, len = lenA)))
#                         mts_A <- ts(A_content, start = create_random_date(), frequency = frequenceA)
#                     } else {
#                         mts_A <- c()
#                         for (k in 1L:len2) mts_A <- cbind(mts_A, as.ts(create_random_type(type = typeA, len = lenA)))
#                     }
#
#                     testthat::expect_false(assert_ts(mts_A, warn = FALSE))
#                 }
#             }
#         }
#
#     }
# })
#
# ### Mauvaise fréquence ---------------------------------------------------------
#
# testthat::test_that("Result FALSE expected with wrong frequency", {
#     for (wrong_freq in weird_frequency) {
#         for (typeA in list_type) {
#             for (startA in list_start) {
#                 for (lenA in list_len) {
#                     ts_A <- create_random_ts(type = typeA, len = lenA, frequency = wrong_freq,
#                                              start = startA)
#                     testthat::expect_false(assert_ts(ts_A, warn = FALSE))
#                 }
#             }
#         }
#     }
# })
#
# ### Pb de cohérence temporelle -------------------------------------------------
#
# testthat::test_that("Result FALSE expected with wrong frequency", {
#     for (wrong_start in wrong_dates) {
#         for (typeA in list_type) {
#             for (frequenceA in list_frequence) {
#                 for (lenA in list_len) {
#                     ts_A <- create_random_ts(type = typeA, len = lenA, frequency = frequenceA,
#                                              start = wrong_start)
#                     testthat::expect_false(assert_ts(ts_A, warn = FALSE))
#                 }
#             }
#         }
#     }
# })
#
# ### Mauvais type ---------------------------------------------------------------
#
# testthat::test_that("Result FALSE expected with wrong type of ts", {
#     for (wrong_ts in wrong_type_ts) {
#         testthat::expect_false(assert_ts(wrong_ts, warn = FALSE))
#     }
# })
#
# # Tests avec erreur (mauvais warn) --------------------------------------
#
# testthat::test_that("Expect error for wrong warn argument", {
#     for (typeA in list_type) {
#         for (frequenceA in list_frequence) {
#             for (startA in list_start) {
#                 for (lenA in list_len) {
#                     for (wrong_arg in object_bank_R[-29L]) {
#                         ts_A <- create_random_ts(type = typeA, len = lenA,
#                                                  frequency = frequenceA,
#                                                  start = startA)
#                         testthat::expect_error(assert_ts(ts_A, warn = wrong_arg), regexp = "L'argument warn doit \u00eatre un bool\u00e6en de longueur 1.")
#                     }
#                 }
#             }
#         }
#     }
# })
#
