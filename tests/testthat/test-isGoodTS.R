#
# # Initialisation ---------------------------------------------------------------
#
# set.seed(2032L)
#
# create_random_type <- function(type, len = NULL) {
#     if (is.null(len)) len <- sample(1L:1000L, size = 1)
#     if (type == "character") return(strsplit(intToUtf8(sample(c(1L:55295L, 57344L:1114111L), size = len, replace = TRUE)), "")[[1]])
#     if (type == "integer") return(sample(-20000000L:20000000L, size = len, replace = TRUE))
#     if (type == "double") return(runif(n = len, min = -10000L, max = 10000L))
#     if (type == "logical") return(sample(x = c(TRUE, FALSE), size = len, replace = TRUE))
#     if (type == "complex") return(complex(real = runif(n = len, min = -10000L, max = 10000),
#                                           imaginary = runif(n = len, min = -10000L, max = 10000L)))
#     if (type == "raw") return(sample(x = as.raw(0L:255L), size = len, replace = TRUE))
#     if (type == "Date") return(sample(x = seq(as.Date('1950/01/01'), as.Date('2022/01/01'), by = "day"), size = len, replace = T))
#     stop("Le type n'est pas reconnu.")
# }
#
# create_random_date <- function() {
#     if (runif(1, 0, 1) > .5) return(sample(1950L:2022L, size = 1L))
#     return(c(sample(1950L:2022L, size = 1L),
#              sample(-20L:20L, size = 1L)))
# }
#
# create_random_ts <- function(type, len = NULL, start = NULL, frequency = NULL) {
#     if (is.null(len)) len <- sample(1L:1000L, size = 1)
#     if (is.null(frequency)) frequency <- sample(c(4L, 12L), size = 1)
#     if (is.null(start)) start <- create_random_date()
#
#     content <- create_random_type(type, len)
#
#     return(ts(content, start = start, frequency = frequency))
# }
#
# list_type <- c("integer", "character", "double", "logical", "complex", "raw", "Date")
# list_len <- c(1L, 2L, 5L, 10L, 100L, 10000L)
# list_frequence <- c(4L, 12L)
# list_start <- list(c(2020L, -1L), c(2020L, 0L), c(2020L, 4L), c(2020L, 5L), c(2020L, 12L), c(2020L, 13L))
#
# weird_frequency <- list(1L, 2, 7, .1, 1/3, 3.5, 365, 365.25, pi)
# object_bank_R <- fuzzr::test_all()
# wrong_dates <- list(2020 + 1/7, pi, 2020 - 1/13)
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
#                     testthat::expect_true(isGoodTS(ts_A, withWarning = TRUE))
#                     testthat::expect_true(isGoodTS(ts_A, withWarning = FALSE))
#                 }
#             }
#         }
#     }
# })
#
#
# # Tests de résultats négatif = FALSE -------------------------------------------
#
# ## Tests avec withWarning = TRUE -----------------------------------------------
#
# ### Mauvais objet R ------------------------------------------------------------
#
# testthat::test_that("Result FALSE expected with wrong object R", {
#     for (wrong_ts in object_bank_R) {
#         testthat::expect_warning({res_logical <- isGoodTS(wrong_ts, withWarning = TRUE)}, regexp = "L'objet dataTS doit \u00eatre un ts unidimensionnel.")
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
#                         mts_A <- lapply(A_content, FUN = ts, start = startA, frequency = frequenceA) |>
#                             do.call(cbind, args = _)
#                     } else {
#                         mts_A <- ts(A_content, start = startA, frequency = frequenceA)
#                     }
#
#                     testthat::expect_warning({res_logical <- isGoodTS(mts_A, withWarning = TRUE)}, regexp = "L'objet dataTS doit \u00eatre un ts unidimensionnel.")
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
#                     testthat::expect_warning({res_logical <- isGoodTS(ts_A, withWarning = TRUE)}, regexp = "L'objet dataTS doit \u00eatre de fr\u00e9quence mensuelle ou trimestrielle.")
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
#                     testthat::expect_warning({res_logical <- isGoodTS(ts_A, withWarning = TRUE)}, regexp = "L'objet dataTS doit \u00eatre coh\u00e9rent avec la temporalit\u00e9 classique.")
#                     testthat::expect_false(res_logical)
#                 }
#             }
#         }
#     }
# })
#
# ### Mauvais type ---------------------------------------------------------------
#
# wrong_type_ts <- list(
#     ts(list(2L), start = 2010L, frequency = 12L),
#     ts(list(2, 3, c(1, 2)), start = 2010L, frequency = 12L),
#     ts(list(2L, 3L, 4L), start = 2010L, frequency = 12L),
#     ts(list(2L, list("3L"), 4L:15L), start = 2010L, frequency = 12L)
# )
#
# testthat::test_that("Result FALSE expected with wrong type of ts", {
#     for (wrong_ts in wrong_type_ts) {
#         testthat::expect_warning({res_logical <- isGoodTS(wrong_ts, withWarning = TRUE)}, regexp = "L'objet dataTS doit être d'un type atomic.")
#         testthat::expect_false(res_logical)
#     }
# })
#
# ## Tests avec withWarning = FALSE ----------------------------------------------
#
# ### Mauvais objet R ------------------------------------------------------------
#
# testthat::test_that("Result FALSE expected with wrong object R", {
#     for (wrong_ts in object_bank_R) {
#         testthat::expect_false(isGoodTS(wrong_ts, withWarning = FALSE))
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
#                     testthat::expect_false(isGoodTS(mts_A, withWarning = FALSE))
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
#                     testthat::expect_false(isGoodTS(ts_A, withWarning = FALSE))
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
#                     testthat::expect_false(isGoodTS(ts_A, withWarning = FALSE))
#                 }
#             }
#         }
#     }
# })
#
# ### Mauvais type ---------------------------------------------------------------
#
# wrong_type_ts <- list(
#     ts(list(2L), start = 2010L, frequency = 12L),
#     ts(list(2, 3, c(1, 2)), start = 2010L, frequency = 12L),
#     ts(list(2L, 3L, 4L), start = 2010L, frequency = 12L),
#     ts(list(2L, list("3L"), 4L:15L), start = 2010L, frequency = 12L)
# )
#
# testthat::test_that("Result FALSE expected with wrong type of ts", {
#     for (wrong_ts in wrong_type_ts) {
#         testthat::expect_false(isGoodTS(wrong_ts, withWarning = FALSE))
#     }
# })
#
# # Tests avec erreur (mauvais withwarning) --------------------------------------
#
# testthat::test_that("Expect error for wrong withwarning argument", {
#     for (typeA in list_type) {
#         for (frequenceA in list_frequence) {
#             for (startA in list_start) {
#                 for (lenA in list_len) {
#                     for (wrong_arg in object_bank_R[-29L]) {
#                         ts_A <- create_random_ts(type = typeA, len = lenA,
#                                                  frequency = frequenceA,
#                                                  start = startA)
#                         testthat::expect_error(isGoodTS(ts_A, withWarning = wrong_arg), regexp = "L'argument withWarning doit être un booléen de longueur 1.")
#                     }
#                 }
#             }
#         }
#     }
# })
#
