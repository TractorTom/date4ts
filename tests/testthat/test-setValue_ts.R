#
# # Tests de résultat --------------------------------------------
#
#
#
# # Tests sur les erreurs de mts --------------------------------------------
#
# mts_trimA  <- ts(mtcars, start = 2004, frequency = 4)
# mts_mensA  <- ts(iris, start = 2015, frequency = 12)
# ts_trimB <- ts(rep(1, 30), start = 2010, frequency = 4)
# ts_mensB <- ts(rep(1, 30), start = 2001, frequency = 12)
#
# testthat::test_that("Several dimensions are not allowed", {
#     testthat::expect_error(combine2ts(mts_mensA, ts_mensB), regexp = "Les objets a et b doivent être des ts unidimensionels.")
#     testthat::expect_error(combine2ts(ts_mensB, mts_mensA), regexp = "Les objets a et b doivent être des ts unidimensionels.")
#
#     testthat::expect_error(combine2ts(mts_trimA, ts_trimB), regexp = "Les objets a et b doivent être des ts unidimensionels.")
#     testthat::expect_error(combine2ts(ts_trimB, mts_trimA), regexp = "Les objets a et b doivent être des ts unidimensionels.")
# })
#
#
# # Tests sur les erreurs d'input --------------------------------------------
#
# object_bank_R <- fuzzr::test_all()
#
# testthat::test_that("miscellaneous input are not allowed", {
#     for (objA in object_bank_R){
#         for (objB in object_bank_R){
#             testthat::expect_error(combine2ts(objA, objB), regexp = "Les objets a et b doivent être des ts unidimensionels.")
#         }
#     }
# })
#
#
# # Tests sur les erreurs de type d'objets --------------------------------------------
#
# ts_type <- list(
#     ts_integer =   ts(rep(1L, 40),      start = 2013, frequency = 12),
#     ts_numeric =   ts(rep(pi, 40),      start = 2010, frequency = 12),
#     ts_complex =   ts(rep(1845+1i, 40), start = 2018, frequency = 12),
#     ts_character = ts(rep("a", 40),     start = 2018, frequency = 12),
#     ts_logical =   ts(rep(TRUE, 40),    start = 2018, frequency = 12),
#     ts_raw =       ts(as.raw(1:40),     start = 2018, frequency = 12)
# )
#
# testthat::test_that("different input type are not allowed", {
#     for (index_A in seq_along(ts_type)){
#         objA <- ts_type[[index_A]]
#         for (index_B in seq_along(ts_type)){
#             objB <- ts_type[[index_B]]
#             if (index_A != index_B) testthat::expect_error(combine2ts(objA, objB), regexp = "Les objets a et b doivent être de même type.")
#         }
#     }
# })
