context("Test structure of eta correlation")

#------------------- eta_pairs start -----------------------------------------
test_that("eta_pairs: params: no result: identical structure", {
  expect_identical(eta_pairs(),
                   structure(
                     list(
                       ptype = "ETA_PAIRS",
                       dname = "eta",
                       strat = FALSE,
                       labels = list(
                         title = "Correlations of random effects",
                         subtitle = "",
                         x = "",
                         y = ""
                       ),
                       point = NULL,
                       type.eta =  "mode",
                       text_color = "black",
                       is.shrink = TRUE,
                       shrink = NULL,
                       is.smooth = TRUE,
                       smooth = NULL,
                       point = NULL,
                       is.hline = FALSE,
                       hline = NULL,
                       gp = pmx_gpar(
                         labels = list(
                           title = "Correlations of random effects",
                           subtitle = "",
                           x = "",
                           y = ""
                         ),
                         discrete = FALSE,
                         is.smooth = FALSE,
                         is.band = FALSE
                       )
                     ),
                     class = c("eta_pairs", "pmx_gpar")
                   ))
})

test_that("eta_pairs: params: no result: eta_pairs", {
  expect_true(inherits(eta_pairs(), "eta_pairs"))
})

test_that("eta_pairs: params: dname is NULL result: eta_pairs", {
  dname <- NULL
  expect_true(inherits(eta_pairs(dname = dname), "eta_pairs"))
})

test_that("eta_pairs: params: dname is string result: eta_pairs", {
  dname <- "test"
  expect_true(inherits(eta_pairs(dname = dname), "eta_pairs"))
})

test_that("eta_pairs: params: integer dname result: error", {
  dname <- 10L
  expect_error(inherits(eta_pairs(dname = dname), "eta_pairs"))
})

test_that("eta_pairs: params: type.eta = mean result: eta_pairs", {
  expect_error(eta_pairs(type.eta = 'eta'))
})

test_that("eta_pairs: params: type.eta = mean, title, dname result: eta_pairs", {
  dname <- "test"
  title <- "test title"
  expect_true(inherits(eta_pairs(type.eta = 'mean', dname = dname, title = title), "eta_pairs"))
})

test_that("eta_pairs: params: type.eta not in (mode, mean) result: error", {
  expect_error(eta_pairs(type.eta = 'eta'))
})
#------------------- eta_pairs end -----------------------------------------