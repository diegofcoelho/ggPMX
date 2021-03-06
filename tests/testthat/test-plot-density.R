context("Test pmx_dens function")

#------------------- pmx_dens start ------------------------------------------
test_that("pmx_dens: params: x equals ETA_COV; result: identical structure",
          {
            x <- "ETA_COV"
            expect_identical(pmx_dens(x),
                             structure(
                               list(
                                 ptype = "PMX_DENS",
                                 strat = TRUE,
                                 x = "ETA_COV",
                                 dname = "predictions",
                                 xlim = 3,
                                 var_line = list(
                                   linetype = 1,
                                   colour = "black",
                                   size = 1
                                 ),
                                 snd_line = list(
                                   linetype = 2,
                                   colour = "black",
                                   size = 1
                                 ),
                                 vline = list(
                                   linetype = 2,
                                   colour = "black",
                                   size = 1
                                 ),
                                 gp = pmx_gpar(
                                   labels = list(
                                     title = sprintf("Density plot of %s", x),
                                     y = "",
                                     x = "",
                                     subtitle = ""
                                   ),
                                   discrete = TRUE,
                                   is.smooth = FALSE
                                 )
                               ),
                               class = c("pmx_dens", "pmx_gpar")
                             ))
          })

test_that("pmx_dens: params: x equals ETA_COV; result: pmx_dens", {
  x <- "ETA_COV"
  expect_true(inherits(pmx_dens(x = x), "pmx_dens"))
})

test_that("pmx_dens: params: x equals NULL; result: pmx_dens", {
  x <- NULL
  expect_true(inherits(pmx_dens(x = x), "pmx_dens"))
})

test_that("pmx_dens: params: integer dname; result: error", {
  dname <- 10L
  expect_error(inherits(pmx_dens(dname = dname), "pmx_dens"))
})

test_that("pmx_dens: params: x equals ETA_COV, dname is NULL; result: pmx_dens",
          {
            x <- "ETA_COV"
            dname <- NULL
            expect_true(inherits(pmx_dens(x = x, dname = dname), "pmx_dens"))
          })

test_that("pmx_dens: params: x is NULL, dname is NULL; result: pmx_dens", {
  x <- NULL
  dname <- NULL
  expect_true(inherits(pmx_dens(x = x, dname = dname), "pmx_dens"))
})

test_that("pmx_dens: params: labels is list, x isn't provided; result: error",
          {
            labels <- list(
              title = sprintf("Density plot"),
              y = "",
              x = "",
              subtitle = ""
            )
            expect_error(inherits(pmx_dens(labels = labels), "pmx_dens"))
          })

test_that("pmx_dens: params: labels is list, x is provided; result: pmx_dens",
          {
            x <- "ETA"
            labels <- list(
              title = sprintf("Density plot"),
              y = "",
              x = "",
              subtitle = ""
            )
            expect_true(inherits(pmx_dens(x = x, labels = labels), "pmx_dens"))
          })

test_that("pmx_dens: params: integer labels; result: error", {
  labels <- 10L
  expect_error(inherits(pmx_dens(labels = labels), "pmx_dens"))
})

test_that("pmx_dens: params: labels character; result: error", {
  labels <- "test label"
  expect_error(inherits(pmx_dens(labels = labels), "pmx_dens"))
})
#------------------- pmx_dens end --------------------------------------------
