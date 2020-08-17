context("Test structure of qq plot object")

#------------------- pmx_qq start ---------------------------------------------
test_that("pmx_qq: params:  x equals eta result: identical structure", {
  x <- "eta"
  labels <- list(
    title = sprintf("QQ plot: %s", x),
    y = "",
    x = "",
    subtitle = ""
  )
  expect_identical(pmx_qq(x = x),
                   structure(
                     list(
                       ptype = "PMX_QQ",
                       strat = TRUE,
                       x = x,
                       dname = "predictions",
                       point = list(
                         shape = 1,
                         colour = "black",
                         size = 1
                       ),
                       is.reference_line = NULL,
                       reference_line = NULL,
                       xmax = TRUE,
                       facets = NULL,
                       is.shrink = NULL,
                       shrink = NULL,
                       is.hline = NULL,
                       hline = NULL,
                       gp = pmx_gpar(
                         labels = labels,
                         discrete = TRUE,
                         is.smooth = FALSE
                       )
                     ),
                     class = c("pmx_qq", "pmx_gpar")
                   ))
})

test_that("pmx_qq: params: x equals eta result: pmx_qq", {
  x <- 'eta'
  expect_true(inherits(pmx_qq(x = x), "pmx_qq"))
})

test_that("pmx_qq: params: x is NULL result: pmx_qq", {
  x <- NULL
  expect_true(inherits(pmx_qq(x = x), "pmx_qq"))
})

test_that("pmx_qq: params: labels is too long result: pmx_qq", {
  labels <- list(rep("hello-", times=10000))
  expect_true(inherits(pmx_qq(labels), "pmx_qq"))
})

test_that("pmx_qq: params: labels is integer result: error", {
  labels <- 10L
  expect_error(pmx_qq(labels = labels))
})

test_that("pmx_qq: params: dname is integer result: ", {
  dname <- 10L
  expect_error(pmx_qq(dname = dname))
})
#------------------- pmx_qq end -----------------------------------------------

#------------------- plot_pmx.pmx_qq start ------------------------------------
test_that("plot_pmx.pmx_qq: params: rx is pmx_qq object and dx is data set 
           result: gg object", {
  x <- c("AGE0")
  rx <- pmx_qq(x)
  ctr <- theophylline()
  dx <- ctr %>% get_data("eta")
  expect_true(inherits(plot_pmx.pmx_qq(x = rx, dx = dx), "gg"))
})

test_that("plot_pmx.pmx_qq: params: x has unlegal value result: NULL", {
  x <- c("endpoint")
  rx <- pmx_qq(x)
  ctr <- theophylline()
  dx <- ctr %>% get_data("eta")
  expect_true(is.null(plot_pmx.pmx_qq(x = rx, dx = dx)))
})

test_that("plot_pmx.pmx_qq: params: x is not pmx_qq object result: error",
          {
            x <- c("endpoint")
            ctr <- theophylline()
            dx <- ctr %>% get_data("eta")
            expect_error(plot_pmx.pmx_qq(x = x, dx = dx))
          })

test_that("plot_pmx.pmx_qq: params: x is NULL result: error", {
  x <- NULL
  ctr <- theophylline()
  dx <- ctr %>% get_data("eta")
  expect_error(plot_pmx.pmx_qq(x = x, dx = dx))
})

#------------------- plot_pmx.pmx_qq end --------------------------------------

#------------------- pmx_qq_stats start ---------------------------------------

test_that("pmx_qq_stats: params: v is geom_point attributes result:
          data.table", {
  ctr <- theophylline()
  dx <- ctr %>% get_data("eta")
  v <- dx$AGE0
  expect_true(inherits(pmx_qq_stats(points = v), "data.table"))
})

test_that("pmx_qq_stats: params: points is NULL result: data.table",
          {
            v <- NULL
            expect_true(inherits(pmx_qq_stats(points = v), "data.table"))
          })
#------------------- pmx_qq_stats end -----------------------------------------
