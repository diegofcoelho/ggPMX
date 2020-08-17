context("Test structure of residual distribution")

#------------------- residual start -------------------------------------------
test_that("residual: params: x and y are axis aesthetics
          result: identical structure", {
  x <- c("warnings")
  y <- c("endpoint")
  aess <- list(x = x, y = y)
  expect_identical(residual(x = x, y = y),
                   structure(
                     list(
                       ptype = "SCATTER",
                       strat = TRUE,
                       dname = "predictions",
                       aess = list(x = x, y = y),
                       point = list(
                         shape = 1,
                         colour = "black",
                         size = 1
                       ),
                       is.hline = FALSE,
                       hline = list(yintercept = 0),
                       facets = NULL,
                       bloq = NULL,
                       gp = pmx_gpar(labels = list(
                         title = paste(rev(list(
                           x = x, y = y
                         )), collapse = " versus "),
                         subtitle = "",
                         x = aess[["x"]],
                         y = aess[["y"]]
                       ))
                     )
                     ,
                     class = c("residual", "pmx_gpar")
                   ))
})

test_that("residual: params: x and y are axis aesthetics result: pmx_gpar", {
  x <- c("warnings")
  y <- c("endpoint")
  expect_true(inherits(residual(x = x, y = y), "pmx_gpar"))
})

test_that("residual: params: x and y equal NULL result: pmx_gpar", {
  x <- NULL
  y <- NULL
  expect_true(inherits(residual(x = x, y = y), "pmx_gpar"))
})

test_that("residual: params: integer labels result: error", {
  x <- c("warnings")
  y <- c("endpoint")
  labels <- 10L
  expect_error(residual(x = x, y = y, labels = labels))
})

test_that("residual: params: integer point result: error", {
  x <- c("warnings")
  y <- c("endpoint")
  point <- 10L
  expect_error(residual(x = x, y = y, point = point))
})

test_that("residual: params: integer dname result: error", {
  x <- c("warnings")
  y <- c("endpoint")
  dname <- 10L
  expect_error(residual(x = x, y = y, point = point))
})

test_that("residual: params: integer hline result: error", {
  x <- c("warnings")
  y <- c("endpoint")
  hline <- 10L
  expect_error(residual(x = x, y = y, hline = hline))
})
#------------------- residual end ---------------------------------------------

#------------------- plot_pmx.residual start ----------------------------------
test_that("plot_pmx.residual: 
           params: rx is residual object and dx is data set result: ggplot", {
  x <- c("AGE0")
  y <- c("DV")
  rx <- residual(x, y)   ## residual data
  ctr <- theophylline()
  dx <- ctr %>% get_data("eta")
  expect_true(inherits(plot_pmx.residual(x = rx, dx = dx), "gg"))
})


test_that("plot_pmx.residual: params: y is not in names of dx result: NULL", {
  x <- c("AGE0")
  y <- c("endpoint")
  rx <- residual(x, y)   ## residual data
  ctr <- theophylline()
  dx <- ctr %>% get_data("eta")
  expect_true(is.null(plot_pmx.residual(x = rx, dx = dx)))
})

test_that("plot_pmx.residual: params: rx is not residual data result: error",
          {
            x <- c("AGE0")
            rx <- x   ## residual data
            ctr <- theophylline()
            dx <- ctr %>% get_data("eta")
            expect_error(plot_pmx.residual(x = rx, dx = dx))
          })

#------------------- plot_pmx.residual end ------------------------------------

#------------------- extend_range start ---------------------------------------

test_that("extend_range: params: test object extend_range
          result: identical value", {
  x <- c(5)
  expect_identical(extend_range(x = x, r = range(x, na.rm = TRUE),
                                f = 0.05), c(5, 5))
})

test_that("extend_range: params: both limits greater then 0
          result: identical value", {
  x <- c(1, 5)
  expect_identical(extend_range(x = x, r = range(x, na.rm = TRUE),
                                f = 0.05), c(0.8, 5.2))
})

test_that("extend_range: params: first limit equals 0
          result: identical value", {
  x <- c(0, 5)
  expect_identical(extend_range(x = x, r = range(x, na.rm = TRUE),
                                f = 0.05), c(0.01, 5.25))
})

test_that("extend_range: params: limit equals NULL result: warning",
          {
            x <- c(NULL)
            expect_warning(extend_range(x = x, r = range(x, na.rm = TRUE),
                                        f = 0.05))
          })
#------------------- extend_range end -----------------------------------------
