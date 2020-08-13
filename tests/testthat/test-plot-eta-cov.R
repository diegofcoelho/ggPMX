context("Test structure of eta cov")

#------------------- pmx_cov start --------------------------------------------
test_that("pmx_cov: params: list of data names result: identical structure", {
  values <- list("WT0","AGE0")
  names(values) <- c("WT0","AGE0")               
  expect_identical(pmx_cov(values),
                   structure(
                     list(
                       values = values,
                       labels = values
                     ),
                     class = c("pmxCOVObject"))
  )
})

test_that("pmx_cov: params: list of data names result: pmxCOVObject", {
  values <- list("WT0","AGE0")
  names(values) <- c("WT0","AGE0") 
  expect_true(inherits(pmx_cov(values), "pmxCOVObject"))
})

test_that("pmx_cov: params: values is string, not a list result: error", {
  values <- "WT0"
  expect_error(inherits(pmx_cov(values), "pmxCOVObject"))
})

test_that("pmx_cov: params: values is int, not a list result: error", {
  values <- 7L
  expect_error(inherits(pmx_cov(values), "pmxCOVObject"))
})

test_that("pmx_cov: params: values is list, labels is not a list result: error", {
  values <- list("WT0","AGE0")
  labels <- "label"
  expect_error(inherits(pmx_cov(values,labels), "pmxCOVObject"))
})
#------------------- pmx_cov end ----------------------------------------------

#------------------- is_pmxcov start ------------------------------------------
test_that("is_pmxcov: params: list of data names result: pmxcov", {
  values <- list("WT0","AGE0")
  names(values) <- c("WT0","AGE0")
  x <- structure(list(values = values, labels = values), class = c("pmxCOVObject"))
  expect_true(is_pmxcov(x))
})

test_that("is_pmxcov: params: x is integer result: not pmxcov", {
  x <- 12L
  expect_false(is_pmxcov(x))
})

test_that("is_pmxcov: params: x is NULL result: pmxcov", {
  x <- NULL
  expect_true(is_pmxcov(x))
})

#------------------- is_pmxcov end --------------------------------------------

#------------------- eta_cov start --------------------------------------------

test_that("eta_cov: params: labels and type result: identical structure", {
  labels <- list(
    title = "EBE vs. covariates",
    subtitle = "",
    x = "",
    y = ""
  )
  expect_identical(eta_cov(labels, type = "cats"),
                   structure(list(
                     ptype = "ETA_COV",
                     strat = FALSE,
                     dname = "eta",
                     type = "cats",
                     show.correl = TRUE,
                     correl = NULL,
                     facets = NULL,
                     point = NULL,
                     covariates = NULL,
                     gp = pmx_gpar(
                       labels = labels,
                       discrete = TRUE)
                   ), class = c("eta_cov", "pmx_gpar"))
  )
})

test_that("eta_cov: params: type result: eta_cov", {
  expect_true(inherits(eta_cov(type = "cats"), "eta_cov"))
})

test_that("eta_cov: params: type, dname is integer result: error", {
  dname<-7L
  expect_error(inherits(eta_cov(type = "cats", dname = dname), "eta_cov"))
})

test_that("eta_cov: params: type, covariates is not pmxcov result: error", {
  covariates<-"test"
  expect_error(inherits(eta_cov(type = "cats", covariates = covariates), "eta_cov"))
})

test_that("eta_cov: params: type not in (cats,conts) pmxcov result: error", {
  expect_error(inherits(eta_cov(type = "dogs"), "eta_cov"))
})

test_that("eta_cov: params: labels is list, type is provided; result: eta_cov",
          {
            x <- "ETA"
            labels <- list(
              title = sprintf("Density plot"),
              y = "",
              x = ""
            )
            expect_true(inherits(eta_cov(type = "cats", labels = labels), "eta_cov"))
          })

test_that("eta_cov: params: integer labels; result: error", {
  labels <- 10L
  expect_error(inherits(eta_cov(type = "cats", labels = labels), "eta_cov"))
})

test_that("eta_cov: params: labels character; result: error", {
  labels <- "test label"
  expect_error(inherits(eta_cov(type = "cats", labels = labels), "eta_cov"))
})
#------------------- eta_cov end ----------------------------------------------