context("Test structure of individual plot")

#------------------- individual start ------------------------------------------
test_that("individual: params: labels, facets, dname, etc. result:
          identical structure",
          {
            labels <- list("EVID", "SEX")
            facets <- list(nrow = 5, ncol = 5)
            expect_identical(
              individual(
                labels = labels,
                facets = facets,
                dname = "predictions",
                is.legend = FALSE,
                use.finegrid = FALSE
              ),
              structure(
                list(
                  ptype = "IND",
                  strat = TRUE,
                  is.legend = FALSE,
                  use.finegrid = FALSE,
                  dname = "predictions",
                  aess = list(x = "TIME", y1 = "PRED", y2 = "IPRED"),
                  labels = labels,
                  point = NULL,
                  ipred_line = NULL,
                  pred_line = NULL,
                  facets = facets,
                  bloq = NULL,
                  gp = pmx_gpar(labels = labels, is.legend = FALSE)
                ),
                class = c("individual", "pmx_gpar")
              )
            )
          })

test_that("individual: params: NA labels, facets, dname, etc. result:
          individual object",
          {
            labels <- list(NA)
            facets <- list(nrow = 5, ncol = 5)
            expect_true(inherits(
              individual(
                labels = labels,
                facets = facets,
                dname = "predictions",
                is.legend = FALSE,
                use.finegrid = FALSE
              ),
              "individual"
            ))
          })

test_that("individual: params: list of labels, facets, dname, etc. result:
          individual object",
          {
            labels <-
              list("TIME", "AMT", "Y", "EVID", "WT0", "AGE0", "SEX", "STUD", "DV")
            facets <- list(nrow = 5, ncol = 5)
            expect_true(inherits(
              individual(
                labels = labels,
                facets = facets,
                dname = "input",
                is.legend = FALSE,
                use.finegrid = FALSE
              ),
              "individual"
            ))
          })

test_that("individual: params: labels is NULL, facets, dname, etc. result:
          error",
          {
            labels <- NULL
            facets <- list(nrow = 5, ncol = 5)
            expect_error(
              individual(
                labels = labels,
                facets = facets,
                dname = "input",
                is.legend = FALSE,
                use.finegrid = FALSE
              )
            )
          })

test_that("individual: params: facets is integer result: error", {
  labels <- list("EVID", "SEX")
  facets <- 2
  expect_error(
    individual(
      labels = labels,
      facets = facets,
      dname = "input",
      is.legend = FALSE,
      use.finegrid = FALSE
    )
  )
})
#------------------- individual end --------------------------------------------
