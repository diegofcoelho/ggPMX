context("Test all plots")
pmxClassHelpers <- test_pmxClass_helpers()

#------------------- pmx_plot_xx start ----------------------------------------
test_that("We can call all pmx_plot_xx with success", {
  ctr <- pmxClassHelpers$ctr
  pmx_plots <- ctr %>% plot_names()
  pmx_function_plots <- sprintf("pmx_plot_%s", pmx_plots)
  
  res <- lapply(pmx_function_plots,
                function(fun) {
                  is_function <-
                    exists(fun, where = "package:ggPMX", mode = "function")
                  if (is_function) {
                    do.call(fun, list(ctr = ctr))
                  } else {
                    if (fun == "pmx_plot_indiv") {
                      ctr %>% pmx_plot_individual(1)
                    }
                  }
                })
  expect_true(all(vapply(res, function(x)
    inherits(x, "gg") || is.null(x), TRUE)))
})
#------------------- pmx_plot_xx end ------------------------------------------

#------------------- pmx_plot_generic start -----------------------------------
test_that("pmx_plot_generic: params: ctr is controller, p is plot
          result: gg object",
          {
            ctr <- pmxClassHelpers$ctr
            pmx_plots <- ctr %>% plot_names()
            pmx_plots <- pmx_plots[!pmx_plots == 'individual']
            expect_true(all(vapply(pmx_plots, function(p)
              inherits(pmx_plot_generic(ctr,  p, defaults_ = NULL), "gg"), TRUE)))
          })

test_that(
  "pmx_plot_generic: params: ctr is controller, pmx_plots is individual plot
  result: NULL",
  {
    ctr <- pmxClassHelpers$ctr
    pmx_plots <- 'individual'
    expect_false(inherits(
      pmx_plot_generic(
        ctr = ctr,
        pname = pmx_plots,
        defaults_ = NULL
      ),
      "gg"
    ))
  }
)
#------------------- pmx_plot_generic end -------------------------------------


#------------------- wrap_pmx_plot_generic start ------------------------------
test_that("wrap_pmx_plot_generic: params: params is NULL result: gg",
          {
            ctr <- pmxClassHelpers$ctr
            pmx_plots <- ctr %>% plot_names()
            pmx_plots <- pmx_plots[!pmx_plots == 'individual']
            expect_true(all(vapply(pmx_plots, function(p)
              inherits(
                wrap_pmx_plot_generic(
                  ctr = ctr,
                  pname = p,
                  params = NULL,
                  defaults_ = NULL
                ),
                "gg"
              ), TRUE)))
          })

test_that("wrap_pmx_plot_generic: params: ctr,  p, params are well set
          result: gg",
          {
            ctr <- pmxClassHelpers$ctr
            pmx_plots <- ctr %>% plot_names()
            pmx_plots <- pmx_plots[!pmx_plots == 'individual']
            params <-
              list(strat.facet = "SEX",
                   bin = pmx_vpc_bin(style = "kmeans", n = 5))
            expect_true(all(vapply(pmx_plots, function(p)
              inherits(
                wrap_pmx_plot_generic(
                  ctr = ctr,
                  pname = p,
                  params = params,
                  defaults_ = NULL
                ),
                "gg"
              ), TRUE)))
          })

test_that("wrap_pmx_plot_generic: params: ctr,  p, params are well set result: gg",
          {
            ctr <- pmxClassHelpers$ctr
            params <-
              list(strat.facet = "SEX",
                   bin = pmx_vpc_bin(style = "kmeans", n = 5))
            expect_error(wrap_pmx_plot_generic(
              ctr = ctr,
              pname = NULL,
              params = params,
              defaults_ = NULL
            ))
          })
#------------------- wrap_pmx_plot_generic end --------------------------------


#------------------- lang_to_expr start ---------------------------------------
test_that("lang_to_expr: params: params result: list",
          {
            ctr <- pmxClassHelpers$ctr
            pmx_plots <- ctr %>% plot_names()
            pmx_plots <- pmx_plots[!pmx_plots == 'individual']
            params <-
              list(strat.facet = "SEX",
                   bin = pmx_vpc_bin(style = "kmeans", n = 5))
            expect_true(inherits(lang_to_expr(params = params), "list"))
          })

test_that("lang_to_expr: params: params doesn't contain filter
          result: identical value",
          {
            ctr <- pmxClassHelpers$ctr
            pmx_plots <- ctr %>% plot_names()
            pmx_plots <- pmx_plots[!pmx_plots == 'individual']
            params <-
              list(strat.facet = "SEX",
                   bin = pmx_vpc_bin(style = "kmeans", n = 5))
            expect_equal(lang_to_expr(params = params), params)
          })

test_that("lang_to_expr: params: params contains filter result: equal value",
          {
            ctr <- pmxClassHelpers$ctr
            pmx_plots <- ctr %>% plot_names()
            pmx_plots <- pmx_plots[!pmx_plots == 'individual']
            params <-
              list(
                strat.facet = "SEX",
                bin = pmx_vpc_bin(style = "kmeans", n = 5),
                filter = expression(ID == 5 & TIME < 2)
              )
            params1 <- lang_to_expr(params = params)
            expect_equal(params1$filter, deparse(params$filter))
          })

test_that("lang_to_expr: params: params is NULL result: equal NULL",
          {
            ctr <- pmxClassHelpers$ctr
            pmx_plots <- ctr %>% plot_names()
            pmx_plots <- pmx_plots[!pmx_plots == 'individual']
            params <- NULL
            expect_equal(lang_to_expr(params = params), NULL)
          })
#------------------- lang_to_expr end -----------------------------------------
