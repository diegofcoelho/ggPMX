context("Test pmx Quantile-quantile plots")

#------------------- pmx_plot_iwres_qq start ------------------------------
test_that("pmx_plot_iwres_qq: params: ctr is controller result: gg", {
  ctr <- theophylline()
  expect_true(inherits(pmx_plot_iwres_qq(ctr), "gg"))
})

test_that("pmx_plot_iwres_qq: params: ctr is controller, strat.color, strat.facet result: gg", {
  ctr <- theophylline()
  expect_true(inherits(
    pmx_plot_iwres_qq(ctr, strat.color = "STUD", strat.facet =  ~ SEX),
    "gg"
  ))
})

test_that("pmx_plot_iwres_qq: params: no result: gg",
          {
            expect_error(pmx_plot_iwres_qq())
          })

test_that("pmx_plot_iwres_qq: params: ctr is not controller result: gg",
          {
            ctr <- theophylline() %>% get_data("eta")
            expect_error(pmx_plot_iwres_qq(ctr))
          })
#------------------- pmx_plot_iwres_qq end --------------------------------

#------------------- pmx_plot_eta_qq start --------------------------------
test_that("pmx_plot_eta_qq: params: ctr is controller result: gg", {
  ctr <- theophylline()
  expect_true(inherits(pmx_plot_eta_qq(ctr), "gg"))
})

test_that("pmx_plot_eta_qq: params: ctr is controller, strat.color, strat.facet result: gg", {
  ctr <- theophylline()
  expect_true(inherits(
    pmx_plot_eta_qq(ctr, strat.color = "STUD", strat.facet =  ~ SEX),
    "gg"
  ))
})

test_that("pmx_plot_eta_qq: params: no result: gg",
          {
            expect_error(pmx_plot_eta_qq())
          })

test_that("pmx_plot_eta_qq: params: ctr is not controller result: gg",
          {
            ctr <- theophylline() %>% get_data("eta")
            expect_error(pmx_plot_eta_qq(ctr))
          })
#------------------- pmx_plot_eta_qq end ----------------------------------

#------------------- pmx_plot_npde_qq start -------------------------------
test_that("pmx_plot_npde_qq: params: ctr is controller result: gg", {
  ctr <- theophylline()
  expect_true(inherits(pmx_plot_npde_qq(ctr), "gg"))
})

test_that("pmx_plot_npde_qq: params: ctr is controller, strat.color, strat.facet result: gg", {
  ctr <- theophylline()
  expect_true(inherits(
    pmx_plot_npde_qq(ctr, strat.color = "STUD", strat.facet =  ~ SEX),
    "gg"
  ))
})

test_that("pmx_plot_npde_qq: params: no result: gg",
          {
            expect_error(pmx_plot_npde_qq())
          })

test_that(
  "pmx_plot_npde_qq: params: ctr is not controller result: gg",
  {
    ctr <- theophylline() %>% get_data("eta")
    expect_error(pmx_plot_npde_qq(ctr))
  }
)
#------------------- pmx_plot_npde_qq end ---------------------------------
