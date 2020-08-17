context("Test plot's settings")
pmxClassHelpers <- test_pmxClass_helpers()

#------------------- plot_pmx.pmx_gpar start ------------------------------------------
test_that("plot_pmx.pmx_gpar: params: gpars is pmx_gpar object, p is plot result: ggplot2 object",
          {
            ctr <- pmxClassHelpers$ctr
            pmx_plots <- ctr %>% plot_names()
            pmx_function_plots <- sprintf("pmx_plot_%s", pmx_plots)
            
            gpars <-
              ggPMX::pmx_gpar(labels = list(
                title = "hello",
                x = 'IPRED',
                y = 'IWRES',
                subtitle = 'hi'
              ))

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
            res <- res[sapply(res, function(x)
              'ggplot' %in% class(x))]
            expect_true(all(vapply(res, function(p)
              inherits(plot_pmx.pmx_gpar(gpars, p), "ggplot")
              , TRUE)))
          })

test_that("plot_pmx.pmx_gpar: 
          params: gpars is pmx_gpar object with discrete = TRUE and ranges are set, p is plot 
          result: ggplot2 object",
          {
            ctr <- pmxClassHelpers$ctr
            pmx_plots <- ctr %>% plot_names()
            pmx_function_plots <- sprintf("pmx_plot_%s", pmx_plots)
            
            gpars <-
              ggPMX::pmx_gpar(labels = list(
                title = "hello",
                x = 'IPRED',
                y = 'IWRES',
                subtitle = 'hi'
              ))
            gpars$discrete <- TRUE
            gpars$ranges$x <- 300
            gpars$ranges$y <- 3
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
            res <- res[sapply(res, function(x)
              'ggplot' %in% class(x))]
            expect_true(all(vapply(res, function(p)
              inherits(plot_pmx.pmx_gpar(gpars, p), "ggplot")
              , TRUE)))
          })

test_that("plot_pmx.pmx_gpar: params: gpars is pmx_gpar object with gpars$smooth set, p is plot result: ggplot2 object",
          {
            ctr <- pmxClassHelpers$ctr
            pmx_plots <- ctr %>% plot_names()
            pmx_function_plots <- sprintf("pmx_plot_%s", pmx_plots)
            
            gpars <-
              ggPMX::pmx_gpar(labels = list(
                title = "hello",
                x = 'IPRED',
                y = 'IWRES',
                subtitle = 'hi'
              ))
            gpars$smooth <-
              list(
                se = FALSE,
                linetype = 2,
                size = 2.5,
                method = "loess",
                colour = "green"
              )
            
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
            res <- res[sapply(res, function(x)
              'ggplot' %in% class(x))]
            expect_true(all(vapply(res, function(p)
              inherits(plot_pmx.pmx_gpar(gpars, p), "ggplot")
              , TRUE)))
          })

test_that("plot_pmx.pmx_gpar: 
          params: gpars is pmx_gpar object with gpars$smooth not a list, p is plot 
          result: error",
          {
            ctr <- pmxClassHelpers$ctr
            pmx_plots <- ctr %>% plot_names()
            pmx_function_plots <- sprintf("pmx_plot_%s", pmx_plots)
            
            gpars <-
              ggPMX::pmx_gpar(labels = list(
                title = "hello",
                x = 'IPRED',
                y = 'IWRES',
                subtitle = 'hi'
              ))
            gpars$smooth <- c(
                se = FALSE,
                linetype = 2,
                size = 2.5,
                method = "loess",
                colour = "green"
              )
            
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
            res <- res[sapply(res, function(x)
              'ggplot' %in% class(x))]
            expect_error(all(sapply(res, function(p)
              plot_pmx.pmx_gpar(gpars, p))))
          })

test_that("plot_pmx.pmx_gpar: params: gpars is pmx_gpar object with labels is NULL, p is plot result: error",
          {
            ctr <- pmxClassHelpers$ctr
            pmx_plots <- ctr %>% plot_names()
            pmx_function_plots <- sprintf("pmx_plot_%s", pmx_plots)
            
            gpars <-
              ggPMX::pmx_gpar(labels = NULL)
            
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
            res <- res[sapply(res, function(x)
              'ggplot' %in% class(x))]
            expect_error(all(sapply(res, function(p)
              plot_pmx.pmx_gpar(gpars, p))))
          })

test_that("plot_pmx.pmx_gpar: params: gpars is pmx_gpar object with gpars$band set, p is plots result: ggplot2 object",
          {
            ctr <- pmxClassHelpers$ctr
            pmx_plots <- ctr %>% plot_names()
            pmx_function_plots <- sprintf("pmx_plot_%s", pmx_plots)
            
            gpars <-
              ggPMX::pmx_gpar(labels = list(
                title = "hello",
                x = 'IPRED',
                y = 'IWRES',
                subtitle = 'hi'
              ))
            gpars$is.band <- TRUE
            gpars$band <-
              list(yintercept = c(-1.96,1.96),
                   linetype = 2, 
                   size = 1.5
              )
            
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
            res <- res[sapply(res, function(x)
              'ggplot' %in% class(x))]
            expect_true(all(vapply(res, function(p)
              inherits(plot_pmx.pmx_gpar(gpars, p), "ggplot")
              , TRUE)))
          })

test_that("plot_pmx.pmx_gpar: 
          params: gpars is pmx_gpar object with gpars$band not a list, p is plot 
          result: error",
          {
            ctr <- pmxClassHelpers$ctr
            pmx_plots <- ctr %>% plot_names()
            pmx_function_plots <- sprintf("pmx_plot_%s", pmx_plots)
            
            gpars <-
              ggPMX::pmx_gpar(labels = list(
                title = "hello",
                x = 'IPRED',
                y = 'IWRES',
                subtitle = 'hi'
              ))
            gpars$is.band <- TRUE
            gpars$band <- c(
              yintercept = c(-1.96,1.96),
              linetype = 2, 
              size = 1.5
            )
            
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
            res <- res[sapply(res, function(x)
              'ggplot' %in% class(x))]
            expect_error(all(sapply(res, function(p)
              plot_pmx.pmx_gpar(gpars, p))))
          })

test_that(
  "plot_pmx.pmx_gpar: 
  params: gpars is pmx_gpar object with labels are NULL, p is plot 
  result: error",
  {
    ctr <- pmxClassHelpers$ctr
    pmx_plots <- ctr %>% plot_names()
    pmx_function_plots <- sprintf("pmx_plot_%s", pmx_plots)
    
    gpars <- ggPMX::pmx_gpar(labels = NULL)
    gpars$smooth <-
      list(
        se = FALSE,
        linetype = 2,
        size = 2.5,
        method = "loess",
        colour = "green"
      )
    
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
    
    res <- res[sapply(res, function(x)
      'ggplot' %in% class(x))]
    expect_error(all(sapply(res, function(p)
      plot_pmx.pmx_gpar(gpars, p))))
  }
)

test_that("plot_pmx.pmx_gpar: params: p is NULL result: error", {
  ctr <- pmxClassHelpers$ctr
  pmx_plots <- ctr %>% plot_names()
  pmx_function_plots <- sprintf("pmx_plot_%s", pmx_plots)
  gpars <- ggPMX::pmx_gpar(labels = list(title = "hello"))
  expect_error(plot_pmx.pmx_gpar(gpar = gpars, p = NULL))
})

test_that("plot_pmx.pmx_gpar: params: gpar is NULL result: NULL", {
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
  
  res <- res[sapply(res, function(x)
    'ggplot' %in% class(x))]
  expect_error(all(sapply(res, function(p)
    plot_pmx.pmx_gpar(gpars = NULL, p))))
})
#------------------- plot_pmx.pmx_gpar end --------------------------------------------

