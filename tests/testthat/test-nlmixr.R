context("Test structure of a nlmixr")
theo_sd <- nlmixr::theo_sd

#------------------- pmx_nlmixr start ------------------------------------------
test_that("pmx_nlmixr: params: no; result: NULL", {
  expect_equal(pmx_nlmixr(), NULL)
})

test_that("pmx_nlmixr: params: ctr is nlmixr object,
          vpc is TRUE; result: pmxClass", {
  one.compartment <- function() {
    ini({
      tka <- 0.45 # Log Ka
      tcl <- 1 # Log Cl
      tv <- 3.45    # Log V
      eta.ka ~ 0.6
      eta.cl ~ 0.3
      eta.v ~ 0.1
      add.sd <- 0.7
    })
    model({
      ka <- exp(tka + eta.ka)
      cl <- exp(tcl + eta.cl)
      v <- exp(tv + eta.v)
      d/dt(depot) = -ka * depot
      d/dt(center) = ka * depot - cl / v * center
      cp = center / v
      cp ~ add(add.sd)
    })
  }
  ctr <- nlmixr::nlmixr(one.compartment, data = theo_sd,
                        est="saem", control=list(print=0))
  expect_true(inherits(pmx_nlmixr(fit = ctr, vpc = TRUE), "pmxClass"))
})

test_that("pmx_nlmixr: params: ctr is nlmixr object,
          vpc is FALSE; result: pmxClass", {
  one.compartment <- function() {
    ini({
      tka <- 0.45 # Log Ka
      tcl <- 1 # Log Cl
      tv <- 3.45    # Log V
      eta.ka ~ 0.6
      eta.cl ~ 0.3
      eta.v ~ 0.1
      add.sd <- 0.7
    })
    model({
      ka <- exp(tka + eta.ka)
      cl <- exp(tcl + eta.cl)
      v <- exp(tv + eta.v)
      d/dt(depot) = -ka * depot
      d/dt(center) = ka * depot - cl / v * center
      cp = center / v
      cp ~ add(add.sd)
    })
  }
  ctr <- nlmixr::nlmixr(object = one.compartment, data = theo_sd,
                        est="saem", control=list(print=0))
  expect_true(inherits(pmx_nlmixr(fit = ctr, vpc = FALSE),"pmxClass"))
})

test_that("pmx_nlmixr: params: ctr is nlmixr object,
cats is not valid value of categorical covariates
           result: pmxClass", {
             one.compartment <- function() {
               ini({
                 tka <- 0.45 # Log Ka
                 tcl <- 1 # Log Cl
                 tv <- 3.45    # Log V
                 eta.ka ~ 0.6
                 eta.cl ~ 0.3
                 eta.v ~ 0.1
                 add.sd <- 0.7
               })
               model({
                 ka <- exp(tka + eta.ka)
                 cl <- exp(tcl + eta.cl)
                 v <- exp(tv + eta.v)
                 d/dt(depot) = -ka * depot
                 d/dt(center) = ka * depot - cl / v * center
                 cp = center / v
                 cp ~ add(add.sd)
               })
             }
             ctr <- nlmixr::nlmixr(object = one.compartment,
                                   data = theo_sd, est="saem", control=list(print=0))
             expect_error(pmx_nlmixr(fit = ctr, cats = c("SEX")))
           })


test_that("pmx_nlmixr: params: ctr is nlmixr object, cats is NULL
           result: warning", {
             one.compartment <- function() {
               ini({
                 tka <- 0.45 # Log Ka
                 tcl <- 1 # Log Cl
                 tv <- 3.45    # Log V
                 eta.ka ~ 0.6
                 eta.cl ~ 0.3
                 eta.v ~ 0.1
                 add.sd <- 0.7
               })
               model({
                 ka <- exp(tka + eta.ka)
                 cl <- exp(tcl + eta.cl)
                 v <- exp(tv + eta.v)
                 d/dt(depot) = -ka * depot
                 d/dt(center) = ka * depot - cl / v * center
                 cp = center / v
                 cp ~ add(add.sd)
               })
             }
             ctr <- nlmixr::nlmixr(object = one.compartment,
                                   data = theo_sd, est="saem",
                                   control=list(print=0))
             expect_warning(pmx_nlmixr(fit = ctr, cats = NULL))
           })


test_that("pmx_nlmixr: params: ctr is nlmixr object, cats
           and conts are empty strings result: pmxClass", {
             one.compartment <- function() {
               ini({
                 tka <- 0.45 # Log Ka
                 tcl <- 1 # Log Cl
                 tv <- 3.45    # Log V
                 eta.ka ~ 0.6
                 eta.cl ~ 0.3
                 eta.v ~ 0.1
                 add.sd <- 0.7
               })
               model({
                 ka <- exp(tka + eta.ka)
                 cl <- exp(tcl + eta.cl)
                 v <- exp(tv + eta.v)
                 d/dt(depot) = -ka * depot
                 d/dt(center) = ka * depot - cl / v * center
                 cp = center / v
                 cp ~ add(add.sd)
               })
             }
             ctr <- nlmixr::nlmixr(object = one.compartment,
                                   data = theo_sd, est="saem",
                                   control=list(print=0))
             expect_true(inherits(pmx_nlmixr(fit = ctr, conts = "", cats = ""),
                                  "pmxClass"))
           })


test_that("pmx_nlmixr: params: ctr is nlmixr object, cats is NULL
           result: pmxClass", {
             one.compartment <- function() {
               ini({
                 tka <- 0.45 # Log Ka
                 tcl <- 1 # Log Cl
                 tv <- 3.45    # Log V
                 eta.ka ~ 0.6
                 eta.cl ~ 0.3
                 eta.v ~ 0.1
                 add.sd <- 0.7
               })
               model({
                 ka <- exp(tka + eta.ka)
                 cl <- exp(tcl + eta.cl)
                 v <- exp(tv + eta.v)
                 d/dt(depot) = -ka * depot
                 d/dt(center) = ka * depot - cl / v * center
                 cp = center / v
                 cp ~ add(add.sd)
               })
             }
             ctr <- nlmixr::nlmixr(object = one.compartment,
                                   data = theo_sd, est="saem",
                                   control=list(print=0))
             expect_true(inherits(pmx_nlmixr(fit = ctr,
                                             conts = c("PRED","IPRED"),
                                             cats = c("IRES","IWRES"),
                                             strats = "DV"), "pmxClass"))
           })

test_that("pmx_nlmixr: params: ctr is nlmixr object,
           endpoint contain not valid values result: error", {
             one.compartment <- function() {
               ini({
                 tka <- 0.45 # Log Ka
                 tcl <- 1 # Log Cl
                 tv <- 3.45    # Log V
                 eta.ka ~ 0.6
                 eta.cl ~ 0.3
                 eta.v ~ 0.1
                 add.sd <- 0.7
               })
               model({
                 ka <- exp(tka + eta.ka)
                 cl <- exp(tcl + eta.cl)
                 v <- exp(tv + eta.v)
                 d/dt(depot) = -ka * depot
                 d/dt(center) = ka * depot - cl / v * center
                 cp = center / v
                 cp ~ add(add.sd)
               })
             }
             ctr <- nlmixr::nlmixr(object = one.compartment,
                                   data = theo_sd,
                                   est="saem",
                                   control=list(print=0))
             ep <- pmx_endpoint(
               code="PRED",
               file.code="2"
             )
             expect_error(pmx_nlmixr(fit = ctr, endpoint = ep))
           })

test_that("pmx_nlmixr: params: ctr is nlmixr object,
          endpoint contain not valid values result: error", {
             one.compartment <- function() {
               ini({
                 tka <- 0.45 # Log Ka
                 tcl <- 1 # Log Cl
                 tv <- 3.45    # Log V
                 eta.ka ~ 0.6
                 eta.cl ~ 0.3
                 eta.v ~ 0.1
                 add.sd <- 0.7
               })
               model({
                 ka <- exp(tka + eta.ka)
                 cl <- exp(tcl + eta.cl)
                 v <- exp(tv + eta.v)
                 d/dt(depot) = -ka * depot
                 d/dt(center) = ka * depot - cl / v * center
                 cp = center / v
                 cp ~ add(add.sd)
               })
             }
             ctr <- nlmixr::nlmixr(object = one.compartment,
                                   data = theo_sd,
                                   est="saem",
                                   control=list(print=0))
             ep <- pmx_endpoint(
               code="PRED",
               file.code="2"
             )
             expect_true(inherits(pmx_nlmixr(fit = ctr, endpoint = NULL),
                                  
                                  "pmxClass"))
           })
#------------------- pmx_nlmixr end --------------------------------------------
