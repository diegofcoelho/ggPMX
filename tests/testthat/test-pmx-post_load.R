context("Test merge input and fingrid data sets")

#------------------- input_finegrid start -------------------------------------
test_that("input_finegrid: params: input, finegrid are data sets 
          result: data.table",
          {
            ctr <- theophylline()
            dx <- ctr %>% get_data("eta")
            finegrid <- ctr %>% get_data("finegrid")
            expect_true(inherits(input_finegrid(dx, finegrid), "data.table"))
          })

test_that("input_finegrid: params: finegrid is NULL result: NULL",
          {
            ctr <- theophylline()
            dx <- ctr %>% get_data("eta")
            finegrid <- NULL
            expect_equal(input_finegrid(dx, finegrid), NULL)
          })


test_that("input_finegrid: params: dx is NULL result: error", {
  ctr <- theophylline()
  dx <- NULL
  finegrid <- ctr %>% get_data("eta")
  expect_error(input_finegrid(dx, finegrid))
})

test_that("input_finegrid: params: integer dx result: error", {
  ctr <- theophylline()
  dx <- 10L
  finegrid <- ctr %>% get_data("eta")
  expect_error(input_finegrid(dx, finegrid))
})
#------------------- input_finegrid end ---------------------------------------



#------------------- post_load start ------------------------------------------
test_that("post_load: params: dxs is data set and input is NULL 
          result: list", {
  ctr <- theophylline()
  dx <- ctr %>% get_data("eta")
  input <- NULL
  expect_true(inherits(
    post_load(
      dxs = dx,
      input,
      sys = "mlx",
      dplot = NULL,
      occ = ""
    ),
    "list"
  ))
})


test_that("post_load: params: dxs and input are data sets result: list", {
  ctr <- theophylline()
  dx <- ctr %>% get_data("eta")
  ipath <-
    file.path(system.file(package = "ggPMX"), "testdata/pk_pd/pk_pd.csv")
  input <- pmx_fread(ipath)
  expect_true(inherits(
    post_load(
      dxs = dx,
      input,
      sys = "mlx",
      dplot = NULL,
      occ = ""
    ),
    "list"
  ))
})

test_that("post_load: params: ds and input are data sets result: list()", {
  ctr <- theophylline()
  dx <- ctr %>% get_data("eta")
  ipath <-
    file.path(system.file(package = "ggPMX"), "testdata/pk_pd/pk_pd.csv")
  input <- pmx_fread(ipath)
  res <-  post_load(
    dxs = dx,
    input,
    sys = "mlx",
    dplot = NULL,
    occ = ""
  )
  expect_equal(res$warnings, list())
})

test_that("post_load: params: ds and input are data sets 
          result: identical value",
          {
            ctr <- theophylline()
            dx <- ctr %>% get_data("eta")
            ipath <-
              file.path(system.file(package = "ggPMX"), 
                        "testdata/pk_pd/pk_pd.csv")
            input <- pmx_fread(ipath)
            res <-  post_load(
              dxs = dx,
              input,
              sys = "mlx",
              dplot = NULL,
              occ = ""
            )
            expect_equal(res$data, dx)
          })

#------------------- post_load end --------------------------------------------

#------------------- post_load_eta start --------------------------------------
test_that("post_load_eta: params: dxs is data set and input is NULL
          result: data.table",
          {
            ctr <- theophylline()
            dx <- ctr %>% get_data("eta")
            ipath <-
              file.path(system.file(package = "ggPMX"), 
                        "testdata/pk_pd/pk_pd.csv")
            input <- pmx_fread(ipath)
            expect_error(post_load_eta(ds = dx,
                                       input))
          })
#------------------- post_load end --------------------------------------------
