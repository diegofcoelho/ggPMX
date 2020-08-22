context("Test MONOLIX individual parameters")

#------------------- read_mlx_ind_est start -----------------------------------
test_that(
  "read_mlx_ind_est: params: path is character path to the file, x is dataset
          result: data.table",
  {
    dx <-
      data.table(id = "ID",
                 subfolder = "ggPMX/testdata/theophylline/Monolix",
                 file = "indiv_eta.txt")
    expect_true(inherits(read_mlx_ind_est(
      path = file.path(
        system.file(package = "ggPMX"),
        "testdata/theophylline/Monolix/indiv_eta.txt"
      ),
      x = dx
    ),
    "data.table"))
  }
)

test_that("read_mlx_ind_est: params: path is NULL result:  error",
          {
            dx <-
              data.table(id = "ID",
                         subfolder = "ggPMX/testdata/theophylline/Monolix",
                         file = "indiv_eta.txt")
            expect_error(read_mlx_ind_est(path = NULL, x = dx))
          })

test_that("read_mlx_ind_est: data set is NULL params: result: error", {
  expect_true(inherits(read_mlx_ind_est(
    path = file.path(
      system.file(package = "ggPMX"),
      "testdata/theophylline/Monolix/indiv_eta.txt"
    ),
    x = NULL
  ),
  "data.table"))
})
#------------------- read_mlx_ind_est end -------------------------------------

#------------------- read_mlx18_ind_est start ---------------------------------
test_that(
  "read_mlx18_ind_est: params: path is character path to the file, x is dataset
           result: data.table",
  {
    dx <-
      data.table(id = "ID",
                 subfolder = "inst/testdata/theophylline/Monolix",
                 file = "indiv_eta.txt")
    expect_true(inherits(read_mlx18_ind_est(
      path = file.path(system.file(package = "ggPMX")),  x = dx
    ),
    "data.table"))
  }
)

test_that("read_mlx18_ind_est: params: x is NULL result: error", {
  expect_error(read_mlx18_ind_est(
    path = file.path(system.file(package = "ggPMX")),  x = NULL))
})

test_that("read_mlx18_ind_est: params: path is NULL result: error", {
  dx <-
    data.table(id = "ID",
               subfolder = "inst/testdata/theophylline/Monolix",
               file = "indiv_eta.txt")
  expect_error(read_mlx18_ind_est(path = NULL,  x = dx))
})
#------------------- read_mlx18_ind_est end -----------------------------------

#------------------- read_input end -------------------------------------------
test_that(
  "read_input: params: ipath is character path to the file,
   dv and dvid are string result: data.table",
  {
    theoph_path <-
      file.path(system.file(package = "ggPMX"), "testdata", "theophylline")
    WORK_DIR <- file.path(theoph_path, "Monolix")
    input_file <- file.path(theoph_path, "data_pk.csv")
    ipath <- input_file
    dv <- "EVID"
    dvid <- "SED"
    expect_true(inherits(read_input(ipath, dv, dvid),
                         "data.table"))
  }
)

test_that("read_input: params: dv equals NULL result: error", {
  theoph_path <-
    file.path(system.file(package = "ggPMX"), "testdata", "theophylline")
  WORK_DIR <- file.path(theoph_path, "Monolix")
  input_file <- file.path(theoph_path, "data_pk.csv")
  ipath <- input_file
  dv <- NULL
  dvid <- "SED"
  expect_error(read_input(ipath, dv, dvid))
})

test_that("read_input: params: dvid equals NULL result: error", {
  theoph_path <-
    file.path(system.file(package = "ggPMX"), "testdata", "theophylline")
  WORK_DIR <- file.path(theoph_path, "Monolix")
  input_file <- file.path(theoph_path, "data_pk.csv")
  ipath <- input_file
  dv <- "EVID"
  dvid <- NULL
  expect_true(inherits(read_input(ipath, dv, dvid),
                       "data.table"))
})

test_that("read_input: params: no file name result: error", {
  theoph_path <-
    file.path(system.file(package = "ggPMX"), "testdata", "theophylline")
  WORK_DIR <- file.path(theoph_path, "Monolix")
  input_file <- file.path(theoph_path, "")
  ipath <- input_file
  dv <- "EVID"
  dvid <- "SED"
  expect_error(read_input(ipath, dv, dvid))
})
#------------------- read_input end -------------------------------------------

#------------------- mlx_ipred start ------------------------------------------
test_that("mlx_ipred: params: x is names of prediction data result: message",
          {
            input_file <-
              file.path(system.file(package = "ggPMX"),
                        "testdata/theophylline/Monolix/predictions.txt")
            x <- names(fread(input_file))
            expect_message(mlx_ipred(x), "NO valid mapping for IPRED")
          })

test_that("mlx_ipred: params: x is names of prediction data result: NULL", {
  input_file <-
    file.path(system.file(package = "ggPMX"),
              "testdata/theophylline/Monolix/predictions.txt")
  x <- names(fread(input_file))
  expect_equal(mlx_ipred(x), NULL)
})
#------------------- mlx_ipred end --------------------------------------------

#------------------- mlx18_ipred start ----------------------------------------
test_that("mlx18_ipred: params: x is names of prediction data result: message",
          {
            input_file <-
              file.path(system.file(package = "ggPMX"),
                        "testdata/theophylline/Monolix/predictions.txt")
            x <- names(fread(input_file))
            expect_message(mlx18_ipred(x), "NO valid mapping for IPRED")
          })

test_that("mlx18_ipred: params: x is names of prediction data result: NULL", {
  input_file <-
    file.path(system.file(package = "ggPMX"),
              "testdata/theophylline/Monolix/predictions.txt")
  x <- names(fread(input_file))
  expect_equal(mlx18_ipred(x), NULL)
})
#------------------- mlx18_ipred end ------------------------------------------

#------------------- mlx18_finegrid_ipred start -------------------------------
test_that("mlx18_finegrid_ipred: params: x is names of
          prediction data result: message",
          {
            input_file <-
              file.path(system.file(package = "ggPMX"),
                        "testdata/theophylline/Monolix/predictions.txt")
            x <- names(fread(input_file))
            expect_message(mlx18_finegrid_ipred(x), 
                           "NO valid mapping for IPRED")
          })

test_that("mlx18_finegrid_ipred: params: x is names of prediction
          data result: NULL",
          {
            input_file <-
              file.path(system.file(package = "ggPMX"),
                        "testdata/theophylline/Monolix/predictions.txt")
            x <- names(fread(input_file))
            expect_equal(mlx18_finegrid_ipred(x), NULL)
          })

#------------------- mlx18_finegrid_ipred end ---------------------------------

#------------------- mlx_iwres start ------------------------------------------

test_that("mlx_iwres: params: x is names of prediction data result: message",
          {
            input_file <-
              file.path(system.file(package = "ggPMX"),
                        "testdata/theophylline/Monolix/predictions.txt")
            x <- names(fread(input_file))
            expect_message(mlx_iwres(x), "NO valid mapping for IWRES")
          })

test_that("mlx_iwres: params: x is names of prediction data result: NULL", {
  input_file <-
    file.path(system.file(package = "ggPMX"),
              "testdata/theophylline/Monolix/predictions.txt")
  x <- names(fread(input_file))
  expect_equal(mlx_iwres(x), NULL)
})

#------------------- mlx_iwres end --------------------------------------------

#------------------- mlx18_iwres start ----------------------------------------
test_that("mlx18_iwres: params: x is names of prediction data result: message",
          {
            input_file <-
              file.path(system.file(package = "ggPMX"),
                        "testdata/theophylline/Monolix/predictions.txt")
            x <- names(fread(input_file))
            expect_message(mlx18_iwres(x), "NO valid mapping for IWRES")
          })

test_that("mlx18_iwres: params: x is names of prediction data result: NULL", {
  input_file <-
    file.path(system.file(package = "ggPMX"),
              "testdata/theophylline/Monolix/predictions.txt")
  x <- names(fread(input_file))
  expect_equal(mlx18_iwres(x), NULL)
})

#------------------- mlx18_iwres end ------------------------------------------

#------------------- read_mlx18_res start -------------------------------------

test_that(
  "read_mlx18_res: params: path is character path to the file, x is dataset
  result: data.table",
  {
    dx <-
      list(
        id = 'ID',
        subfolder = "/theophylline/Monolix",
        file = "predictions.txt",
        names = list(18.0, 228, 111.1151)
      )
    names(dx[["names"]]) <- c("time", "y1", "popPred")
    path0 <-
      file.path(system.file(package = "ggPMX"), "testdata/theophylline/")
    expect_true(inherits(read_mlx18_res(path = path0,  x = dx),
                         "data.table"))
  }
)

test_that("read_mlx18_res: params: path is NULL, x is dataset result:  error",
          {
            dx <-
              list(
                id = 'ID',
                subfolder = "/theophylline/Monolix",
                file = "predictions.txt",
                names = list(18.0, 228, 111.1151)
              )
            expect_error(read_mlx18_res(path = NULL, x = dx))
          })

test_that("read_mlx18_res: params: data set is NULL result: error",
          {
            expect_error(read_mlx18_res(path = path0,  x = NULL))
          })

#------------------- read_mlx18_res end ---------------------------------------

#------------------- read_mlx_par_est start -----------------------------------

test_that(
  "read_mlx_par_est: params: path is character path to the file, x is dataset
           result: data.table",
  {
    dx <-
      list(id = 'ID',
           names = c("parameter", "Sline", "Rline", "pvalues_lin"))
    path0 <-
      file.path(system.file(package = "ggPMX"),
                "testdata/theophylline/Monolix/estimates.txt")
    expect_true(inherits(read_mlx_par_est(path = path0,  x = dx),
                         "data.table"))
  }
)

test_that("read_mlx_par_est: params: path is NULL, x is dataset result: error",
          {
            dx <-
              list(id = 'ID',
                   names = c("parameter", "Sline", "Rline", "pvalues_lin"))
            expect_error(read_mlx_par_est(path = NULL,  x = dx))
          })

test_that("read_mlx_par_est: data set is NULL result: error",
          {
            path0 <-
              file.path(system.file(package = "ggPMX"),
                        "testdata/theophylline/Monolix/estimates.txt")
            expect_error(read_mlx_par_est(path = path0,  x = NULL))
          })
#------------------- read_mlx_par_est end -------------------------------------
