library(move2)
library(withr)
library(rlang)
library(purrr)

if(rlang::is_interactive()){
  library(testthat)
  source("tests/app-testing-helpers.r")
  set_interactive_testing()
}


test_data <- readRDS(testthat::test_path("data/input3_move2.rds"))


#' "RFunction()" ===============================================================

test_that("App input validation does its job", {

  expect_error(
    rFunction(data = test_data, usr = usr, pwd = pwd), 
    regexp = "\"workflow_title\" is missing"
  )
  
  expect_error(
    rFunction(data = test_data, usr = ""), 
    regexp = "Input for Workflow ID \\(`usr`\\) is missing."
  )
  
  expect_error(
    rFunction(data = test_data, usr = "WHITESPACE IN USR"), 
    regexp = "Invalid Workflow ID \\(`usr`\\): string must not contain any whitespaces."
  )
  
  expect_error(
    rFunction(data = test_data, usr = usr, pwd = ""), 
    regexp = "Input for Workflow password \\(`pwd`\\) is missing."
  )
  
  expect_error(
    rFunction(data = test_data, usr = usr, pwd = pwd, workflow_title = ""), 
    regexp = "Input for Workflow title \\(`workflow_title`\\) is missing."
  )
  
  expect_error(
    rFunction(data = test_data, usr = usr, pwd = pwd, workflow_title = "ffd", product_file = ""), 
    regexp = "Input for Product filename \\(`product_file`\\) is missing."
  )
  
  expect_error(
    rFunction(data = test_data, usr = usr, pwd = pwd, workflow_title = "blah"), 
    regexp = "\"product_file\" is missing"
  )
  
  expect_error(
    rFunction(data = test_data, usr = usr, pwd = pwd, workflow_title = "blah", product_file = "somefile"), 
    regexp = "Either the Title of the App \\(`app_title`\\) or its position in the Workflow \\(`app_pos`\\) must be specified."
  )
  
  expect_error(
    rFunction(data = test_data, usr = usr, pwd = 2, workflow_title = "blah", product_file = "somefile"), 
    regexp = "pwd is not a string"
  )
  
  expect_error(
    rFunction(data = test_data, usr = usr, pwd = pwd, workflow_title = "blah", 
              app_title = 23, product_file = "somefile"), 
    regexp = "app_title is not a string"
  )
  
  expect_error(
    rFunction(data = test_data, usr = usr, pwd = pwd, workflow_title = "blah", 
              app_title = "sometitle", app_pos = "ah", product_file = "somefile"), 
    regexp = "app_pos is not a numeric or integer vector"
  )
  
  expect_error(
    rFunction(data = test_data, usr = usr, pwd = pwd, track_combine = "WHATEVER"), 
    regexp = "`track_combine` must be one of \"merge\" or \"rename\", not \"WHATEVER\""
  )
  
})


test_that("App provides useful feedback when user specifies targets incorrrectly", {
  
  # wrong App name
  expect_error(
    rFunction(
      data = test_data, usr = usr, pwd = pwd, workflow_title = "MOCK",
      app_title = "WRONG APP NAME", product_file = "app-output"),
    regexp = "There is no App with name matching 'WRONG APP NAME' in Workflow 'MOCK: Workflow Instance 001'"
  )
  
  # wrong App name and/or position
  expect_error(
    rFunction(
      data = test_data, usr = usr, pwd = pwd, workflow_title = "MOCK",
      app_title = "WRONG APP NAME", app_pos = 2, product_file = "app-output"),
    regexp = "There is no App with name matching 'WRONG APP NAME' in position #2 of Workflow 'MOCK: Workflow Instance 001'"
  )
  
  # wrong App position
  expect_error(
    rFunction(
      data = test_data, usr = usr, pwd = pwd, workflow_title = "MOCK",
      app_pos = 20, product_file = "app-output"),
    regexp = "There is no App available in position #20 of Workflow 'MOCK: Workflow Instance 001'"
  )
   
  # wrong Product filename
  expect_error(
    rFunction(
      data = test_data, usr = usr, pwd = pwd, workflow_title = "MOCK",
      app_pos = 2, product_file = "WRONG_PRODUCT_FILENAME"),
    regexp = "There is no Product named 'WRONG_PRODUCT_FILENAME' in App 'Add Local and Solar Time' in Workflow 'MOCK: Workflow Instance 001'"
  )
  
  # Multiple copies of App in workflow
  expect_error(
    rFunction(
      data = test_data, usr = usr, pwd = pwd, workflow_title = "MOCK",
      app_title = "Adapt/Filter Unrealistic Values", product_file = "app-output"),
    regexp = "Unable to unambiguously identify the specified target App"
  )
  
  # Multiple files with same basename in App, when extension not included in `product_file`
  expect_error(
    rFunction(
      data = test_data, usr = usr, pwd = pwd, workflow_title = "MOCK",
      app_title = "Write Raster", product_file = "data_raster"),
    regexp = "Unable to unambiguously identify the target Product"
  )
  
  
  expect_error(
    rFunction(
      data = test_data, usr = usr, pwd = pwd, workflow_title = "MOCK",
      app_pos = 5, product_file = "data_shps"),
    regexp = "'.zip' files are currently not supported for appending products from other Workflows to the input data"
  )
  
})



test_that("App fetches and appends the correct target product", {
  
  withr::local_envvar("APP_ARTIFACTS_DIR"="../../data/output/")
  
  actual <- rFunction(
    data = test_data, usr = usr, pwd = pwd, workflow_title = "MOCK",
    app_title = "Adapt/Filter Unrealistic Values", app_pos = 4, 
    product_file = "app-output") |> 
    attr("appended_products") |> 
    purrr::pluck(1)
  
  expect_equal(actual$metadata$appTitle, "Adapt/Filter Unrealistic Values")
  expect_equal(actual$metadata$appPositionInWorkflow, 4)
  expect_true(inherits(actual$object, "MoveStack"))
  
  
  actual <- rFunction(
    data = test_data, usr = usr, pwd = pwd, workflow_title = "MOCK", 
    app_pos = 11, product_file = "app-output") |> 
    attr("appended_products") |> 
    purrr::pluck(1)
  
  expect_equal(actual$metadata$appTitle, "MoveStack to Telemtry.list")
  expect_true(is.list(actual$object))
  expect_true(inherits(actual$object$TAWNY_8891, "telemetry"))
  

    
  actual <- rFunction(
    data = test_data, usr = usr, pwd = pwd, workflow_title = "MOCK",
    app_title = "Standardise Formats and Calculate Basic Statistics", 
    product_file = "summarystats.csv") |> 
    attr("appended_products") |> 
    purrr::pluck(1)
  
  expect_equal(actual$metadata$appPositionInWorkflow, 8)
  expect_contains(names(actual$object), c("first_obs", "last_obs", "total_obs", "max_kmph"))
  
})



test_that("App works with 'empty' input", {
  
  withr::local_envvar("APP_ARTIFACTS_DIR"="../../data/output/")
  
  empty_test_data <- test_data |> filter_track_data(track == "inexistent track name")
  
  actual <- rFunction(
    data = empty_test_data, usr = usr, pwd = pwd, workflow_title = "Mock Workflow",
    app_pos = 8, product_file = "summarystats") |> 
    attr("appended_products") |> 
    purrr::pluck(1)
  
  expect_equal(round(actual$object$max_kmph, 1), c(16.9, 29.6))
  expect_equal(round(actual$object$total_km), c(371, 191))
    
})




test_that("Retrieved product is stacked to input when of type move2_loc", {
  
  withr::local_envvar("APP_ARTIFACTS_DIR"="../../data/output/")
  
  # merge option
  out_app1 <- rFunction(
    data = test_data, usr = usr, pwd = pwd, workflow_title = "Mock Workflow", 
    app_pos = 8, product_file = "app-output", track_combine = "merge")
  
  expect_gt(nrow(out_app1), nrow(test_data))
  expect_equal(mt_n_tracks(out_app1), 3)
  
  app_prods <- attr(out_app1, "appended_products")
  expect_true(app_prods[[1]]$metadata$append_type == "stacked")
  
  # consecutive call to App, this turn stacking with "rename" option
  out_app2 <- rFunction(
    data = out_app1, usr = usr, pwd = pwd, workflow_title = "Mock Workflow", 
    app_pos = 8, product_file = "app-output", track_combine = "rename")
  
  expect_gt(nrow(out_app2), nrow(out_app1))
  expect_equal(mt_n_tracks(out_app2), 5)
  
  app_prods <- attr(out_app2, "appended_products")
  expect_length(app_prods, 2)
  expect_true(app_prods[[2]]$metadata$append_type == "stacked")
  expect_null(app_prods[[2]]$object)
  
})



test_that("Retrieved product is annexed to input when NOT of type move2_loc", {
  
  withr::local_envvar("APP_ARTIFACTS_DIR"="../../data/output/")
  
  # retrieving move1 object
  out_app1 <- rFunction(
    data = test_data, usr = usr, pwd = pwd, workflow_title = "Mock Workflow", 
    app_pos = 4, product_file = "app-output")
  
  expect_equal(nrow(out_app1), nrow(test_data))
  expect_equal(mt_n_tracks(out_app1), mt_n_tracks(test_data))
  
  app_prods <- attr(out_app1, "appended_products")
  expect_length(app_prods, 1)
  expect_true(app_prods[[1]]$metadata$append_type == "annexed")
  expect_s4_class(app_prods[[1]]$object, class = "MoveStack")
  
  
  # sequentially retrieving data.frame object
  out_app2 <- rFunction(
    data = out_app1, usr = usr, pwd = pwd, workflow_title = "Mock Workflow", 
    app_title = "Fit a Continuous-Time Movement Model (ctmm)", 
    product_file = "model_summary.txt")
  
  expect_equal(nrow(out_app1), nrow(out_app2))
  expect_equal(mt_n_tracks(out_app2), mt_n_tracks(out_app1))
  
  app_prods <- attr(out_app2, "appended_products")
  expect_length(app_prods, 2)
  expect_true(app_prods[[2]]$metadata$append_type == "annexed")
  expect_s3_class(app_prods[[2]]$object, "data.frame")
  
})




test_that("App works as a starting App", {
  
  withr::local_envvar("APP_ARTIFACTS_DIR"="../../data/output/")
  
  # retrieving a move2_loc - retrieved data becomes main data
  out <- rFunction(
    data = NULL, usr = usr, pwd = pwd, workflow_title = "Mock Workflow", 
    app_pos = 7, product_file = "app-output")
  
  app_prods <- attr(out, "appended_products")
  expect_length(app_prods, 1)
  expect_true(app_prods[[1]]$metadata$append_type == "stacked")
  expect_null(app_prods[[1]]$object)
  
  
  # retrieving a data.frame - annexed to main data, which is an empty move2_loc
  out <- rFunction(
    data = NULL, usr = usr, pwd = pwd, workflow_title = "Mock Workflow", 
    app_pos = 12, product_file = "model_summary")
  
  expect_equal(nrow(out), 0)
  
  app_prods <- attr(out, "appended_products")
  expect_true(app_prods[[1]]$metadata$append_type == "annexed")
  expect_s3_class(app_prods[[1]]$object, "data.frame")
  
})




#' "is_move2_loc()" ===================================================
test_that("move2_loc objects are correctly identified", {
  
  expect_true(is_move2_loc(test_data))
  expect_true(
    test_data |> 
      sf::st_set_geometry(
        sf::st_sfc(lapply(
          1:nrow(test_data), 
          # at least one non-empty location
          function(x) if(x == 1) sf::st_point(c(1, 11)) else sf::st_point()
        ))) |> 
      is_move2_loc()
  )
  
  expect_false(is_move2_loc(to_move(test_data)))
  expect_false(is_move2_loc(data.frame()))
  expect_false(
    test_data |> 
      sf::st_set_geometry(st_sfc(lapply(1:nrow(test_data), function(x) st_point()))) |> 
      is_move2_loc()
  )

})




#' "get_workflow_products()" ===================================================
test_that("correct list of products in workflow is retrieved", {
  testthat::local_edition(3)
  expect_snapshot(get_workflow_products(usr, pwd))
})


test_that("right condition class and informative message issued for invalid usr and/or pwd", {
  
  expect_error(
    get_workflow_products(usr = "wrong", pwd = "credentials"), 
    regexp = "API request error: Failed to retrieve Workflow details due to invalid Workflow API ID",
    class = "httr2_http_401"
  )
  
})



#' "get_product_object()" ===================================================

test_that("data from correct target product is downloaded", {
  
  # .csv file
  actual <- get_product_object(
    usr = usr, pwd = pwd, 
    product_link = "https://www.moveapps.org/web-partner/v1/workflowInstances/08036056-76a4-4498-9482-0995e42db1f9/apps/1/results/data_wtime.csv",
    file_ext = "csv"
  ) 
  
  expect_contains(names(actual), expected =  c("sunrise_timestamp", "sunset_timestamp"))
  
  
  # app output move2 object stored in .rds
  actual <- get_product_object(
    usr = usr, pwd = pwd, 
    product_link = "https://www.moveapps.org/web-partner/v1/workflowInstances/08036056-76a4-4498-9482-0995e42db1f9/apps/7/results/app-output.rds",
    file_ext = "rds"
  ) 
  
  expect_contains(names(actual), expected = c("dist_m", "kmph", "temperature", "altitude"))
  expect_true(mt_is_move2(actual))
  
  
  # .txt file
  actual <- get_product_object(
    usr = usr, pwd = pwd, 
    product_link = "https://www.moveapps.org/web-partner/v1/workflowInstances/08036056-76a4-4498-9482-0995e42db1f9/apps/11/results/ctmm_select_summary.txt",
    file_ext = "txt"
  ) 
  
  expect_contains(names(actual), expected =  c("unit", "low", "est", "high"))
  expect_equal(round(max(actual$low)), 9971)
  
  
  # app output in movestack format - First copy of app in workflow
  actual <- get_product_object(
    usr = usr, pwd = pwd, 
    product_link = "https://www.moveapps.org/web-partner/v1/workflowInstances/08036056-76a4-4498-9482-0995e42db1f9/apps/4/results/app-output.rds",
    file_ext = "rds"
  )
  
  expect_length(actual$timestamp, 250)
  expect_equal(round(sd(actual$ground.speed, na.rm = TRUE), 5), 6.93687)
  expect_true(inherits(actual, "MoveStack"))
  
  
  # app output in movestack format - Second copy of app in workflow, after some
  # data being filtered out in upstream apps
  actual <- get_product_object(
    usr = usr, pwd = pwd, 
    product_link = "https://www.moveapps.org/web-partner/v1/workflowInstances/08036056-76a4-4498-9482-0995e42db1f9/apps/9/results/app-output.rds",
    file_ext = "rds"
  )
  
  expect_true(inherits(actual, "MoveStack"))
  expect_length(actual$timestamp, 248)
  expect_equal(round(mean(actual$kmph, na.rm = TRUE), 5), 7.56027)
  
  
  # ctmm model object stored in rds file
  actual <- get_product_object(
      usr = usr, pwd = pwd, 
      product_link = "https://www.moveapps.org/web-partner/v1/workflowInstances/08036056-76a4-4498-9482-0995e42db1f9/apps/11/results/model.rds",
      file_ext = "rds"
    ) 
  
  expect_equal(actual$Bateleur_8889$features, c("major", "minor", "angle", "tau position", "tau velocity"))
  expect_equal(actual$TAWNY_8891$link, "identity")
  
})



test_that("right condition class and informative message issued for unsupported file types",{
  
  expect_error(
    get_product_object(
      usr = usr, pwd = pwd, 
      product_link = "https://www.moveapps.org/web-partner/v1/workflowInstances/08036056-76a4-4498-9482-0995e42db1f9/apps/4/results/data_shps.zip",
      file_ext = "zip"
    ),
    regexp = "'.zip' files are currently not supported for appending products from other Workflows to the input data",
    class = "unsupported_file_extension"
  )

  expect_error(
    get_product_object(
      usr = usr, pwd = pwd, 
      product_link ="https://www.moveapps.org/web-partner/v1/workflowInstances/08036056-76a4-4498-9482-0995e42db1f9/apps/7/results/speeds.png",
      file_ext = "png"
    ),
    regexp = "'.png' files are currently not supported for appending products from other Workflows to the input data",
    class = "unsupported_file_extension"
  )
  
})


