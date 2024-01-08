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
  expect_equal(round(max(actual$low)), 10213)
  
  
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
  expect_length(actual$timestamp, 247)
  expect_equal(round(mean(actual$kmph, na.rm = TRUE), 5), 7.74966)
  
  
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


