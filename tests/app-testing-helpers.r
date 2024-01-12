
#////////////////////////////////////////////////////////////////////////////////
## helper to get app key to then retrieve app secrets 
get_app_key <- function() {
  
  require(rlang)
  
  key <- Sys.getenv("API_APP_KEY")
  if (identical(key, "")) {
    rlang::abort(message = c(
      "No App key found",
      "x" = "You won't be able to proceed with the App testing. Sorry!", 
      "i" = "You can request the App key from the App developers for testing purposes (bruno@dmpstats.co.uk)",
      "i" = "Set the provided App key via usethis::edit_r_environ() using enviroment variable named 'API_APP_KEY'"
    ))
  }
  key
}



# /////////////////////////////////////////////////////////////////////////////
## helper to set the Workflow credentials in the current environment
set_wrkflw_creds <- function(){
  
  require(httr2)
  
  # Retrieve Workflow credentials       
  app_key <- get_app_key()
  # upload rds with encrypted credentials
  wrkfl_creds <- httr2::secret_read_rds("tests/wrkflw_creds_encr.rds", key = I(app_key))
  usr <<- wrkfl_creds$usr
  pwd <<- wrkfl_creds$pwd
  
  cat("\nusr and pwd of the test Workflow are now available in the current R environment\n\n")
  
}



# /////////////////////////////////////////////////////////////////////////////
## helper to set interactive testing of main App RFunction (e.g. in testthat 
## interactive mode,  on on any given script)
##
set_interactive_testing <- function(){
  
  source("RFunction.R")
  source("src/common/logger.R")
  source("src/io/app_files.R")
  
  set_wrkflw_creds()

}






## /////////////////////////////////////////////////////////////////////////////
# helper to run SDK testing with different settings
run_sdk <- function(data, usr, pwd, 
                    workflow_title, 
                    app_title = NULL, 
                    app_pos = NULL, 
                    product_file, 
                    track_combine = "rename"){
  
  require(jsonlite)
  is.numeric(data) # just a trick to force error when object passed on to data doesn't exists
  
  # get environmental variables specified in .env
  dotenv::load_dot_env(".env")
  app_config_file <- Sys.getenv("CONFIGURATION_FILE")
  source_file <- Sys.getenv("SOURCE_FILE")
  
  # store default app configuration
  dflt_app_config <- jsonlite::fromJSON(app_config_file)
  # get default input data
  dflt_dt <- readRDS(source_file)
  
  # set configuration to specified inputs
  new_app_config <- dflt_app_config
  new_app_config$usr <- usr
  new_app_config$pwd <- pwd
  new_app_config$workflow_title <- workflow_title
  new_app_config$app_title <- app_title
  new_app_config$app_pos <- app_pos
  new_app_config$product_file <- product_file
  new_app_config$track_combine <- track_combine
  
  
  # overwrite config file with current inputs
  write(
    jsonlite::toJSON(new_app_config, pretty = TRUE, auto_unbox = TRUE), 
    file = app_config_file
  )
  
  # overwrite app's source file with current input data
  saveRDS(data, source_file)
  
  # run SDK for the current settings
  try(source("sdk.R"))
  
  # reset to default config and data
  write(
    jsonlite::toJSON(dflt_app_config,  pretty = TRUE, auto_unbox = TRUE), 
    file = app_config_file
  )
  saveRDS(dflt_dt, source_file)
  
  invisible()
}



