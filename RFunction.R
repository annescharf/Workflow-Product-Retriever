library('move2')
library("httr2")
library("assertthat")
library("rlang")
library("dplyr")
library("openssl")
library("fs")
library("readr")

# NOTE 1: HTTP requests to the MoveApps API built below based on the example code provided in
# <https://github.com/movestore/movestore.github.io/blob/master/web-partner-api/example.html>.

# NOTE 2: Code relies on the current the structure and attribute names of the
# API. If some of these conventions change in the future, the code will most likely
# be exposed to errors in HTTP requests
#
# NOTE 3: Currently only appending data from products stored as csv, txt or rds files
#
# NOTE 4: the file extension of the 'output_file' generated in Apps is not
# currently provided by the API, so here it is assumed they are all stored and
# available as rds files
#
# Note 5: Currently assuming Apps only appear once in a workflow. Repeated Apps
# in the same workflow will cause this App to crash


rFunction = function(data, usr, pwd, workflow_title, app_title, product_file){
  
  # input validation -----------------------------------------------------------
  assertthat::assert_that(mt_is_move2(data))
  assertthat::assert_that(assertthat::is.string(usr))
  assertthat::assert_that(assertthat::is.string(pwd))
  assertthat::assert_that(assertthat::is.string(app_title))
  assertthat::assert_that(assertthat::is.string(product_file))
  
  # input processing -----------------------------------------------------------
  
  # Deal with inconsistency in naming of App output files between what is shown
  # in the App Outputs panel in MoveApps and what is returned in the API response
  if(grepl("app-output", x = product_file)){
    product_file <- "output_file"
  }
  # Get basename and extension of target file
  product_file_base <- fs::path_ext_remove(product_file)
  product_file_ext <- fs::path_ext(product_file)
  
  
  # fetch links to products from all Apps in the workflow of interest -----------
  logger.info("Fetching API links to products in target worflow")
  wf_products_resp <- get_workflow_products(usr, pwd)
  
  
  # Process response  --------------------------------------------------------
  wf_products <- purrr::map(wf_products_resp$results, as.data.frame) |> 
    purrr::list_rbind() |> 
    # add useful info
    dplyr::mutate(
      workflow_title = workflow_title,
      instance_title = wf_products_resp$workflowInstanceTitle, 
      .before = 1
    ) |> 
    # split basename and extension
    dplyr::mutate(
      file_basename = fs::path_ext_remove(fileName),
      file_ext = fs::path_ext(fileName),
      # api does not return file extension of output_file, so assuming it's ALWAYS rds
      file_ext = ifelse(file_basename == "output_file", "rds", file_ext)
    )

  
  # Check if App and product exists --------------------------------------------
  logger.info("Check presence of target file in workflow")

  # products in target app
  app_products <- dplyr::filter(wf_products, appTitle == app_title)
  
  if(nrow(app_products) == 0){
    rlang::abort(message = c(
      paste0("There is no App called '", app_title, "' in workflow '", 
             workflow_title, "'"),
      "i" = "Make sure the title of the target App is specified correctly in `app_title` (case-sensitive)"),
      call = NULL)
  }
  
  # target product
  prod_meta <- app_products |>
    dplyr::filter(file_basename == product_file_base)
  
  if(nrow(prod_meta) == 0){
    rlang::abort(message = c(
      paste0("There is no Product named '", product_file, "' in App '", 
             app_title, "' in Workflow '", workflow_title, "'"),
      "i" = paste0("Make sure the target product is an output of the specified",
                   " App and its filename is defined correctly in `product_file` (case-sensitive)")), 
      call = NULL)
  }
  
  
  # Retrieve the target file  --------------------------------------------------
  # NOTE: assumes files in a given app have unique names
  logger.info("Downloading and processing data in target product")
  
  prod_data <- get_product_data(usr, pwd, prod_meta$self, prod_meta$file_ext)

  # Attach fetched product data to input data ----------------------------------
  logger.info("Append target product data to input dataset")
  
  obj_to_append <- list(
    metadata = prod_meta |> dplyr::select(-self),
    data = prod_data
  )
  
  # get current auxiliary data, if any
  aux_data <- attr(data, "appended_data")
  
  # either append as a newly created list, or add to previous appended list
  if(is.null(aux_data)){
    attr(data, "appended_data") <- list(obj_to_append)
  }else{
    attr(data, "appended_data") <- append(aux_data, list(obj_to_append))
  }
  
  logger.info("Job done!")
  
  return(data)
}






#' /////////////////////////////////////////////////////////////////////////////
#' Retrieve metadata of products generated in target workflow
#' 
get_workflow_products <- function(usr, pwd){
  
  # MoveApps API base server url
  base_url <- "https://www.moveapps.org/web-partner/v1/workflowInstances/"
  
  # build api request
  wf_prods_req <- httr2::request(base_url) |> 
    httr2::req_url_path_append(usr) |> 
    httr2::req_url_path_append("artifacts/index") |> 
    httr2::req_headers(
      # encoding to base-64 a bit fiddly. jsonlite::base64_enc() didn't work.
      # openssl::base64_encode() appears to do the trick
      Authorization = paste0("Basic ", openssl::base64_encode(paste0(usr, ":", pwd))), 
      Accept = "application/json"
    )
  
  # submit request and convert from json format
  rlang::try_fetch(
    wf_prods_req |> httr2::req_perform() |> httr2::resp_body_json(),
    httr2_http_401 = function(cnd){
      rlang::abort(
        message = "HTTP 401 Unauthorized: API request error due to invalid `usr` and/or `pwd`",
        parent = NA,
        error = cnd
      )
    }
  )
}


#' /////////////////////////////////////////////////////////////////////////////
#' Download product from API and convert to tibble
#' 
get_product_data <- function(usr, pwd, product_link, file_ext){
  
  if(file_ext %notin% c("csv", "rds", "txt")){
    rlang::abort(
      message = c(
        paste0("'.", file_ext, "' is an unsupported format for products to append to input data"),
        "i" = "Currently only supporting products stored in '.csv', '.txt' and '.rds' files"),
      class = "unsupported_file_extension",
      call = NULL
    )
  }
  
  # build http request
  prod_req <- httr2::request(product_link) |> 
    httr2::req_headers(
      Authorization = paste0("Basic ", openssl::base64_encode(paste0(usr, ":", pwd)))
    )
  
  # Submit request
  prod_resp <- httr2::req_perform(prod_req)
  
  # Write returned API response as an R object
  if(file_ext %in% c("csv", "txt")){
    # convert data in body section to string and convert to tibble. 
    # read_csv() accepts literal data as input
    prod_data <- prod_resp |> 
      httr2::resp_body_string() |>
      readr::read_delim()

  }else if(file_ext == "rds"){
    
    # get type of compression 
    compression <- strsplit(httr2::resp_content_type(prod_resp), split = "/")[[1]][2]
    
    prod_data <- prod_resp |> 
      httr2::resp_body_raw() |> 
      memDecompress(type = compression) |> 
      unserialize()
  }
  
  return(prod_data)
}





#' Useful wee helpers ///////////////////////////////////////////////////////
"%notin%" <- Negate("%in%")
                    