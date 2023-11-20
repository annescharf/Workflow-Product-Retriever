library('move2')
library("httr2")
library("assertthat")
library("rlang")
library("dplyr")
library("openssl")
library("fs")
library("readr")
library("move") # "indirect" dependency: required to open/read appended objects in movestack format
#library("ctmm")


# NOTE 1: HTTP requests to the MoveApps API built below based on the example code provided in
# <https://github.com/movestore/movestore.github.io/blob/master/web-partner-api/example.html>.

# NOTE 2: Code relies on the current the structure and attribute names of MoveApps's
# API. If some of its naming conventions change in the future, the code will most likely
# be exposed to errors in HTTP requests.
#
# NOTE 3: Currently only supporting products stored as csv, txt or rds files
#
# NOTE 4: file extension of the 'output_file' generated in Apps is not currently
# provided by the API, so here we assumed it's always stored as an rds file.
#
# Note 5: there is an inconsistency between the name of the output file in the
# MoveApps GUI ('app-output.rds') and that in the API ('output_file'). Currently
# hard-coding the renaming of "app-output" as "output_file", but this is
# unguarded to potential/eventual harmonizing between API and GUI in the naming
# of App output files
#
# TODO: Expand support for Products comprising raster data and shapefiles


rFunction = function(data = NULL, 
                     usr, 
                     pwd, 
                     workflow_title,
                     app_title = NULL,
                     app_pos = NULL, 
                     product_file){
  
  # input validation -----------------------------------------------------------
  if(!is.null(data)) assertthat::assert_that(mt_is_move2(data))
  assertthat::assert_that(assertthat::is.string(usr))
  assertthat::assert_that(assertthat::is.string(pwd))
  assertthat::assert_that(assertthat::is.string(workflow_title))
  if(!is.null(app_title)) assertthat::assert_that(assertthat::is.string(app_title))
  if(!is.null(app_pos)) assertthat::assert_that(is.numeric(app_pos))
  assertthat::assert_that(assertthat::is.string(product_file))
  
  if(is.null(app_title) & is.null(app_pos)){
    stop("At least one of the parameters `app_title` or `app_pos` must be specified", call. = FALSE)
  }
  
  
  # input processing -----------------------------------------------------------
  
  # generate empty move2 object to append retrieved objects to. This is to allow
  # this App to be deployed as a MoveApps Workflow Starting App
  if(is.null(data)){
    data <- data.frame(timestamp = 1, track = "a", x = 0, y = 0) |> 
      mt_as_move2("timestamp", "track", coords = c("x", "y")) |> 
      filter_track_data(.track_id = "b") # make it empty
  }
  
  # Deal with inconsistency in naming of App output files between what is shown
  # in the App Outputs panel in MoveApps and what is returned in the API
  # response 
  if(grepl("app-output", x = product_file)){
    product_file <- "output_file.rds"
  }
  
  # Get basename and extension of target file
  product_file_base <- fs::path_ext_remove(product_file)
  product_file_ext <- fs::path_ext(product_file)
  
  
  # fetch links to products from all Apps in the workflow of interest -----------
  logger.info("Fetching API endpoints to Apps and associated Products in target Workflow")
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
      # shift app positions one place as API counts them from zero :)
      appPositionInWorkflow = appPositionInWorkflow + 1,
      file_basename = fs::path_ext_remove(fileName),
      # api does not return file extension of output_file, so assuming it's ALWAYS rds
      file_ext = ifelse(file_basename == "output_file", "rds", fs::path_ext(fileName))
    )

  
  
  # List products' metadata in target app  -------------------------------------
  logger.info("Listing Products metadata in target App")
  
  if(not_null(app_pos) & is.null(app_title)){
    
    app_products <- wf_products |> 
      dplyr::filter(appPositionInWorkflow == app_pos)
    
    # non-existent/invalid user-specified App position
    if(nrow(app_products) == 0){
      rlang::abort(message = c(
        paste0("There is no App available in position #", app_pos, " of Workflow '", 
               workflow_title, "'"),
        "i" = "Please check the target Workflow page to get a valid App position number."),
        call = NULL
      )
    }
    
    # set app title as stated in the API
    app_title <- app_products$appTitle
    
  }else if(not_null(app_pos) & not_null(app_title)){
    
    app_products <- wf_products |> 
      dplyr::filter(appPositionInWorkflow == app_pos, appTitle == app_title)
    
    # non-existent/invalid user-specified App title in user-specified position
    if(nrow(app_products) == 0){
      rlang::abort(message = c(
        paste0("There is no App with name matching '", app_title, "' in position #", 
               app_pos, " of Workflow '", workflow_title, "'"),
        "i" = "Make sure parameters `app_title` and `app_pos` point coherently to the target App."
      ),
      call = NULL
      )
    }
  } else if(is.null(app_pos) & not_null(app_title)){
    
    app_products <- wf_products |> 
      dplyr::filter(appTitle == app_title)
    
    # non-existent/invalid user-specified App title
    if(nrow(app_products) == 0){
      rlang::abort(message = c(
        paste0("There is no App with name matching '", app_title, "' in Workflow '", 
               workflow_title, "'"),
        "i" = paste0("Please check the Workflow page and make sure the",
                     " title of the target App is spelled accurately in",
                     " parameter `app_title` (case-sensitive)")
      ),
      call = NULL
      )
    }
    
    # Dealing with multiple copies of same app in a Workflow, when only app_title is specified
    # Assumes Products in any given App have unique filenames
    if(any(duplicated(app_products$fileName))){
      rlang::abort(message = c(
        "Unable to unambiguously identify the specified target App",
        "x" = paste0("There is more than one copy of App '", app_title, 
                     "' in the target Workflow '", workflow_title, "'"),
        "i" = "Please provide the target App position in parameter `app_pos`"
      ),
      call = NULL
      )
    }
  }
  
  
  # Get target Product metadata  --------------------------------------------
  logger.info("Getting details of target Product")
  
  # target product
  prod_meta <- app_products |>
    dplyr::filter(file_basename == product_file_base)
  
  # non-existent/invalid user-specified Product name 
  if(nrow(prod_meta) == 0){
    rlang::abort(message = c(
      paste0("There is no Product named '", product_file, "' in App '", 
             app_title, "' in Workflow '", workflow_title, "'"),
      "i" = paste0("Make sure the target product is an output of the specified",
                   " App and its filename is defined correctly in `product_file`",
                   " (case-sensitive)")), 
      call = NULL)
  }
  
  
  # Dealing multiple files in App with same basename, but different extensions
  if(nrow(prod_meta) > 1){

    # If extension missing, throw error asking user to include it in filename
    if(product_file_ext == ""){
      rlang::abort(message = c(
        "Unable to unambiguously identify the target Product",
        "x" = paste0("Found more than one Product with basename '", product_file, 
               "' in App '", app_title, "' in Workflow '", workflow_title, "'"),
        "i" = paste0("Please include the file extension when specifying the",
        " filename of the target Product (parameter `product_file`)")), 
        call = NULL)
      
    } else{
      prod_meta <- prod_meta |>
        dplyr::filter(file_ext == product_file_ext)
    }
  }
  
  
  
  # Retrieve the target file  --------------------------------------------------
  
  # NOTE: assumes files in a given app have unique filenames (i.e. basename + extension)
  logger.info("Downloading and processing data in target Product")
  
  prod_object <- get_product_object(usr, pwd, prod_meta$self, prod_meta$file_ext)

  
  
  # Attach fetched product data to input data ----------------------------------
  logger.info("Appending target Product object to input dataset")
  
  to_append <- list(
    metadata = prod_meta |> dplyr::select(-self),
    object = prod_object
  )
  
  # get current appended products (from upstream copy of 'Workflow-Products-Retriever'), if any
  appended_products <- attr(data, "appended_products")
  
  # either append as a newly created list, or add to previous appended list
  if(is.null(appended_products)){
    attr(data, "appended_products") <- list(to_append)
    appended_pos <- 1
  }else{
    attr(data, "appended_products") <- append(appended_products, list(to_append))
    appended_pos <- length(appended_products) + 1
  }
  
  
  # Log out info on appended product
  cat(
    paste0(
      "\nThe following Product was appended to list element #", appended_pos,
      " of attribute `appended_products` of the input data object: \n\n",
      "  Product: '", product_file, "' \n",
      "File Size: '", prod_meta$fileSize, "' \n",
      " Modified: '", prod_meta$modifiedAt, "' \n", 
      "      App: '", prod_meta$appTitle, "' \n",
      " Workflow: '", prod_meta$workflow_title, "' \n",
      " Instance: '", prod_meta$instance_title, "' \n\n"
    )
  )
  
  
  # Export metadata of appended product as artifact --------------------------------
  readr::write_csv(
    to_append$metadata, 
    file = appArtifactPath("appended_product_metadata.csv")
  )
  
  
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
        error = cnd, 
        class = "httr2_http_401"
      )
    }
  )
}


#' /////////////////////////////////////////////////////////////////////////////
#' Download product from API and convert to tibble
#' 
get_product_object <- function(usr, pwd, product_link, file_ext){
  
  filetypes <- c("rds","csv", "txt")
  
  if(file_ext %notin% filetypes){
    rlang::abort(
      message = c(
        "Unsupported target Product file type",
        "x" = paste0("'.", file_ext, "' files are currently not supported for appending",
               " products from other Workflows to the input data"),
        "i" = paste0("Presently only supporting Products stored as ", 
                     combine_words(filetypes, before = "'.", after = "'"), " files")
      ),
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
  
  # Parse returned API response as an R object
  if(file_ext %in% c("csv", "txt")){
    # convert data in body section to string and convert to tibble. 
    # read_delim() accepts literal data as input
    prod_obj <- prod_resp |> 
      httr2::resp_body_string() |>
      readr::read_delim(show_col_types = FALSE)

  }else if(file_ext == "rds"){
    
    # get type of compression 
    compression <- strsplit(httr2::resp_content_type(prod_resp), split = "/")[[1]][2]
    
    prod_obj <- prod_resp |> 
      httr2::resp_body_raw() |> 
      memDecompress(type = compression) |> 
      unserialize()
  }
  
  return(prod_obj)
}





#'////////////////////////////////////////////////////////////////////////////////////
# hacked version of stolen `knitr::combine_words`, to avoid dependency on whole {knitr}
combine_words <- function(words, sep = ", ", and = " and ", before = "", after = before, 
                          oxford_comma = FALSE) 
{
  n = length(words)
  if (n == 0) 
    return(words)
  words = paste0(before, words, after)
  if (n == 1) 
    return(words)
  if (n == 2) 
    return(paste(words, collapse = if (is_blank(and)) sep else and))
  if (oxford_comma && grepl("^ ", and) && grepl(" $", sep)) 
    and = gsub("^ ", "", and)
  words[n] = paste0(and, words[n])
  if (!oxford_comma) {
    words[n - 1] = paste0(words[n - 1:0], collapse = "")
    words = words[-n]
  }
  paste(words, collapse = sep)
}


is_blank <- function(x){
  grepl("^\\s*$", x)
}
  


#' Useful wee helpers ///////////////////////////////////////////////////////
"%notin%" <- Negate("%in%")
not_null <- Negate(is.null)



                    