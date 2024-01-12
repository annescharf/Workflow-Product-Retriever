# ------------------------- #
#         Preamble
# ------------------------- #

library(fs)
library(move2)

# Helpers
source("tests/app-testing-helpers.r")

# Read input datasets for testing
data_path <- "data/raw/"
test_inputs <- fs::dir_map(path = data_path, fun = readRDS)
names(test_inputs) <- basename(path_ext_remove(dir_ls(data_path)))


# ---------------------------------------- #
# ----   Interactive RFunction testing  ----
# ---------------------------------------- #

# set up local environment to run RFunction interactively
set_interactive_testing()

# appending a move2 object
output <- rFunction(
  data = test_inputs$input2, 
  usr = usr, 
  pwd = pwd, 
  workflow_title = "mock",
  app_pos = 8, 
  product_file = "app-output", 
  track_combine = "merge")

attr(output, "appended_products")


# appending a moveStack object
output <- rFunction(
  data = test_inputs$input2, 
  usr = usr, 
  pwd = pwd, 
  workflow_title = "mock",
  app_title = "move2_loc to moveStack",
  product_file = "app-output")

attr(output, "appended_products")


# data == NULL AND retrieved data is move2_loc object
output <- rFunction(
  data = NULL, 
  usr = usr, 
  pwd = pwd, 
  workflow_title = "mock",
  app_pos = 8,
  product_file = "app-output")

attr(output, "appended_products")

# data == NULL AND retrieved data is not move2_loc
output <- rFunction(
  data = NULL, 
  usr = usr, 
  pwd = pwd, 
  workflow_title = "mock",
  app_pos = 2,
  product_file = "data_wtime")

attr(output, "appended_products")




# ---------------------------------------- #
# ----    Automated Unit testing        ----
# ---------------------------------------- #

testthat::test_file("tests/testthat/test_RFunction.R")




# ---------------------------------------- #
# ----    MoveApps SDK testing          ----
# ---------------------------------------- #

set_wrkflw_creds()

# Appending a csv file
run_sdk(
  data = test_inputs$input1, 
  usr = usr, pwd = pwd, 
  workflow_title = "Mock WF", 
  app_title = "Add Local and Solar Time", 
  product_file = "data_wtime")

output <- readRDS("data/output/output.rds"); output
attr(output, "appended_products")



# Appending a ctmm model object
run_sdk(
  data = test_inputs$input2, 
  usr = usr, 
  pwd = pwd, 
  workflow_title = "mock",
  app_pos = 12, 
  product_file = "model")

output <- readRDS("data/output/output.rds"); output

appended_prods <- attr(output, "appended_products")
appended_prods[[1]]$metadata
appended_prods[[1]]$object$Bateleur_8889 |> class()




# ----- Simulating applying the App sequentially to append two products 

# -- Two retrieved move2_nonloc products
# 1st App
run_sdk(
  data = test_inputs$input4, 
  usr = usr, 
  pwd = pwd, 
  workflow_title = "mock", 
  app_title = "Standardise Formats and Calculate Basic Statistics", 
  product_file = "summarystats") 

output_app1 <- readRDS("data/output/output.rds"); output_app1

# 2nd App
run_sdk(
  data = output_app1,
  usr = usr, 
  pwd = pwd, 
  workflow_title = "mock",
  app_title = "Fit a Continuous-Time Movement Model (ctmm)", 
  product_file = "model_summary.txt"
)

output_app2 <- readRDS("data/output/output.rds"); output_app2

# retrieve outputs' attribute `appended_products` 
appended_prods <- attr(output_app2, "appended_products")
length(appended_prods)

# first object appended - stored in first element of `appended_products`
appended_prods[[1]]$metadata
appended_prods[[1]]$object

# second object appended - stored in second element of `appended_products`
appended_prods[[2]]$metadata
appended_prods[[2]]$object



# -- Two retrieved move2_loc products
# 1st App
run_sdk(
  data = test_inputs$input4, 
  usr = usr, 
  pwd = pwd, 
  workflow_title = "mock", 
  app_title = "Standardise Formats and Calculate Basic Statistics", 
  product_file = "app-output") 

output_app1 <- readRDS("data/output/output.rds"); output_app1
attr(output_app1, "appended_products")


# 2nd App
run_sdk(
  data = output_app1,
  usr = usr, 
  pwd = pwd, 
  workflow_title = "mock",
  app_title = "Standardise Formats and Calculate Basic Statistics", 
  product_file = "app-output", 
  track_combine = "merge"
)

output_app2 <- readRDS("data/output/output.rds"); output_app2
attr(output_app2, "appended_products")



# ------- Incorrectly specified parameters
run_sdk(
  data = test_inputs$input3,
  usr = usr, 
  pwd = pwd, 
  workflow_title = "mock",
  app_title = "Fit a Continuous-Time Movement Mod", 
  product_file = "model_summary.txt"
)

run_sdk(
  data = test_inputs$input1,
  usr = usr, 
  pwd = pwd, 
  workflow_title = "mock",
  app_pos = 5, 
  product_file = "wrong filename.csv"
)


run_sdk(
  data = test_inputs$input2,
  usr = usr, 
  pwd = pwd, 
  workflow_title = "mock",
  app_pos = 8, 
  product_file = "distances.png"
)

run_sdk(
  data = output_app1,
  usr = usr, 
  pwd = pwd, 
  workflow_title = "mock",
  app_title = "Adapt/Filter Unrealistic Values",
  product_file = "app-output.rds"
)


run_sdk(
  data = output_app1,
  usr = usr, 
  pwd = pwd, 
  workflow_title = "mock",
  app_title = "Write Raster",
  product_file = "data_raster"
)


run_sdk(
  data = test_inputs$input3,
  usr = "WRONG-USER", 
  pwd = pwd, 
  workflow_title = "mock",
  app_title = "Fit a Continuous-Time Movement Mod", 
  product_file = "model_summary.txt"
)

run_sdk(
  data = test_inputs$input3,
  usr = usr,
  pwd = "WRONG-PASS", 
  workflow_title = "mock",
  app_title = "Fit a Continuous-Time Movement Mod", 
  product_file = "model_summary.txt"
)

run_sdk(
  data = test_inputs$input3,
  usr = "", 
  pwd = pwd, 
  workflow_title = "mock",
  app_title = "Fit a Continuous-Time Movement Mod", 
  product_file = "model_summary.txt"
)


run_sdk(
  data = test_inputs$input3,
  usr = "STRING WITH WHITESPACE", 
  pwd = pwd, 
  workflow_title = "mock",
  app_title = "Fit a Continuous-Time Movement Mod", 
  product_file = "model_summary.txt"
)



