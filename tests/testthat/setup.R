source(file.path("..", "..", "src", "common", "logger.R"))
source(file.path("..", "..", "src", "io", "app_files.R"))
source(file.path("..", "..", "src", "io", "io_handler.R"))
Sys.setenv("LOCAL_APP_FILES_DIR" = "../../data/local_app_files")
# the system under test (sut)
source(file.path("..", "..", "./RFunction.R"))

source("../app-testing-helpers.r")

app_key <- get_app_key()
wrkfl_creds <- httr2::secret_read_rds("../wrkflw_creds_encr.rds", key = I(app_key))
usr <- wrkfl_creds$usr
pwd <- wrkfl_creds$pwd
