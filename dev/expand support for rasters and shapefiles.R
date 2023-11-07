library(move2)
library(purrr)
library(fs)
library(rlang)


source("tests/app-testing-helpers.r")
set_wrkflw_creds()
# --------------------------------------------------------------------------------------
# Integrating raster products

# generating mock file outputs using current options in "Write Raster" App
r <- raster(system.file("external/test.grd", package="raster"))
# take a small part
r <- crop(r, extent(179880, 180800, 329880, 330840) )
# write to an integer binary file
writeRaster(r,filename = "data_raster.grd", format="raster")
writeRaster(r,filename = "data_raster.asc", format="ascii")
writeRaster(r,filename = "data_raster.nc", format="CDF")



prod_req_gri <- httr2::request("https://www.moveapps.org/web-partner/v1/workflowInstances/08036056-76a4-4498-9482-0995e42db1f9/apps/5/results/data_raster.gri") |> 
  httr2::req_headers(
    Authorization = paste0("Basic ", openssl::base64_encode(paste0(usr, ":", pwd)))
  )

# Submit request
prod_resp_gri <- httr2::req_perform(prod_req_gri)

prod_resp_gri |> resp_raw()
prod_resp_gri |> resp_body_raw() |> writeBin("dev/data_raster_api.gri")



prod_req_grd <- httr2::request("https://www.moveapps.org/web-partner/v1/workflowInstances/08036056-76a4-4498-9482-0995e42db1f9/apps/5/results/data_raster.grd") |> 
  httr2::req_headers(
    Authorization = paste0("Basic ", openssl::base64_encode(paste0(usr, ":", pwd)))
  )

prod_resp_grd <- httr2::req_perform(prod_req_grd)

prod_resp_grd |> resp_raw()
prod_resp_grd |> resp_body_string() |> write("dev/data_raster_api.grd")
  

r_api <- raster("dev/data_raster_api.gri")
r_moveapps <- raster("dev/data_raster.gri")


raster::all.equal(r_api, r_moveapps)




library(raster)
prod_resp |> 
  resp_body_string() |> 
  textConnection() |> 
  readLines() |> 
  raster()

prod_resp |> resp_encoding()
prod_resp |> resp_content_type()
prod_resp |> resp_raw()
prod_resp |> resp_body_raw() |> raster()

prod_resp |> resp_body_raw() |> readBin()



tmp = tempfile()
prod_resp |> resp_body_raw() |> writeBin("data_raster.gri")
raster("data_raster.gri")



prod_resp |> resp_body_raw() |> readBin(what = "data_raster.gri")


# --------------------------------------------------------------------------------------
# Integrating shapefile products

rod_req_shp <- httr2::request("https://www.moveapps.org/web-partner/v1/workflowInstances/08036056-76a4-4498-9482-0995e42db1f9/apps/4/results/data_shps.zip") |> 
  httr2::req_headers(
    Authorization = paste0("Basic ", openssl::base64_encode(paste0(usr, ":", pwd)))
  )

prod_resp_shp <- httr2::req_perform(rod_req_shp)

prod_resp_shp |> resp_raw()
