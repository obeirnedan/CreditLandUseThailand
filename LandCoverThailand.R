#############################################################
## Crop the global Esa data into data for just Thailand
## dobeirne@mail.ubc.ca - Feb 2023
#############################################################

##############################
# 0 - Load librairies
##############################


library(dplyr)
library(readxl)
library(raster)
library(sf)
library(ncdf4)
library(raster)
library(sf)
library(rgdal)
library(ClusterR)
library(snow)
library(foreach)
library(doParallel)


############################## 
# 1 - Source file 
##############################


setwd("~/R/CreditLandUseThailand")

############################## 
# 2 - Read in Shaoes, Rasters, then crop
##############################

#create filepath variable for ease of use later
#lc_path <- file.path("./data/LandCover_ESA")
lc_path <- file.path("./data/thai_forest_cover_dennis") #Trying Dennis's Data in 24th Apr 

land_cover   <- list.files(path=lc_path, pattern='.grd', #changed to grd from nc 24 apr
                           all.files=TRUE, full.names=FALSE)

esa_all    <- lapply(file.path(lc_path, land_cover), raster)


nat_border <- st_read("./data/admin_boundary_shapes/tha_admbnda_adm0_rtsd_20220121.shp") %>%
  st_transform(.,crs(esa_all[[1]])) %>%
  as_Spatial(.)

esa_thai <- vector("list", length(esa_all))

for (i in 1:length(esa_all)){
  cat("trying ", i)
  esa_thai[[i]] <- raster::crop(esa_all[[i]], nat_border) %>%
    raster::mask(nat_border)
  cat("Finished:", i)
}

############################## 
# 2 - Write Rasters to File to be used in data_clean.R
##############################
save(esa_thai,file= "./data/LandCoverThailand/esa_thai_apr.Rdata")

for (i in 1:length(land_cover)){
  writeRaster(esa_thai[[i]], filename = paste0("./data/LandCoverThailand_apr/", land_cover[i]), overwrite=TRUE)
}


############################## 
# 2 - Figure Out exact area of shapefile
##############################

tamb_border <- st_read("./data/admin_boundary_shapes/tha_admbnda_adm3_rtsd_20220121.shp")
lil_dist <- tamb_border %>% filter(ADM3_PCODE == 'TH120401')
rast_small <- raster::crop(esa_thai[[1]], lil_dist)
lil_shape <- rasterToPolygons(rast_small, dissolve=FALSE)
plot(lil_shape)
wsshape <- st_as_sf(lil_shape) %>% st_as_sf(merge = F)
wsshape <- st_transform(wsshape, crs = "+proj=utm +zone=47 +datum=WGS84 +units=m +no_defs")





wsshape$WsArea <- st_area(wsshape)/100000 # so it is in km2
