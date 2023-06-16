
#### WEST ARM PARK ####

library(terra)
library(sf)
library(readr)
library(stringr)
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(bfast)
library(landscapemetrics)
library(landscapetools)
library(rgdal)
library(raster)
library(rgeos)
library(stats)

dir <- setwd("C://Users//wendiz3.stu//OneDrive - UBC//Desktop//BCParks")

metrics <- list_lsm(level = "class")

#### load park boundaries ####
westarm <- st_read("Data//west_arm_park.shp") # BC Albers

#### make the grid based on park boundaries ####
gridsize <- 1000  # size of squares, in units of the CRS (metre for BC Albers)

#big rectangular grids covering the whole extent of west arm
grid1 <- st_make_grid(westarm, square = TRUE, cellsize = c(gridsize, gridsize)) %>% st_sf() 
plot(grid1, col = "white")

#do a logical test to check which cells intersect with the park boundary 
westarm_grid_index <- st_intersects(westarm, grid1)

#index all true values
westarm_grid <- grid1[westarm_grid_index[[1]],]

#plot west arm grids
plot(st_geometry(westarm_grid))
plot(westarm_grid)

# write to a shapefile
# st_write(westarm_grid, "park grids\\westarm_grid.shp", driver="ESRI Shapefile")

#### add gridID to park grid data frame ####
#westarm_grid$gridID <- 1:nrow(westarm_grid)

#### write csv to save the park grids ####
# write.csv(westarm_grid, "westarm_grid.csv", row.names = FALSE)

#### prep forest rasters ####
# westarm_89 <- rast("inraster/westarm_89.TIF") # BC Albers
# westarm_99 <- rast("inraster/westarm_99.TIF")
# westarm_09 <- rast("inraster/westarm_09.TIF")
# westarm_19 <- rast("inraster/westarm_19.TIF")
# 
# westarm_89_masked <- terra::mask(westarm_89, vect(westarm_grid))
# westarm_99_masked <- terra::mask(westarm_99, vect(westarm_grid))
# westarm_09_masked <- terra::mask(westarm_09, vect(westarm_grid))
# westarm_19_masked <- terra::mask(westarm_19, vect(westarm_grid))
# 
# plot(westarm_89)
# plot(westarm_99_masked)

#### metrics ####

# 1989

# westarm_89_out <- tibble()
# for (i in 1:nrow(westarm_grid)) {
#   tryCatch({
#     print(i)
#     westarm_crop_89 <-
#       terra::crop(westarm_89_masked, westarm_grid[i,], snap = "in")
#     
#     westarm_89_metrics <-
#       lsm_c_para_mn(westarm_crop_89) %>%
#       bind_rows(lsm_c_cpland(westarm_crop_89)) %>%
#       mutate(gridID = i)
#     
#     westarm_89_out <-
#       bind_rows(westarm_89_out, westarm_89_metrics)
#   }, warning = function(w) {
#     
#     print(w)
#   }, error = function(e) {
#     print(e)
#   })
# }
# 
# write_csv(westarm_89_out, "metrics\\westarm_89.csv")
# 
# 
# # 1999
# 
# westarm_99_out <- tibble()
# for (i in 1:nrow(westarm_grid)) {
#   tryCatch({
#     print(i)
#     westarm_crop_99 <-
#       terra::crop(westarm_99_masked, westarm_grid[i,], snap = "in")
#     
#     westarm_99_metrics <-
#       lsm_c_para_mn(westarm_crop_99) %>%
#       bind_rows(lsm_c_cpland(westarm_crop_99)) %>%
#       mutate(gridID = i)
#     
#     westarm_99_out <-
#       bind_rows(westarm_99_out, westarm_99_metrics)
#   }, warning = function(w) {
#     
#     print(w)
#   }, error = function(e) {
#     print(e)
#   })
# }
# 
# write_csv(westarm_99_out, "metrics\\westarm_99.csv")
# 
# # 2009
# 
# westarm_09_out <- tibble()
# for (i in 1:nrow(westarm_grid)) {
#   tryCatch({
#     print(i)
#     westarm_crop_09 <-
#       terra::crop(westarm_09_masked, westarm_grid[i,], snap = "in")
#     
#     westarm_09_metrics <-
#       lsm_c_para_mn(westarm_crop_09) %>%
#       bind_rows(lsm_c_cpland(westarm_crop_09)) %>%
#       mutate(gridID = i)
#     
#     westarm_09_out <-
#       bind_rows(westarm_09_out, westarm_09_metrics)
#   }, warning = function(w) {
#     
#     print(w)
#   }, error = function(e) {
#     print(e)
#   })
# }
# 
# write_csv(westarm_09_out, "metrics\\westarm_09.csv")
# 
# # 2019
# 
# westarm_19_out <- tibble()
# for (i in 1:nrow(westarm_grid)) {
#   tryCatch({
#     print(i)
#     westarm_crop_19 <-
#       terra::crop(westarm_19_masked, westarm_grid[i,], snap = "in")
#     
#     westarm_19_metrics <-
#       lsm_c_para_mn(westarm_crop_19) %>%
#       bind_rows(lsm_c_cpland(westarm_crop_19)) %>%
#       mutate(gridID = i)
#     
#     westarm_19_out <-
#       bind_rows(westarm_19_out, westarm_19_metrics)
#   }, warning = function(w) {
#     
#     print(w)
#   }, error = function(e) {
#     print(e)
#   })
# }
# 
# write_csv(westarm_19_out, "metrics\\westarm_19.csv")

#### subset by class and metrics ####

# # 1989
# 
# westarm_89 <- read.csv("metrics/westarm_89.csv")
# westarm_89_forest_cpland <- subset(westarm_89, class == 1 & metric == "cpland")
# write.csv(westarm_89_forest_cpland, "metrics_arcgis_in/westarm_89_forest_cpland.csv")
# 
# westarm_89_shrub_cpland <- subset(westarm_89, class == 2 & metric == "cpland")
# write.csv(westarm_89_shrub_cpland, "metrics_arcgis_in/westarm_89_shrub_cpland.csv")
# 
# westarm_89_forest_para <- subset(westarm_89, class == 1 & metric == "para_mn")
# write.csv(westarm_89_forest_para, "metrics_arcgis_in/westarm_89_forest_para.csv")
# 
# westarm_89_shrub_para <- subset(westarm_89, class == 2 & metric == "para_mn")
# write.csv(westarm_89_shrub_para, "metrics_arcgis_in/westarm_89_shrub_para.csv")
# 
# # 1999
# 
# westarm_99 <- read.csv("metrics/westarm_99.csv")
# westarm_99_forest_cpland <- subset(westarm_99, class == 1 & metric == "cpland")
# write.csv(westarm_99_forest_cpland, "metrics_arcgis_in/westarm_99_forest_cpland.csv")
# 
# westarm_99_shrub_cpland <- subset(westarm_99, class == 2 & metric == "cpland")
# write.csv(westarm_99_shrub_cpland, "metrics_arcgis_in/westarm_99_shrub_cpland.csv")
# 
# westarm_99_forest_para <- subset(westarm_99, class == 1 & metric == "para_mn")
# write.csv(westarm_99_forest_para, "metrics_arcgis_in/westarm_99_forest_para.csv")
# 
# westarm_99_shrub_para <- subset(westarm_99, class == 2 & metric == "para_mn")
# write.csv(westarm_99_shrub_para, "metrics_arcgis_in/westarm_99_shrub_para.csv")
# 
# # 2009
# 
# westarm_09 <- read.csv("metrics/westarm_09.csv")
# westarm_09_forest_cpland <- subset(westarm_09, class == 1 & metric == "cpland")
# write.csv(westarm_09_forest_cpland, "metrics_arcgis_in/westarm_09_forest_cpland.csv")
# 
# westarm_09_shrub_cpland <- subset(westarm_09, class == 2 & metric == "cpland")
# write.csv(westarm_09_shrub_cpland, "metrics_arcgis_in/westarm_09_shrub_cpland.csv")
# 
# westarm_09_forest_para <- subset(westarm_09, class == 1 & metric == "para_mn")
# write.csv(westarm_09_forest_para, "metrics_arcgis_in/westarm_09_forest_para.csv")
# 
# westarm_09_shrub_para <- subset(westarm_09, class == 2 & metric == "para_mn")
# write.csv(westarm_09_shrub_para, "metrics_arcgis_in/westarm_09_shrub_para.csv")
# 
# # 2019
# 
# westarm_19 <- read.csv("metrics/westarm_19.csv")
# westarm_19_forest_cpland <- subset(westarm_19, class == 1 & metric == "cpland")
# write.csv(westarm_19_forest_cpland, "metrics_arcgis_in/westarm_19_forest_cpland.csv")
# 
# westarm_19_shrub_cpland <- subset(westarm_19, class == 2 & metric == "cpland")
# write.csv(westarm_19_shrub_cpland, "metrics_arcgis_in/westarm_19_shrub_cpland.csv")
# 
# westarm_19_forest_para <- subset(westarm_19, class == 1 & metric == "para_mn")
# write.csv(westarm_19_forest_para, "metrics_arcgis_in/westarm_19_forest_para.csv")
# 
# westarm_19_shrub_para <- subset(westarm_19, class == 2 & metric == "para_mn")
# write.csv(westarm_19_shrub_para, "metrics_arcgis_in/westarm_19_shrub_para.csv")
# 
# #### join ####
# 
# # 1989
# 
# westarm_89_shrub_cpland <- merge(westarm_89_shrub_cpland, westarm_grid, all=T)
# westarm_89_forest_cpland <- merge(westarm_89_forest_cpland, westarm_grid, all = T)
# 
# westarm_89_shrub_para <- merge(westarm_89_shrub_para, westarm_grid, all = T)
# westarm_89_forest_para <- merge(westarm_89_forest_para, westarm_grid, all = T)
# 
# # 1999
# 
# westarm_99_shrub_cpland <- merge(westarm_99_shrub_cpland, westarm_grid, all=T)
# westarm_99_forest_cpland <- merge(westarm_99_forest_cpland, westarm_grid, all = T)
# 
# westarm_99_shrub_para <- merge(westarm_99_shrub_para, westarm_grid, all = T)
# westarm_99_forest_para <- merge(westarm_99_forest_para, westarm_grid, all = T)
# 
# # 2009
# 
# westarm_09_shrub_cpland <- merge(westarm_09_shrub_cpland, westarm_grid, all=T)
# westarm_09_forest_cpland <- merge(westarm_09_forest_cpland, westarm_grid, all = T)
# 
# westarm_09_shrub_para <- merge(westarm_09_shrub_para, westarm_grid, all = T)
# westarm_09_forest_para <- merge(westarm_09_forest_para, westarm_grid, all = T)
# 
# # 2019
# 
# westarm_19_shrub_cpland <- merge(westarm_19_shrub_cpland, westarm_grid, all=T)
# westarm_19_forest_cpland <- merge(westarm_19_forest_cpland, westarm_grid, all = T)
# 
# westarm_19_shrub_para <- merge(westarm_19_shrub_para, westarm_grid, all = T)
# westarm_19_forest_para <- merge(westarm_19_forest_para, westarm_grid, all = T)
# 
# #### write shapefile ####
# 
# # 1989
# 
# st_write(westarm_89_shrub_cpland,"metrics_arcgis_in/in/westarm_89_shrub_cpland.shp", driver="ESRI Shapefile")
# st_write(westarm_89_forest_cpland,"metrics_arcgis_in/in/westarm_89_forest_cpland.shp", driver="ESRI Shapefile")
# 
# st_write(westarm_89_shrub_para,"metrics_arcgis_in/in/westarm_89_shrub_para.shp", driver="ESRI Shapefile")
# st_write(westarm_89_forest_para,"metrics_arcgis_in/in/westarm_89_forest_para.shp", driver="ESRI Shapefile")
# 
# # 1999
# 
# st_write(westarm_99_shrub_cpland,"metrics_arcgis_in/in/westarm_99_shrub_cpland.shp", driver="ESRI Shapefile")
# st_write(westarm_99_forest_cpland,"metrics_arcgis_in/in/westarm_99_forest_cpland.shp", driver="ESRI Shapefile")
# 
# st_write(westarm_99_shrub_para,"metrics_arcgis_in/in/westarm_99_shrub_para.shp", driver="ESRI Shapefile")
# st_write(westarm_99_forest_para,"metrics_arcgis_in/in/westarm_99_forest_para.shp", driver="ESRI Shapefile")
# 
# # 2009
# 
# st_write(westarm_09_shrub_cpland,"metrics_arcgis_in/in/westarm_09_shrub_cpland.shp", driver="ESRI Shapefile")
# st_write(westarm_09_forest_cpland,"metrics_arcgis_in/in/westarm_09_forest_cpland.shp", driver="ESRI Shapefile")
# 
# st_write(westarm_09_shrub_para,"metrics_arcgis_in/in/westarm_09_shrub_para.shp", driver="ESRI Shapefile")
# st_write(westarm_09_forest_para,"metrics_arcgis_in/in/westarm_09_forest_para.shp", driver="ESRI Shapefile")
# 
# # 2019
# 
# st_write(westarm_19_shrub_cpland,"metrics_arcgis_in/in/westarm_19_shrub_cpland.shp", driver="ESRI Shapefile")
# st_write(westarm_19_forest_cpland,"metrics_arcgis_in/in/westarm_19_forest_cpland.shp", driver="ESRI Shapefile")
# 
# st_write(westarm_19_shrub_para,"metrics_arcgis_in/in/westarm_19_shrub_para.shp", driver="ESRI Shapefile")
# st_write(westarm_19_forest_para,"metrics_arcgis_in/in/westarm_19_forest_para.shp", driver="ESRI Shapefile")

#### two sample t-test ####

# select grid edge and core from empty grids
westarm <- readOGR("Data/west_arm_park.shp")
westarm_grid <- readOGR("park grids/westarm_grid.shp")
westarm_line <- as(westarm, "SpatialLinesDataFrame")
grid_edge_index <- rgeos::intersect(westarm_grid, westarm_line)

grid_edge <- westarm_grid %>%
  subset(westarm_grid$gridID %in% grid_edge_index$gridID)

grid_core <- westarm_grid %>%
  subset(!westarm_grid$gridID %in% grid_edge_index$gridID)

plot(grid_edge)
plot(westarm, add = T)

plot(grid_core)
plot(westarm, add = T)

# select grid edge and core from metrics grids

flist_metrics <- list.files(path = "metrics_arcgis_in/westarm_shp",
                            pattern = ".shp",
                            full.names = T)

t_list <- c()
df_list <- c()
p_list <- c()
file_name_list <- c()

for (i in flist_metrics){
  
  # load in the metrics shapefile
  metrics_shp <- readOGR(i, verbose = T)
  
  # convert metrics shapefile as data frame
  df <- as.data.frame(metrics_shp)
  
  # add a new column denoting edge and core
  df <- df %>% 
    mutate(place = case_when(df$gridID %in% grid_edge_index$gridID ~ "edge",
                             !df$gridID %in% grid_edge_index$gridID ~ "core"))
  
  # run the t-test
  t_test <- t.test(value ~ place, data = df)
  
  # save the t test results
  t_list <- append(t_list, t_test$statistic[1])
  df_list <- append(df_list, t_test$parameter[1])
  p_list <- append(p_list, t_test$p.value[1])
  
  # get file names from file paths
  file_name <- basename(i)
  file_name <- gsub("\\.shp*","",file_name)
  file_name_list <- append(file_name_list, file_name)
  
  # display boxplots
  ggplot(df, aes(place, value))+geom_boxplot()+labs(title = file_name)
  
}

list_list <- list(file_name_list, t_list, p_list, df_list)
output <- as.data.frame(do.call(cbind, list_list))
output <- rename(output, metrics = V1, t_value = V2, p_value = V3, df = V4)
write.csv(output, "t_test_csv/westarm.csv")
