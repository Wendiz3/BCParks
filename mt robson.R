#### MOUNT ROBSON PARK ####

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

dir <- setwd("C://Users//wendiz3.stu//OneDrive - UBC//Desktop//BCParks")

#### load park boundaries ####
mtrobson <- st_read("Data//mount_robson_park.SHP") # BC Albers

#### make the grid based on park boundaries ####
gridsize <- 1000  # size of squares, in units of the CRS (metre for BC Albers)

#big rectangular grids covering the whole extent of mtrobson
grid1 <- st_make_grid(mtrobson, square = TRUE, cellsize = c(gridsize, gridsize)) %>% st_sf() 
plot(grid1, col = "white")

#do a logical test to check which cells intersect with the park boundary 
mtrobson_grid_index <- st_intersects(mtrobson, grid1)

#index all true values
mtrobson_grid <- grid1[mtrobson_grid_index[[1]],]
 
#plot mtrobson grids
plot(st_geometry(mtrobson_grid))
plot(mtrobson_grid)
 
# write to a shapefile
st_write(mtrobson_grid, "park grids\\mtrobson_grid.shp", driver="ESRI Shapefile")

#### add gridID to park grid data frame ####
mtrobson_grid$gridID <- 1:nrow(mtrobson_grid)
 
#### write csv to save the park grids ####
# write.csv(mtrobson_grid, "mtrobson_grid.csv", row.names = FALSE)
 
#### prep rasters ####
mtrobson_89 <- rast("inraster/mtrobson_89.TIF") # BC Albers
mtrobson_99 <- rast("inraster/mtrobson_99.TIF")
mtrobson_09 <- rast("inraster/mtrobson_09.TIF")
mtrobson_19 <- rast("inraster/mtrobson_19.TIF")


#reclassification matrix to fix many small numbers (converts to int)
m <- c(0, 1, 0,
       1, 2, 1)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
 
mtrobson_frst_89 <- project(mtrobson_frst_89, crs(mtrobson)) %>% classify(., rclmat, right = F)
mtrobson_frst_99 <- project(mtrobson_frst_99, crs(mtrobson)) %>% classify(., rclmat, right = F)
mtrobson_frst_09 <- project(mtrobson_frst_09, crs(mtrobson)) %>% classify(., rclmat, right = F)
mtrobson_frst_19 <- project(mtrobson_frst_19, crs(mtrobson)) %>% classify(., rclmat, right = F)

mtrobson_89_masked <- terra::mask(mtrobson_89, vect(mtrobson_grid))
mtrobson_99_masked <- terra::mask(mtrobson_99, vect(mtrobson_grid)) 
mtrobson_09_masked <- terra::mask(mtrobson_09, vect(mtrobson_grid))
mtrobson_19_masked <- terra::mask(mtrobson_19, vect(mtrobson_grid))
 
plot(mtrobson_89)
plot(mtrobson_89_masked)

#### metrics ####

#1989

mtrobson_89_out <- tibble()
for (i in 1:nrow(mtrobson_grid)) {
   tryCatch({
     print(i)
     mtrobson_crop_89 <- terra::crop(mtrobson_89_masked, mtrobson_grid[i,], snap = "in")
     mtrobson_89_metrics <- lsm_c_para_mn(mtrobson_crop_89) %>% 
     bind_rows(lsm_c_cpland(mtrobson_crop_89)) %>% 
     mutate(gridID = i)
     
     mtrobson_89_out <-
       bind_rows(mtrobson_89_out, mtrobson_89_metrics)
   }, warning = function(w) {
     
     print(w)
   }, error = function(e) {
     print(e)
   })
 }

write_csv(mtrobson_89_out, "metrics\\mtrobson_89.csv")
 
 
# 1999
 
mtrobson_99_out <- tibble()
for (i in 1:nrow(mtrobson_grid)) {
   tryCatch({
     print(i)
     mtrobson_crop_99 <-
       terra::crop(mtrobson_99_masked, mtrobson_grid[i,], snap = "in")
     
     mtrobson_99_metrics <-
       lsm_c_para_mn(mtrobson_crop_99) %>%
       bind_rows(lsm_c_cpland(mtrobson_crop_99)) %>%
       mutate(gridID = i)
     
     mtrobson_99_out <-
       bind_rows(mtrobson_99_out, mtrobson_99_metrics)
   }, warning = function(w) {
     
     print(w)
   }, error = function(e) {
     print(e)
   })
 }
 
write_csv(mtrobson_99_out, "metrics\\mtrobson_99.csv")
 
# 2009
 
mtrobson_09_out <- tibble()
for (i in 1:nrow(mtrobson_grid)) {
   tryCatch({
     print(i)
     mtrobson_crop_09 <-
       terra::crop(mtrobson_09_masked, mtrobson_grid[i,], snap = "in")
     
     mtrobson_09_metrics <-
       lsm_c_para_mn(mtrobson_crop_09) %>%
       bind_rows(lsm_c_cpland(mtrobson_crop_09)) %>%
       mutate(gridID = i)
     
     mtrobson_09_out <-
       bind_rows(mtrobson_09_out, mtrobson_09_metrics)
   }, warning = function(w) {
     
     print(w)
   }, error = function(e) {
     print(e)
   })
 }
 
write_csv(mtrobson_09_out, "metrics\\mtrobson_09.csv")
 
# 2019
 
 mtrobson_19_out <- tibble()
 for (i in 1:nrow(mtrobson_grid)) {
   tryCatch({
     print(i)
     mtrobson_crop_19 <-
       terra::crop(mtrobson_19_masked, mtrobson_grid[i,], snap = "in")
     
     mtrobson_19_metrics <-
       lsm_c_para_mn(mtrobson_crop_19) %>%
       bind_rows(lsm_c_cpland(mtrobson_crop_19)) %>%
       mutate(gridID = i)
     
     mtrobson_19_out <-
       bind_rows(mtrobson_19_out, mtrobson_19_metrics)
   }, warning = function(w) {
     
     print(w)
   }, error = function(e) {
     print(e)
   })
 }
 
write_csv(mtrobson_19_out, "metrics\\mtrobson_19.csv")

#### subset by class and metrics ####

# 1989

mtrobson_89 <- read.csv("metrics/mtrobson_89.csv")
mtrobson_89_forest_cpland <- subset(mtrobson_89, class == 1 & metric == "cpland")
write.csv(mtrobson_89_forest_cpland, "metrics_arcgis_in/mtrobson_89_forest_cpland.csv")
 
mtrobson_89_shrub_cpland <- subset(mtrobson_89, class == 2 & metric == "cpland")
write.csv(mtrobson_89_shrub_cpland, "metrics_arcgis_in/mtrobson_89_shrub_cpland.csv")
 
mtrobson_89_forest_para <- subset(mtrobson_89, class == 1 & metric == "para_mn")
write.csv(mtrobson_89_forest_para, "metrics_arcgis_in/mtrobson_89_forest_para.csv")

mtrobson_89_shrub_para <- subset(mtrobson_89, class == 2 & metric == "para_mn")
write.csv(mtrobson_89_shrub_para, "metrics_arcgis_in/mtrobson_89_shrub_para.csv")
 
# 1999
 
mtrobson_99 <- read.csv("metrics/mtrobson_99.csv")
mtrobson_99_forest_cpland <- subset(mtrobson_99, class == 1 & metric == "cpland")
write.csv(mtrobson_99_forest_cpland, "metrics_arcgis_in/mtrobson_99_forest_cpland.csv")
 
mtrobson_99_shrub_cpland <- subset(mtrobson_99, class == 2 & metric == "cpland")
write.csv(mtrobson_99_shrub_cpland, "metrics_arcgis_in/mtrobson_99_shrub_cpland.csv")
 
mtrobson_99_forest_para <- subset(mtrobson_99, class == 1 & metric == "para_mn")
write.csv(mtrobson_99_forest_para, "metrics_arcgis_in/mtrobson_99_forest_para.csv")
 
mtrobson_99_shrub_para <- subset(mtrobson_99, class == 2 & metric == "para_mn")
write.csv(mtrobson_99_shrub_para, "metrics_arcgis_in/mtrobson_99_shrub_para.csv")
 
# 2009

mtrobson_09 <- read.csv("metrics/mtrobson_09.csv")
mtrobson_09_forest_cpland <- subset(mtrobson_09, class == 1 & metric == "cpland")
write.csv(mtrobson_09_forest_cpland, "metrics_arcgis_in/mtrobson_09_forest_cpland.csv")
 
mtrobson_09_shrub_cpland <- subset(mtrobson_09, class == 2 & metric == "cpland")
write.csv(mtrobson_09_shrub_cpland, "metrics_arcgis_in/mtrobson_09_shrub_cpland.csv")
 
mtrobson_09_forest_para <- subset(mtrobson_09, class == 1 & metric == "para_mn")
write.csv(mtrobson_09_forest_para, "metrics_arcgis_in/mtrobson_09_forest_para.csv")
 
mtrobson_09_shrub_para <- subset(mtrobson_09, class == 2 & metric == "para_mn")
write.csv(mtrobson_09_shrub_para, "metrics_arcgis_in/mtrobson_09_shrub_para.csv")

# 2019
 
mtrobson_19 <- read.csv("metrics/mtrobson_19.csv")
mtrobson_19_forest_cpland <- subset(mtrobson_19, class == 1 & metric == "cpland")
write.csv(mtrobson_19_forest_cpland, "metrics_arcgis_in/mtrobson_19_forest_cpland.csv")
 
mtrobson_19_shrub_cpland <- subset(mtrobson_19, class == 2 & metric == "cpland")
write.csv(mtrobson_19_shrub_cpland, "metrics_arcgis_in/mtrobson_19_shrub_cpland.csv")
 
mtrobson_19_forest_para <- subset(mtrobson_19, class == 1 & metric == "para_mn")
write.csv(mtrobson_19_forest_para, "metrics_arcgis_in/mtrobson_19_forest_para.csv")
 
mtrobson_19_shrub_para <- subset(mtrobson_19, class == 2 & metric == "para_mn")
write.csv(mtrobson_19_shrub_para, "metrics_arcgis_in/mtrobson_19_shrub_para.csv")

#### join ####

# 1989
 
mtrobson_89_shrub_cpland <- merge(mtrobson_89_shrub_cpland, mtrobson_grid, all=T)
mtrobson_89_forest_cpland <- merge(mtrobson_89_forest_cpland, mtrobson_grid, all = T)
 
mtrobson_89_shrub_para <- merge(mtrobson_89_shrub_para, mtrobson_grid, all = T)
mtrobson_89_forest_para <- merge(mtrobson_89_forest_para, mtrobson_grid, all = T)
 
# 1999
 
mtrobson_99_shrub_cpland <- merge(mtrobson_99_shrub_cpland, mtrobson_grid, all=T)
mtrobson_99_forest_cpland <- merge(mtrobson_99_forest_cpland, mtrobson_grid, all = T)
 
mtrobson_99_shrub_para <- merge(mtrobson_99_shrub_para, mtrobson_grid, all = T)
mtrobson_99_forest_para <- merge(mtrobson_99_forest_para, mtrobson_grid, all = T)

# 2009
# 
mtrobson_09_shrub_cpland <- merge(mtrobson_09_shrub_cpland, mtrobson_grid, all=T)
mtrobson_09_forest_cpland <- merge(mtrobson_09_forest_cpland, mtrobson_grid, all = T)
 
mtrobson_09_shrub_para <- merge(mtrobson_09_shrub_para, mtrobson_grid, all = T)
mtrobson_09_forest_para <- merge(mtrobson_09_forest_para, mtrobson_grid, all = T)
 
# 2019
 
mtrobson_19_shrub_cpland <- merge(mtrobson_19_shrub_cpland, mtrobson_grid, all=T)
mtrobson_19_forest_cpland <- merge(mtrobson_19_forest_cpland, mtrobson_grid, all = T)
 
mtrobson_19_shrub_para <- merge(mtrobson_19_shrub_para, mtrobson_grid, all = T)
mtrobson_19_forest_para <- merge(mtrobson_19_forest_para, mtrobson_grid, all = T)

#### write shapefile ####
 
# 1989
 
st_write(mtrobson_89_shrub_cpland,"metrics_arcgis_in/in/mtrobson_89_shrub_cpland.shp", driver="ESRI Shapefile")
st_write(mtrobson_89_forest_cpland,"metrics_arcgis_in/in/mtrobson_89_forest_cpland.shp", driver="ESRI Shapefile")
 
st_write(mtrobson_89_shrub_para,"metrics_arcgis_in/in/mtrobson_89_shrub_para.shp", driver="ESRI Shapefile")
st_write(mtrobson_89_forest_para,"metrics_arcgis_in/in/mtrobson_89_forest_para.shp", driver="ESRI Shapefile")

# 1999
 
# st_write(mtrobson_99_shrub_cpland,"metrics_arcgis_in/in/mtrobson_99_shrub_cpland.shp", driver="ESRI Shapefile")
st_write(mtrobson_99_forest_cpland,"metrics_arcgis_in/in/mtrobson_99_forest_cpland.shp", driver="ESRI Shapefile")
 
st_write(mtrobson_99_shrub_para,"metrics_arcgis_in/in/mtrobson_99_shrub_para.shp", driver="ESRI Shapefile")
st_write(mtrobson_99_forest_para,"metrics_arcgis_in/in/mtrobson_99_forest_para.shp", driver="ESRI Shapefile")

# 2009
 
st_write(mtrobson_09_shrub_cpland,"metrics_arcgis_in/in/mtrobson_09_shrub_cpland.shp", driver="ESRI Shapefile")
st_write(mtrobson_09_forest_cpland,"metrics_arcgis_in/in/mtrobson_09_forest_cpland.shp", driver="ESRI Shapefile")
 
st_write(mtrobson_09_shrub_para,"metrics_arcgis_in/in/mtrobson_09_shrub_para.shp", driver="ESRI Shapefile")
st_write(mtrobson_09_forest_para,"metrics_arcgis_in/in/mtrobson_09_forest_para.shp", driver="ESRI Shapefile")

# 2019
 
st_write(mtrobson_19_shrub_cpland,"metrics_arcgis_in/in/mtrobson_19_shrub_cpland.shp", driver="ESRI Shapefile")
st_write(mtrobson_19_forest_cpland,"metrics_arcgis_in/in/mtrobson_19_forest_cpland.shp", driver="ESRI Shapefile")
 
st_write(mtrobson_19_shrub_para,"metrics_arcgis_in/in/mtrobson_19_shrub_para.shp", driver="ESRI Shapefile")
st_write(mtrobson_19_forest_para,"metrics_arcgis_in/in/mtrobson_19_forest_para.shp", driver="ESRI Shapefile")

#### t-test ####

# select grid edge and core from empty grids
mtrobson <- readOGR("Data/mount_robson_park.shp")
mtrobson_grid <- readOGR("park grids/mtrobson_grid.shp")
mtrobson_line <- as(mtrobson, "SpatialLinesDataFrame")
grid_edge_index <- rgeos::intersect(mtrobson_grid, mtrobson_line)

grid_edge <- mtrobson_grid %>%
  subset(mtrobson_grid$gridID %in% grid_edge_index$gridID)

grid_core <- mtrobson_grid %>%
  subset(!mtrobson_grid$gridID %in% grid_edge_index$gridID)

plot(grid_edge)
plot(mtrobson, add = T)

plot(grid_core)
plot(mtrobson, add = T)

flist <- list.files(path = "metrics_arcgis_in/mtrobson_shp",
                    pattern = ".shp",
                    full.names = T)

t_list <- c()
df_list <- c()
p_list <- c()
file_name_list <- c()

for (i in flist){
  
  metrics_shp <- readOGR(i, verbose = T)
  
  df <- as.data.frame(metrics_shp)
  df <- df %>% 
    mutate(place = case_when(df$gridID %in% grid_edge_index$gridID ~ "edge",
                             !df$gridID %in% grid_edge_index$gridID ~ "core"))
  
  t_test <- t.test(value ~ place, data = df)
  
  t_list <- append(t_list, t_test$statistic[1])
  df_list <- append(df_list, t_test$parameter[1])
  p_list <- append(p_list, t_test$p.value[1])
  
  file_name <- basename(i)
  file_name <- gsub("\\.shp*","",file_name)
  file_name_list <- append(file_name_list, file_name)
}

list_list <- list(file_name_list, t_list, p_list, df_list)
output <- as.data.frame(do.call(cbind, list_list))
output <- rename(output, metrics = V1, t_value = V2, p_value = V3, df = V4)
write.csv(output, "t_test_csv/mtrobson.csv")

ggplot(df, aes(place, value))+geom_boxplot()
