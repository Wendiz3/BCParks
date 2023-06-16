#### Manning PARK ####

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

#### load park boundaries ####
manning <- st_read("Data//e_c__manning_park.SHP") # BC Albers

#### make the grid based on park boundaries ####
gridsize <- 1000  # size of squares, in units of the CRS (metre for BC Albers)

#big rectangular grids covering the whole extent of manning
grid1 <- st_make_grid(manning, square = TRUE, cellsize = c(gridsize, gridsize)) %>% st_sf() 
plot(grid1, col = "white")

#do a logical test to check which cells intersect with the park boundary 
manning_grid_index <- st_intersects(manning, grid1)

#index all true values
manning_grid <- grid1[manning_grid_index[[1]],]

#plot manning grids
plot(st_geometry(manning_grid))
plot(manning_grid)

# write to a shapefile
# st_write(manning_grid, "park grids\\manning_grid.shp", driver="ESRI Shapefile")

#### add gridID to park grid data frame ####
manning_grid$gridID <- 1:nrow(manning_grid)

### write csv to save the park grids ####
write.csv(manning_grid, "manning_grid.csv", row.names = FALSE)

### prep forest rasters ####
manning_89 <- rast("inraster/manning_89.TIF") # BC Albers
manning_99 <- rast("inraster/manning_99.TIF")
manning_09 <- rast("inraster/manning_09.TIF")
manning_19 <- rast("inraster/manning_19.TIF")

manning_89_masked <- terra::mask(manning_89, vect(manning_grid))
manning_99_masked <- terra::mask(manning_99, vect(manning_grid))
manning_09_masked <- terra::mask(manning_09, vect(manning_grid))
manning_19_masked <- terra::mask(manning_19, vect(manning_grid))

plot(manning_89)
plot(manning_99_masked)

### metrics ####

# 1989

manning_89_out <- tibble()
for (i in 1:nrow(manning_grid)) {
  tryCatch({
    print(i)
    manning_crop_89 <-
      terra::crop(manning_89_masked, manning_grid[i,], snap = "in")
    
    manning_89_metrics <-
      lsm_c_para_mn(manning_crop_89) %>%
      bind_rows(lsm_c_cpland(manning_crop_89)) %>%
      mutate(gridID = i)
    
    manning_89_out <-
      bind_rows(manning_89_out, manning_89_metrics)
  }, warning = function(w) {
    
    print(w)
  }, error = function(e) {
    print(e)
  })
}

write_csv(manning_89_out, "metrics\\manning_89.csv")


# 1999

manning_99_out <- tibble()
for (i in 1:nrow(manning_grid)) {
  tryCatch({
    print(i)
    manning_crop_99 <-
      terra::crop(manning_99_masked, manning_grid[i,], snap = "in")
    
    manning_99_metrics <-
      lsm_c_para_mn(manning_crop_99) %>%
      bind_rows(lsm_c_cpland(manning_crop_99)) %>%
      mutate(gridID = i)
    
    manning_99_out <-
      bind_rows(manning_99_out, manning_99_metrics)
  }, warning = function(w) {
    
    print(w)
  }, error = function(e) {
    print(e)
  })
}

write_csv(manning_99_out, "metrics\\manning_99.csv")

# 2009

manning_09_out <- tibble()
for (i in 1:nrow(manning_grid)) {
  tryCatch({
    print(i)
    manning_crop_09 <-
      terra::crop(manning_09_masked, manning_grid[i,], snap = "in")
    
    manning_09_metrics <-
      lsm_c_para_mn(manning_crop_09) %>%
      bind_rows(lsm_c_cpland(manning_crop_09)) %>%
      mutate(gridID = i)
    
    manning_09_out <-
      bind_rows(manning_09_out, manning_09_metrics)
  }, warning = function(w) {
    
    print(w)
  }, error = function(e) {
    print(e)
  })
}

write_csv(manning_09_out, "metrics\\manning_09.csv")

# 2019

manning_19_out <- tibble()
for (i in 1:nrow(manning_grid)) {
  tryCatch({
    print(i)
    manning_crop_19 <-
      terra::crop(manning_19_masked, manning_grid[i,], snap = "in")
    
    manning_19_metrics <-
      lsm_c_para_mn(manning_crop_19) %>%
      bind_rows(lsm_c_cpland(manning_crop_19)) %>%
      mutate(gridID = i)
    
    manning_19_out <-
      bind_rows(manning_19_out, manning_19_metrics)
  }, warning = function(w) {
    
    print(w)
  }, error = function(e) {
    print(e)
  })
}

write_csv(manning_19_out, "metrics\\manning_19.csv")

### subset by class and metrics ####

# 1989

manning_89 <- read.csv("metrics/manning_89.csv")
manning_89_forest_cpland <- subset(manning_89, class == 1 & metric == "cpland")
write.csv(manning_89_forest_cpland, "metrics_arcgis_in/manning_89_forest_cpland.csv")

manning_89_shrub_cpland <- subset(manning_89, class == 2 & metric == "cpland")
write.csv(manning_89_shrub_cpland, "metrics_arcgis_in/manning_89_shrub_cpland.csv")

manning_89_forest_para <- subset(manning_89, class == 1 & metric == "para_mn")
write.csv(manning_89_forest_para, "metrics_arcgis_in/manning_89_forest_para.csv")

manning_89_shrub_para <- subset(manning_89, class == 2 & metric == "para_mn")
write.csv(manning_89_shrub_para, "metrics_arcgis_in/manning_89_shrub_para.csv")

# 1999

manning_99 <- read.csv("metrics/manning_99.csv")
manning_99_forest_cpland <- subset(manning_99, class == 1 & metric == "cpland")
write.csv(manning_99_forest_cpland, "metrics_arcgis_in/manning_99_forest_cpland.csv")

manning_99_shrub_cpland <- subset(manning_99, class == 2 & metric == "cpland")
write.csv(manning_99_shrub_cpland, "metrics_arcgis_in/manning_99_shrub_cpland.csv")

manning_99_forest_para <- subset(manning_99, class == 1 & metric == "para_mn")
write.csv(manning_99_forest_para, "metrics_arcgis_in/manning_99_forest_para.csv")

manning_99_shrub_para <- subset(manning_99, class == 2 & metric == "para_mn")
write.csv(manning_99_shrub_para, "metrics_arcgis_in/manning_99_shrub_para.csv")

# 2009

manning_09 <- read.csv("metrics/manning_09.csv")
manning_09_forest_cpland <- subset(manning_09, class == 1 & metric == "cpland")
write.csv(manning_09_forest_cpland, "metrics_arcgis_in/manning_09_forest_cpland.csv")

manning_09_shrub_cpland <- subset(manning_09, class == 2 & metric == "cpland")
write.csv(manning_09_shrub_cpland, "metrics_arcgis_in/manning_09_shrub_cpland.csv")

manning_09_forest_para <- subset(manning_09, class == 1 & metric == "para_mn")
write.csv(manning_09_forest_para, "metrics_arcgis_in/manning_09_forest_para.csv")

manning_09_shrub_para <- subset(manning_09, class == 2 & metric == "para_mn")
write.csv(manning_09_shrub_para, "metrics_arcgis_in/manning_09_shrub_para.csv")

# 2019

manning_19 <- read.csv("metrics/manning_19.csv")
manning_19_forest_cpland <- subset(manning_19, class == 1 & metric == "cpland")
write.csv(manning_19_forest_cpland, "metrics_arcgis_in/manning_19_forest_cpland.csv")

manning_19_shrub_cpland <- subset(manning_19, class == 2 & metric == "cpland")
write.csv(manning_19_shrub_cpland, "metrics_arcgis_in/manning_19_shrub_cpland.csv")

manning_19_forest_para <- subset(manning_19, class == 1 & metric == "para_mn")
write.csv(manning_19_forest_para, "metrics_arcgis_in/manning_19_forest_para.csv")

manning_19_shrub_para <- subset(manning_19, class == 2 & metric == "para_mn")
write.csv(manning_19_shrub_para, "metrics_arcgis_in/manning_19_shrub_para.csv")

### join ####

# 1989

manning_89_shrub_cpland <- merge(manning_89_shrub_cpland, manning_grid, all=T)
manning_89_forest_cpland <- merge(manning_89_forest_cpland, manning_grid, all = T)

manning_89_shrub_para <- merge(manning_89_shrub_para, manning_grid, all = T)
manning_89_forest_para <- merge(manning_89_forest_para, manning_grid, all = T)

# 1999

manning_99_shrub_cpland <- merge(manning_99_shrub_cpland, manning_grid, all=T)
manning_99_forest_cpland <- merge(manning_99_forest_cpland, manning_grid, all = T)

manning_99_shrub_para <- merge(manning_99_shrub_para, manning_grid, all = T)
manning_99_forest_para <- merge(manning_99_forest_para, manning_grid, all = T)

# 2009

manning_09_shrub_cpland <- merge(manning_09_shrub_cpland, manning_grid, all=T)
manning_09_forest_cpland <- merge(manning_09_forest_cpland, manning_grid, all = T)

manning_09_shrub_para <- merge(manning_09_shrub_para, manning_grid, all = T)
manning_09_forest_para <- merge(manning_09_forest_para, manning_grid, all = T)

# 2019

manning_19_shrub_cpland <- merge(manning_19_shrub_cpland, manning_grid, all=T)
manning_19_forest_cpland <- merge(manning_19_forest_cpland, manning_grid, all = T)

manning_19_shrub_para <- merge(manning_19_shrub_para, manning_grid, all = T)
manning_19_forest_para <- merge(manning_19_forest_para, manning_grid, all = T)

#### write shapefile ####

# 1989

st_write(manning_89_shrub_cpland,"metrics_arcgis_in/in/manning_89_shrub_cpland.shp", driver="ESRI Shapefile")
st_write(manning_89_forest_cpland,"metrics_arcgis_in/in/manning_89_forest_cpland.shp", driver="ESRI Shapefile")

st_write(manning_89_shrub_para,"metrics_arcgis_in/in/manning_89_shrub_para.shp", driver="ESRI Shapefile")
st_write(manning_89_forest_para,"metrics_arcgis_in/in/manning_89_forest_para.shp", driver="ESRI Shapefile")

# 1999

st_write(manning_99_shrub_cpland,"metrics_arcgis_in/in/manning_99_shrub_cpland.shp", driver="ESRI Shapefile")
st_write(manning_99_forest_cpland,"metrics_arcgis_in/in/manning_99_forest_cpland.shp", driver="ESRI Shapefile")

st_write(manning_99_shrub_para,"metrics_arcgis_in/in/manning_99_shrub_para.shp", driver="ESRI Shapefile")
st_write(manning_99_forest_para,"metrics_arcgis_in/in/manning_99_forest_para.shp", driver="ESRI Shapefile")

# 2009

st_write(manning_09_shrub_cpland,"metrics_arcgis_in/in/manning_09_shrub_cpland.shp", driver="ESRI Shapefile")
st_write(manning_09_forest_cpland,"metrics_arcgis_in/in/manning_09_forest_cpland.shp", driver="ESRI Shapefile")

st_write(manning_09_shrub_para,"metrics_arcgis_in/in/manning_09_shrub_para.shp", driver="ESRI Shapefile")
st_write(manning_09_forest_para,"metrics_arcgis_in/in/manning_09_forest_para.shp", driver="ESRI Shapefile")

# 2019

st_write(manning_19_shrub_cpland,"metrics_arcgis_in/in/manning_19_shrub_cpland.shp", driver="ESRI Shapefile")
st_write(manning_19_forest_cpland,"metrics_arcgis_in/in/manning_19_forest_cpland.shp", driver="ESRI Shapefile")

st_write(manning_19_shrub_para,"metrics_arcgis_in/in/manning_19_shrub_para.shp", driver="ESRI Shapefile")
st_write(manning_19_forest_para,"metrics_arcgis_in/in/manning_19_forest_para.shp", driver="ESRI Shapefile")

select grid edge and core from metrics grids

select grid edge and core from empty grids
manning <- readOGR("Data/e_c__manning_park.shp")
manning_grid <- readOGR("park grids/manning_grid.shp")
manning_line <- as(manning, "SpatialLinesDataFrame")
grid_edge_index <- rgeos::intersect(manning_grid, manning_line)

grid_edge <- manning_grid %>%
  subset(manning_grid$gridID %in% grid_edge_index$gridID)

grid_core <- manning_grid %>%
  subset(!manning_grid$gridID %in% grid_edge_index$gridID)

plot(grid_edge)
plot(manning, add = T)

plot(grid_core)
plot(manning, add = T)

flist <- list.files(path = "metrics_arcgis_in/manning_shp",
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
write.csv(output, "t_test_csv/manning.csv")

ggplot(df, aes(place, value))+geom_boxplot()






