#### TWEEDSMUIR PARK ####

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
tweedsmuir <- st_read("Data//tweedsmuir_park.SHP") # BC Albers

#### make the grid based on park boundaries ####
gridsize <- 1000  # size of squares, in units of the CRS (metre for BC Albers)

#big rectangular grids covering the whole extent of tweedsmuir
grid1 <- st_make_grid(tweedsmuir, square = TRUE, cellsize = c(gridsize, gridsize)) %>% st_sf() 
plot(grid1, col = "white")

#do a logical test to check which cells intersect with the park boundary 
tweedsmuir_grid_index <- st_intersects(tweedsmuir, grid1)

#index all true values
tweedsmuir_grid <- grid1[tweedsmuir_grid_index[[1]],]

#plot tweedsmuir grids
plot(st_geometry(tweedsmuir_grid))
plot(tweedsmuir_grid)

# write to a shapefile
# st_write(tweedsmuir_grid, "park grids\\tweedsmuir_grid.shp", driver="ESRI Shapefile")

#### add gridID to park grid data frame ####
tweedsmuir_grid$gridID <- 1:nrow(tweedsmuir_grid)

#### write csv to save the park grids ####
# write.csv(tweedsmuir_grid, "tweedsmuir_grid.csv", row.names = FALSE)

#### prep forest rasters ####
tweedsmuir_89 <- rast("inraster/tweedsmuir_89.TIF") # BC Albers
tweedsmuir_99 <- rast("inraster/tweedsmuir_99.TIF")
tweedsmuir_09 <- rast("inraster/tweedsmuir_09.TIF")
tweedsmuir_19 <- rast("inraster/tweedsmuir_19.TIF")

tweedsmuir_89_masked <- terra::mask(tweedsmuir_89, vect(tweedsmuir_grid))
tweedsmuir_99_masked <- terra::mask(tweedsmuir_99, vect(tweedsmuir_grid))
tweedsmuir_09_masked <- terra::mask(tweedsmuir_09, vect(tweedsmuir_grid))
tweedsmuir_19_masked <- terra::mask(tweedsmuir_19, vect(tweedsmuir_grid))

plot(tweedsmuir_89)
plot(tweedsmuir_99_masked)

#### metrics ####

1989

tweedsmuir_89_out <- tibble()
for (i in 1:nrow(tweedsmuir_grid)) {
  tryCatch({
    print(i)
    tweedsmuir_crop_89 <-
      terra::crop(tweedsmuir_89_masked, tweedsmuir_grid[i,], snap = "in")
    
    tweedsmuir_89_metrics <-
      lsm_c_para_mn(tweedsmuir_crop_89) %>%
      bind_rows(lsm_c_cpland(tweedsmuir_crop_89)) %>%
      mutate(gridID = i)
    
    tweedsmuir_89_out <-
      bind_rows(tweedsmuir_89_out, tweedsmuir_89_metrics)
  }, warning = function(w) {
    
    print(w)
  }, error = function(e) {
    print(e)
  })
}

write_csv(tweedsmuir_89_out, "metrics\\tweedsmuir_89.csv")


# 1999

tweedsmuir_99_out <- tibble()
for (i in 1:nrow(tweedsmuir_grid)) {
  tryCatch({
    print(i)
    tweedsmuir_crop_99 <-
      terra::crop(tweedsmuir_99_masked, tweedsmuir_grid[i,], snap = "in")
    
    tweedsmuir_99_metrics <-
      lsm_c_para_mn(tweedsmuir_crop_99) %>%
      bind_rows(lsm_c_cpland(tweedsmuir_crop_99)) %>%
      mutate(gridID = i)
    
    tweedsmuir_99_out <-
      bind_rows(tweedsmuir_99_out, tweedsmuir_99_metrics)
  }, warning = function(w) {
    
    print(w)
  }, error = function(e) {
    print(e)
  })
}

write_csv(tweedsmuir_99_out, "metrics\\tweedsmuir_99.csv")

# 2009

tweedsmuir_09_out <- tibble()
for (i in 1:nrow(tweedsmuir_grid)) {
  tryCatch({
    print(i)
    tweedsmuir_crop_09 <-
      terra::crop(tweedsmuir_09_masked, tweedsmuir_grid[i,], snap = "in")
    
    tweedsmuir_09_metrics <-
      lsm_c_para_mn(tweedsmuir_crop_09) %>%
      bind_rows(lsm_c_cpland(tweedsmuir_crop_09)) %>%
      mutate(gridID = i)
    
    tweedsmuir_09_out <-
      bind_rows(tweedsmuir_09_out, tweedsmuir_09_metrics)
  }, warning = function(w) {
    
    print(w)
  }, error = function(e) {
    print(e)
  })
}

write_csv(tweedsmuir_09_out, "metrics\\tweedsmuir_09.csv")

# 2019

tweedsmuir_19_out <- tibble()
for (i in 1:nrow(tweedsmuir_grid)) {
  tryCatch({
    print(i)
    tweedsmuir_crop_19 <-
      terra::crop(tweedsmuir_19_masked, tweedsmuir_grid[i,], snap = "in")
    
    tweedsmuir_19_metrics <-
      lsm_c_para_mn(tweedsmuir_crop_19) %>%
      bind_rows(lsm_c_cpland(tweedsmuir_crop_19)) %>%
      mutate(gridID = i)
    
    tweedsmuir_19_out <-
      bind_rows(tweedsmuir_19_out, tweedsmuir_19_metrics)
  }, warning = function(w) {
    
    print(w)
  }, error = function(e) {
    print(e)
  })
}

write_csv(tweedsmuir_19_out, "metrics\\tweedsmuir_19.csv")

#### subset by class and metrics ####

1989

tweedsmuir_89 <- read.csv("metrics/tweedsmuir_89.csv")
tweedsmuir_89_forest_cpland <- subset(tweedsmuir_89, class == 1 & metric == "cpland")
write.csv(tweedsmuir_89_forest_cpland, "metrics_arcgis_in/tweedsmuir_89_forest_cpland.csv")

tweedsmuir_89_shrub_cpland <- subset(tweedsmuir_89, class == 2 & metric == "cpland")
write.csv(tweedsmuir_89_shrub_cpland, "metrics_arcgis_in/tweedsmuir_89_shrub_cpland.csv")

tweedsmuir_89_forest_para <- subset(tweedsmuir_89, class == 1 & metric == "para_mn")
write.csv(tweedsmuir_89_forest_para, "metrics_arcgis_in/tweedsmuir_89_forest_para.csv")

tweedsmuir_89_shrub_para <- subset(tweedsmuir_89, class == 2 & metric == "para_mn")
write.csv(tweedsmuir_89_shrub_para, "metrics_arcgis_in/tweedsmuir_89_shrub_para.csv")

# 1999

tweedsmuir_99 <- read.csv("metrics/tweedsmuir_99.csv")
tweedsmuir_99_forest_cpland <- subset(tweedsmuir_99, class == 1 & metric == "cpland")
write.csv(tweedsmuir_99_forest_cpland, "metrics_arcgis_in/tweedsmuir_99_forest_cpland.csv")

tweedsmuir_99_shrub_cpland <- subset(tweedsmuir_99, class == 2 & metric == "cpland")
write.csv(tweedsmuir_99_shrub_cpland, "metrics_arcgis_in/tweedsmuir_99_shrub_cpland.csv")

tweedsmuir_99_forest_para <- subset(tweedsmuir_99, class == 1 & metric == "para_mn")
write.csv(tweedsmuir_99_forest_para, "metrics_arcgis_in/tweedsmuir_99_forest_para.csv")

tweedsmuir_99_shrub_para <- subset(tweedsmuir_99, class == 2 & metric == "para_mn")
write.csv(tweedsmuir_99_shrub_para, "metrics_arcgis_in/tweedsmuir_99_shrub_para.csv")

# 2009

tweedsmuir_09 <- read.csv("metrics/tweedsmuir_09.csv")
tweedsmuir_09_forest_cpland <- subset(tweedsmuir_09, class == 1 & metric == "cpland")
write.csv(tweedsmuir_09_forest_cpland, "metrics_arcgis_in/tweedsmuir_09_forest_cpland.csv")

tweedsmuir_09_shrub_cpland <- subset(tweedsmuir_09, class == 2 & metric == "cpland")
write.csv(tweedsmuir_09_shrub_cpland, "metrics_arcgis_in/tweedsmuir_09_shrub_cpland.csv")

tweedsmuir_09_forest_para <- subset(tweedsmuir_09, class == 1 & metric == "para_mn")
write.csv(tweedsmuir_09_forest_para, "metrics_arcgis_in/tweedsmuir_09_forest_para.csv")

tweedsmuir_09_shrub_para <- subset(tweedsmuir_09, class == 2 & metric == "para_mn")
write.csv(tweedsmuir_09_shrub_para, "metrics_arcgis_in/tweedsmuir_09_shrub_para.csv")

# 2019

tweedsmuir_19 <- read.csv("metrics/tweedsmuir_19.csv")
tweedsmuir_19_forest_cpland <- subset(tweedsmuir_19, class == 1 & metric == "cpland")
write.csv(tweedsmuir_19_forest_cpland, "metrics_arcgis_in/tweedsmuir_19_forest_cpland.csv")

tweedsmuir_19_shrub_cpland <- subset(tweedsmuir_19, class == 2 & metric == "cpland")
write.csv(tweedsmuir_19_shrub_cpland, "metrics_arcgis_in/tweedsmuir_19_shrub_cpland.csv")

tweedsmuir_19_forest_para <- subset(tweedsmuir_19, class == 1 & metric == "para_mn")
write.csv(tweedsmuir_19_forest_para, "metrics_arcgis_in/tweedsmuir_19_forest_para.csv")

tweedsmuir_19_shrub_para <- subset(tweedsmuir_19, class == 2 & metric == "para_mn")
write.csv(tweedsmuir_19_shrub_para, "metrics_arcgis_in/tweedsmuir_19_shrub_para.csv")

#### join ####

# 1989

tweedsmuir_89_shrub_cpland <- merge(tweedsmuir_89_shrub_cpland, tweedsmuir_grid, all=T)
tweedsmuir_89_forest_cpland <- merge(tweedsmuir_89_forest_cpland, tweedsmuir_grid, all = T)

tweedsmuir_89_shrub_para <- merge(tweedsmuir_89_shrub_para, tweedsmuir_grid, all = T)
tweedsmuir_89_forest_para <- merge(tweedsmuir_89_forest_para, tweedsmuir_grid, all = T)

# 1999

tweedsmuir_99_shrub_cpland <- merge(tweedsmuir_99_shrub_cpland, tweedsmuir_grid, all=T)
tweedsmuir_99_forest_cpland <- merge(tweedsmuir_99_forest_cpland, tweedsmuir_grid, all = T)

tweedsmuir_99_shrub_para <- merge(tweedsmuir_99_shrub_para, tweedsmuir_grid, all = T)
tweedsmuir_99_forest_para <- merge(tweedsmuir_99_forest_para, tweedsmuir_grid, all = T)

# 2009

tweedsmuir_09_shrub_cpland <- merge(tweedsmuir_09_shrub_cpland, tweedsmuir_grid, all=T)
tweedsmuir_09_forest_cpland <- merge(tweedsmuir_09_forest_cpland, tweedsmuir_grid, all = T)

tweedsmuir_09_shrub_para <- merge(tweedsmuir_09_shrub_para, tweedsmuir_grid, all = T)
tweedsmuir_09_forest_para <- merge(tweedsmuir_09_forest_para, tweedsmuir_grid, all = T)

# 2019

tweedsmuir_19_shrub_cpland <- merge(tweedsmuir_19_shrub_cpland, tweedsmuir_grid, all=T)
tweedsmuir_19_forest_cpland <- merge(tweedsmuir_19_forest_cpland, tweedsmuir_grid, all = T)

tweedsmuir_19_shrub_para <- merge(tweedsmuir_19_shrub_para, tweedsmuir_grid, all = T)
tweedsmuir_19_forest_para <- merge(tweedsmuir_19_forest_para, tweedsmuir_grid, all = T)

#### write shapefile ####

# 1989

st_write(tweedsmuir_89_shrub_cpland,"metrics_arcgis_in/in/tweedsmuir_89_shrub_cpland.shp", driver="ESRI Shapefile")
st_write(tweedsmuir_89_forest_cpland,"metrics_arcgis_in/in/tweedsmuir_89_forest_cpland.shp", driver="ESRI Shapefile")

st_write(tweedsmuir_89_shrub_para,"metrics_arcgis_in/in/tweedsmuir_89_shrub_para.shp", driver="ESRI Shapefile")
st_write(tweedsmuir_89_forest_para,"metrics_arcgis_in/in/tweedsmuir_89_forest_para.shp", driver="ESRI Shapefile")

# 1999

st_write(tweedsmuir_99_shrub_cpland,"metrics_arcgis_in/in/tweedsmuir_99_shrub_cpland.shp", driver="ESRI Shapefile")
st_write(tweedsmuir_99_forest_cpland,"metrics_arcgis_in/in/tweedsmuir_99_forest_cpland.shp", driver="ESRI Shapefile")

st_write(tweedsmuir_99_shrub_para,"metrics_arcgis_in/in/tweedsmuir_99_shrub_para.shp", driver="ESRI Shapefile")
st_write(tweedsmuir_99_forest_para,"metrics_arcgis_in/in/tweedsmuir_99_forest_para.shp", driver="ESRI Shapefile")

# 2009

st_write(tweedsmuir_09_shrub_cpland,"metrics_arcgis_in/in/tweedsmuir_09_shrub_cpland.shp", driver="ESRI Shapefile")
st_write(tweedsmuir_09_forest_cpland,"metrics_arcgis_in/in/tweedsmuir_09_forest_cpland.shp", driver="ESRI Shapefile")

st_write(tweedsmuir_09_shrub_para,"metrics_arcgis_in/in/tweedsmuir_09_shrub_para.shp", driver="ESRI Shapefile")
st_write(tweedsmuir_09_forest_para,"metrics_arcgis_in/in/tweedsmuir_09_forest_para.shp", driver="ESRI Shapefile")

# 2019

st_write(tweedsmuir_19_shrub_cpland,"metrics_arcgis_in/in/tweedsmuir_19_shrub_cpland.shp", driver="ESRI Shapefile")
st_write(tweedsmuir_19_forest_cpland,"metrics_arcgis_in/in/tweedsmuir_19_forest_cpland.shp", driver="ESRI Shapefile")

st_write(tweedsmuir_19_shrub_para,"metrics_arcgis_in/in/tweedsmuir_19_shrub_para.shp", driver="ESRI Shapefile")
st_write(tweedsmuir_19_forest_para,"metrics_arcgis_in/in/tweedsmuir_19_forest_para.shp", driver="ESRI Shapefile")


#### t-test ####

# select grid edge and core from grids
tweedsmuir <- readOGR("Data/tweedsmuir_park.shp")
tweedsmuir_grid <- readOGR("park grids/tweedsmuir_grid.shp")
tweedsmuir_line <- as(tweedsmuir, "SpatialLinesDataFrame")
grid_edge_index <- rgeos::intersect(tweedsmuir_grid, tweedsmuir_line)

grid_edge <- tweedsmuir_grid %>%
  subset(tweedsmuir_grid$gridID %in% grid_edge_index$gridID)

grid_core <- tweedsmuir_grid %>%
  subset(!tweedsmuir_grid$gridID %in% grid_edge_index$gridID)

plot(grid_edge)
plot(tweedsmuir, add = T)

plot(grid_core)
plot(tweedsmuir, add = T)

flist <- list.files(path = "metrics_arcgis_in/tweedsmuir_shp",
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
write.csv(output, "t_test_csv/tweedsmuir.csv")

ggplot(df, aes(place, value))+geom_boxplot()
