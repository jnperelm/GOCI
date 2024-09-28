# Regrid chlorophyll and Kd490 datasets to simulate coarser spatial resolution grids at 1-day temporal resolution

library(raster)
library(data.table)

source("scripts/GOCI_functions.R")

#####################
### Chlorophyll-a ###
#####################

files <- list.files(paste0("GOCI_data/full_res"), pattern="*\\_chlor.RData", full.names=TRUE)
filenames <- list.files(paste0("GOCI_data/full_res"), pattern="*\\_chlor.RData", full.names=FALSE)
basenames = gsub("\\.RData$","", list.files(path = paste0("GOCI_data/full_res"), pattern="*\\_chlor.RData", full.names=FALSE))


for(i in 1:length(files)) {
  
  # i = 31
  
  load(files[i])

  # Pull out closest daily measurement to noon to get daily estimate for coarser resolutions (raw data is every 1-hr between 9am-4pm)
  timestamps <- as.POSIXct(gsub("X","",names(chlor_okinawa)), format = "%Y.%m.%d.%H.%M.%S", tz = "Asia/Tokyo")
  indices <- as.POSIXct(paste0(unique(format(as.Date(names(chlor_okinawa), format = "X%Y.%m.%d.%H.%M.%S"), format = "%Y.%m.%d")),".12.16.00"),
                        format = "%Y.%m.%d.%H.%M.%S", tz = "Asia/Tokyo")

  a=data.table(Value=timestamps)
  a[,merge:=Value]
  b=data.table(Value=indices)
  b[,merge:=Value]
  setkeyv(a,c('merge'))
  setkeyv(b,c('merge'))
  Merge_a_b=a[b,roll='nearest']
  keep_idx = which(timestamps %in% Merge_a_b$Value)
  daily_raster <- raster::subset(chlor_okinawa, keep_idx)

  ### Moderate spatial resolution (750m @ 1x per day noon) ###
  
  # resample to higher resolution so we can aggregate with even integer
  modres_grid = raster(ncol = 2667, nrow = 1067)  # first create new raster grid with 0.00375 x 0.00375 grid size (divide ncol/nrow by 0.75)
  extent(modres_grid) <- extent(daily_raster) # set lat lon extent
  chlor_375m <- resample(daily_raster, modres_grid, method = "bilinear")
  #chlor_375m <- resample(daily_raster, modres_grid, method = "ngb")
  
  is_na_raster <- calc(chlor_375m, fun = function(x) { ifelse(is.na(x), 0, 1) })
  
  # aggregate the is_na_raster to count non-NA cells at the desired resolution
  # a simple sum here as an example,
  # adjust aggregation factor as necessary
  non_na_count <- aggregate(is_na_raster, fact = 2, fun = sum)
  names(non_na_count) <- names(daily_raster)
  
  # create and save NA raster so we can count how many NA values went into each coarse pixel. Crop and do this only for Okinawa Island
  na_count = 4-non_na_count
  e <- as(extent(127.7, 128.1, 26, 26.5), 'SpatialPolygons')
  crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"
  na_count <- crop(na_count, e)
  saveRDS(na_count, file = paste0("GOCI_data/moderate_res/na_count/modres_nacount_",basenames[i],".rds"))

  # determine the total number of cells that contribute to each aggregated cell
  # for a 4km resolution from 500m, adjust as necessary
  total_cells <- 2*2
  
  # create a mask based on your NA threshold criterion
  na_threshold = 0.5 # 50%
  mask <- calc(non_na_count, fun = function(x) { ifelse(x / total_cells > na_threshold, 1, NA) })
  
  # aggregate the original raster using mean but without considering NAs yet
  mean_raster <- aggregate(chlor_375m, fact = 2, fun = mean, na.rm = TRUE)
  
  # apply the mask to the aggregated mean raster
  chlor_750m <- mask * mean_raster
  names(chlor_750m) <- names(daily_raster)
  
  # save moderate res file
  saveRDS(chlor_750m, file = paste0("GOCI_data/moderate_res/modres_",basenames[i],".rds"))
  

  ### Coarse spatial resolution (4km @ 1x per day noon) ###
  
  is_na_raster <- calc(daily_raster, fun = function(x) { ifelse(is.na(x), 0, 1) })
  
  # aggregate the is_na_raster to count non-NA cells at the desired resolution
  # a simple sum here as an example,
  # adjust aggregation factor as necessary
  non_na_count <- aggregate(is_na_raster, fact = 8, fun = sum)
  names(non_na_count) <- names(daily_raster)
  
  # create and save NA raster so we can count how many NA values went into each coarse pixel. Crop and do this only for Okinawa Island
  na_count = 64-non_na_count
  e <- as(extent(127.7, 128.1, 26, 26.5), 'SpatialPolygons')
  crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"
  na_count <- crop(na_count, e)
  save(na_count, file = paste0("GOCI_data/coarse_res/na_count/coarseres_nacount_",filenames[i]))
  
  # determine the total number of cells that contribute to each aggregated cell
  # for a 4km resolution from 500m, adjust as necessary
  total_cells <- 8*8
  
  # create a mask based on your NA threshold criterion
  na_threshold = 0.5 # 50%
  mask <- calc(non_na_count, fun = function(x) { ifelse(x / total_cells > na_threshold, 1, NA) })
  
  # aggregate the original raster using mean but without considering NAs yet
  mean_raster <- aggregate(daily_raster, fact = 8, fun = mean, na.rm = TRUE)
  
  # apply the mask to the aggregated mean raster
  chlor_4km <- mask * mean_raster
  names(chlor_4km) <- names(daily_raster)
  
  # save moderate res file
  save(chlor_4km, file = paste0("GOCI_data/coarse_res/coarseres_",filenames[i]))
  
  print(paste0("Completed ",i," of ", length(files)," chlor-a files."))
  
}


#############
### Kd490 ###
#############

files <- list.files(paste0("GOCI_data/full_res"), pattern="*\\_kd490.RData", full.names=TRUE)
filenames <- list.files(paste0("GOCI_data/full_res"), pattern="*\\_kd490.RData", full.names=FALSE)
basenames = gsub("\\.RData$","", list.files(path = paste0("GOCI_data/full_res"), pattern="*\\_kd490.RData", full.names=FALSE))


for(i in 1:length(files)) {
  
  # i = 61
  
  load(files[i])
  
  # Pull out closest daily measurement to noon to get daily estimate for coarser resolutions (raw data is every 1-hr between 9am-4pm)
  timestamps <- as.POSIXct(gsub("X","",names(kd490_okinawa)), format = "%Y.%m.%d.%H.%M.%S", tz = "Asia/Tokyo")
  indices <- as.POSIXct(paste0(unique(format(as.Date(names(kd490_okinawa), format = "X%Y.%m.%d.%H.%M.%S"), format = "%Y.%m.%d")),".12.16.00"),
                        format = "%Y.%m.%d.%H.%M.%S", tz = "Asia/Tokyo")
  
  a=data.table(Value=timestamps)
  a[,merge:=Value]
  b=data.table(Value=indices)
  b[,merge:=Value]
  setkeyv(a,c('merge'))
  setkeyv(b,c('merge'))
  Merge_a_b=a[b,roll='nearest']
  keep_idx = which(timestamps %in% Merge_a_b$Value)
  daily_raster <- raster::subset(kd490_okinawa, keep_idx)
  
  ### Moderate spatial resolution (750m @ 1x per day noon) ###
  
  # resample to higher resolution so we can aggregate with even integer
  modres_grid = raster(ncol = 2667, nrow = 1067)  # first create new raster grid with 0.00375 x 0.00375 grid size (divide ncol/nrow by 0.75)
  extent(modres_grid) <- extent(daily_raster) # set lat lon extent
  kd490_375m <- resample(daily_raster, modres_grid, method = "bilinear")
  
  is_na_raster <- calc(kd490_375m, fun = function(x) { ifelse(is.na(x), 0, 1) })
  
  # aggregate the is_na_raster to count non-NA cells at the desired resolution
  # a simple sum here as an example,
  # adjust aggregation factor as necessary
  non_na_count <- aggregate(is_na_raster, fact = 2, fun = sum)
  names(non_na_count) <- names(daily_raster)
  
  # create and save NA raster so we can count how many NA values went into each coarse pixel. Crop and do this only for Okinawa Island
  na_count = 4-non_na_count
  e <- as(extent(127.7, 128.1, 26, 26.5), 'SpatialPolygons')
  crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"
  na_count <- crop(na_count, e)
  saveRDS(readAll(na_count), file = paste0("GOCI_data/moderate_res/na_count/modres_nacount_",basenames[i],".rds"))
  
  # determine the total number of cells that contribute to each aggregated cell
  # for a 4km resolution from 500m, adjust as necessary
  total_cells <- 2*2
  
  # create a mask based on your NA threshold criterion
  na_threshold = 0.5 # 50%
  mask <- calc(non_na_count, fun = function(x) { ifelse(x / total_cells > na_threshold, 1, NA) })
  
  # aggregate the original raster using mean but without considering NAs yet
  mean_raster <- aggregate(kd490_375m, fact = 2, fun = mean, na.rm = TRUE)
  
  # apply the mask to the aggregated mean raster
  kd490_750m <- mask * mean_raster
  names(kd490_750m) <- names(daily_raster)
  
  # save moderate res file
  saveRDS(readAll(kd490_750m), file = paste0("GOCI_data/moderate_res/modres_",basenames[i],".rds"))
  
   print(paste0("Completed ",i," of ", length(files)," kd490-a files."))

  
  ### Coarse spatial resolution (4km @ 1x per day noon) ###
  
  is_na_raster <- calc(daily_raster, fun = function(x) { ifelse(is.na(x), 0, 1) })
  
  # aggregate the is_na_raster to count non-NA cells at the desired resolution
  # a simple sum here as an example,
  # adjust aggregation factor as necessary
  non_na_count <- aggregate(is_na_raster, fact = 8, fun = sum)
  names(non_na_count) <- names(daily_raster)
  
  # create and save NA raster so we can count how many NA values went into each coarse pixel. Crop and do this only for Okinawa Island
  na_count = 64-non_na_count
  e <- as(extent(127.7, 128.1, 26, 26.5), 'SpatialPolygons')
  crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"
  na_count <- crop(na_count, e)
  save(na_count, file = paste0("GOCI_data/coarse_res/na_count/coarseres_nacount_",filenames[i]))
  
  # determine the total number of cells that contribute to each aggregated cell
  # for a 4km resolution from 500m, adjust as necessary
  total_cells <- 8*8
  
  # create a mask based on your NA threshold criterion
  na_threshold = 0.5 # 50%
  mask <- calc(non_na_count, fun = function(x) { ifelse(x / total_cells > na_threshold, 1, NA) })
  
  # aggregate the original raster using mean but without considering NAs yet
  mean_raster <- aggregate(daily_raster, fact = 8, fun = mean, na.rm = TRUE)
  
  # apply the mask to the aggregated mean raster
  kd490_4km <- mask * mean_raster
  names(kd490_4km) <- names(daily_raster)

  # save moderate res file
  save(kd490_4km, file = paste0("GOCI_data/coarse_res/coarseres_",filenames[i]))
  
  print(paste0("Completed ",i," of ", length(files)," kd490-a files."))
  
}


