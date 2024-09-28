###############################################################################################
### Generate time series of chlorophyll and kd490 for each pixel at each spatial resolution ###
###############################################################################################

library(raster)

# Create buffer polygons that will be used to extract adjacent pixels to boundary. This has to be done from SpatialPolygonsDataFrame, so will need to 
# work from the original bathy_okinawa_poly object. This means adding the additional width for the 1/2 diagonal pixel distance
# (i.e. 1 adj pixel = 1/2 diagonal pixel dist. + one pixel width)

#load("Envirn_data/ETOPO_2022_15s_bathy_okinawa.RData")
#bathy_okinawa[bathy_okinawa[] < -30 ] = NA 
#bathy_okinawa_poly = rasterToPolygons(bathy_okinawa)

#adj_pixel_4km <- raster::buffer(bathy_okinawa_poly, width = 6828.427) # 2828.427 + 4000
#save(adj_pixel_4km, file = paste0("Envirn_data/adj_pixel_boundary_4km.RData"))

#adj_pixel_750m <- raster::buffer(bathy_okinawa_poly, width = 4280.3301) # 530.3301 + 5*750
#save(adj_pixel_750m, file = paste0("Envirn_data/adj_pixel_boundary_750m.RData"))

#adj_pixel_500m <- raster::buffer(bathy_okinawa_poly, width = 4353.5534) #353.5534 + 8*500
#save(adj_pixel_500m, file = paste0("Envirn_data/adj_pixel_boundary_500m.RData"))

# Created extended boundary for missed events
#adj_pixel_500m_ext <- raster::buffer(bathy_okinawa_poly, width = 9353.553) #353.5534 + 18*500
#save(adj_pixel_500m_ext, file = paste0("Envirn_data/adj_pixel_boundary_500m_ext.RData"))

# Subset data for pixels adjacent to mask boundary (1 pixel for coarse, 5 pixels for 750m, 8 pixels for 500m)
# 1 pixel for 4km, 5 pixels for 750m, 8 pixels for 500m

####################################
### 4 km, 1x per day time series ###
####################################

load("Envirn_data/adj_pixel_boundary_4km.RData")
files_chlor <- list.files(path = paste0("GOCI_data/coarse_res/masked"), pattern="*\\_chlor.rds", full.names=TRUE)
files_kd490 <- list.files(path = paste0("GOCI_data/coarse_res/masked"), pattern="*\\_kd490.rds", full.names=TRUE)

### Chlorophyll ###
# extract only the pixels around the mask boundary (i.e. mask everything except those pixels)
s_chlor <- stack(lapply(files_chlor, readRDS))
s_chlor <- raster::mask(s_chlor, adj_pixel_4km)
#plot(calc(s_chlor, fun=mean,na.rm=T))

# subset s_chlor for only Okinawa island for now
e <- as(extent(127.2, 128.4, 26, 27), 'SpatialPolygons')
crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"
s_chlor <- crop(s_chlor, e); gc()

x<-as.data.frame(matrix(,nrow=length(names(s_chlor)), ncol=ncell(s_chlor)+1))
x[1]<-names(s_chlor)

for (i in 1:ncell(s_chlor)) {
  # i = 21
  y <- as.vector(s_chlor[i])
  q<-which(is.na(y))
  if (length(q)<nrow(x)){
    x[,i+1] <- y
    names(x)[i+1] <- paste0("pixel_",i)
  }
  else if (length(q)==nrow(x)){
    next
  }
 
  print(paste("completed",i,"of",ncell(s_chlor),"pixels"))
  
}

coarse_chlor_ts <- x
coarse_chlor_ts <- x[,colSums(is.na(x))<nrow(x)]
#saveRDS(coarse_chlor_ts, file = "GOCI_data/coarse_res/masked/coarse_chlor_ts.rds")
saveRDS(coarse_chlor_ts, file = "GOCI_data/coarse_res/masked/coarse_chlor_ts_cropped.rds")

### Kd490 ###

# extract only the pixels around the mask boundary (i.e. mask everything except those pixels)
s_kd490 <- stack(lapply(files_kd490, readRDS))

x<-as.data.frame(matrix(,nrow=length(names(s_kd490)), ncol=ncell(s_kd490)+1))
x[1]<-names(s_kd490)

for (i in 1:ncell(s_kd490)) {
  # i = 2
  y <- as.vector(s_kd490[i])
  q<-which(is.na(y))
  if (length(q)<length(x)){
    x[,i+1] <- y
    names(x)[i+1] <- paste0("pixel_",i)
  }
  else if (length(q)==length(x)){
    next
  }

  print(paste("completed",i,"of",ncell(s_kd490),"pixels"))
  
}

coarse_kd490_ts <- x
saveRDS(coarse_kd490_ts, file = "GOCI_data/coarse_res/masked/coarse_kd490_ts.rds")

rm(x,s_kd490,coarse_kd490_ts, adj_pixel_4km)

#####################################
### 750 m, 1x per day time series ###
#####################################

load("Envirn_data/adj_pixel_boundary_750m.RData")
files_chlor <- list.files(path = paste0("GOCI_data/moderate_res/masked"), pattern="*\\_chlor.rds", full.names=TRUE)
files_kd490 <- list.files(path = paste0("GOCI_data/moderate_res/masked"), pattern="*\\_kd490.rds", full.names=TRUE)

### Chlorophyll ###
# extract only the pixels around the mask boundary (i.e. mask everything except those pixels)
s_chlor <- stack(lapply(files_chlor, readRDS))
s_chlor <- raster::mask(s_chlor, adj_pixel_500m_750m)

x<-as.data.frame(matrix(,nrow=length(names(s_chlor)), ncol=ncell(s_chlor)+1))
x[1]<-names(s_chlor)

for (i in 1:ncell(s_chlor)) {
  # i = 48628
  y <- as.vector(s_chlor[i])
  q<-which(is.na(y))
  if (length(q)<length(x)){
    x[,i+1] <- y
    names(x)[i+1] <- paste0("pixel_",i)
  }
  else if (length(q)==length(x)){
    next
  }

  print(paste("completed",i,"of",ncell(s_chlor),"pixels"))
  
}

moderate_chlor_ts <- x
saveRDS(moderate_chlor_ts, file = "GOCI_data/moderate_res/masked/moderate_chlor_ts.rds")

rm(s_chlor,x,moderate_chlor_ts)

### Kd490 ###

# extract only the pixels around the mask boundary (i.e. mask everything except those pixels)
s_kd490 <- stack(lapply(files_kd490, readRDS))
s_kd490 <- raster::mask(s_kd490, adj_pixel_750m)

x<-as.data.frame(matrix(,nrow=length(names(s_kd490)), ncol=ncell(s_kd490)+1))
x[1]<-names(s_kd490)

for (i in 1:ncell(s_kd490)) {
  # i = 2
  y <- as.vector(s_kd490[i])
  q<-which(is.na(y))
  if (length(q)<length(x)){
    x[,i+1] <- y
    names(x)[i+1] <- paste0("pixel_",i)
  }
  else if (length(q)==length(x)){
    next
  }

  print(paste("completed",i,"of",ncell(s_kd490),"pixels"))
  
}

moderate_kd490_ts <- x
saveRDS(moderate_kd490_ts, file = "GOCI_data/moderate_res/masked/moderate_kd490_ts.rds")

rm(s_kd490,x,moderate_kd490_ts)

#####################################
### 500 m, 8x per day time series ###
#####################################

load("Envirn_data/adj_pixel_boundary_500m_ext.RData")
files_chlor <- list.files(path = paste0("GOCI_data/full_res/masked"), pattern="*\\_chlor.rds", full.names=TRUE)
files_kd490 <- list.files(path = paste0("GOCI_data/full_res/masked"), pattern="*\\_kd490.rds", full.names=TRUE)

### Chlorophyll ###
# extract only the pixels around the mask boundary (i.e. mask everything except those pixels)
s_chlor <- stack(lapply(files_chlor, readRDS))
s_chlor <- raster::mask(s_chlor, adj_pixel_500m_ext)

x<-as.data.frame(matrix(,nrow=length(names(s_chlor)), ncol=ncell(s_chlor)+1))
x[1]<-names(s_chlor)

for (i in 1:ncell(s_chlor)) {
  # i = 2
  y <- as.vector(s_chlor[i])
  q<-which(is.na(y))
  if (length(q)<length(x)){
    x[,i+1] <- y
    names(x)[i+1] <- paste0("pixel_",i)
  }
  else if (length(q)==length(x)){
    next
  }

  print(paste("completed",i,"of",ncell(s_chlor),"pixels"))
  
}

full_chlor_ts <- x
saveRDS(full_chlor_ts, file = "GOCI_data/full_res/masked/full_chlor_ts_ext.rds")

### Kd490 ###

# extract only the pixels around the mask boundary (i.e. mask everything except those pixels)
s_kd490 <- stack(lapply(files_kd490, readRDS))
s_kd490 <- raster::mask(s_kd490, adj_pixel_500m_750m)

x<-as.data.frame(matrix(,nrow=length(names(s_kd490)), ncol=ncell(s_kd490)+1))
x[1]<-names(s_kd490)

for (i in 1:ncell(s_kd490)) {
  # i = 2
  y <- as.vector(s_kd490[i])
  q<-which(is.na(y))
  if (length(q)<length(x)){
    x[,i+1] <- y
    names(x)[i+1] <- paste0("pixel_",i)
  }
  else if (length(q)==length(x)){
    next
  }

  print(paste("completed",i,"of",ncell(s_kd490),"pixels"))
  
}

full_kd490_ts <- x
saveRDS(full_kd490_ts, file = "GOCI_data/full_res/masked/full_kd490_ts.rds")



