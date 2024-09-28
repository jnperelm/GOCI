# Preprocessing GOCI data files
# Combine into monthly or yearly files

library(raster)
library(terra)
library(ncdf4)
library(stringr)
library(lubridate)
library(sf)
library(sp)
library(geosphere)
library(ggplot2)
library(viridis)


# Get polygons for Okinawa islands
# Japan shp file from UT Austin GeoData

#japan <- shapefile("Japan_shp/JPN_adm0.shp")
#okinawa <- crop(japan, extent(122,132,24,28))
#plot(okinawa)
#axis(1)
#axis(2)
#box(col = 'black')

#shapefile(okinawa, "okinawa.shp")


# Load Okinawa shalpefile
# Add 30 km buffer around each island
# Use these expanded polygons to crop bathymetry data around each island.

okinawa <- shapefile("Envirn_data/okinawa_shp/okinawa.shp")
okinawa_buffer <- raster::buffer(okinawa, width = 30000)
#plot(okinawa_buffer)
#axis(1)
#axis(2)
#box(col = 'black')

# Load ETOPO 2022 V1 bathymetry data for Okinawa Prefecure region
## Crop bathy data for 30m distance from all coastlines

#ncfile = "EnvirnETOPO_2022_v1_15s_24_28_122_132.nc"
#bathy = raster(ncfile, varname = "z")
#bathy_okinawa <- mask(bathy,okinawa_buffer)
#save(bathy_okinawa, file = paste0("Envirn_data/ETOPO_2022_15s_bathy_okinawa.RData"))

# Create list of .nc files for each month folder. Skip folder if already preprocessed

folders = list.dirs(path = "G:/PRP/GOCI", recursive = F, full.names = F)

for (i in 2:length(folders[-c(123,124)])) {
  
  # i = 2
  
  
  if(!file.exists(paste0("G:/PRP/GOCI/",folders[i],"_chlor.RData"))) {
    
    files <- list.files(paste0("G:/PRP/GOCI/", folders[i]), 
                        pattern="*\\.nc", full.names=TRUE)
    
    list_chlor = NULL
    list_kd = NULL
    for (j in 1:length(files)) {
      
      # j = 1
      
      # identify timestamp from file name and save it as the time variable for each file
      
      time  = str_match(files[j], "GOCI_mapped_\\s*(.*?)\\s*_bmw_L3.nc")[,2]
      time = gsub("_", " ", time)
      datetime = as.POSIXct(time, format = "%Y%m%d %H%M", tz = "UTC")
      datetime = with_tz(datetime, "Asia/Tokyo"); datetime
      
      # create raster objects for chlor-a and kd-490 (have to be separate) from each file
      # stack rasters as layers into single raster for each variable
      
      nc = nc_open(files[j]) 
      lon <- ncdf4::ncvar_get(nc, nc$var[["lon"]])
      lat <- ncdf4::ncvar_get(nc, nc$var[["lat"]])
      chlor_a <- ncdf4::ncvar_get(nc, nc$var[["chlor_a"]])
      kd_490 <- ncdf4::ncvar_get(nc, nc$var[["kd_490"]])
      nc_close(nc)
      
      rast1 <- raster(rast(t(chlor_a), ext=c(range(lon), range(lat)), crs="+proj=longlat +datum=WGS84 +no_defs"))
      rast2 <- raster(rast(t(kd_490), ext=c(range(lon), range(lat)), crs="+proj=longlat +datum=WGS84 +no_defs"))
      
      rast1 = setZ(rast1, datetime, "time")
      rast2 = setZ(rast2, datetime, "time")
      
      names(rast1) <- datetime
      names(rast2) <- datetime
      
      list_chlor[[j]] = rast1
      list_kd[[j]] = rast2
      
      print(paste("completed",j,"of",length(files),"files for folder", folders[i]))
      
    }
    
    # stack layers for all days in a given month for each variable & save as RData files
    
    chlor <- stack(list_chlor)
    kd490 <- stack(list_kd)
    
    # mask GOCI data outside of polygons
    
    chlor_okinawa <- mask(chlor,okinawa_buffer)
    values(chlor_okinawa)[values(chlor_okinawa) < 0] = NA
    kd490_okinawa <- mask(kd490,okinawa_buffer)
    values(kd490_okinawa)[values(kd490_okinawa) < 0] = NA
    
    save(chlor_okinawa, file = paste0(folders[i],"_chlor.RData"))
    save(kd490_okinawa, file = paste0(folders[i],"_kd490.RData"))
    
    print(paste("completed",i,"of",length(folders),"folders"))
    
  }
  
  else(print(paste0("Files already exist for month ",folders[i],". Moving to next month")))
  
}


# Plot one day to show whole region
load("~/GOCIproj/GOCI_data/full_res/202103_chlor.RData")
plot(mean(chlor_okinawa))
mean_chl = mean(chlor_okinawa, na.rm=T)
plot(mean_chl)
mean_chl = as.data.frame(mean_chl, xy=T)

png(paste0("GOCI_data/03_2021_map.png"), height = 5, width = 11, res = 500, units = "in")
ggplot(na.omit(mean_chl)) + 
  geom_polygon(okinawa, mapping=aes(long,lat,group=group), lwd=0.2, fill = "black") +
  #geom_polygon(okinawa_buffer, mapping=aes(long,lat,group=group), color="black", fill=NA, lwd=0.2) +
  geom_tile(aes(x=x,y=y, fill=log(layer))) + xlab("Lon") + ylab("Lat") + #ylim(26,27) + xlim(127.2,128.4) +
  scale_fill_viridis(option = "D", name = "log(chlor-a)") + theme_bw() +
  theme(panel.background = element_rect(fill = "gray81"), axis.text = element_text(size = 14), axis.title = element_text(size = 14),
        legend.text = element_text(size=14), legend.title = element_text(size = 14))
dev.off()






