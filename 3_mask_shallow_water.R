
library(raster)

# load 15-arc second bathymetry data
#load("Envirn_data/ETOPO_2022_15s_bathy_okinawa.RData")

# subset contour data for everything shallower than 30m depth contour
#bathy_okinawa[bathy_okinawa[] < -30 ] = NA 
#bathy_okinawa_poly = rasterToPolygons(bathy_okinawa)
#plot(bathy_okinawa_poly)

# add a buffer around 30m depth contour equal to 1/2 diagonal pixel distance of 500m GOCI data 
# [sqrt((500^2) + (500^2))/2] = 353.5534
# [sqrt((750^2) + (750^2))/2] = 530.3301
# [sqrt((4000^2) + (4000^2))/2] = 2828.427
# bathy_buffered_poly_500m <- raster::buffer(bathy_okinawa_poly, width = 353.5534)
# save(bathy_buffered_poly_500m, file = paste0("Envirn_data/bathy_okinawa_30m_buffered_poly_500m.RData"))
# bathy_buffered_poly_750m <- raster::buffer(bathy_okinawa_poly, width = 530.3301)
# save(bathy_buffered_poly_750m, file = paste0("Envirn_data/bathy_okinawa_30m_buffered_poly_750m.RData"))
# bathy_buffered_poly_4km <- raster::buffer(bathy_okinawa_poly, width = 2828.427)
# save(bathy_buffered_poly_4km, file = paste0("Envirn_data/bathy_okinawa_30m_buffered_poly_4km.RData"))


# Load buffered 30m polygon
load("Envirn_data/bathy_okinawa_30m_buffered_poly_500m.RData")
load("Envirn_data/bathy_okinawa_30m_buffered_poly_750m.RData")
load("Envirn_data/bathy_okinawa_30m_buffered_poly_4km.RData")

# Create list of folders (res) to loop through for masking
folders <- list.dirs(path = "GOCI_data", recursive = F)[-4]

# Loop through files (chlor and kd490) within each folder for masking
# use mask file that corresponds correctly with each resolution

for(i in 1:length(folders)) {

  # i = 2
  
  files <- list.files(path = paste0(folders[i]), pattern="*\\.RData", full.names=TRUE)
  basenames = gsub("\\.RData$","", list.files(path = paste0(folders[i]), pattern="*\\.RData", full.names=FALSE))
  
  if(folders[i] == "GOCI_data/coarse_res") 
  {
    bathy_buffered <- bathy_buffered_poly_4km
  } else if(folders[i] == "GOCI_data/moderate_res")
  {
    bathy_buffered <- bathy_buffered_poly_750m
  } else if(folders[i] == "GOCI_data/full_res")
  {
    bathy_buffered <- bathy_buffered_poly_500m
    }
  
  for(j in 1:length(files)) {
    
    # j = 1
  
    if(!file.exists(paste0(folders[i],"/masked/masked_",basenames[j],".rds"))) {
      
      # load GOCI data files & get dataset name
      GOCI <- get(load(files[j]))

      # mask shallow seafloor-influenced data
      GOCI_masked <- raster::mask(readAll(GOCI), bathy_buffered, inverse = TRUE)

      # save new masked file as .rds (readAll makes sure to save all raster information, not just references to it)
      saveRDS(readAll(GOCI_masked), 
              file = paste0(folders[i],"/masked/masked_",basenames[j],".rds"))
      
      print(paste("completed",j,"of",length(files),"files in", folders[i]))
      
      rm(list=setdiff(ls(), c("bathy_buffered_poly_750m", "bathy_buffered_poly_500m", "bathy_buffered_poly_4km", "bathy_buffered", "folders","i","basenames","files")))
      
    } 
    
    else(print(paste0("Files already exist for ",basenames[j],". Moving to next file")))

  }
}

