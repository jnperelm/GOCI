library(zoo)
library(dplyr)
library(tidyr)
library(data.table)
library(santoku)
library(raster)
library(lubridate)
library(pals)
library(ggplot2)

source("scripts/GOCI_functions.R")

##################################################################
# Metric:Number of episodic events detected at 8xd 500 m (GOCI)  #
# compared to 8day 4 km (commonly used)                          #
##################################################################

# Generate rasters of 1 and 0 for episodic events detection for both res
thresholds <- readRDS("outputs/episodic_events/thresholds_monthly_from_4km.rds") 
#chlor_ts <- as.data.frame(readRDS("GOCI_data/coarse_res/masked/coarse_chlor_ts_8day_cropped.rds"))
chlor_ts <- as.data.frame(readRDS("GOCI_data/full_res/masked/full_chlor_ts_ext.rds"))
chlor_ts$V1 = as.POSIXct(gsub("X","",chlor_ts$V1), format = "%Y.%m.%d.%H.%M.%S", tz = "Asia/Tokyo")
ind = which(colSums(chlor_ts[,-1], na.rm=T)>0)+1
chlor_ts <- chlor_ts[,c(1,ind)]
chlor_ts$month_year <- lubridate::month(as.Date(as.yearmon(chlor_ts$V1, format = "%m_%Y", tz = "Asia/Tokyo")))

datetime = chlor_ts$V1

pix <- readRDS("~/GOCIproj/outputs/okinawa_pixels_latlon_fullres_ext.rds")
#pix <- readRDS("~/GOCIproj/outputs/okinawa_pixels_latlon_coarseres.rds")

ind = which(colnames(chlor_ts) %in% pix$pixel)
chlor_ts <- chlor_ts[,c(15849,ind)]
chlor_ts <- chlor_ts[,-1]

df <- chlor_ts[NA,-1]
rownames(df) <- seq(1:nrow(df))
month_year = as.data.frame(chlor_ts$month_year)
names(month_year) <- "month_year"
thresh = left_join(month_year, thresholds, by = "month_year")

# define episodic events from monthly thresholds with 1 or 0

for(i in 1:ncol(chlor_ts[,-1])) {
  #i = 1
  
  df[,i] <- ifelse(chlor_ts[,i+1] > thresh$chlor_thresh, 1 ,0)
  print(paste("completed", i, "episodes of", ncol(df),"pixels"))
}

df$days8 <- datetime

df[is.na(df)] <- 0

#saveRDS(df, file = paste0("outputs/episodic_events/episodes_8day_4km_1_0_cropped.rds"))
saveRDS(df, file = paste0("outputs/episodic_events/episodes_8xd_500m_1_0_ext.rds"))

##############################################################################################################################

# Assign 8-day time res to each 8xd 500 m grid cell and extract one time step from each res 
# Make sure timesteps match between resolutions

chlor_c <- readRDS("outputs/episodic_events/episodes_8day_4km_1_0_cropped.rds")
chlor_f <- readRDS("outputs/episodic_events/episodes_8xd_500m_1_0_ext.rds")
#chlor_f$V1 = as.POSIXct(chlor_f$V1, format = "X%Y.%m.%d.%H.%M.%S", tz = "Asia/Tokyo")
#chlor_f <- subset(chlor_f, V1 >= "2011-03-31 13:16:00 JST")
chlor_f <- subset(chlor_f, V1 > "2011-03-31")
chlor_f$days8 = as.Date(chop_width(chlor_f$V1, days(8), labels = lbl_endpoints(fmt = "%y-%m-%d")), tz = "Asia/Tokyo")

load("~/GOCIproj/Envirn_data/adj_pixel_boundary_4km.RData")
load("Envirn_data/bathy_okinawa_30m_buffered_poly_4km.RData")

# To separate out unique episodic events, change all "1s" that follow another "1" to zero
first_row = chlor_f[1,-c(15848,15849)] # pull out first row to keep 1 and 0 when using lag()
chlor_f2 <- chlor_f %>%
  mutate_all(~ifelse(is.na(.), 0, .)) %>%
  select(-c("V1","days8")) %>%
  mutate_all(~ifelse(lag(.)==1,0,.))
chlor_f2[1,] <- first_row # replace bad first row with original first row

chlor_f2 = cbind(chlor_f[,c(15848,15849)],chlor_f2)

s_chlor <- readRDS("GOCI_data/full_res/masked/full_chlor_stacked_masked_ext.rds")
s_chlor2 <- readRDS("GOCI_data/coarse_res/masked/coarse_chlor_stacked_masked_cropped.rds")

chlor_f2 = readRDS("outputs/episodic_events/chlor_f2.rds")
chlor_f2[is.na(chlor_f2)] <- 0

dates_8 = unique(chlor_f$days8)
list_chlor_sum = NULL
full_events = list(NULL)

for(i in 1:length(dates_8)) {
  
  # i = 1
  
  date = dates_8[i]
  
  # create raster for unique full 8xd episodes 1/0 within single 8day chunk
  # only count consecutive 1's as a single episode
  c_full = chlor_f2[chlor_f2$days8==date,]
  
  # for each timestep within 8-day window, if there are any events occurring, count as single event.
  
  for(j in 1:nrow(c_full)) {
    c_full_new = as.data.frame(t(c_full[i,-c(1,2)]))
    c_full_new <- c_full_new %>%
      mutate(pixel = rownames(c_full_new), pixelnum = as.numeric(substring(rownames(c_full_new),7))) %>%
      complete(pixelnum=1:48000)
      #complete(pixelnum=1:1600000)
    names(c_full_new)[2] <- "value"
    rf <- raster(ncol = 240, nrow = 200, ext = extent(s_chlor))
    #rf <- raster(ncol = 2000, nrow = 800, ext = extent(chlor_okinawa)) # full res
    values(rf) <- c_full_new$value
    names(rf) <- date
    
    full_events[[j]] = aggregate(rf, fact = 8, fun = max)
    
    print(paste("completed", j, "of", nrow(c_full),"pixels"))
    
  }
  
  full_events_stack = stack(full_events)
  full_events_sum = calc(full_events_stack, sum)
  
  # convert nan to NA
  full_events_sum <- as.data.frame(full_events_sum) %>% mutate_if(is.numeric, ~ifelse(is.nan(.), NA, .))
  
  # bring in coarse res data, identify pixels that had already identified an episodic event (i.e. all 1's), and mask those pixel numbers in full_events data
  c_coarse = transpose(chlor_c[chlor_c$days8==date,-117], keep.names = "pixel")
  pixel_mask <- c_coarse %>%
    mutate(pixelnum = as.numeric(substring(pixel,7))) %>%
    complete(pixelnum=1:750) %>% rename(ep = `V1`) %>% as.data.frame() %>%
    subset(ep == 1)
  
  full_events_sum$layer[pixel_mask$pixelnum] <- NA

  # convert missed events data frames back into rasters for stacking
  events_missed_sum <- raster(ncol = 30, nrow = 25, ext = extent(s_chlor2)) # coarse res
  values(events_missed_sum) <- full_events_sum[,1]
  names(events_missed_sum) <- date
  
  list_chlor_sum[[i]] = events_missed_sum
  
  print(paste("completed", i, "of", length(dates_8),"time points"))
}

full_sum <- sum(stack(list_chlor_sum), na.rm=T)

saveRDS(readAll(full_sum),file = paste0("outputs/episodic_events/missed_events_4km_ext2.rds"))

############ Map plots and total number for each metric ################

full_sum <- readRDS("~/GOCIproj/outputs/episodic_events/missed_events_4km_ext2.rds")
pix <- colnames(as.data.frame(readRDS("GOCI_data/coarse_res/masked/coarse_chlor_ts_8day_cropped.rds")))[-1]
#load("Envirn_data/bathy_okinawa_30m_buffered_poly_4km.RData")
#full_sum = raster::mask(full_sum, bathy_buffered_poly_4km)

EE_sum = as.data.frame(full_sum, xy=T)
EE_sum$pixel = paste0("pixel_",seq(1:nrow(EE_sum)))

EE_sum = EE_sum %>%
  subset(pixel %in% pix)

okinawa <- shapefile("Envirn_data/okinawa_shp/okinawa.shp")

png(paste0("outputs/episodic_events/figures/missed_events_4km_ext_final.png"), height = 5, width = 7, res = 500, units = "in")
ggplot(EE_sum) + 
  geom_polygon(okinawa, mapping=aes(long,lat,group=group), lwd=0.2, fill = "black") +
  geom_tile(aes(x=x,y=y, fill=layer)) + xlab("Lon") + ylab("Lat") + ylim(26,27) + xlim(127.2,128.4) +
  scale_fill_viridis(option = "D", name = "Missed \nEvents") + theme_bw() +
  theme(panel.background = element_rect(fill = "gray81"), axis.text = element_text(size = 14), axis.title = element_text(size = 14),
  legend.text = element_text(size=14), legend.title = element_text(size = 14))
dev.off()



