#############################################################################################
# How does the ability to detect episodic, extreme events (e.g., large freshwater outflows) #
# that affect coral reefs change with spatial and temporal resolution?                      #
#############################################################################################

library(zoo)
library(dplyr)
library(tidyr)
library(data.table)
library(raster)


source("scripts/GOCI_functions.R")

#################################################################################################################################

##### Create thresholds to define episodic events from island regions at the coarse (4 km) monthly grid #####
##### Once thresholds created, skip down to second half of script to generate episodic events data #####

chlor_ts <- as.data.frame(readRDS("GOCI_data/coarse_res/masked/coarse_chlor_ts_monthly.rds"))#[,-1])

ind = which(colSums(chlor_ts[,-1], na.rm=T)>0)+1
chlor_ts <- chlor_ts[,c(1,ind)]

chlor_ts$month_year <- lubridate::month(as.Date(as.yearmon(chlor_ts$month_year, format = "%m_%Y", tz = "Asia/Tokyo")))

pix <- readRDS("~/GOCIproj/outputs/coarseres_latlon_pixels.rds")
pix <- pix %>%
  subset(x>=127.2 & x<=128.4) %>%
  subset(y>=26 & y<=27)

ind = which(colnames(chlor_ts) %in% pix$pixel)
chlor_ts <- chlor_ts[,c(1,ind)]

# rename each pixel by region ID for region averaging -- now only looking at Okinawa island region, so all same region ("C")
chlor_ts = melt(setDT(chlor_ts), id.vars = c("month_year"), variable.name = "pixel")

pix_unique = unique(chlor_ts$pixel)

df <- data.frame(month_year = c(1:12),
                 chlor_thresh = c(rep(NA, length(c(1:12)))))
df2 <- data.frame(pixel = c(pix_unique),
                  chlor_episodes = c(rep(0, length(pix_unique))))

# define regional thresholds
for(i in 1:length(unique(chlor_ts$month_year))) {
  #i = 1
  chlor_month <- chlor_ts[chlor_ts$month_year == i,]
  monthly_mean <- mean(chlor_month$value,na.rm=T)
  monthly_sd <- sd(chlor_month$value, na.rm=T)
  chlor_thresh <- monthly_mean + 2*monthly_sd
  chlor_month$ee <- ifelse(chlor_month$value > chlor_thresh, 1, 0)
  chlor_month <- chlor_month %>%
    group_by(pixel) %>%
    summarise(ee = sum(ee, na.rm=T))

  df[df$month_year==i,]$chlor_thresh <- chlor_thresh
  df2$chlor_episodes <- df2$chlor_episodes + chlor_month$ee

  print(paste("completed", i, "thresholds of", nrow(df),"pixels"))
}

saveRDS(df, file = paste0("outputs/episodic_events/thresholds_monthly_from_4km.rds"))
saveRDS(df2, file = paste0("outputs/episodic_events/episodes_monthly_4km_final.rds"))

#################################################################################################################################

### Find frequency of episodic events ###

thresholds <- readRDS("outputs/episodic_events/thresholds_monthly_from_4km.rds") # generated from above code

chlor_ts <- as.data.frame(readRDS("GOCI_data/full_res/masked/full_chlor_ts.rds"))

# Pull out closest daily measurement to noon to get daily estimate
#timestamps <- as.POSIXct(gsub("X","",chlor_ts$V1), format = "%Y.%m.%d.%H.%M.%S", tz = "Asia/Tokyo")
#indices <- as.POSIXct(paste0(unique(format(as.Date(chlor_ts$V1, format = "X%Y.%m.%d.%H.%M.%S"), format = "%Y.%m.%d")),".12.16.00"),
#                      format = "%Y.%m.%d.%H.%M.%S", tz = "Asia/Tokyo")

#a=data.table(Value=timestamps)
#a[,merge:=Value]
#b=data.table(Value=indices)
#b[,merge:=Value]
#setkeyv(a,c('merge'))
#setkeyv(b,c('merge'))
#Merge_a_b=a[b,roll='nearest']
#keep_idx = which(timestamps %in% Merge_a_b$Value)
#chlor_ts <- chlor_ts[keep_idx,]

chlor_ts$V1 = as.POSIXct(gsub("X","",chlor_ts$V1), format = "%Y.%m.%d.%H.%M.%S", tz = "Asia/Tokyo")

ind = which(colSums(chlor_ts[,-1], na.rm=T)>0)+1
chlor_ts <- chlor_ts[,c(1,ind)]

ntime = nrow(chlor_ts)
chlor_ts$month_year <- lubridate::month(as.Date(as.yearmon(chlor_ts$V1, format = "%m_%Y", tz = "Asia/Tokyo")))

pix <- readRDS("~/GOCIproj/outputs/coarseres_latlon_pixels.rds")
#pix <- readRDS("~/GOCIproj/outputs/okinawa_pixels_latlon_fullres_ext.rds")
#pix <- readRDS("~/GOCIproj/outputs/okinawa_pixels_latlon_modres.rds")
#pix <- readRDS("~/GOCIproj/outputs/okinawa_pixels_latlon_fullres.rds")

pix <- pix %>%
  subset(x>=127.2 & x<=128.4) %>%
  subset(y>=26 & y<=27)

ind = which(colnames(chlor_ts) %in% pix$pixel)
chlor_ts <- chlor_ts[,c(22870,ind)]

# rename each pixel by region ID for region averaging -- now only looking at Okinawa island region, so all same region ("C")
chlor_ts = melt(setDT(chlor_ts), id.vars = c("month_year"), variable.name = "pixel")

pix_unique = unique(chlor_ts$pixel)

df <- data.frame(pixel = c(pix_unique),
                 chlor_episodes = c(rep(0, length(pix_unique))),
                 chlor_freq = c(rep(0, length(pix_unique))))

# define episodic events from monthly thresholds
for(i in 1:length(unique(chlor_ts$month_year))) {
  #i = 1
  chlor_month <- chlor_ts[chlor_ts$month_year == i,]
  chlor_thresh <- thresholds$chlor_thresh[i]
  chlor_month$ee <- ifelse(chlor_month$value > chlor_thresh, 1, 0)
  chlor_month <- chlor_month %>%
    group_by(pixel) %>%
    summarise(ee = sum(ee, na.rm=T))

  df$chlor_episodes <- df$chlor_episodes + chlor_month$ee
  
  print(paste("completed", i, "episodes of", nrow(df),"months"))
}

df$chlor_freq <- df$chlor_episodes/ntime

saveRDS(df, file = paste0("outputs/episodic_events/episodes_monthly_500m_final.rds"))

