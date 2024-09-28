library(raster)
library(dplyr)


full_grid <- readRDS("~/GOCIproj/outputs/episodic_events/episodes_8xd_500m_final.rds")
coarse_grid <- readRDS("~/GOCIproj/outputs/episodic_events/episodes_daily_4km_final.rds")

# Add in missing (i.e. all NA) pixels for full res only #
full_grid <- full_grid %>%
  mutate(pixelnum = as.numeric(substring(pixel,7))) %>%
  complete(pixelnum=1:1600000)

coarse_grid <- coarse_grid %>%
  mutate(pixelnum = as.numeric(substring(pixel,7))) %>%
  complete(pixelnum=1:25000)

r_full <- raster(ncol = 2000, nrow = 800, ext = extent(chlor_okinawa)) # full res
r_coarse <- raster(ncol = 250, nrow = 100, ext = extent(chlor_4km)) # coarse res

values(r_full) <- full_grid$V1
values(r_coarse) <- coarse_grid$V1

ext <- extent(127.2,128.4,26,27)
r_full = crop(r_full, ext)
r_coarse = crop(r_coarse, ext)

r_full = as.data.frame(r_full, xy=T)

# extract coarse pixels to empty full grid to fill in offshore gaps that arenʻt present in real full res
r_full2 <- cbind(r_full[,c(1,2)], raster::extract(r_coarse, r_full[,c(1,2)], method="simple"))
names(r_full2)[3] <- "extra"
r_full2$og = r_full$layer
r_full2 = r_full2 %>% 
  mutate(new = coalesce(og, extra))

r_coarse = as.data.frame(r_coarse, xy=T)

area_full = length(which(!is.na(r_full2$new)))*(500^2)
area_coarse = length(which(!is.na(r_coarse$layer)))*(4000^2)

area_gained = (area_full - area_coarse)/(1000^2)
improvement = area_gained/((area_gained + area_coarse)/(1000^2))

okinawa <- shapefile("Envirn_data/okinawa_shp/okinawa.shp")

png(paste0("outputs/figures/area_gained.png"), height = 5, width = 6, res = 500, units = "in")
ggplot() + 
  geom_polygon(okinawa, mapping=aes(long,lat,group=group), lwd=0.2, fill = "black") +
  geom_tile(na.omit(r_coarse), mapping=aes(x=x,y=y), fill="olivedrab4") +
  geom_tile(na.omit(r_full), mapping=aes(x=x,y=y), fill="royalblue3", alpha = 0.7) + xlab("Lon") + ylab("Lat") + ylim(26,27) + xlim(127.2,128.4) +
  scale_fill_viridis(option = "D", name = "") + theme_bw() +
  theme(panel.background = element_rect(fill = "gray81"), axis.text = element_text(size = 14), axis.title = element_text(size = 14),
        legend.text = element_text(size=14), legend.title = element_text(size = 14))
dev.off()

#####################################################################
# How much closer to shore is the high res data compared to low res #
# (i.e. average distance to first pixel along boundary)?            #
#####################################################################
# full res
load("Envirn_data/ETOPO_2022_15s_bathy_okinawa.RData")
bathy_okinawa[bathy_okinawa[] < -30 ] = NA 
ext <- extent(127.2,128.4,26,27)
bathy_okinawa = crop(bathy_okinawa, ext)
bathy_okinawa_poly = rasterToPolygons(bathy_okinawa)
adj_pixel_500m <- raster::buffer(bathy_okinawa_poly, width = 853.5534) # 353 + 1*500
#save(adj_pixel_500m, file = paste0("Envirn_data/adj_pixel_boundary_500m_singlepix.RData"))

okinawa <- readRDS("~/GOCIproj/outputs/okinawa_pixels_latlon_fullres.rds")

# Add in missing (i.e. all NA) pixels #
okinawa <- okinawa %>%
  mutate(pixelnum = as.numeric(substring(pixel,7))) %>%
  complete(pixelnum=1:1600000) # fullres

# convert dataframe to raster for mapping out pixels #
r <- raster(ncol = 2000, nrow = 800, ext = extent(chlor_okinawa)) # full res
values(r) <- as.factor(okinawa$ID)
names(r) <- "ID"

r_new <- raster::mask(r, adj_pixel_500m)
plot(r_new)

df <- as.data.frame(r_new, xy=T)
df$pixel = paste0("pixel_", seq(1:nrow(df)))
df = df[complete.cases(df),]

dists <- readRDS("~/GOCIproj/outputs/correlations/daily_corr_all_spatial_log_dist2shore.rds")
dists = subset(dists, scale == "500m")

new = merge(df, dists[,c(1,2,6)], by = c("x","y"))
full = mean(new$dist2shore)/1000

# coarse
coarse_grid <- readRDS("~/GOCIproj/outputs/episodic_events/episodes_daily_4km_final.rds")
coarse_grid <- coarse_grid %>%
  mutate(pixelnum = as.numeric(substring(pixel,7))) %>%
  complete(pixelnum=1:25000)

r_coarse <- raster(ncol = 250, nrow = 100, ext = extent(chlor_4km)) # coarse res
values(r_coarse) <- coarse_grid$chlor_episodes
df <- as.data.frame(r_coarse, xy=T)
df = df[complete.cases(df),]

dists <- readRDS("~/GOCIproj/outputs/correlations/daily_corr_all_spatial_log_dist2shore.rds")
dists = subset(dists, scale == "4km")

new = merge(df, dists[,c(1,2,6)], by = c("x","y"))
coarse = mean(new$dist2shore)/1000

