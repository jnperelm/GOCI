################################################################################################
# Evaluate correlations between chlor-a and kd490 across different spatial and temporal scales #
################################################################################################

library(raster)
library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)

source("scripts/GOCI_functions.R")

##### Do extreme events change at higher spatial/temporal resolutions? #####

# load comparison outputs #
df <- readRDS("~/GOCIproj/outputs/episodic_events/episodes_8day_4km_final.rds")
#df <- readRDS("outputs/episodic_events/episodes_8xd_500m_final_ext.rds")

# Add in missing (i.e. all NA) pixels for full res only to convert to raster #
df <- df %>%
  mutate(pixelnum = as.numeric(substring(pixel,7))) %>%
  complete(pixelnum=1:25000)
  #complete(pixelnum=1:712356)
  #complete(pixelnum=1:1600000)
  

# convert dataframe to raster for mapping out pixels #

r <- raster(ncol = 250, nrow = 100, ext = extent(chlor_4km)) # coarse res
#r <- raster(ncol = 1334, nrow = 534, ext = extent(modres_201103_chlor)) # mod res
#r <- raster(ncol = 2000, nrow = 800, ext = extent(chlor_okinawa)) # full res

values(r) <- df$chlor_episodes
names(r) <- "episodes"
episodes_df <- as.data.frame(r, xy = T)

okinawa <- shapefile("Envirn_data/okinawa_shp/okinawa.shp")

episodes_sub <- episodes_df %>%
  subset(x>=127.2 & x<=128.4) %>%
  subset(y>=26 & y<=27)

png(paste0("outputs/episodic_events/figures/corarseres_8day_zoom_final.png"), height = 5, width = 7, res = 500, units = "in")
ggplot(na.omit(episodes_sub)) + 
  geom_polygon(okinawa, mapping=aes(long,lat,group=group), lwd=0.2, fill = "black") +
  #geom_polygon(okinawa_buffer, mapping=aes(long,lat,group=group), color="black", fill=NA, lwd=0.2) +
  geom_tile(aes(x=x,y=y, fill=episodes)) + xlab("Lon") + ylab("Lat") + ylim(26,27) + xlim(127.2,128.4) +
  scale_fill_viridis(option = "D", name = "Number\nof Events") + theme_bw() +
  theme(panel.background = element_rect(fill = "gray81"), axis.text = element_text(size = 16), 
        axis.title = element_text(size = 14), legend.text = element_text(size=16), legend.title = element_text(size = 16))
dev.off()

# save episodes dataframes for comparison
coarse_mo = episodes_sub
coarse_mo$scale = '4km_mo'
coarse_8d = episodes_sub
coarse_8d$scale = '4km_8d'
coarse_1d = episodes_sub
coarse_1d$scale = '4km_1d'
mod_mo = episodes_sub
mod_mo$scale = '750m_mo'
mod_8d = episodes_sub
mod_8d$scale = '750m_8d'
mod_1d = episodes_sub
mod_1d$scale = '750m_1d'
full_mo = episodes_sub
full_mo$scale = '500m_mo'
full_8d = episodes_sub
full_8d$scale = '500m_8d'
full_1d = episodes_sub
full_1d$scale = '500m_1d'
full_8xd = episodes_sub
full_8xd$scale = '500m_8xd'

coarse_mo2 = episodes_sub
coarse_mo$scale = '4km monthly'
coarse_8d2 = episodes_sub
coarse_8d$scale = '4km 8day'
coarse_1d2 = episodes_sub
coarse_1d$scale = '4km 1day'
mod_mo2 = episodes_sub
mod_mo$scale = '750m monthly'
mod_8d2 = episodes_sub
mod_8d$scale = '750m 8day'
mod_1d2 = episodes_sub
mod_1d$scale = '750m 1day'
full_mo2 = episodes_sub
full_mo$scale = '500m monthly'
full_8d2 = episodes_sub
full_8d$scale = '500m 8day'
full_1d2 = episodes_sub
full_1d$scale = '500m 1day'
full_8xd2 = episodes_sub
full_8xd$scale = '500m 8x/day'

df_all = rbind(coarse_mo,coarse_8d,coarse_1d,mod_mo,mod_8d,mod_1d,
               full_mo,full_8d,full_1d,full_8xd)
df_all$scale = factor(df_all$scale, levels = c('4km monthly','4km 8day','4km 1day',
                                               '750m monthly','750m 8day','750m 1day',
                                               '500m monthly','500m 8day','500m 1day','500m 8x/day'))

saveRDS(df_all, file = paste0("outputs/episodes_chlor_all_num_final.rds"))
#saveRDS(df_all2, file = paste0("outputs/episodes_chlor_all_freq_final.rds"))

library(plyr)
meds <- ddply(df_all, .(scale), summarise, med = median(episodes, na.rm=T))
df_all$scale <- factor(df_all$scale, levels = c('4km monthly','750m monthly','500m monthly',
                                                '4km 8day','750m 8day','500m 8day',
                                                '4km 1day','750m 1day','500m 1day','500m 8x/day'))

png(paste0("outputs/episodic_events/figures/EpisodeNums_chlor_boxplots_final.png"), height = 8.5, width = 8.5, res = 500, units = "in")
df_all %>%
  ggplot(aes(x=episodes,y =scale)) +
  #geom_histogram(alpha=0.6, binwidth = 5) +
  geom_boxplot(alpha=0.6, binwidth = 5) +
  geom_text(data = meds, aes(y = scale, x = med, label = med), size = 5, vjust = -2.15, color = "darkred") +
  geom_text(data = meds, aes(y = scale, x = med, label = med), size = 5, vjust = -2.15, color = "darkred") +
  geom_text(data = meds, aes(y = scale, x = med, label = med), size = 5, vjust = -2.15, color = "darkred") +
  geom_text(data = meds, aes(y = scale, x = med, label = med), size = 5, vjust = -2.15, color = "darkred") +
  geom_text(data = meds, aes(y = scale, x = med, label = med), size = 5, vjust = -2.15, color = "darkred") +
  theme_bw() + 
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    axis.text = element_text(size = 20), 
    axis.title = element_text(size = 20),
    
  ) +
  xlab("Episodes") + ylab("")# +
dev.off()

