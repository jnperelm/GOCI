################################################################################################
# Evaluate correlations between chlor-a and kd490 across different spatial and temporal scales #
################################################################################################

library(raster)
library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)

source("scripts/GOCI_functions.R")

##### Do correlations decouple at higher spatial/temporal resolutions? #####

# load comparison outputs #
df <- readRDS("~/GOCIproj/outputs/correlations/alltime/var_comparisons__8xd_500m_log.rds")

# If < 10 observations for a given pixel, make NA #
df <- df %>%
  mutate(corr = ifelse(n_obs<10,NA,corr),
         p_val = ifelse(n_obs<10,NA,p_val),
         slope = ifelse(n_obs<10,NA,slope))

# Add in missing (i.e. all NA) pixels to recreate raster #
df <- df %>%
  mutate(pixelnum = as.numeric(substring(pixel,7))) %>%
  complete(pixelnum=1:1600000) # fullres
  #complete(pixelnum=1:712356) # modres
  #complete(pixelnum=1:25000) # coarseres

# convert dataframe to raster for mapping out pixels #

#r <- raster(ncol = 250, nrow = 100, ext = extent(chlor_4km)) # coarse res
#r <- raster(ncol = 1334, nrow = 534, ext = extent(modres_201103_chlor)) # mod res
r <- raster(ncol = 2000, nrow = 800, ext = extent(chlor_okinawa)) # full res

values(r) <- df$n_obs
names(r) <- "corr"
cor_df <- as.data.frame(r, xy = T)
cor_df$ID <- region_ID(cor_df)

okinawa <- shapefile("Envirn_data/okinawa_shp/okinawa.shp")

# Plot correlations on map

png(paste0("outputs/correlations/alltime/figures/fullres_8day_corrmap_log.png"), height = 5, width = 7, res = 500, units = "in")
ggplot(na.omit(cor_df)) + 
  geom_polygon(okinawa, mapping=aes(long,lat,group=group), lwd=0.2, fill = "black") +
  #geom_polygon(adj_pixel_4km, mapping=aes(long,lat,group=group), color="black", fill=NA, lwd=0.2) +
  geom_tile(aes(x=x,y=y, fill=corr)) + xlab("Lon") + ylab("Lat") + ylim(26,27) + xlim(127.2,128.4) +
  scale_fill_viridis(option = "D", name = "Corr", lim = c(0,1)) + theme_bw() +
  #geom_rect(panels, mapping = aes(
  #  xmin = x_min,
  #  xmax = x_max,
  #  ymin = y_min,
  #  ymax = y_max), alpha = 0.1, fill=NA) +
  #facet_wrap(~ID, scales = "free") +
   theme(panel.background = element_rect(fill = "gray81"), axis.text = element_text(size = 14), axis.title = element_text(size = 14),
         legend.text = element_text(size=14), legend.title = element_text(size = 14))
dev.off()

# save correlations to comparison dataframe
df_coarse = cor_df
df_coarse$scale = '4km'
df_mod = cor_df
df_mod$scale = '750m'
df_full = cor_df
df_full$scale = '500m'
df_all = rbind(df_coarse, df_mod, df_full)
df_all$scale = factor(df_all$scale, levels = c('4km','750m','500m'))

saveRDS(df_all, file = paste0("outputs/correlations/alltime/daily_slope_all_spatial_log.rds"))

df_8xd = cor_df
df_8xd$scale = '8xd'
df_1d = cor_df
df_1d$scale = '1d'
df_8d = cor_df
df_8d$scale = '8d'
df_mo = cor_df
df_mo$scale = '1mo'
df_all = rbind(df_8xd, df_1d, df_8d, df_mo)
df_all$scale = factor(df_all$scale, levels = c('1mo','8d','1d','8xd'))

saveRDS(df_all, file = paste0("outputs/correlations/alltime/fullres_slope_all_times_log.rds"))

boxplot(df_all$corr~df_all$scale, xlab = "Slope", ylab = "", cex.axis=1.4, cex.lab=1.4, horizontal=T, ylim = c(0,1))

# are the means significantly different between scales?
library(dunn.test)
kruskal.test(corr ~ scale, data = df_all)
dunn.test(df_all$corr, df_all$scale, method = "bonferroni")

