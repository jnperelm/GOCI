################################################################################################
# Evaluate correlations between chlor-a and kd490 across different spatial and temporal scales #
################################################################################################

# Do so on a log scale, as this mitigates outliers and is typically the way we analyze this highly skewed data

setwd("/home/jperelman/GOCIproj")

print("This is fullres 8x/day comparisons")
start_time = Sys.time()

library(raster)
library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)
library(data.table)

source("scripts/GOCI_functions.R")

##### Do correlations decouple at higher spatial/temporal resolutions? #####

chlor_ts <- as.data.frame(readRDS("GOCI_data/full_res/masked/full_chlor_ts_monthly.rds")[,-1])
kd490_ts <-as.data.frame(readRDS("GOCI_data/full_res/masked/full_kd490_ts_monthly.rds")[,-1])


# Pull out closest daily measurement to noon to get daily estimate
timestamps <- as.POSIXct(gsub("X","",chlor_ts$V1), format = "%Y.%m.%d.%H.%M.%S", tz = "Asia/Tokyo")
indices <- as.POSIXct(paste0(unique(format(as.Date(chlor_ts$V1, format = "X%Y.%m.%d.%H.%M.%S"), format = "%Y.%m.%d")),".12.16.00"),
                      format = "%Y.%m.%d.%H.%M.%S", tz = "Asia/Tokyo")

a=data.table(Value=timestamps)
a[,merge:=Value]
b=data.table(Value=indices)
b[,merge:=Value]
setkeyv(a,c('merge'))
setkeyv(b,c('merge'))
Merge_a_b=a[b,roll='nearest']
keep_idx = which(timestamps %in% Merge_a_b$Value)

chlor_ts <- chlor_ts[keep_idx,-1]
kd490_ts <- kd490_ts[keep_idx,-1]


df <- data.frame(pixel = c(colnames(chlor_ts)),
                 corr = c(rep(NA, length(colnames(chlor_ts)))),
                 p_val = c(rep(NA, length(colnames(chlor_ts)))),
                 slope = c(rep(NA, length(colnames(chlor_ts)))),
                 n_obs = c(rep(NA, length(colnames(chlor_ts)))),
                 prop_na = c(rep(NA, length(colnames(chlor_ts)))))

for(i in 1:ncol(chlor_ts)) {
  
  # i = 12524
  
  q<-which(is.na(chlor_ts[,i]))
  if (length(q)<length(chlor_ts[,i])-1){
    
    lmod <- lm(log(kd490_ts[,i])~log(chlor_ts[,i]))
    
    df$corr[i] <- cor(log(chlor_ts[,i]), log(kd490_ts[,i]), 
                      method = "pearson", 
                      use = "complete.obs")
    df$p_val[i] <- overall_p(lmod)
    df$slope[i] <- coef(lmod)[2]
    df$n_obs[i] <- length(lmod$fitted.values)
    df$prop_na[i] <- sum(is.na(chlor_ts[,i]))/length(chlor_ts[,i])
  }
  else if(length(q) >= length(chlor_ts[,i])-1) {
    df$corr[i] <- NA
    df$p_val[i] <- NA
    df$slope[i] <- NA
    df$n_obs[i] <- 0
    df$prop_na[i] <- 1
  }
  
  print(paste("completed", i, "comparisons of", nrow(df),"pixels"))
  
}

saveRDS(df, file = paste0("outputs/correlations/alltime/var_comparisons__monthly_500m_log.rds"))

end_time = Sys.time()
timestamp = end_time - start_time
print(timestamp)



