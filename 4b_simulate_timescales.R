################################################
# Simulate coarser timescales (8-day, monthly) #
################################################

library(lubridate)
library(santoku)
library(dplyr)

### Average data across 8-day intervals ###

# 4 km #
coarse_chlor_ts <- readRDS("GOCI_data/coarse_res/masked/coarse_chlor_ts_cropped.rds")#[,-1]
coarse_kd490_ts <-readRDS("GOCI_data/coarse_res/masked/coarse_kd490_ts.rds")#[,-1]

coarse_chlor_ts$V1 <- as.Date(coarse_chlor_ts$V1, format = "X%Y.%m.%d.%H.%M.%S",tz = "Asia/Tokyo")
coarse_kd490_ts$V1 <- as.Date(coarse_kd490_ts$V1, format = "X%Y.%m.%d.%H.%M.%S",tz = "Asia/Tokyo")
coarse_chlor_ts$days8 = as.Date(chop_width(coarse_chlor_ts$V1, days(8), labels = lbl_endpoints(fmt = "%y-%m-%d")), tz = "Asia/Tokyo")
coarse_kd490_ts$days8 = as.Date(chop_width(coarse_kd490_ts$V1, days(8), labels = lbl_endpoints(fmt = "%y-%m-%d")), tz = "Asia/Tokyo")

coarse_chlor_8day <- coarse_chlor_ts[,-1] %>%
  group_by(days8) %>% summarize_all(~mean(., na.rm=T))
saveRDS(coarse_chlor_8day, file = "GOCI_data/coarse_res/masked/coarse_chlor_ts_8day_cropped.rds")

coarse_kd490_8day <- coarse_kd490_ts[,-1] %>%
  group_by(days8) %>% summarize_all(~mean(., na.rm=T))
saveRDS(coarse_kd490_8day, file = "GOCI_data/coarse_res/masked/coarse_kd490_ts_8day.rds")

# 750 m #
mod_chlor_ts <- readRDS("GOCI_data/moderate_res/masked/moderate_chlor_ts.rds")#[,-1]
mod_kd490_ts <-readRDS("GOCI_data/moderate_res/masked/moderate_kd490_ts.rds")#[,-1]

mod_chlor_ts$V1 <- as.Date(mod_chlor_ts$V1, format = "X%Y.%m.%d.%H.%M.%S",tz = "Asia/Tokyo")
mod_kd490_ts$V1 <- as.Date(mod_kd490_ts$V1, format = "X%Y.%m.%d.%H.%M.%S",tz = "Asia/Tokyo")

mod_chlor_ts$days8 = as.Date(chop_width(mod_chlor_ts$V1, days(8), labels = lbl_endpoints(fmt = "%y-%m-%d")), tz = "Asia/Tokyo")
mod_kd490_ts$days8 = as.Date(chop_width(mod_kd490_ts$V1, days(8), labels = lbl_endpoints(fmt = "%y-%m-%d")), tz = "Asia/Tokyo")

mod_chlor_8day <- mod_chlor_ts[,-1] %>%
  group_by(days8) %>% summarize_all(~mean(., na.rm=T))
saveRDS(mod_chlor_8day, file = "GOCI_data/moderate_res/masked/moderate_chlor_ts_8day.rds")

mod_kd490_8day <- mod_kd490_ts[,-1] %>%
  group_by(days8) %>% summarize_all(~mean(., na.rm=T))
saveRDS(mod_kd490_8day, file = "GOCI_data/moderate_res/masked/moderate_kd490_ts_8day.rds")

# 500 m #

full_chlor_ts <- readRDS("GOCI_data/full_res/masked/full_chlor_ts.rds")#[,-1]
full_kd490_ts <-readRDS("GOCI_data/full_res/masked/full_kd490_ts.rds")#[,-1]

# Pull out closest daily measurement to noon to get daily estimate
timestamps <- as.POSIXct(gsub("X","",full_chlor_ts$V1), format = "%Y.%m.%d.%H.%M.%S", tz = "Asia/Tokyo")
indices <- as.POSIXct(paste0(unique(format(as.Date(full_chlor_ts$V1, format = "X%Y.%m.%d.%H.%M.%S"), format = "%Y.%m.%d")),".12.16.00"),
                      format = "%Y.%m.%d.%H.%M.%S", tz = "Asia/Tokyo")

a=data.table(Value=timestamps)
a[,merge:=Value]
b=data.table(Value=indices)
b[,merge:=Value]
setkeyv(a,c('merge'))
setkeyv(b,c('merge'))
Merge_a_b=a[b,roll='nearest']
keep_idx = which(timestamps %in% Merge_a_b$Value)

full_chlor_ts <- full_chlor_ts[keep_idx,]
full_kd490_ts <- full_kd490_ts[keep_idx,]

full_chlor_ts$V1 <- as.Date(full_chlor_ts$V1, format = "X%Y.%m.%d.%H.%M.%S",tz = "Asia/Tokyo")
full_kd490_ts$V1 <- as.Date(full_kd490_ts$V1, format = "X%Y.%m.%d.%H.%M.%S",tz = "Asia/Tokyo")
full_chlor_ts$days8 = as.Date(chop_width(full_chlor_ts$V1, days(8), labels = lbl_endpoints(fmt = "%y-%m-%d")), tz = "Asia/Tokyo")
full_kd490_ts$days8 = as.Date(chop_width(full_kd490_ts$V1, days(8), labels = lbl_endpoints(fmt = "%y-%m-%d")), tz = "Asia/Tokyo")

full_chlor_8day <- full_chlor_ts[,-1] %>%
  group_by(days8) %>% summarize_all(~mean(., na.rm=T))
saveRDS(full_chlor_8day, file = "GOCI_data/full_res/masked/full_chlor_ts_8day.rds")

full_kd490_8day <- full_kd490_ts[,-1] %>%
  group_by(days8) %>% summarize_all(~mean(., na.rm=T))
saveRDS(full_kd490_8day, file = "GOCI_data/full_res/masked/full_kd490_ts_8day.rds")


### Average data across 1-month intervals ###

# 4 km #
coarse_chlor_ts <- readRDS("GOCI_data/coarse_res/masked/coarse_chlor_ts.rds")#[,-1]
coarse_kd490_ts <-readRDS("GOCI_data/coarse_res/masked/coarse_kd490_ts.rds")#[,-1]

coarse_chlor_ts$V1 <- as.Date(coarse_chlor_ts$V1, format = "X%Y.%m.%d.%H.%M.%S",tz = "Asia/Tokyo")
coarse_kd490_ts$V1 <- as.Date(coarse_kd490_ts$V1, format = "X%Y.%m.%d.%H.%M.%S",tz = "Asia/Tokyo")
coarse_chlor_ts$month_year <- paste0(lubridate::month(coarse_chlor_ts$V1),"_",lubridate::year(coarse_chlor_ts$V1))
coarse_kd490_ts$month_year <- paste0(lubridate::month(coarse_kd490_ts$V1),"_",lubridate::year(coarse_kd490_ts$V1))

coarse_chlor_month <- coarse_chlor_ts[,-1] %>%
  group_by(month_year) %>% summarize_all(~mean(., na.rm=T))
saveRDS(coarse_chlor_month, file = "GOCI_data/coarse_res/masked/coarse_chlor_ts_monthly.rds")

coarse_kd490_month <- coarse_kd490_ts[,-1] %>%
  group_by(month_year) %>% summarize_all(~mean(., na.rm=T))
saveRDS(coarse_kd490_month, file = "GOCI_data/coarse_res/masked/coarse_kd490_ts_monthly.rds")


# 750 km #
mod_chlor_ts <- readRDS("GOCI_data/moderate_res/masked/moderate_chlor_ts.rds")#[,-1]
mod_kd490_ts <-readRDS("GOCI_data/moderate_res/masked/moderate_kd490_ts.rds")#[,-1]

mod_chlor_ts$V1 <- as.Date(mod_chlor_ts$V1, format = "X%Y.%m.%d.%H.%M.%S",tz = "Asia/Tokyo")
mod_kd490_ts$V1 <- as.Date(mod_kd490_ts$V1, format = "X%Y.%m.%d.%H.%M.%S",tz = "Asia/Tokyo")
mod_chlor_ts$month_year <- paste0(lubridate::month(mod_chlor_ts$V1),"_",lubridate::year(mod_chlor_ts$V1))
mod_kd490_ts$month_year <- paste0(lubridate::month(mod_kd490_ts$V1),"_",lubridate::year(mod_kd490_ts$V1))

mod_chlor_month <- mod_chlor_ts[,-1] %>%
  group_by(month_year) %>% summarize_all(~mean(., na.rm=T))
saveRDS(mod_chlor_month, file = "GOCI_data/moderate_res/masked/moderate_chlor_ts_monthly.rds")

mod_kd490_month <- mod_kd490_ts[,-1] %>%
  group_by(month_year) %>% summarize_all(~mean(., na.rm=T))
saveRDS(mod_kd490_month, file = "GOCI_data/moderate_res/masked/moderate_kd490_ts_monthly.rds")

# 500 m #
full_chlor_ts <- readRDS("GOCI_data/full_res/masked/full_chlor_ts.rds")#[,-1]
full_kd490_ts <-readRDS("GOCI_data/full_res/masked/full_kd490_ts.rds")#[,-1]

full_chlor_ts$V1 <- as.Date(full_chlor_ts$V1, format = "X%Y.%m.%d.%H.%M.%S",tz = "Asia/Tokyo")
full_kd490_ts$V1 <- as.Date(full_kd490_ts$V1, format = "X%Y.%m.%d.%H.%M.%S",tz = "Asia/Tokyo")
full_chlor_ts$month_year <- paste0(lubridate::month(full_chlor_ts$V1),"_",lubridate::year(full_chlor_ts$V1))
full_kd490_ts$month_year <- paste0(lubridate::month(full_kd490_ts$V1),"_",lubridate::year(full_kd490_ts$V1))

full_chlor_month <- full_chlor_ts[,-1] %>%
  group_by(month_year) %>% summarize_all(~mean(., na.rm=T))
saveRDS(full_chlor_month, file = "GOCI_data/full_res/masked/full_chlor_ts_monthly.rds")

full_kd490_month <- full_kd490_ts[,-1] %>%
  group_by(month_year) %>% summarize_all(~mean(., na.rm=T))
saveRDS(full_kd490_month, file = "GOCI_data/full_res/masked/full_kd490_ts_monthly.rds")






