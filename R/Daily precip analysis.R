library(prism)
library(raster)
library(lubridate)

#Analyze daily prism precipitation data
#Get daily data
#get_prism_dailys(type = "ppt", "1982-01-01", "2009-12-31", keepZip = FALSE)
all_files2 <- ls_prism_data(absPath = TRUE)[,2]
daily_files <- grep("stable", all_files2, value = TRUE)

#Check dates
dates <- substr(daily_files, nchar(daily_files) - 15, nchar(daily_files) - 8)
diff_dates <- diff(as.Date(dates, format = "%Y%m%d"))

#Get daily data - loop through all files
ptm <- proc.time()
daily_rasters <- stack(daily_files)
proc.time() - ptm

#Store values on disk - by year
years <- 1981:2010
dates <- as.Date(dates, format = "%Y%m%d")
for (i in 1:length(years)){
  days <- which(year(dates) == years[i])
  ppt_vals <- daily_rasters[[days]][]
  
  # ptm <- proc.time()
  summary_data <- apply(ppt_vals, 1, function(x){
    #Remove all values < 1 mm
    x[x < 1] <- 0
    #Average daily of values greater than 0
    mean <- mean(x[x > 0])
    #number of days with rain
    n_days <- sum(x > 0)
    #Intensity categories
    n_mod <- sum(x >= 12.7 & x < 25.4)
    n_heavy <- sum(x >= 25.4 & x < 76.2)
    n_vheavy <- sum(x >= 76.2)
    n_extreme <- sum(x >= 152.4)
    #Inter-storm duration
    delta <- mean(diff(which(x > 0)))
    
    #return(data.frame(mean, n_days, n_mod, n_heavy, n_vheavy, n_extreme, delta))
    return(c(mean, n_days, n_mod, n_heavy, n_vheavy, n_extreme, delta))
    
  })
  # proc.time() - ptm
  
  summary_data_df <- t(summary_data)
  colnames(summary_data_df) <- c("mean_ppt", "n_days", "n_mod", "n_heavy", "n_vheavy", "n_extreme", "delta")
  
  write.csv(summary_data_df, paste0("~/WORK/SWMM_Rain_Gardens/PRISM Analysis/Annual summary ", years[i], ".csv"), row.names = FALSE)
  # mat <- ff::ff(NA, vmode = "double", dim = c(ncell(daily_rasters), nlayers(daily_rasters)),
  #               filename = "C:/Users/rwl21875/Documents/WORK/SWMM_Rain_Gardens/PRISM Analysis/daily_ppt.ffdata")
  gc()
}


daily_data <- lapply(daily_files[1:4], function(x){
  vals <- as.vector(getValues(raster(x)))
})

daily_data2 <- do.call("rbind", daily_data)

summary_data <- apply(daily_data2, 1, function(x){
  #Remove all values < 1 mm
  x[x < 1] <- 0
  #Average daily of values greater than 0
  mean <- mean(x[x > 0])
  #number of days with rain
  n_days <- sum(x > 0)
  #Intensity categories
  n_mod <- sum(x >= 12.7 & x < 25.4)
  n_heavy <- sum(x >= 25.4 & x < 76.2)
  n_vheavy <- sum(x >= 76.2)
  n_extreme <- sum(x >= 152.4)
  #Inter-storm duration
  delta <- mean(diff(which(x > 0)))
  
  return(data.frame(mean, n_days, n_mod, n_heavy, n_vheavy, n_extreme, delta))
  
})

summary_data_df <- do.call("rbind", summary_data)
#colnames(summary_data) <- c("mean", "n_days", "n_mod", "n_heavy", "n_vheavy", "n_extreme", "delta")

summary_rasters <- apply(summary_data_df, 2, function(x, ann){
  raster(as.matrix(x, ncol = ncol(ann), byrow = FALSE), template = ann)
}, ann = ann)

plot(stack(summary_rasters))