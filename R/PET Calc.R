library(insol)
library(humidity)
library(prism)
library(raster)
library(dplyr)

options(prism.path = "~/PRISM")

#Monthly normal temp
get_prism_normals(type = "tmean", resolution = "4km", mon = 1:12, keepZip = FALSE)

temp_files <- ls_prism_data(absPath = TRUE)[,2]
temp_files <- grep("tmean", temp_files, value = TRUE)

temp_rasters <- lapply(temp_files, raster)
coords <- coordinates(temp_rasters[[1]])

#Calculate day length for each point by day and average by month
jd2010=JD(seq(ISOdate(2010,1,1),ISOdate(2010,12,31),by='day'))
months <- lubridate::month(as.Date(0:364, origin = as.Date("1990-01-01")))
length <- apply(coords, 1, function(x, months){
  n <- daylength(lat = x[2], long = x[1], jd = jd2010, tmz = 0)[3]
  data <- data.frame(n, months) %>%
    group_by(months) %>%
    summarize(mean_n = mean(n))
  
  return(data$mean_n)
}, months)

temp_vals <- t(getValues(stack(temp_rasters)))

calc_PET <- function(tmean, length){
  esat <- 6.108 * exp(17.26939 * tmean / (tmean + 237.3))
  rhosat <- 216.7 * esat / (tmean + 273.3)
  
  # PET <- if_else(tmean < 0, 0,
  #                0.1651 * length / 12 * rhosat * 1.2)
  PET <- if_else(tmean < 0, 0,
                 0.0055 * (length / 12) ^ 2 * rhosat * 25.4)
  
  return(PET)
}

#Calc PET in mm/month
#Convert daily PET to monthly
days_in_month <- as.numeric(diff(seq(as.Date("1990-01-01"), as.Date("1991-01-01"), by = "month")))
PET <- matrix(NA, nrow = nrow(temp_vals), ncol = ncol(temp_vals))
for (i in 1:nrow(temp_vals)){
  PET[i,] <- calc_PET(temp_vals[i,], length[i,]) * days_in_month[i]
}

#Get monthly precip
all_files <- ls_prism_data()[,1]
ppt_files <- grep("normal", all_files, value = TRUE)
ppt_files <- grep("ppt", ppt_files, value = TRUE)[1:12]

ppt_values <- t(getValues(prism_stack(ppt_files)))

#Calculate P - PET - monthly by points
P_PET <- ppt_values - PET

#Summarize to annual
P_PET_avg <- apply(P_PET, 2, sum)

#Convert to raster
P_PET_raster <- raster(matrix(P_PET_avg, ncol = ncol(temp_rasters[[1]]), nrow = nrow(temp_rasters[[1]]), byrow = TRUE),
                       template = temp_rasters[[1]])

breaks <- c(-2500, -355, 0, 330, 914, 5500)
cols <- RColorBrewer::brewer.pal(n = 5, "RdBu")
plot(P_PET_raster, breaks = breaks, col = cols)

#Write raster to file - fixed bug in Julian day
writeRaster(P_PET_raster, "~/WORK/SWMM_Rain_Gardens/PRISM Analysis/P_PET_new.tif")
