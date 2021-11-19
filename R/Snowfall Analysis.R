library(prism)
library(raster)

options(prism.path = "~/PRISM")

all_files <- ls_prism_data(absPath = TRUE)[,2]
temp_files <- grep("tmean", all_files, value = TRUE)

temp_rasters <- lapply(temp_files, raster)

ppt_files <- grep("normal", all_files, value = TRUE)
ppt_files <- grep("ppt", ppt_files, value = TRUE)[1:12]

ppt_rasters <- lapply(ppt_files, raster)

#Find out if it falls as snow or not
snow_list <- list()
for (i in 1:length(temp_rasters)){
  snow_list[[i]] <- ppt_rasters[[i]]
  snow_list[[i]][temp_rasters[[i]] > 0] <- 0
}

snow_all <- Reduce("+", snow_list)
ann_ppt <- Reduce("+", ppt_rasters)

snow_prop <- snow_all / ann_ppt

plot(snow_prop)
plot(snow_all)

plot(stack(snow_list))

writeRaster(snow_prop, "~/WORK/SWMM_Rain_Gardens/PRISM Analysis/Snow Prop.tif")
