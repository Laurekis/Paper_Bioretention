library(prism)
library(raster)
library(dplyr)
library(stringr)
library(cluster)

#Set path for storing data
options(prism.path = "~/WORK/PRISM")
options(prism.path = "D:/FileHistory/rwl21875@uga.edu/ENGR300104L (2)/Data/C/Users/rwl21875/Documents/PRISM")

#Normal precip
#ann_norm <- get_prism_normals(type = "ppt", resolution = "4km", annual = TRUE, keepZip = FALSE)
#prism_image(ls_prism_data()[1,1])

#mon_norm <- get_prism_normals(type = "ppt", resolution = "4km", mon = 1:12, keepZip = FALSE)

all_files <- ls_prism_data(absPath = TRUE)[,2]
all_files2 <- ls_prism_data()[,1]
month_files <- grep("30yr_normal", all_files, value = TRUE)[1:12]
month_rasters <- lapply(month_files, raster)

winter <- month_rasters[[12]] + month_rasters[[1]] + month_rasters[[2]]
spring <- month_rasters[[3]] + month_rasters[[4]] + month_rasters[[5]]
summer <- month_rasters[[6]] + month_rasters[[7]] + month_rasters[[8]]
fall <- month_rasters[[9]] + month_rasters[[10]] + month_rasters[[11]]

seasons <- stack(winter, spring, summer, fall)

par(mfrow = c(2,2))
plot(seasons, zlim = c(0, 2200))

#sum_ann <- Reduce("+", month_rasters)
ann_file <- grep("annual", all_files, value = TRUE)
ann <- raster(ann_file)

writeRaster(ann, "~/WORK/SWMM_Rain_Gardens/PRISM Analysis/annual_ppt.tif")

#Calculate seasonality index
diff <- lapply(month_rasters, function(x, ann){
  abs(x - ann / 12)
}, ann)

SI <- 1 / ann * Reduce("+", diff)

plot(SI)

writeRaster(SI, "~/WORK/SWMM_Rain_Gardens/PRISM Analysis/SI.tif")

#calculate fraction of precipitation falling in summer
percent_summer <- summer / ann
plot(percent_summer)

writeRaster(percent_summer, "~/WORK/SWMM_Rain_Gardens/PRISM Analysis/percent_summer.tif")

#correlation between annual rainfall and seasonality
# cor.test(as.vector(as.matrix(ann)), as.vector(as.matrix(SI)))
# plot(as.vector(as.matrix(ann)), as.vector(as.matrix(SI)))

#Get daily data
daily_files <- list.files("~/WORK/SWMM_Rain_Gardens/PRISM Analysis", pattern = "summary", full.names = TRUE)

daily_data <- lapply(daily_files, function(x){
  data <- read.csv(x) %>%
    mutate(ID = 1:n())
})

daily_data2 <- do.call("rbind", daily_data) %>%
  group_by(ID) %>%
  summarize_all(mean, na.rm = TRUE)

summary_rasters <- apply(daily_data2[, 2:ncol(daily_data2)], 2, function(x, ann){
  raster(as.matrix(x, ncol = ncol(ann), byrow = FALSE), template = ann)
}, ann = ann)

plot(stack(summary_rasters))

#Write rasters
for (i in 1:length(summary_rasters)){
  writeRaster(summary_rasters[[i]], paste0("~/WORK/SWMM_Rain_Gardens/PRISM Analysis/", names(summary_rasters)[i], ".tif"))
}

summary_rasters_agg <- lapply(summary_rasters, function(x){
  aggregate(x, fact = 5, fun = mean, expand = TRUE)
})

#Aggregate rasters and put variables into data frame
ann_agg <- aggregate(ann, fact = 5, fun = mean, expand = TRUE)
SI_agg <- aggregate(SI, fact = 5, fun = mean, expand = TRUE)
sum_agg <- aggregate(percent_summer, fact = 5, fun = mean, expand = TRUE)
P_PET <- raster("~/WORK/SWMM_Rain_Gardens/PRISM Analysis/P_PET_new.tif")
P_PET_agg <- aggregate(P_PET, fact = 5, fun = mean, expand = TRUE)
snow_prop <- raster("~/WORK/SWMM_Rain_Gardens/PRISM Analysis/Snow Prop.tif")
data <- data.frame(ann = as.vector(as.matrix(ann_agg)),
                   P_PET = as.vector(as.matrix(P_PET_agg)),                   
                   SI = as.vector(as.matrix(SI_agg)),
                   p_sum = as.vector(as.matrix(sum_agg)),
                   mean_daily = as.vector(as.matrix(summary_rasters_agg$mean_ppt)),
                   n_days = as.vector(as.matrix(summary_rasters_agg$n_days)),
                   #n_heavy = as.vector(as.matrix(summary_rasters_agg$n_heavy)),
                   n_vheavy = as.vector(as.matrix(summary_rasters_agg$n_vheavy)),
                   delta = as.vector(as.matrix(summary_rasters_agg$delta))) %>%
  filter(!is.na(ann))

data_all <- data.frame(ann = as.vector(as.matrix(ann)),
                   P_PET = as.vector(as.matrix(P_PET)),
                   snow_prop = as.vector(as.matrix(snow_prop)),
                   SI = as.vector(as.matrix(SI)),
                   p_sum = as.vector(as.matrix(percent_summer)),
                   n_days = as.vector(as.matrix(summary_rasters$n_days)),
                   #mean_daily = as.vector(as.matrix(summary_rasters$mean_ppt)),
                   n_heavy = as.vector(as.matrix(summary_rasters$n_heavy)),
                   n_vheavy = as.vector(as.matrix(summary_rasters$n_vheavy)),
                   delta = as.vector(as.matrix(summary_rasters$delta))) %>%
  filter(!is.na(P_PET))

cor <- cor(data_all)
cor_p <- corrplot::cor.mtest(data_all)
corrplot::corrplot(cor, p.mat = cor_p$p, sig.level = 0.1, method = "number")

data_scale <- scale(data)

# library(factoextra)
# library(FactoMineR)
# 
# set.seed(1234)
# fviz_nbclust(data_scale, kmeans, method = "wss", k.max = 30)
# k5 <- kmeans(data_scale, centers = 10, iter.max = 20)
# fviz_cluster(k5, geom = "point", data = data)
# 
# # k3 <- kmeans(data, centers = 1000, iter.max = 20)
# # cah <- HCPC(k3$centers, graph = FALSE, nb.clust = -1)
# # plot.HCPC(cah, choice = "tree")
# 
# cluster_vals <- as.vector(as.matrix(ann_agg))
# cluster_vals[!is.na(cluster_vals)] <- k5$cluster
# cluster_raster <- raster(matrix(cluster_vals, ncol = ncol(ann_agg), byrow = FALSE), template = ann_agg)
# 
# plot(stack(ann_agg, SI_agg, sum_agg, summary_rasters_agg$n_days, summary_rasters_agg$n_vheavy,
#            summary_rasters_agg$delta, cluster_raster))


##################3
#Heirarchical clustering
# dist_mat <- dist(data)
# clusters <- hclust(dist_mat)
# plot(clusters)
# 
# hc <- cluster::diana(data_scale)
# cluster::pltree(hc, cex = 0.2)
# 
# clusterCut <- cutree(hc, 10)
# 
# cluster_vals2 <- as.vector(as.matrix(ann_agg))
# cluster_vals2[!is.na(cluster_vals2)] <- clusterCut
# cluster_raster2 <- raster(matrix(cluster_vals2, ncol = ncol(ann_agg), byrow = FALSE), template = ann_agg)
# plot(cluster_raster2)


#################3
#Principal components
pca <- prcomp(data_all, center = TRUE, scale = TRUE)
summary(pca)
pca$rotation

#Do clustering on rotation matrix of variable loadings - only top 3
#fviz_nbclust(pca$x[,1:3], kmeans, method = "wss", k.max = 10)
#Find optimal number of clusters
set.seed(9876)
n_clust <- 2:20
avg_sil <- numeric()
for (i in 1:length(n_clust)){
  test <- clara(scale(data_all), k = n_clust[i], rngR = TRUE, samples = 100, sampsize = 1000)
  sil <- summary(silhouette(test))
  avg_sil[i] <- sil$avg.width
}

plot(n_clust, avg_sil, type = "b", ylim = c(0,  1))

#Find optimal number of clusters
n_clust <- 1:10
tot_wss <- numeric()
for (i in n_clust){
  test <- kmeans(pca$x[,1:4], centers = i, iter.max = 20, nstart = 5)
  tot_wss[i] <- test$tot.withinss
}
plot(n_clust, tot_wss, type = "b")

set.seed(1234)
n_k <- 8
#k5 <- kmeans(pca$x[,1:3], centers = n_k, iter.max = 50, nstart = 10)
k5 <- clara(pca$x[,1:4], k = n_k, rngR = TRUE, samples = 100, sampsize = 1000)

cluster_vals <- as.vector(as.matrix(ann))
cluster_vals[!is.na(cluster_vals)] <- k5$clustering
cluster_raster <- raster(matrix(cluster_vals, ncol = ncol(ann), byrow = FALSE), template = ann)
plot(cluster_raster, col = RColorBrewer::brewer.pal(n_k, "Set1"))

#Compare to clustering on full dataset
#k_full <- kmeans(scale(data_all), centers = n_k, iter.max = 50, nstart = 10)
k_full <- clara(scale(data_all), k = n_k, rngR = TRUE, samples = 100, sampsize = 1000)
cluster_vals_all <- as.vector(as.matrix(ann))
cluster_vals_all[!is.na(cluster_vals_all)] <- k_full$clustering
cluster_raster_all <- raster(matrix(cluster_vals_all, ncol = ncol(ann), byrow = FALSE), template = ann)
plot(cluster_raster_all, col = RColorBrewer::brewer.pal(n_k, "Set1"))
summary(silhouette(k_full))$avg.width

#Calculate summary stats by group
data_summary <- data_all %>%
  mutate(Group = k_full$clustering) %>%
  group_by(Group) %>%
  summarize_all(list(~mean(.), ~median(.), ~min(.), ~max(.)))

data_means <- data_all %>%
  mutate(Group = k_full$clustering) %>%
  group_by(Group) %>%
  summarize_all(mean) %>%
  round(2)

#Plot stats by group
data_grouped <- data_all %>%
  mutate(Group = k_full$clustering)

# source("~/WORK/R Functions/Plot Functions.R")
# vars <- colnames(data_grouped)[1:9]
# var_names <- c("Annual PPT [mm]", "PPT - PET [mm]", "Snow PPT / Annual PPT", "Seasonality Index", "Summer PPT / Annual PPT",
#                "Num Days w/ PPT", "Num Days Intense PPT", "Num Days Very Intense PPT", "Inter-storm Dur. [days]")
# 
# png("~/WORK/SWMM_Rain_Gardens/PRISM Analysis/Group Boxplots.png", type = "cairo", units = "in",
#     height = 6, width = 6, res = 500)
# par(mfrow = c(3, 3), mar = c(2, 4, 1, 0.5))
# for (i in 1:length(vars)){
#   rodplot(as.formula(paste(vars[i], "~ Group")), data_grouped, ylab = var_names[i], las = 1,
#           col = RColorBrewer::brewer.pal(n_k, "Set1"), xlab = "", range = 0)
# }
# dev.off()

################################################
#Raster plots
rasters_all <- list(ann / 1000, P_PET / 1000, snow_prop, SI, percent_summer, summary_rasters$n_days,  
                    summary_rasters$n_heavy, summary_rasters$n_vheavy, summary_rasters$delta)

#Get states shapefile
states <- sf::st_read("~/WORK/General_GIS/statesp020.shp")
#plot(sf::st_geometry(states))

source("~/WORK/R Functions/Plot Functions.R")
var_names <- c("Annual PPT [m]", "PPT - PET [m]", "Snow PPT / Annual PPT", "Seasonality Index", "Summer PPT / Annual PPT",
               "Num Days w/ PPT", "Num Days Intense PPT", "Num Days Very Intense PPT", "Inter-storm Dur. [days]")

png("~/WORK/SWMM_Rain_Gardens/PRISM Analysis/PPT Rasters.png", type = "cairo", units = "in",
    height = 6, width = 8, res = 500)
par(mfrow = c(3, 3), mar = c(2, 3, 2, 2), oma = c(0, 0, 0, 1), mgp = c(2, 0.7, 0))
colors <- c("Blues", "BrBG", "PuBu", "RdPu", "RdPu", "PuBu", "cividis", "cividis", "YlOrBr")
n_cols <- c(14, 14, 255, 255, 255, 255, 14, 14, 14)
colors2 <- purrr::map2(colors, n_cols, function(x, y){
  cRamp_legend(y, x)
})
breaks <- list(round(c(seq(0, 3, length.out = 12), 4, 5, 6), 1),
               round(c(-5.5, seq(-2, 2, length.out = 12), 5.5), 1),
               NULL,
               NULL,
               NULL,
               NULL,
               round(c(seq(0, 30, length.out = 12), 40, 50, 60), 1),
               round(c(seq(0, 6, length.out = 12), 9, 12, 16), 1),
               round(c(seq(0, 20, length.out = 12), 32, 44, 60), 1))
for (i in 1:length(rasters_all)){
  if (i %in% c(1, 2, 7, 8, 9)){
    plot(rasters_all[[i]], col = colors2[[i]], las = 1, main = var_names[i], cex.axis = 0.8, breaks = breaks[[i]], legend = FALSE)
    plot(rasters_all[[i]], legend.only = TRUE, zlim = range(breaks[[i]]), col = colors2[[i]])
  } else{
    plot(rasters_all[[i]], col = colors2[[i]], las = 1, main = var_names[i], cex.axis = 0.8)
  }

  plot(sf::st_geometry(states), add = TRUE, border = "gray60", lwd = 0.5)
}
dev.off()

#Raster plot of groups
png("~/WORK/SWMM_Rain_Gardens/PRISM Analysis/PPT Regions_new.png", type = "cairo", units = "in",
    height = 4, width = 6, res = 500)
par(mfrow = c(1, 1), mar = c(3, 3, 2, 1))
plot(cluster_raster_all, col = RColorBrewer::brewer.pal(n_k, "Set1"), las = 1, main = "Rainfall Regions")
plot(sf::st_geometry(states), add = TRUE, border = "gray20", lwd = 1)
dev.off()

writeRaster(cluster_raster_all, "~/WORK/SWMM_Rain_Gardens/PRISM Analysis/PPT Regions_new.tif", overwrite = TRUE)


####################################
#Boxplots of pca
pca_grouped <- as.data.frame(pca$x) %>%
  mutate(Group = k5$clustering)

par(mfrow = c(1, 3))
vars <- colnames(pca_grouped)[1:3]
for (i in 1:length(vars)){
  rodplot(as.formula(paste(vars[i], "~ Group")), pca_grouped, las = 1,
          col = RColorBrewer::brewer.pal(n_k, "Set1"), xlab = "", range = 0)
}

############################################
#Compare values between parts of regions that are very separate (e.g. southeast and Seattle, midwest and Florida)
regions <- raster("~/WORK/SWMM_Rain_Gardens/PRISM Analysis/PPT Regions_new.tif")
coords <- xyFromCell(regions, 1:length(regions))
lon <- matrix(coords[,1], ncol = ncol(regions), byrow = TRUE)
lon <- as.vector(lon)
lat <- matrix(coords[,2], ncol = ncol(regions), byrow = TRUE)
lat <- as.vector(lat)

#Make sure data_all includes NAs
data_all2 <- data_all %>%
  mutate(region = as.vector(as.matrix(regions)),
         lon = lon,
         lat = lat)

region2 <- filter(data_all2, region == 2) %>%
  mutate(area = if_else(lon < -110, "NW", "SE"))
region3 <- filter(data_all2, region == 3) %>%
  mutate(area = if_else(lon > -90 & lat < 35, "FL", "MW"))
region1 <- filter(data_all2, region == 1) %>%
  mutate(area = if_else(lon > -100, "Gulf", "NW"))

data_grouped <- data_all2 %>%
  mutate(region = as.character(region),
         region = case_when(region == "2" & lon < -110 ~ "2A",
                            region == "3" & lon > -90 & lat < 35 ~ "3A",
                            TRUE ~ region),
         region = factor(region, levels = c("1", "2", "2A", "3", "3A", "4", "5", "6", "7", "8")),
         ann = ann / 1000,
         P_PET = P_PET / 1000) %>%
  group_by(region)

source("~/WORK/R Functions/Plot Functions.R")
library(vioplot)
vars <- colnames(data_grouped)[1:9]
var_names <- c("Annual PPT [m]", "PPT - PET [m]", "Snow PPT / Annual PPT", "Seasonality Index", "Summer PPT / Annual PPT",
               "Num Days w/ PPT", "Num Days Intense PPT", "Num Days Very Intense PPT", "Inter-storm Dur. [days]")

png("~/WORK/SWMM_Rain_Gardens/PRISM Analysis/Group Boxplots.png", type = "cairo", units = "in",
    height = 6, width = 7.5, res = 500)
colors <- RColorBrewer::brewer.pal(8, "Set1")
colors <- R.utils::insert(colors, ats = c(3, 4), values = adjustcolor(colors[c(2, 3)], alpha.f = 0.6))
par(mfrow = c(3, 3), mar = c(2, 3, 1, 0.5), mgp = c(2, 0.6, 0), oma = c(2, 0, 0, 0))
for (i in 1:length(vars)){
  # rodplot(as.formula(paste(vars[i], "~ region")), data_grouped, ylab = var_names[i], las = 1,
  #         col = colors, xlab = "", xaxt = "n", outpch = 16, outcol = adjustcolor(colors, 0.4), outcex = 0.7)
  # box <- rodplot(as.formula(paste(vars[i], "~ region")), data_grouped, ylab = var_names[i], las = 1,
  #        col = colors, xlab = "", xaxt = "n", outline = FALSE, ylim = range(data_grouped[,vars[i]], na.rm= TRUE))
  # points(jitter(box$group), box$out, pch = 16, col = rep(adjustcolor(colors, alpha.f = 0.5), as.numeric(table(box$group))))
  # axis(side = 1, at = seq(1, 9, 2), labels = levels(data_grouped$region)[seq(1, 9, 2)])
  # axis(side = 1, at = seq(2, 10, 2), labels = levels(data_grouped$region)[seq(2, 10, 2)])
  if (i == 3){

    subset <- data_grouped %>%
      filter(!is.na(region))
    
    region_add <- subset %>%
      filter(row_number() == 1,
             region %in% c("1", "2", "2A", "3A", "7"))
    
    subset <- filter(subset, !(region %in% c("1", "2", "2A", "3A", "7"))) %>%
      rbind(region_add)
    
    max_vals <- summarize(data_grouped,
                          max_snow = max(snow_prop, na.rm = TRUE))

    vioplot(as.formula(paste(vars[i], "~ region")), subset, ylab = var_names[i], las = 1,
            col = colors, xlab = "", cex = 0.8, xaxt = "n")
    
    segments(x0 = c(1:3, 5, 9), x1 = c(1:3, 5, 9), y0 = 0, y1 = max_vals$max_snow[c(1:3, 5, 9)])
    points(x = c(1:3, 5, 9), y = rep(0, 5), pch = 15, cex = 0.5)
    axis(side = 1, at = seq(1, 9, 2), labels = levels(data_grouped$region)[seq(1, 9, 2)])
    axis(side = 1, at = seq(2, 10, 2), labels = levels(data_grouped$region)[seq(2, 10, 2)])
    
  }else{
    vioplot(as.formula(paste(vars[i], "~ region")), data_grouped, ylab = var_names[i], las = 1,
            col = colors, xlab = "", cex = 0.8, xaxt = "n")
    axis(side = 1, at = seq(1, 9, 2), labels = levels(data_grouped$region)[seq(1, 9, 2)])
    axis(side = 1, at = seq(2, 10, 2), labels = levels(data_grouped$region)[seq(2, 10, 2)])
  }
}
mtext("Rainfall Region", side = 1, line = 0.5, outer = TRUE)
dev.off()

png("~/WORK/SWMM_Rain_Gardens/PRISM Analysis/Region 2 Differences.png", type = "cairo", units = "in",
    height = 6, width = 6, res = 500)
par(mfrow = c(3, 3), mar = c(2, 4, 1, 0.5))
vars <- colnames(region2)[1:9]
colors <- RColorBrewer::brewer.pal(8, "Set1")
for (i in 1:length(vars)){
  rodplot(as.formula(paste(vars[i], "~ area")), region2, ylab = var_names[i], las = 1,
          col = c(colors[2], adjustcolor(colors[2], alpha.f = 0.7)), xlab = "", range = 0)
}
dev.off()

png("~/WORK/SWMM_Rain_Gardens/PRISM Analysis/Region 3 Differences.png", type = "cairo", units = "in",
    height = 6, width = 6, res = 500)
par(mfrow = c(3, 3), mar = c(2, 4, 1, 0.5))
for (i in 1:length(vars)){
  rodplot(as.formula(paste(vars[i], "~ area")), region3, ylab = var_names[i], las = 1,
          col = c(colors[3], adjustcolor(colors[3], alpha.f = 0.7)), xlab = "", range = 0)
}
dev.off()

png("~/WORK/SWMM_Rain_Gardens/PRISM Analysis/Region 1 Differences.png", type = "cairo", units = "in",
    height = 6, width = 6, res = 500)
par(mfrow = c(3, 3), mar = c(2, 4, 1, 0.5))
for (i in 1:length(vars)){
  rodplot(as.formula(paste(vars[i], "~ area")), region1, ylab = var_names[i], las = 1,
          col = c(colors[1], adjustcolor(colors[1], alpha.f = 0.7)), xlab = "", range = 0)
}
dev.off()

par(mfrow = c(1,1))
plot(lat ~ lon, region3, col = as.factor(region3$area), pch = 16, cex = 0.5)

par(mfrow = c(1,1))
plot(lat ~ lon, region2, col = as.factor(region2$area), pch = 16, cex = 0.5)

par(mfrow = c(1,1))
plot(lat ~ lon, region1, col = as.factor(region1$area), pch = 16, cex = 0.5)


#Split regions
colors <- RColorBrewer::brewer.pal(8, "Set1")
plot(regions, col = colors)
data <- data.frame(region = as.vector(as.matrix(regions)),
                   lat = lat,
                   lon = lon) %>%
  mutate(region = case_when(region == 2 & lon < -110 ~ 9,
                            region == 3 & lon > -90 & lat < 35 ~ 10,
                            TRUE ~ region))

colors2 <- c(colors, adjustcolor(colors[2], alpha.f = 0.7), adjustcolor(colors[3], alpha.f = 0.7))

regions2 <- raster(matrix(data$region, ncol = ncol(regions), byrow = FALSE), template = regions)
plot(regions2, col = colors2)

writeRaster(regions2, "~/WORK/SWMM_Rain_Gardens/PRISM Analysis/PPT_regions_subdivided_new.tif", overwrite = TRUE)
