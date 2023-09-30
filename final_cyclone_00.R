
packages = c("ggmap",
             "ggplot2", "readr","dplyr", "gdata", "funHDDC")

## Now load or install&load all
package_check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)
source("00_subsetlist.R")
source("01_algorithm_distance.R")
source("02_algorithm_clustering.R")
source("03_algorithm_find_outlier.R")
source("find_optimal_theta.R")
source("clust_theta.R")

whole_track_df <- read_csv('whole_track_cyclone.csv')
whole_track_df_copy <- whole_track_df
whole_track_df_copy$vis_lon[intersect(which(whole_track_df$vis_lon < 6),
                                      which(whole_track_df$vis_lon >= 0))] <- 0




# cyclone_calculation<- lapply(1:length(unique(whole_track_df$id)),
#                              function(i){
#                                index <- which(whole_track_df$id == unique(whole_track_df$id)[i])
#                                len_time <- length(which(whole_track_df$id == unique(whole_track_df$id)[i]))
#                               
#                                return(list(
#                                  argvals = whole_track_df$time[index], 
#                                  subj = rep(unique(whole_track_df$id)[i], len_time),
#                                  y = cbind(whole_track_df$cal_lon[index], whole_track_df$lat[index])
#                                ))
#                              })
#distance_global <- distance_mfd(cyclone_calculation)

#save(distance_global, file = "distance_gb.Rdata")
load("distance_gb.Rdata")

#theta_global <- find_optimal_theta(distance_global, NULL, 0.05, 0.75, "silhouette")
  
#clust_global<-clustresult(distance_global, theta)

neighbours_global <- findneighbours(distance_global, theta = 0.05)
cluster_global <- clustering_warping(neighbours_global)  
clust_global <- include_isolatepoints(cluster_global[[2]], minprop = 0.03, 
                                     distance_global, alpha_c = 0.85)

###############
###################
###############################  #### In the review paper we consider cyclones influencing North America 
### Hence we take clustl = 4, 7
whole_track_df$Time <- whole_track_df$time
#### west Pacific clustl = c(1, 8)
# clustl = 1 ### Northwest Pacific
# clustl = 2 ## South Asia and India
# clustl = 3 ## Indian Ocean
# clustl = 4 ### North America (west and east) ### We use clustering to separate the west and east
# clustl = 5 ### South Pacific Ocean towards America
# clustl = 6 ### South Pacific Ocean towards Africa
# clustl = 7 ### East of North America
# clustl = 8 ### Northwest Pacific towards North America direction
myLocation <- c(-180, -65, 179.99, 65)

myMap <- get_map(location = myLocation,
                 source = "stamen",
                 maptype = "watercolor",
                 crop = FALSE)

clustl <- 4
subset_index <- unlist(lapply(subsetlist(clust_global$clust, clustl), function(l){l$element}))
subset_data <- subset(whole_track_df, id %in% subset_index)

ggmap(myMap) +
    #geom_point(aes(x = lon, y = lat),
    #           data = rbind(SI_track_df,NI_track_df), alpha = .5, color="black") +
    
    geom_path(aes(x = vis_lon, y = lat, group=name, colour = Time), 
              data = subset_data) +
    
    labs(title = "Global Cyclone Tracks (1842-2021)",x = "Longitude",y = "Latitude") +
    theme(plot.title = element_text(hjust = 0.5),plot.margin = margin(0.8, 1.2, 0.6, 1.2))+
    scale_colour_gradient(low = "red", high = "yellow", na.value = NA,
                          breaks = seq(0, 1, by = 0.2), labels = seq(0.0, 1.0, by = 0.2),
                          limits = c(0, 1))
##### do clustering for the second subset to separate the Pacific cyclone influencing California and the Atlantic cyclones

distance_subset <- distance_global[subset_index, subset_index]
#theta <- find_optimal_theta(distance_NP_index, NULL, 0.05,  0.75, "silh")
theta_candidate <- seq(0.01, 0.30, by = 0.01)
clust_dataframe <- t(sapply(theta_candidate, function(theta) {
    clust_result <- clust_theta(distance_subset, NULL, 0.05, theta, 0.87)
}))

theta <- theta_candidate[clust_dataframe[, 2] == max(clust_dataframe[, 2])]
neighbours_subset <- findneighbours(distance_subset, theta)

cluster_subset <- clustering_warping(neighbours_subset)  
clust_subset <- include_isolatepoints(cluster_subset[[2]], minprop = 0.05,
                                        distance_subset, alpha_c = 0.87)


clustl <- 2
np_subset_index <- subset_index[unlist(lapply(subsetlist(clust_subset$clust, clustl), function(l){l$element}))]
np_subset_data <- subset(whole_track_df, id %in% np_subset_index)


ggmap(myMap) +
    #geom_point(aes(x = lon, y = lat),
    #           data = rbind(SI_track_df,NI_track_df), alpha = .5, color="black") +
    
    geom_path(aes(x = vis_lon, y = lat, group=name, colour = Time), 
              data = np_subset_data) +
    
    #guides(fill=guide_legend(title="Time"))+
    #theme(legend.position = "none")+
    
    labs(title = "Global Cyclone Tracks (1842-2021)",x = "Longitude",y = "Latitude") +
    theme(plot.title = element_text(hjust = 0.5),plot.margin = margin(0.8, 1.2, 0.6, 1.2))+
    scale_colour_gradient(low = "red", high = "yellow", na.value = NA,
                          breaks = seq(0, 1, by = 0.2), labels = seq(0.0, 1.0, by = 0.2),
                          limits = c(0, 1))
####################################
clustl = 7

NP_index <- c(unlist(lapply(subsetlist(clust_global$clust, clustl), function(l){l$element})), np_subset_index)
save(NP_index, file = "NP_index.RData")
NP_subset <- subset(whole_track_df, id %in% NP_index)

myLocation <- c(-180, -65, 179.99, 65)

myMap <- get_map(location = myLocation,
                 source = "stamen",
                 maptype = "watercolor",
                 crop = FALSE)

ggmap(myMap) +
    #geom_point(aes(x = lon, y = lat),
    #           data = rbind(SI_track_df,NI_track_df), alpha = .5, color="black") +
    
    geom_path(aes(x = vis_lon, y = lat, group=name, colour = Time), 
              data = NP_subset) +
    
    #guides(fill=guide_legend(title="Time"))+
    #theme(legend.position = "none")+
    
    labs(title = "Global Cyclone Tracks (1842-2021)",x = "Longitude",y = "Latitude") +
    theme(plot.title = element_text(hjust = 0.5),plot.margin = margin(0.8, 1.2, 0.6, 1.2))+
    scale_colour_gradient(low = "red", high = "yellow", na.value = NA,
                          breaks = seq(0, 1, by = 0.2), labels = seq(0.0, 1.0, by = 0.2),
                          limits = c(0, 1))

pdf("./figures/Atlantic_cyclone.pdf", width = 7, height = 4)
worldmap <- map_data ("world", wrap = c(0, 360))
ggplot(aes(x = long, y = lat), data = worldmap) + 
    geom_path(aes(group = group), 
              #fill = "#f9f9f9", 
              colour = "grey65") + 
    scale_y_continuous(limits = c(0, 70)) +
    scale_x_continuous(limits = c(235, 350)) +
    coord_equal() +  theme_bw() +
    geom_path(aes(x = cal_lon, y = lat, group = id, colour = Time),
              data = NP_subset) + 
    labs(fill = "Time") +
    #guides(fill = guide_legend(title = "Time")) + 
    #theme(legend.position = "none") +
    
    labs(title = expression(bold("(b) North Atlantic Cyclone Tracks")), x = "Longitude",y = "Latitude") +
    theme(plot.title = element_text(hjust = 0.5, size = 15), 
          axis.text = element_text(size = 14), 
          axis.title = element_text(size = 13),
          plot.margin = margin(1, 1.2, 0.5, 1)) + 
    scale_colour_gradient(low = "red",high = "yellow", na.value = NA,
                          breaks = seq(0, 1, by = 0.2), labels = seq(0.0, 1.0, by = 0.2),
                          limits = c(0, 1))
dev.off()


################################ clustering in west pacific cyclone tracks
distance_NP <- distance_global[NP_index, NP_index]
save(distance_NP, file = "distance_NP.RData")
#theta <- find_optimal_theta(distance_NP_index, NULL, 0.05,  0.75, "silh")
theta_candidate <- seq(0.01, 0.30, by = 0.01)
clust_dataframe <- t(sapply(theta_candidate, function(theta) {
    clust_result <- clust_theta(distance_NP, NULL, 0.05, theta, 0.87)
}))

theta <- theta_candidate[clust_dataframe[, 2] == max(clust_dataframe[, 2])]
neighbours_NP <- findneighbours(distance_NP, theta)
save(neighbours_NP, file = "neighbours_NP.RData")

cluster_NP <- clustering_warping(neighbours_NP)  
clust_NP <- include_isolatepoints(cluster_NP[[2]], minprop = 0.05,
                                           distance_NP, alpha_c = 0.87)
save(clust_NP, file = "clust_NP.RData")


### If we use 