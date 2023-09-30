#### North Atlantic Cyclone clustering based on k-means clustering
whole_track_df <- read_csv('whole_track_cyclone.csv')
source("clust_hierarchical_distance.R")
load("distance_NP.RData") ## distance_NP
load("neighbours_NP.RData") ## neighbours_NP
load("NP_index.RData")  ## NP_index
hierarchical_NP <- clust_hierarchical_distance(distance_NP, n = nrow(distance_NP))

worldmap <- map_data ("world", wrap = c(0, 360))
############################################# figures of clustering result
for (clustl in 1:2){
  
  loc <- ifelse(clustl == 1, "(c)", "(d)")
  index <- which(hierarchical_NP$Best.partition == 3 - clustl)
  neighbour_number <- apply(neighbours_NP[index, index], 2, sum)
  median <- index[which(neighbour_number == max(neighbour_number))][1]
  firstquarter <- index[order(neighbour_number, decreasing = T)[1:round(0.25 * (length(neighbour_number)))]]
  secondquarter <- index[order(neighbour_number, decreasing = T)[round(0.25 * (length(neighbour_number))+1):round(0.5 * (length(neighbour_number)))] ]
  thirdquarter <- index[order(neighbour_number, decreasing = T)[round(0.5 * (length(neighbour_number)) + 1):round(0.75 * (length(neighbour_number)))] ]
  # shift coordinates to recenter worldmap
  pdf(file = paste("./figures/clust_hierarchical_", clustl, ".pdf", sep = ""), width = 6, height = 4)
  ggplot(aes(x = long, y = lat), data = worldmap) + 
    geom_path(aes(group = group), 
              #fill = "#f9f9f9", 
              colour = "grey65") + 
    scale_y_continuous(limits = c(0, 65)) +
    scale_x_continuous(limits = c(235, 350)) +
    coord_equal() +  theme_bw() +
    geom_path(aes(x = cal_lon, y = lat, group = id, alpha = 1 - time),
              data = subset(whole_track_df, id %in% NP_index[thirdquarter]), color = "pink") +
    geom_path(aes(x = cal_lon, y = lat, group = id, alpha = 1 - time),
              data = subset(whole_track_df, id %in% NP_index[secondquarter]), color = "magenta") +
    geom_path(aes(x = cal_lon, y = lat, group = id, alpha = 1 - time),
              data = subset(whole_track_df, id %in% NP_index[firstquarter]), color = "purple") +
    #geom_path(aes(x = cal_lon, y = lat, group = id, alpha = 1 - time), 
    #          data = subset(whole_track_df, id %in% NP_index[outlier_index]), color = "red") +
    geom_path(aes(x = cal_lon, y = lat, group = id, alpha = 1 - time),
              data = subset(whole_track_df, id %in% NP_index[median], col = grey(time)), color = "black") + 
    
    #guides(fill=guide_legend(title="Time"))+
    theme(legend.position = "none") +
    
    labs(title = paste(loc," Cluster ", clustl, " (hierarchical clustering)", sep = ""),
         x = "Longitude", y = "Latitude") +
    theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"), 
          axis.text = element_text(size = 14), 
          axis.title = element_text(size = 13),
          plot.margin = margin(1, 1.2, 1, 1.2))
  #scale_colour_gradient(low="red",high="yellow",na.value=NA)
  dev.off()
}
