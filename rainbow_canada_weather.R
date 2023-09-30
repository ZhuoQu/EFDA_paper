packages = c("ggmap", "rainbow",
             "ggplot2", "fda", "ddalpha")

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

data("CanadianWeather")
### Try MSBD but the visualization effect is not good
list_data <- vector("list", length = 35)
for (l in 1:35) {
    list_data[[l]]$args <- 1:12
    list_data[[l]]$vals <- cbind(CanadianWeather$monthlyTemp[, l],
                                 CanadianWeather$monthlyPrecip[, l])
}
d <- depthf.simplicialBand(objectsf = list_data, dataf = list_data)
order_decrease <- order(d, decreasing = TRUE)
# par(mfrow = c(1, 3), mai = c(0.5, 0.55, 0.3, 0.07), 
#     mar = c(3.5, 3.5, 2, 1), mgp = c(2, 1, 0))
# color_palette <- c("red", "orange", "yellow", "green", "cyan", "blue", "purple")
# CanadianWeather$coordinates
# plot(CanadianWeather$coordinates[, 2], CanadianWeather$coordinates[, 1], type = "n")
# lapply(1:length(order_decrease), function(l) {
#     points(CanadianWeather$coordinates[order_decrease[l], 2], 
#            CanadianWeather$coordinates[order_decrease[l], 1],
#            col = color_palette[ceiling(l / 5)], pch = 19)
#     # text(CanadianWeather$coordinates[order_decrease[l], 2], 
#     #      CanadianWeather$coordinates[order_decrease[l], 1], 
#     #      labels = order_decrease[l])
# })
# plot(1:12, CanadianWeather$monthlyTemp[, 1], type = "n", 
#      ylim = range(CanadianWeather$monthlyTemp, na.rm = TRUE), ylab ="Temperature (\u00B0C)",
#      xlab = "Months", main = "(c) Monthly Temperature Curves in Canada", cex.main = 1)
# lapply(1:length(order_decrease), function(k) {
#     lines(1:12, CanadianWeather$monthlyTemp[, order_decrease[k]], 
#       col = color_palette[ceiling(k / 5)])
#     # text(12, CanadianWeather$monthlyTemp[12, order_decrease[k]], 
#     #      labels = order_decrease[k])
# })
# # lapply(1:length(order_decrease), function(k) {
# #   points(1:12, CanadianWeather$monthlyTemp[, order_decrease[k]], 
# #          cex = 0.7)
# #     text(12, CanadianWeather$monthlyTemp[12, order_decrease[k]], 
# #          labels = order_decrease[k])
# # })
# 
# plot(1:12, CanadianWeather$monthlyPrecip[, 1], type = "n", 
#      ylim = range(CanadianWeather$monthlyPrecip, na.rm = TRUE), ylab ="Precipitation (mm)",
#      xlab = "Months", main = "(c) Monthly Precipitation Curves in Canada", cex.main = 1)
# lapply(1:length(order_decrease), function(k) {
#   lines(1:12, CanadianWeather$monthlyPrecip[, order_decrease[k]],
#         col = color_palette[ceiling(k / 5)]
#         )
#     # text(12, CanadianWeather$monthlyPrecip[12, order_decrease[k]], 
#     #      labels = order_decrease[k])
# })
# # lapply(order_decrease, function(k) {
# #   points(1:12, CanadianWeather$monthlyPrecip[, k], cex = 0.7)
# # })


#### now let's apply MBD on temperature data
##extremed_temp <- extremal_depth(t(CanadianWeather$monthlyTemp))
##order_decrease <- order(extremed_temp, decreasing = TRUE)
color_palette <- c("red", "orange", "yellow", "green", "cyan", "blue", "purple")
CanadianWeather$coordinates
pdf(file = "./figures/canada_weather.pdf", width = 8, height = 4)
# plot(CanadianWeather$coordinates[, 2], CanadianWeather$coordinates[, 1], type = "n")
# lapply(1:length(order_decrease), function(l) {
#     points(CanadianWeather$coordinates[order_decrease[l], 2], 
#            CanadianWeather$coordinates[order_decrease[l], 1],
#            col = color_palette[ceiling(l / 5)], pch = 19)
#     text(CanadianWeather$coordinates[order_decrease[l], 2], 
#          CanadianWeather$coordinates[order_decrease[l], 1], 
#          labels = order_decrease[l])
# })
par(mfrow = c(1, 2), mai = c(0.5, 0.55, 0.3, 0.07), 
    mar = c(3.5, 3.5, 2, 1), mgp = c(2, 1, 0))
# plot(CanadianWeather$coordinates[, 2], CanadianWeather$coordinates[, 1], type = "n")
# lapply(1:length(order_decrease), function(l) {
#     points(CanadianWeather$coordinates[order_decrease[l], 2], 
#            CanadianWeather$coordinates[order_decrease[l], 1],
#            col = color_palette[ceiling(l / 5)], pch = 19)
#     # text(CanadianWeather$coordinates[order_decrease[l], 2], 
#     #      CanadianWeather$coordinates[order_decrease[l], 1], 
#     #      labels = order_decrease[l])
# })
plot(1:12, CanadianWeather$monthlyTemp[, 1], type = "n", 
     ylim = range(CanadianWeather$monthlyTemp, na.rm = TRUE), ylab ="Temperature (\u00B0C)",
     xlab = "Months", main = "(c) Monthly Temperature Curves in Canada", cex.main = 1)
lapply(1:length(order_decrease), function(k) {
    lines(1:12, CanadianWeather$monthlyTemp[, order_decrease[k]], 
          col = color_palette[ceiling(k / 5)])
    # text(12, CanadianWeather$monthlyTemp[12, order_decrease[k]], 
    #      labels = order_decrease[k])
})
# lapply(1:length(order_decrease), function(k) {
#   points(1:12, CanadianWeather$monthlyTemp[, order_decrease[k]], 
#          cex = 0.7)
#     text(12, CanadianWeather$monthlyTemp[12, order_decrease[k]], 
#          labels = order_decrease[k])
# })

plot(1:12, CanadianWeather$monthlyPrecip[, 1], type = "n", 
     ylim = range(CanadianWeather$monthlyPrecip, na.rm = TRUE), 
     ylab = "Precipitation (mm)",
     xlab = "Months", main = "(c) Monthly Precipitation Curves in Canada", cex.main = 1)
lapply(1:length(order_decrease), function(k) {
    lines(1:12, CanadianWeather$monthlyPrecip[, order_decrease[k]],
          col = color_palette[ceiling(k / 5)]
    )
    # text(12, CanadianWeather$monthlyPrecip[12, order_decrease[k]], 
    #      labels = order_decrease[k])
})
dev.off()
