data("CanadianWeather")
packages <- c("ggplot2", "reshape2", "grid")
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

temp <- melt(CanadianWeather$monthlyTemp)

p1 <- ggplot(data = temp, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  ylab("Stations") + xlab("Months") +
  labs(title = "(a) Heatmap of Canada Temperature Curves") + 
  theme(plot.title = element_text(face = "bold", 
                                  size = 24, hjust = 0.5), 
        axis.title.x = element_text(size = 19),
        axis.title.y = element_text(size = 19),
        axis.text.x = element_text(size = 19),
        legend.title = element_text(size = 19)) +
  scale_fill_gradient2(low = "white", high = "red", mid = "yellow", 
                       midpoint = 3.96, 
                       limit = range(temp$value), 
                       space = "Lab", name = "Scale") 

precip <- melt(CanadianWeather$monthlyPrecip)

p2 <- ggplot(data = precip, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  ylab("Stations") + xlab("Months") +
  labs(title = "(b) Heatmap of Canada Precipitation Curves") + 
  theme(plot.title = element_text(face = "bold", 
                                  size = 24, hjust = 0.5), 
        axis.title.x = element_text(size = 19),
        axis.title.y = element_text(size = 19),
        axis.text.x = element_text(size = 19),
        legend.title = element_text(size = 19)) +
  scale_fill_gradient2(low = "white", high = "red", mid = "yellow", 
                       na.value = "grey",
                       midpoint = 2, 
                       limit = c(0, 12.3), 
                       space = "Lab", name = "Scale") 
pdf("./figures/heatmap_canada_weather.pdf", height = 12, width = 25)
par(mfrow=c(1, 2), mar = c(2.5, 2.5, 1.5, 0.5), mgp = c(1, 0.3, 0))
plot.new()
vp <- viewport(height = unit(1,"npc"), width = unit(1 / 2, "npc"), 
               just = c("right", "bottom"), x = 1 / 2, y = 0)
print(p1, vp = vp)

plot.new()
vp <- viewport(height = unit(1, "npc"), width = unit(1 / 2, "npc"),
               just = c("right", "bottom"), x = 2/2, y = 0)
print(p2, vp = vp)
dev.off()
