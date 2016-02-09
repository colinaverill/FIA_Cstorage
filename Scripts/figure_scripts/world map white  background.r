d <- read.csv("analysis_data/Trees.Soils.csv")
d<- subset(d,d$latitude<50)

require(grid)
require(ggplot2)

png(filename='Figures/site.map.png',width=11,height=8,units='in',res=300)

mp <- NULL
#grab world map and choose colors
mapWorld <- borders("usa", colour='black',fill='white', lwd=0.4)
mp <- ggplot() +   mapWorld
#add points to map
mp <- mp+ geom_point(aes(x=d$longitude, y=d$latitude) ,color='dark green', size=2,pch=16) 
#change up bacground gridlines, labels and colors.
mp<- mp + theme(axis.text.y=element_blank(),
                axis.text.x=element_blank(),
                axis.title.y=element_blank(),
                axis.title.x=element_blank(),
                axis.ticks=element_blank(), 
                panel.background = element_rect(fill='white'),
                plot.background = element_rect(fill='white'),
                panel.grid.major=element_blank(),
                panel.grid.minor=element_blank())
mp

dev.off()