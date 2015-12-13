d<- readRDS('analysis_data/Trees.Soils.rds')
d<- read.csv('analysis_data/soilC.stock.Trees.Soils')
#calcualte EM relative abundance
d$relEM<- d$DIA.EM / (d$DIA.EM + d$DIA.AM + d$DIA.EITHER + d$DIA.OTHER)
d$cn<- d$C.storage/d$N.storage

#negative effect of EM relative abundance on total C storage
m1<- lm(C.storage~relEM,data=d)
plot(residuals(m1)~fitted(m1))
plot(C.storage~relEM,data=d)
summary(m1)


#negative interactive effect- log transformed N values are all negative thats why. 
m1<- lm(log(C.storage) ~ log(N.storage)*relEM+ pH_H2O + mat + map,data=subset(d,d$cn<80))
plot(residuals(m1)~fitted(m1))
summary(m1)
plot(log(C.storage)~log(N.storage),data=d)

#positive effect on CN.
#note. MAT not significant, only significant if EM removed. Including EM ups r2 from 0.22 to 0.29, and makes MAT insignificant
#vif does not say these are overly confounded
m2<- lm(cn~relEM+pH_H2O+mat+map,data=subset(d,d$cn<80))
plot(residuals(m2)~fitted(m2))
summary(m2)
plot(cn~relEM,data=subset(d,d$cn<80))
require(car)
vif(m2)
plot(pH_H2O~relEM,data=d)
plot(relEM~mat,data=d)
summary(lm(relEM~mat,data=d)) #mat and EM not super correlated. R2 = 0.03. 
summary(lm(pH_H2O~relEM,data=d)) #all EM only drops pH about half a unit. 

d$resid<- residuals(lm(log(C.storage)~log(N.storage),data=d))
plot(resid~mat,data=d)
plot(resid~map,data=d)
plot(resid~pH_H2O,data=d)
abline(lm((d$resid~d$pH_H2O)),lty=2,lwd=3,col='purple')
plot(resid~relEM,data=d)
plot(resid~interaction,data=d)

#run same analysis of C vs. P storage in mineral soil
#no EM effect. This shit is not an artifact. We'd expect if this was driven by chemistry that we would observe a similar relationship. 
dP<- subset(d,d$m.BrayP.storage<100000)
plot(log(m.C.storage)~log(m.BrayP.storage),data=dP)
m1<-lm(log(m.C.storage)~log(m.BrayP.storage)*relEM + pH_H2O + mat + map, data=dP)
plot(residuals(m1)~fitted(m1))
vif(m1)
summary(m1)


#modeling residuals of C vs N log transformed relationship
d$interaction<- d$relEM * log(d$N.storage)
m1<-lm(resid~relEM*log(N.storage) + pH_H2O + mat + map, data=d)
m1<- lm(resid ~ interaction + pH_H2O + mat + map,data=d)
qqnorm(residuals(m1))
plot(residuals(m1)~fitted(m1))
vif(m1)
summary(m1)
summary(lm(resid~pH_H2O,data=d))
summary(lm(resid~relEM,data=d))
summary(lm(resid~mat,data=d))
summary(lm(resid~map,data=d))
summary(lm(resid~interaction,data=d))

dat<- data.frame(d$C.storage,d$N.storage,d$resid,d$pH_H2O,d$relEM,d$mat,d$map)
dat<-na.omit(dat)
colnames(dat) <- c('C.storage','N.storage','resid','pH_H2O','relEM','mat','map')
m1<- lm(resid~relEM*log(N.storage) + pH_H2O + mat + map, data=dat)
dat$fitted <- fitted(m1)
plot(dat$resid~dat$fitted, pch=16, cex=0.6)
abline(lm(dat$resid~dat$fitted),lwd=3, col='red')
mtext('R2 = 0.45',side=3,line=-2,at=-0.75,cex=2)

#make a map of where sites are
require(ggplot2)
library(grid)
require(wesanderson)

#make some colors for the points that scale with soil C concentration
rbPal<- colorRampPalette(wes_palette(21, name = "Zissou", type = "continuous"))
d$col<- rbPal(50)[as.numeric(cut(log(d$C.storage),breaks=21))]
d$col<- wes_palette(3,name="GrandBudapest")[2]

#make map
mp <- NULL
#grab world map and choose colors
mapWorld <- borders("usa", colour=wes_palette(1,name="GrandBudapest"),fill='black', lwd=0.4)
mp <- ggplot() +   mapWorld
#add points to map
mp <- mp+ geom_point(aes(x=d$longitude, y=d$latitude) ,color=d$col, size=1.6) 
#change up bacground gridlines, labels and colors.
mp<- mp + theme(axis.text.y=element_blank(),
                axis.text.x=element_blank(),
                axis.title.y=element_blank(),
                axis.title.x=element_blank(),
                axis.ticks=element_blank(), 
                panel.background = element_rect(fill='black'),
                plot.background = element_rect(fill='black'),
                panel.grid.major=element_blank(),
                panel.grid.minor=element_blank())
mp
