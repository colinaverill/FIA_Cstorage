#Figure 1. Ploto f residuals of C vs. N by environmental drivers.

d <- read.csv("analysis_data/Trees.Soils.csv")

require(car)
require(lmSupport)

#calculate abundance of EM trees (by DBH) and 
d$rel.EM<- d$DIA.EM / d$DIA.TOT
d$EM.basal <- d$BASAL.EM / (d$BASAL.EM + d$BASAL.AM + d$BASAL.EITHER + d$BASAL.OTHER)
d$storage.CN <- d$C.storage / d$N.storage

#exclude some observations with C:N of storage less than 9 or greater than 100
d<- subset(d,d$storage.CN>9)
d<- subset(d,d$storage.CN<100)
d$residuals <- residuals(lm(log(C.storage)~log(N.storage),data=d))

##the model- just for reference. 
#m1<- lm(log(C.storage)~  rel.EM*log(N.storage) + pH_H2O + mat + map, data=d)
#qqnorm(residuals(m1))
#plot(residuals(m1)~fitted(m1))
#vif(m1)
#summary(m1)
#modelEffectSizes(m1)


#important: pick colors from wes anderson color palette.
require(wesanderson)
col.line<- wes_palette("Zissou",5)[5]
col.text<- 'black'
col.point<- wes_palette("Zissou",5)[1]
col.background <- 'white'
col.axis <- 'black'

png(filename='Figures/residuals.vs.predictors.png',width=11,height=10,units='in',res=300)

par(mfrow=c(2,2))
par(bg=col.background)
par(oma=c(1,1,1,0))
par(mai=c(.8,.8,.4,.8))
par(mai=c(1,1,.4,1))
plot(residuals~pH_H2O,data=d,col=col.point, pch=16, cex=0.6,axes=F,xlab=NA,ylab=NA)
title(xlab='pH',col.lab=col.text, cex.lab=2)
abline(lm(residuals~pH_H2O,data=d),lwd=3,col=col.line)
box(col=col.text, lwd=1.5)
axis(1,col.axis=col.axis,col=col.axis,col.lab=col.axis,col.ticks=col.axis,cex.axis=1.4)
axis(2,col.axis=col.axis,col=col.axis,col.lab=col.axis,col.ticks=col.axis,cex.axis=1.4)
r.sq<- round(summary(lm(residuals~pH_H2O,data=d))$r.squared, 2)
lab<- bquote(italic(R)^2 == .(format(r.sq)))
mtext(lab,side=3, col=col.text, cex=1.5, line=-2.1,adj=.95)

#EM
par(mai=c(1,0,.4,2))
plot(residuals~rel.EM,data=d,col=col.point, pch=16, cex=0.6,axes=F,xlab=NA,ylab=NA)
title(xlab='EM relative abundance',col.lab=col.text,cex.lab=2)
abline(lm(residuals~rel.EM,data=d),lwd=3,col=col.line)
box(col=col.text,lwd=1.5)
axis(1,col.axis=col.axis,col=col.axis,col.lab=col.axis,col.ticks=col.axis,cex.axis=1.4)
axis(2,col.axis=col.axis,col=col.axis,col.lab=col.axis,col.ticks=col.axis,cex.axis=1.4)
r.sq<- round(summary(lm(residuals~rel.EM,data=d))$r.squared, 2)
lab<- bquote(italic(R)^2 == .(format(r.sq)))
mtext(lab,side=3, col=col.text, cex=1.5, line=-2.1,adj=.95)

#temperature
par(mai=c(1,1,0.4,1))
plot(residuals~mat,data=d,col=col.point, pch=16, cex=0.6,axes=F,xlab=NA,ylab=NA)
title(xlab='mean annual temperature',col.lab=col.text,cex.lab=2)
abline(lm(residuals~mat,data=d),lwd=3,col=col.line)
box(col=col.text,lwd=1.5)
axis(1,col.axis=col.axis,col=col.axis,col.lab=col.axis,col.ticks=col.axis,cex.axis=1.4)
axis(2,col.axis=col.axis,col=col.axis,col.lab=col.axis,col.ticks=col.axis,cex.axis=1.4)
r.sq<- round(summary(lm(residuals~mat,data=d))$r.squared, 2)
lab<- bquote(italic(R)^2 == .(format(r.sq)))
mtext(lab,side=3, col=col.text, cex=1.5, line=-2.1,adj=.95)

#precipitation
par(mai=c(1,0,0.4,2))
plot(residuals~map,data=d,col=col.point, pch=16, cex=0.6,axes=F,xlab=NA,ylab=NA)
title(xlab='mean annual precipitation',col.lab=col.text,cex.lab=2)
abline(lm(residuals~map,data=d),lwd=3,col=col.line)
box(col=col.text,lwd=1.5)
axis(1,col.axis=col.axis,col=col.axis,col.lab=col.axis,col.ticks=col.axis,cex.axis=1.4)
axis(2,col.axis=col.axis,col=col.axis,col.lab=col.axis,col.ticks=col.axis,cex.axis=1.4)
r.sq<- round(summary(lm(residuals~map,data=d))$r.squared, 2)
lab<- bquote(italic(R)^2 == .(format(r.sq)))
mtext(lab,side=3, col=col.text, cex=1.5, line=-2.1,adj=.95)

mtext("C vs. N residual", side =2,col=col.text,outer=T,cex=2, line=-2)

dev.off()