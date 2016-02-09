#calculating effect sizes of different predictors. 
#generate effect size figures
d <- read.csv("analysis_data/Trees.Soils.csv")
require(car)
d$rel.EM<- d$DIA.EM / d$DIA.TOT
d$storage.CN <- d$C.storage / d$N.storage
#exclude some observations with C:N of storage less than 9 or greater than 80
d<- subset(d,d$storage.CN>9)
d<- subset(d,d$storage.CN<80)

#error bar function
errb <- function (x, y, ebl, ebu = ebl, length = 0.08, ...){
  arrows(x, y + ebu, x, y - ebl, angle = 90, code = 3,
         length = length, ...)
}

#run the model
m1<-lm(log(C.storage)~log(N.storage)*rel.EM + mat + map + pH_H2O, data=d)

#5 levels of the range (0%,25%,50%,75%,100%)
ph.range <- c(min(d$pH_H2O,na.rm=T),
              (max(d$pH_H2O,na.rm=T) - min(d$pH_H2O,na.rm=T))*0.25 + min(d$pH_H2O,na.rm=T),
              (max(d$pH_H2O,na.rm=T) - min(d$pH_H2O,na.rm=T))*0.50 + min(d$pH_H2O,na.rm=T),
              (max(d$pH_H2O,na.rm=T) - min(d$pH_H2O,na.rm=T))*0.75 + min(d$pH_H2O,na.rm=T),
              (max(d$pH_H2O,na.rm=T))
)
mat.range<- c(min(d$mat,na.rm=T),
              (max(d$mat,na.rm=T) - min(d$mat,na.rm=T))*0.25 + min(d$mat,na.rm=T),
              (max(d$mat,na.rm=T) - min(d$mat,na.rm=T))*0.50 + min(d$mat,na.rm=T),
              (max(d$mat,na.rm=T) - min(d$mat,na.rm=T))*0.75 + min(d$mat,na.rm=T),
              (max(d$mat,na.rm=T))
)
map.range<- c(min(d$map,na.rm=T),
              (max(d$map,na.rm=T) - min(d$map,na.rm=T))*0.25 + min(d$map,na.rm=T),
              (max(d$map,na.rm=T) - min(d$map,na.rm=T))*0.50 + min(d$map,na.rm=T),
              (max(d$map,na.rm=T) - min(d$map,na.rm=T))*0.75 + min(d$map,na.rm=T),
              (max(d$map,na.rm=T))
)
em.range <- c(0,0.25,0.5,0.75,1)

#calculate effect sizes across range (back transformed to total carbon)
coef  <- coef(m1)
m.N   <- mean(log(d$N.storage),na.rm=T)
m.EM  <- mean(d$rel.EM,na.rm=T)
m.mat <- mean(d$mat,na.rm=T)
m.map <- mean(d$map,na.rm=T)
m.ph  <- mean(d$pH_H2O,na.rm=T)

ph.effect  <- exp(coef[1] + m.N*coef[2] + m.EM*coef[3] + m.mat*coef[4] + m.map*coef[5] + ph.range*coef[6] + m.N*m.EM*coef[7])
mat.effect <- exp(coef[1] + m.N*coef[2] + m.EM*coef[3] + mat.range*coef[4] + m.map*coef[5] + m.ph*coef[6] + m.N*m.EM*coef[7])
map.effect <- exp(coef[1] + m.N*coef[2] + m.EM*coef[3] + m.mat*coef[4] + map.range*coef[5] + m.ph*coef[6] + m.N*m.EM*coef[7])
em.effect  <- exp(coef[1] + m.N*coef[2] + em.range*coef[3] + m.mat*coef[4] + m.map*coef[5] + m.ph*coef[6] + m.N*em.range*coef[7])

#calculate errors of these effect sizes
#uncertainty of any soil C estimate the same for each observation.
#square root of the sum of the squared errors, and then exponentiated
errors <- coef(summary(m1))[,2]
error  <- exp(sqrt(sum((errors)^2)))

#calculate relative effect size.
r.ph.effect  <- ((ph.effect  / ph.effect [5])*100) - 100
r.mat.effect <- ((mat.effect / mat.effect[5])*100) - 100
r.map.effect <- ((map.effect / map.effect[1])*100) - 100
r.em.effect  <- ((em.effect  / em.effect [1])*100) - 100

#calculate relative error
ph.err  <- (error /  ph.effect[5])*100
mat.err <- (error / mat.effect[5])*100
map.err <- (error / map.effect[1])*100
em.err  <- (error /  em.effect[1])*100

labx <- c('0','25%','50%','75%','100%')
position <- c(0.75,1.75,2.5,3.5,4.5)
require(wesanderson)

#wes_palette('GrandBudapest',4)
col1<- wes_palette('Zissou',5)[1]
col2<- wes_palette('Zissou',5)[4]

require(wesanderson)
col.line<- wes_palette("Zissou",5)[5]
col.text<- 'black'
col.point<- wes_palette("Zissou",5)[1]
col.background <- 'white'
col.axis <- 'black'

png(filename='Figures/predictor.effect.sizes.png',width=10,height=10,units='in',res=300)

par(mfrow=c(2,2))
par(bg=col.background)
par(oma=c(1,2,1,0))
#plot 1- pH effects
par(mai=c(1,1,0.4,0.4))
plot<-barplot(r.ph.effect,col=col.point,ylim=c(0,160),names=labx,col.axis=col.axis,cex.axis=1.4,cex.names=1.4)
errb(plot,r.ph.effect,ebl=ph.err,ebu=ph.err,col=col.line,lwd=1.5)
axis(1,col.axis=col.axis,col=col.axis,col.lab=col.axis,cex.axis=1.4,labels=NA,col.ticks=NA)
axis(2,col.axis=col.axis,col=col.axis,col.lab=col.axis,col.ticks=col.axis,cex.axis=1.4,labels=NA)
mtext('pH effect',col=col.text,cex=2,line=-1)

#plot2- EM effects
par(mai=c(1,0,.4,1.4))
plot<-barplot(r.em.effect,col=col.point,ylim=c(0,160),names=labx,col.axis=col.axis,cex.axis=1.4,cex.names=1.4)
errb(plot,r.em.effect,ebl=em.err,ebu=em.err,col=col.line,lwd=1.5)
axis(1,col.axis=col.axis,col=col.axis,col.lab=col.axis,cex.axis=1.4,labels=NA,col.ticks=NA)
axis(2,col.axis=col.axis,col=col.axis,col.lab=col.axis,col.ticks=col.axis,cex.axis=1.4,labels=NA)
mtext('EM effect',col=col.text,cex=2,line=-1)

#plot3- MAT effects
par(mai=c(1,1,0.4,0.4))
plot<-barplot(r.mat.effect,col=col.point,ylim=c(0,160),names=labx,col.axis=col.axis,cex.axis=1.4,cex.names=1.4)
errb(plot,r.mat.effect,ebl=mat.err,ebu=mat.err,col=col.line,lwd=1.5)
axis(1,col.axis=col.axis,col=col.axis,col.lab=col.axis,cex.axis=1.4,labels=NA,col.ticks=NA)
axis(2,col.axis=col.axis,col=col.axis,col.lab=col.axis,col.ticks=col.axis,cex.axis=1.4,labels=NA)
mtext('Temperature effect',col=col.text,cex=2,line=-1)

#plot4- MAP effects
par(mai=c(1,0,.4,1.4))
plot<-barplot(r.map.effect,col=col.point,ylim=c(0,160),names=labx,col.axis=col.axis,cex.axis=1.4,cex.names=1.4)
errb(plot,r.map.effect,ebl=map.err,ebu=map.err,col=col.line,lwd=1.5)
axis(1,col.axis=col.axis,col=col.axis,col.lab=col.axis,cex.axis=1.4,labels=NA,col.ticks=NA)
axis(2,col.axis=col.axis,col=col.axis,col.lab=col.axis,col.ticks=col.axis,cex.axis=1.4,labels=NA)
mtext('Precipitation effect',col=col.text,cex=2,line=-1)

mtext("percentage change in C relative to lowest over range",col=col.text,side=2,outer=T,cex=2,line=-2)

dev.off()