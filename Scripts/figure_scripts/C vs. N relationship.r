#relationship between C vs. N in data set. 
###FIA Figure for AGU###
d <- read.csv("analysis_data/Trees.Soils.csv")
require(car)

d$rel.EM<- d$DIA.EM / d$DIA.TOT
d$storage.CN <- d$C.storage / d$N.storage
#exclude some observations with C:N of storage less than 9 or greater than 80
d<- subset(d,d$storage.CN>9)
d<- subset(d,d$storage.CN<80)


#important: pick colors from wes anderson color palette.
require(wesanderson)
col.line<- wes_palette("Zissou",5)[5]
col.text<- 'black'
col.point<- wes_palette("Zissou",5)[1]
col.background <- 'white'
col.axis <- 'black'

png(filename='Figures/C.vs.N.png',width=10,height=10,units='in',res=300)

par(bg=col.background)
par(mar=c(5,5,5,5))
plot(log(C.storage)~log(N.storage),data=d,col=col.point, pch=16, cex=0.4,axes=F,xlab=NA,ylab=NA)
title(xlab='ln(N.storage)',col.lab=col.text, cex.lab=2)
title(ylab='ln(C.storage)',col.lab=col.text, cex.lab=2)
abline(lm(log(C.storage)~log(N.storage),data=d),lwd=3,col=col.line)
box(col=col.text,lwd=1.5)
axis(1,col.axis=col.axis,col=col.axis,col.lab=col.axis,col.ticks=col.axis,cex.axis=1.4)
axis(2,col.axis=col.axis,col=col.axis,col.lab=col.axis,col.ticks=col.axis,cex.axis=1.4)

dev.off()