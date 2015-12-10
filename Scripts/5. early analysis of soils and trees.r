d<- readRDS('analysis_data/Trees.Soils.rds')

#calcualte EM relative abundance
d$relEM<- d$DIA.EM / (d$DIA.EM + d$DIA.AM + d$DIA.EITHER + d$DIA.OTHER)

#negative effect of EM relative abundance on total C storage
m1<- lm(C.storage~relEM,data=d)
plot(residuals(m1)~fitted(m1))
plot(C.storage~relEM,data=d)
summary(m1)


#negative interactive effect
m1<- lm(log(C.storage) ~ log(N.storage)*relEM+ pH_H2O,data=subset(d,d$cn<80))
plot(residuals(m1)~fitted(m1))
summary(m1)
plot(log(C.storage)~log(N.storage),data=d)

#positive effect on CN. how can all of this be true. 
m2<- lm(cn~relEM+pH_H2O,data=subset(d,d$cn<80))
plot(residuals(m2)~fitted(m2))
summary(m2)
plot(cn~relEM,data=subset(d,d$cn<80))
require(car)
vif(m2)
plot(pH_H2O~relEM,data=d)
summary(lm(pH_H2O~relEM,data=d)) #all EM only drops pH about half a unit. 
