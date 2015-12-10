#kill scientific notation
options(scipen=999)

#load in soil chemical and location data
#FF references to litter + humus layer. MIN_1 is 0-10cm, MIN_2 is 10-20cm
#FF has bulk density, total C, total N and soil moisture measurements in the chemistry table. Has 4 measurements of depth (North, South, East, West) in location table.
#MS has everything from FF plus soil P, pH, micronutrients, and inorganic C (if pH > 7.5)
soil.chem.FIA <- read.csv('FIA_soils/SOILS_LAB.CSV') #5403 unique sites here.
soil.loc  <- read.csv('FIA_soils/SOILS_SAMPLE_LOC.CSV') #7880 unique sites in here. 

#isolate variables of interest
soil.chem<- soil.chem.FIA[,c('PLT_CN','LAYER_TYPE','FIELD_MOIST_SOIL_WT','AIR_DRY_SOIL_WT',"OVEN_DRY_SOIL_WT","FIELD_MOIST_WATER_CONTENT_PCT","RESIDUAL_WATER_CONTENT_PCT","TOTAL_WATER_CONTENT_PCT","BULK_DENSITY","COARSE_FRACTION_PCT","C_ORG_PCT","C_INORG_PCT","C_TOTAL_PCT","N_TOTAL_PCT","PH_H2O","PH_CACL2","EXCHNG_NA","EXCHNG_K","EXCHNG_MG","EXCHNG_CA","EXCHNG_AL","ECEC","EXCHNG_MN","EXCHNG_FE","EXCHNG_NI","EXCHNG_CU","EXCHNG_ZN","EXCHNG_CD","EXCHNG_PB","EXCHNG_S","BRAY1_P","OLSEN_P")]

#first "dig out" (haha) the FF data to calculate soil C and N stocks in the organic layer.
#soils have two different layering structures:
#1. FF_TOTAL / MIN_1 / MIN_2 <- organic horizon and mineral soil to 20cm depth. 
#2. L_ORG / ORG_1 / ORG_2 <- organic horizon litter layer and then the soil organic horizon to 20cm depth. 

#averaging values within a soil layer by plot
#CA double checked how many rows should be there before/after. There are a few duplicate mineral and organic soils in addition to the expected multiple observations of FF_TOTAL. 
soil.chem<- aggregate(.~ PLT_CN  + LAYER_TYPE,data=soil.chem,FUN=mean,na.action=na.pass)
soil.chem<- soil.chem[order(soil.chem$PLT_CN),] #5403 unique plots

#get FF depth from soil.loc table- 7880 unique plots in here. Convert from inches to centimeters
#we are losing 2 observations from soil.chem on the merge. 
soil.loc$FF_DEPTH <- soil.loc$FORFLTHK * 2.54
depth.mean<- aggregate(FF_DEPTH~PLT_CN, data=soil.loc,FUN=mean,na.action=na.pass) #this is aggregating depth observations by PLT_CN
soil.chem<- merge(soil.chem,depth.mean, by="PLT_CN") #5401 unique sites. 


#get a vector in the data set that is just depth- 
soil.chem$DEPTH <- NA
soil.chem$DEPTH <- ifelse(soil.chem$LAYER_TYPE=='FF_TOTAL',soil.chem$FF_DEPTH,10)
soil.chem$DEPTH <- ifelse(soil.chem$LAYER_TYPE=='L_ORG'   ,soil.chem$FF_DEPTH,soil.chem$DEPTH)


# many missing depth observations in FF layer, but they have total mass and bulk density
#check fit between calcualted depth and observed depth in data set for the FF. 
#pretty bad! why is it falling on multiple lines.  different sized sampling frames? You cannot use the bulk + total weight data to back out depth.
what<- subset(soil.chem.FIA,soil.chem.FIA$LAYER_TYPE=='FF_TOTAL')
what$depth.calc<- what$OVEN_DRY_SOIL_WT / (30.54*pi * what$BULK_DENSITY)
what<- merge(what,depth.mean, by="PLT_CN") 
plot(FF_DEPTH ~ depth.calc, data=what, pch=16, cex=0.5)
abline(0,1,lwd=3,lty=2,col='purple')


#test if you can impute bulk density data from sampling volume information. Works in Mineral horizons.
bd.test <- subset(soil.chem,soil.chem$LAYER_TYPE=='MIN_1')
bd.test$bd.calc <- bd.test$OVEN_DRY_SOIL_WT / (181)
plot(BULK_DENSITY~bd.calc, data=bd.test, pch=16, cex=0.5)
abline(0,1,col='purple',lwd=3,lty=2)

#test bulk density caclculation with FF_TOTAL observations-
#doesn't work for organic layer, likely for the same reason as depth. Clearly the sampling frames vary in area. 
bd.test2<-subset(soil.chem,soil.chem$LAYER_TYPE=='FF_TOTAL')
bd.test2$bd.calc<- bd.test2$OVEN_DRY_SOIL_WT / ((15.24^2)*pi * bd.test2$DEPTH)
plot(BULK_DENSITY~bd.calc, data=bd.test2, pch=16, cex=0.5)
abline(0,1,col='purple',lwd=3,lty=2)

#Does work for ORG_1 and ORG_2 horizons. These were sampled with the BD sampler. 
bd.test3<-subset(soil.chem,soil.chem$LAYER_TYPE=='ORG_1')
bd.test3$bd.calc<- bd.test3$OVEN_DRY_SOIL_WT / 181
plot(BULK_DENSITY~bd.calc, data=bd.test3, pch=16, cex=0.5)
abline(0,1,col='purple',lwd=3,lty=2)

#based on these comparions I will calculate BD in MIN_1, MIN_2, ORG_1 and ORG_2 if missing, based on the dry soil mass and a 181cm^3 soil core volume. 
#I will not calculate FF_TOTAL or L_ORG bulk density, as analysis of known data shows a wide spread in volume sampled. 
test<- soil.chem[soil.chem$LAYER_TYPE %in% c('MIN_1','MIN_2','ORG_1','ORG_2'),]
test2<- soil.chem[soil.chem$LAYER_TYPE %in% c('FF_TOTAL','L_ORG'),]
#calculate BD for relevant soil layers
test$BULK_DENSITY<- ifelse(is.na(test$BULK_DENSITY)==T,test$OVEN_DRY_SOIL_WT/181,test$BULK_DENSITY)
#merge all all layers back together as soil.chem
soil.chem<-rbind(test,test2)
soil.chem<- soil.chem[order(soil.chem$PLT_CN),] #order data set by PLT_CN

#kill all observations missing soil C- 5281 sites remaining. 
to.remove <- soil.chem[is.na(soil.chem$C_ORG_PCT),] 
soil.chem<- soil.chem[!soil.chem$PLT_CN %in% to.remove$PLT_CN,]

#kill all observations missing depth information- 4355 unqiue sites remaining. 
to.remove <- soil.chem[is.na(soil.chem$DEPTH),] 
soil.chem<- soil.chem[!soil.chem$PLT_CN %in% to.remove$PLT_CN,]

#kill all observations with missing bulk density information- 3451 unique sites remaining.  
to.remove <- soil.chem[is.na(soil.chem$BULK_DENSITY),] 
soil.chem<- soil.chem[!soil.chem$PLT_CN %in% to.remove$PLT_CN,]

#calculate soil C and N storage in kg C / m2 by horizon
soil.chem$C.storage<- soil.chem$BULK_DENSITY * (soil.chem$C_ORG_PCT/100) * soil.chem$DEPTH * 10000 * (1/1000)
soil.chem$N.storage<- soil.chem$BULK_DENSITY * (soil.chem$N_TOTAL_PCT/100) * soil.chem$DEPTH * 10000 * (1/1000)

#sum across horizons to get a single soil C storage number per site. 
output           <-aggregate(soil.chem$C.storage~soil.chem$PLT_CN,FUN='sum')
colnames(output) <- c('PLT_CN','C.storage')
output$N.storage <- aggregate(soil.chem$N.storage~soil.chem$PLT_CN,FUN='sum',na.action=na.pass)[,2]
output$depth     <- aggregate(soil.chem$DEPTH    ~soil.chem$PLT_CN,FUN='sum',na.action=na.pass)[,2]

#sanity check plot
plot(C.storage~N.storage,data=output, pch=16, cex=0.3)
abline(lm(C.storage~N.storage,data=output),lty=2,lwd=3,col='purple')

#calculate mineral horizon micronutrient storage- mg nutrient / m2
mineral.chem            <- soil.chem[soil.chem$LAYER_TYPE %in% c('MIN_1','MIN_2'),]
mineral.chem$Na.storage <- mineral.chem$EXCHNG_NA * mineral.chem$BULK_DENSITY * mineral.chem$DEPTH * (1/1000) * 10000
mineral.chem$K.storage  <- mineral.chem$EXCHNG_K  * mineral.chem$BULK_DENSITY * mineral.chem$DEPTH * (1/1000) * 10000
mineral.chem$Mg.storage <- mineral.chem$EXCHNG_MG * mineral.chem$BULK_DENSITY * mineral.chem$DEPTH * (1/1000) * 10000
mineral.chem$Ca.storage <- mineral.chem$EXCHNG_CA * mineral.chem$BULK_DENSITY * mineral.chem$DEPTH * (1/1000) * 10000
mineral.chem$Al.storage <- mineral.chem$EXCHNG_AL * mineral.chem$BULK_DENSITY * mineral.chem$DEPTH * (1/1000) * 10000
mineral.chem$Mn.storage <- mineral.chem$EXCHNG_MN * mineral.chem$BULK_DENSITY * mineral.chem$DEPTH * (1/1000) * 10000
mineral.chem$Fe.storage <- mineral.chem$EXCHNG_FE * mineral.chem$BULK_DENSITY * mineral.chem$DEPTH * (1/1000) * 10000
mineral.chem$Ni.storage <- mineral.chem$EXCHNG_NI * mineral.chem$BULK_DENSITY * mineral.chem$DEPTH * (1/1000) * 10000
mineral.chem$Cu.storage <- mineral.chem$EXCHNG_CU * mineral.chem$BULK_DENSITY * mineral.chem$DEPTH * (1/1000) * 10000
mineral.chem$Zn.storage <- mineral.chem$EXCHNG_ZN * mineral.chem$BULK_DENSITY * mineral.chem$DEPTH * (1/1000) * 10000
mineral.chem$Cd.storage <- mineral.chem$EXCHNG_CD * mineral.chem$BULK_DENSITY * mineral.chem$DEPTH * (1/1000) * 10000
mineral.chem$Pb.storage <- mineral.chem$EXCHNG_PB * mineral.chem$BULK_DENSITY * mineral.chem$DEPTH * (1/1000) * 10000
mineral.chem$S.storage  <- mineral.chem$EXCHNG_S  * mineral.chem$BULK_DENSITY * mineral.chem$DEPTH * (1/1000) * 10000
mineral.chem$Bray.P.storage  <- mineral.chem$BRAY1_P * mineral.chem$BULK_DENSITY * mineral.chem$DEPTH * (1/1000) * 10000
mineral.chem$Olsen.P.storage <- mineral.chem$OLSEN_P * mineral.chem$BULK_DENSITY * mineral.chem$DEPTH * (1/1000) * 10000

#aggregate mineral soil C, N and micronutrient storage, as well as depth and average pH. 
m.output              <- aggregate(mineral.chem$C.storage~mineral.chem$PLT_CN,FUN='sum')
colnames(m.output)    <- c('PLT_CN','m.C.storage')
m.output$m.N.storage  <- aggregate(mineral.chem$N.storage~mineral.chem$PLT_CN,FUN='sum',na.action=na.pass)[,2]
m.output$m.depth      <- aggregate(mineral.chem$DEPTH    ~mineral.chem$PLT_CN,FUN='sum',na.action=na.pass)[,2]
m.output$pH_H2O       <- aggregate(mineral.chem$PH_H2O   ~mineral.chem$PLT_CN,FUN='mean',na.action=na.pass)[,2]
m.output$pH_CaCl2     <- aggregate(mineral.chem$PH_CACL2 ~mineral.chem$PLT_CN,FUN='mean',na.action=na.pass)[,2]
m.output$m.Na.storage <- aggregate(mineral.chem$Na.storage~mineral.chem$PLT_CN,FUN='sum',na.action=na.pass)[,2]
m.output$m.K.storage  <- aggregate(mineral.chem$K.storage ~mineral.chem$PLT_CN,FUN='sum',na.action=na.pass)[,2]
m.output$m.Mg.storage <- aggregate(mineral.chem$Mg.storage~mineral.chem$PLT_CN,FUN='sum',na.action=na.pass)[,2]
m.output$m.Ca.storage <- aggregate(mineral.chem$Ca.storage~mineral.chem$PLT_CN,FUN='sum',na.action=na.pass)[,2]
m.output$m.Al.storage <- aggregate(mineral.chem$Al.storage~mineral.chem$PLT_CN,FUN='sum',na.action=na.pass)[,2]
m.output$m.Mn.storage <- aggregate(mineral.chem$Mn.storage~mineral.chem$PLT_CN,FUN='sum',na.action=na.pass)[,2]
m.output$m.Fe.storage <- aggregate(mineral.chem$Fe.storage~mineral.chem$PLT_CN,FUN='sum',na.action=na.pass)[,2]
m.output$m.Ni.storage <- aggregate(mineral.chem$Ni.storage~mineral.chem$PLT_CN,FUN='sum',na.action=na.pass)[,2]
m.output$m.Cu.storage <- aggregate(mineral.chem$Cu.storage~mineral.chem$PLT_CN,FUN='sum',na.action=na.pass)[,2]
m.output$m.Zn.storage <- aggregate(mineral.chem$Zn.storage~mineral.chem$PLT_CN,FUN='sum',na.action=na.pass)[,2]
m.output$m.Cd.storage <- aggregate(mineral.chem$Cd.storage~mineral.chem$PLT_CN,FUN='sum',na.action=na.pass)[,2]
m.output$m.Pb.storage <- aggregate(mineral.chem$Pb.storage~mineral.chem$PLT_CN,FUN='sum',na.action=na.pass)[,2]
m.output$m.S.storage  <- aggregate(mineral.chem$S.storage ~mineral.chem$PLT_CN,FUN='sum',na.action=na.pass)[,2]
m.output$m.BrayP.storage   <- aggregate(mineral.chem$Bray.P.storage  ~mineral.chem$PLT_CN,FUN='sum',na.action=na.pass)[,2]
m.output$m.OlsenP.storage  <- aggregate(mineral.chem$Olsen.P.storage ~mineral.chem$PLT_CN,FUN='sum',na.action=na.pass)[,2]


#merge mineral soil output table (m.output) with total soil data (output), inserting NAs if there is no data for the mineral soil. 
output <- merge(output,m.output,by='PLT_CN',all=T)

#save output as csv file- 3451 observations make it through, from 5403 initial. 
write.csv(output,'FIA_soils/FIAsoil_output_CA.csv')

###NEXT STEPS####
#5. Pair this to the tree growth data. 