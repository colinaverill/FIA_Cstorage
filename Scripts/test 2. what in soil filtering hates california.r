#figuring out what is killing all the west coast observations in the script that builds the soil profiles on an aerial basis. 
#kill scientific notation, otherwise it collapses the PLT_CN values when you try to look at them. 
options(scipen=999)

#counting function to count the number of unique sites in any dataset by the PLT_CN key. 
ct <- function(x) {out<-length(unique(x$PLT_CN))
                   return(out)}

#load in soil chemical and location data
soil.chem.FIA <- read.csv('FIA_soils/SOILS_LAB.CSV') #5403 unique sites here.
soil.loc  <- read.csv('FIA_soils/SOILS_SAMPLE_LOC.CSV') #7880 unique sites in here. 
#isolate variables of interest
soil.chem<- soil.chem.FIA[,c('PLT_CN','LAYER_TYPE','FIELD_MOIST_SOIL_WT','AIR_DRY_SOIL_WT',"OVEN_DRY_SOIL_WT","FIELD_MOIST_WATER_CONTENT_PCT","RESIDUAL_WATER_CONTENT_PCT","TOTAL_WATER_CONTENT_PCT","BULK_DENSITY","COARSE_FRACTION_PCT","C_ORG_PCT","C_INORG_PCT","C_TOTAL_PCT","N_TOTAL_PCT","PH_H2O","PH_CACL2","EXCHNG_NA","EXCHNG_K","EXCHNG_MG","EXCHNG_CA","EXCHNG_AL","ECEC","EXCHNG_MN","EXCHNG_FE","EXCHNG_NI","EXCHNG_CU","EXCHNG_ZN","EXCHNG_CD","EXCHNG_PB","EXCHNG_S","BRAY1_P","OLSEN_P")]

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

#impute missing bulk density for mineral 1 and 2 and organic 1 and 2 layers
test<- soil.chem[soil.chem$LAYER_TYPE %in% c('MIN_1','MIN_2','ORG_1','ORG_2'),]
test2<- soil.chem[soil.chem$LAYER_TYPE %in% c('FF_TOTAL','L_ORG'),]
#calculate BD for relevant soil layers
test$BULK_DENSITY<- ifelse(is.na(test$BULK_DENSITY)==T,test$OVEN_DRY_SOIL_WT/181,test$BULK_DENSITY)
#merge all all layers back together as soil.chem
soil.chem<-rbind(test,test2)
soil.chem<- soil.chem[order(soil.chem$PLT_CN),] #order data set by PLT_CN


#####REAL TESTING BEGINS HERE####


#First hunch: there are many forest floor depth observations missing, this is what geographically biases us, as they are not included in the final subset.

# many missing depth observations in FF layer, but they have total mass and bulk density. If you know the sampling frame area (the bike tire), you can calculate
#check fit between calcualted depth and observed depth in data set for the FF. 
#pretty bad! It falls on multiple lines.  different sized sampling frames? You cannot use the bulk + total weight data to back out depth.
what<- subset(soil.chem.FIA,soil.chem.FIA$LAYER_TYPE=='FF_TOTAL')
what$depth.calc<- what$OVEN_DRY_SOIL_WT / (30.54*pi * what$BULK_DENSITY)
what<- merge(what,depth.mean, by="PLT_CN") 
plot(FF_DEPTH ~ depth.calc, data=what, pch=16, cex=0.2)
abline(0,1,lwd=3,lty=2,col='purple')


#check if the missing depth observations are because they hav a missing average value (the FORFLTHK), but have the N/S/E/W values (FORFLTHKN, etc.)
#isolate observations that have missing FF_DEPTH observations
test <- subset(soil.chem, soil.chem$LAYER_TYPE=='FF_TOTAL')
test <- subset(test, is.na(test$FF_DEPTH))
#grab the observations from the soil.loc table for FORFLTHK and the FORFLTHK N/S/E/W observations, append to this table. 
#average N/S/E/W observations from sol.loc
soil.loc$FF_AVG <- rowMeans(soil.loc[,c("FORFLTHKN","FORFLTHKS","FORFLTHKE","FORFLTHKW")],na.rm=T)
soil.loc$FF_AVG <- soil.loc$FF_AVG*2.54 #convert to centimeters
#count number of missing observations saved.
sum(is.na(soil.loc$FF_DEPTH))
sum(is.na(soil.loc$FF_AVG))
#saves 2 observations. woof.
test<- merge(test,soil.loc[,c('FF_AVG')],by='PLT_CN')

#Is it possible that the missing FF_DEPTH observations are due to the fact that it was OH all the way down?
#these would be profiles that just have ORG1 + ORG2. 
test2<- soil.chem[soil.chem$PLT_CN %in% test$PLT_CN,]
#nope. 

#make a map to ensure these missing FF_DEPTH observations are really the california observations.
#should have done this first.
#grab plot table to extract lat/lon for these sites
PLOT <- readRDS('analysis_data/FIA_plot_table.rds')
test.map <- merge(test   ,PLOT, by="PLT_CN")
require(grid)
require(ggplot2)

mp <- NULL
#grab world map and choose colors
mapWorld <- borders("usa", colour='white',fill='black', lwd=0.4)
mp <- ggplot() +   mapWorld
#add points to map
mp <- mp+ geom_point(aes(x=test.map$LON, y=test.map$LAT) ,color='green', size=1) 
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


#okay, so the missing FF depth does kill 945 observations, but they are not the california observations.
#I would like to kill this problem, but its not my real problem that causes geographic bias and kills california. 


###2ND HUNCH###
#The other filtering set that destroyed me was removing all sites with missing BD observations.
#first make map
test.map <- soil.chem[is.na(soil.chem$BULK_DENSITY),] #this is 1055 sites. 
test.map <- merge(test.map   ,PLOT, by="PLT_CN")
mp <- NULL
#grab world map and choose colors
mapWorld <- borders("usa", colour='white',fill='black', lwd=0.4)
mp <- ggplot() +   mapWorld
#add points to map
mp <- mp+ geom_point(aes(x=test.map$LON, y=test.map$LAT) ,color='green', size=1) 
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

#OKAY. YEAH. THOSE WEST COAST FUCKERS ARE TOO LAID BACK TO REPORT BULK DENSITY I GUESS.
(sum(!is.na(test.map$OVEN_DRY_SOIL_WT)))
#okay, 1003/1064 report oven dry soil weight, 937 of them have FF_DEPTH. If we knew sampling frame size we could calculate BD. 
#we don't. we've tried this before. Multiple sizes of sampling frame must have been used. This is why you can back out BD for MS1/2 and ORG1/2 horizons but not forest floor.
#infuriating. Sent an email to FIA to see if they know anything I don't. 