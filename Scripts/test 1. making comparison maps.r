#making comparison maps

#grab data sets to plot
soil.loc   <- read.csv('FIA_soils/SOILS_SAMPLE_LOC.CSV') #all soils within the soil location file from FIA. 7880 unique sites.
all.soils  <- read.csv('FIA_soils/SOILS_LAB.CSV') #all soils with chemistry data. 5403 unique sites.
soils.built<- read.csv('FIA_soils/FIAsoil_output_CA.csv') #3451 soils allowed for complete construction of C and N layers to 30cm depth. 
soils.pass <- read.csv('analysis_data/Trees.Soils.csv')#all soils that pass through FIA filtering. 2725 unqiue sites.

#rename some things in soils.pass
colnames(soils.pass)[which(names(soils.pass) == c('latitude','longitude'))] <- c('LAT','LON')

#load plot table from FIA to get lat/long
PLOT <- readRDS('analysis_data/FIA_plot_table.rds')

#grab lat long for sites that don't have it
soil.loc    <- merge(soil.loc   ,PLOT, by="PLT_CN") #7542 sites pass. okay. 
all.soils   <- merge(all.soils  ,PLOT, by="PLT_CN") #all 5403 unique sites pass.
soils.built <- merge(soils.built,PLOT, by="PLT_CN") #all 3451 unique sites pass. 

#kill redundant observations within soil.loc and all.soils. This just takes the first line of data of each unique PLT_CN value.
soil.loc  <-  soil.loc[!duplicated( soil.loc$PLT_CN),]
all.soils <- all.soils[!duplicated(all.soils$PLT_CN),]

#subset to only include the columns you like- lat and long and plt_cn
soil.loc    <-    soil.loc[c("PLT_CN","LAT","LON")]
all.soils   <-   all.soils[c("PLT_CN","LAT","LON")]
soils.built <- soils.built[c("PLT_CN","LAT","LON")]
soils.pass  <-  soils.pass[c("PLT_CN","LAT","LON")]

#okay now you have 3 data sets. start plotting these fuckers I think. 
#or merge all 3 together, create vectors indicating what is where. 
col.1<- 'green'
col.2<- 'pink'

#comparing which data have chemistry vs. have a sampled location. 
map.data <- soil.loc
map.data$loc.vs.chem <- ifelse(map.data$PLT_CN %in% all.soils$PLT_CN,col.1,col.2)


require(ggplot2)
require(grid)

#make map - green points are in both data sets, pink points only in the soil.loc table.
#so, the dsitinction between the soil.loc and all sites w/ chemistry does not seem to be whats killing us.
mp <- NULL
#grab world map and choose colors
mapWorld <- borders("usa", colour='white',fill='black', lwd=0.4)
mp <- ggplot() +   mapWorld
#add points to map
mp <- mp+ geom_point(aes(x=map.data$LON, y=map.data$LAT) ,color=map.data$loc.vs.chem, size=1) 
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

#Now compare all soils with chemistry to all soil profiles that made it through calculation on an aerial basis. 
map.data <- all.soils
map.data$chem.vs.aerial <- ifelse(map.data$PLT_CN %in% soils.built$PLT_CN,col.1,col.2)

#make map. green points are in both data sets, pink points didn't make it through aerial calculation
#THIS IS IT. Something about the calculation to an aerial basis is killing the entire west coast.
mp <- NULL
#grab world map and choose colors
mapWorld <- borders("usa", colour='white',fill='black', lwd=0.4)
mp <- ggplot() +   mapWorld
#add points to map
mp <- mp+ geom_point(aes(x=map.data$LON, y=map.data$LAT) ,color=map.data$chem.vs.aerial, size=1) 
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


#All aerial calculated profiles that did not make it through FIA filtering are in pink.
map.data <- soils.built
map.data$post.filter <- ifelse(map.data$PLT_CN %in% soils.pass$PLT_CN,col.1,col.2)


#this seems acceptable. no strong regional bias.
mp <- NULL
#grab world map and choose colors
mapWorld <- borders("usa", colour='white',fill='black', lwd=0.4)
mp <- ggplot() +   mapWorld
#add points to map
mp <- mp+ geom_point(aes(x=map.data$LON, y=map.data$LAT) ,color=map.data$post.filter, size=1) 
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