#this script would go faster if you just killed all the FIA observations that aren't in the soils data set first thing. 
#I think you want all the trees though, for when you analyze just tree distributions. 

#This script uses the output of  "1. FIA Extract and Filter.r", and calculates 
#AM/EM DBH biomass, AM/EM TPA, AM/EM DBH growth,and AM/EM change in stem density
#It saves this information, and then creates a second product that is the subset
#of plots that can be paired with soils data. 

FIA.out <- readRDS('mycFIA.out.rds')
names(FIA.out)[names(FIA.out) == 'MYCO_ASSO.x'] <- 'MYCO_ASSO'

#first calculate diameters of mycorrhizal classes (EM, AM, EITHER, OTHER)
#at the beginning and end of the measurement period
#51% EM, 43% AM, 5% EITHER, <1% OTHER (by number of stems)
FIA.out$PREVDIA.EM     <- ifelse(FIA.out$MYCO_ASSO == 'ECM'   , FIA.out$PREVDIA,0)
FIA.out$DIA.EM         <- ifelse(FIA.out$MYCO_ASSO == 'ECM'   , FIA.out$DIA    ,0)
FIA.out$PREVDIA.AM     <- ifelse(FIA.out$MYCO_ASSO == 'AM'    , FIA.out$PREVDIA,0)
FIA.out$DIA.AM         <- ifelse(FIA.out$MYCO_ASSO == 'AM'    , FIA.out$DIA    ,0)
FIA.out$PREVDIA.EITHER <- ifelse(FIA.out$MYCO_ASSO == 'EITHER', FIA.out$PREVDIA,0)
FIA.out$DIA.EITHER     <- ifelse(FIA.out$MYCO_ASSO == 'EITHER', FIA.out$DIA    ,0)
FIA.out$PREVDIA.OTHER  <- ifelse((FIA.out$PREVDIA.EM + FIA.out$PREVDIA.AM + FIA.out$PREVDIA.EITHER) == 0, FIA.out$PREVDIA,0)
FIA.out$DIA.OTHER      <- ifelse((FIA.out$DIA.EM + FIA.out$DIA.AM + FIA.out$DIA.EITHER) == 0, FIA.out$DIA,0)

#Flag stems as AM, EM, EITHER or OTHER- binary 1 or 0 T/F descriptor. 
FIA.out$PREVSTEM.EM    <- ifelse(FIA.out$PREVDIA.EM     > 0, 1, 0)
FIA.out$STEM.EM        <- ifelse(FIA.out$DIA.EM         > 0, 1, 0)
FIA.out$PREVSTEM.AM    <- ifelse(FIA.out$PREVDIA.AM     > 0, 1, 0)
FIA.out$STEM.AM        <- ifelse(FIA.out$DIA.AM         > 0, 1, 0)
FIA.out$PREVSTEM.EITHER<- ifelse(FIA.out$PREVDIA.EITHER > 0, 1, 0)
FIA.out$STEM.EITHER    <- ifelse(FIA.out$DIA.EITHER     > 0, 1, 0)
FIA.out$PREVSTEM.OTHER <- ifelse(FIA.out$PREVDIA.OTHER  > 0, 1, 0)
FIA.out$STEM.OTHER     <- ifelse(FIA.out$DIA.OTHER      > 0, 1, 0)

#aggregating data- DBH
allTrees.out                <- aggregate(FIA.out$PREVDIA.EM     ~ FIA.out$PLT_CN, FUN='sum',na.rm=T, na.action=na.pass)
colnames(allTrees.out) <- c('PLT_CN','PREVDIA.EM')
allTrees.out$DIA.EM         <- aggregate(FIA.out$DIA.EM         ~ FIA.out$PLT_CN, FUN='sum',na.rm=T, na.action=na.pass)[,2]
allTrees.out$PREVDIA.AM     <- aggregate(FIA.out$PREVDIA.AM     ~ FIA.out$PLT_CN, FUN='sum',na.rm=T, na.action=na.pass)[,2]
allTrees.out$DIA.AM         <- aggregate(FIA.out$DIA.AM         ~ FIA.out$PLT_CN, FUN='sum',na.rm=T, na.action=na.pass)[,2]
allTrees.out$PREVDIA.EITHER <- aggregate(FIA.out$PREVDIA.EITHER ~ FIA.out$PLT_CN, FUN='sum',na.rm=T, na.action=na.pass)[,2]
allTrees.out$DIA.EITHER     <- aggregate(FIA.out$DIA.EITHER     ~ FIA.out$PLT_CN, FUN='sum',na.rm=T, na.action=na.pass)[,2]
allTrees.out$PREVDIA.OTHER  <- aggregate(FIA.out$PREVDIA.OTHER  ~ FIA.out$PLT_CN, FUN='sum',na.rm=T, na.action=na.pass)[,2]
allTrees.out$DIA.OTHER      <- aggregate(FIA.out$DIA.OTHER      ~ FIA.out$PLT_CN, FUN='sum',na.rm=T, na.action=na.pass)[,2]
allTrees.out$PREVDIA.TOT    <- aggregate(FIA.out$PREVDIA        ~ FIA.out$PLT_CN, FUN='sum',na.rm=T, na.action=na.pass)[,2]
allTrees.out$DIA.TOT        <- aggregate(FIA.out$DIA            ~ FIA.out$PLT_CN, FUN='sum',na.rm=T, na.action=na.pass)[,2]
#aggregating data- Stem number
allTrees.out$PREVSTEM.EM        <- aggregate(FIA.out$PREVSTEM.EM        ~ FIA.out$PLT_CN, FUN='sum',na.rm=T, na.action=na.pass)[,2]
allTrees.out$STEM.EM            <- aggregate(FIA.out$STEM.EM            ~ FIA.out$PLT_CN, FUN='sum',na.rm=T, na.action=na.pass)[,2]
allTrees.out$PREVSTEM.AM        <- aggregate(FIA.out$PREVSTEM.AM        ~ FIA.out$PLT_CN, FUN='sum',na.rm=T, na.action=na.pass)[,2]
allTrees.out$STEM.AM            <- aggregate(FIA.out$STEM.AM            ~ FIA.out$PLT_CN, FUN='sum',na.rm=T, na.action=na.pass)[,2]
allTrees.out$PREVSTEM.EITHER    <- aggregate(FIA.out$PREVSTEM.EITHER    ~ FIA.out$PLT_CN, FUN='sum',na.rm=T, na.action=na.pass)[,2]
allTrees.out$STEM.EITHER        <- aggregate(FIA.out$STEM.EITHER        ~ FIA.out$PLT_CN, FUN='sum',na.rm=T, na.action=na.pass)[,2]
allTrees.out$PREVSTEM.OTHER     <- aggregate(FIA.out$PREVSTEM.OTHER     ~ FIA.out$PLT_CN, FUN='sum',na.rm=T, na.action=na.pass)[,2]
allTrees.out$STEM.OTHER         <- aggregate(FIA.out$STEM.OTHER         ~ FIA.out$PLT_CN, FUN='sum',na.rm=T, na.action=na.pass)[,2]
test.fun<- function(x) sum(!is.na(x))
allTrees.out$PREVSTEM.TOT       <- aggregate(FIA.out$PREVDIA            ~ FIA.out$PLT_CN, FUN='test.fun',     na.action=na.pass)[,2]
allTrees.out$STEM.TOT           <- aggregate(FIA.out$DIA                ~ FIA.out$PLT_CN, FUN='test.fun',     na.action=na.pass)[,2]
#pop in other relevant site data. Using the 'median' function in aggregate
#beacause each site has 1 unique value in the mycFIA.out.rds file, replicated over every tree observation
#there is probably a much faster way to do this. 
allTrees.out$REMPER              <- aggregate(FIA.out$REMPER      ~ FIA.out$PLT_CN, FUN='median',na.rm=T,na.action=na.pass)[,2]
allTrees.out$prevTPA             <- aggregate(FIA.out$PREVTPAsum  ~ FIA.out$PLT_CN, FUN='median',na.rm=T,na.action=na.pass)[,2]
allTrees.out$TPA                 <- aggregate(FIA.out$TPAsum      ~ FIA.out$PLT_CN, FUN='median',na.rm=T,na.action=na.pass)[,2]
allTrees.out$latitude            <- aggregate(FIA.out$LAT         ~ FIA.out$PLT_CN, FUN='median',na.rm=T,na.action=na.pass)[,2]
allTrees.out$longitude           <- aggregate(FIA.out$LON         ~ FIA.out$PLT_CN, FUN='median',na.rm=T,na.action=na.pass)[,2]
allTrees.out$elevation           <- aggregate(FIA.out$ELEV        ~ FIA.out$PLT_CN, FUN='median',na.rm=T,na.action=na.pass)[,2]

#save this output file. Then merge with soil data. save that separately. 
saveRDS(allTrees.out, file='analysis_data/allTrees.rds')

#merge Trees and Soils, save!
allTrees.out<- readRDS('analysis_data/allTrees.rds')
#Only 300 records match. God damn.
Soils<- read.csv('FIA_soils/FIAsoil_output_CA.csv')
Trees.Soils <- merge(allTrees.out,Soils,by = "PLT_CN")
saveRDS(Trees.Soils,file='analysis_data/Trees.Soils.rds')