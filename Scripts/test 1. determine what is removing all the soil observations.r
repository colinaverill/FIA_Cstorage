##test what is narrowing ~3.5k soil observations to ~300
rm(list=ls())
library(data.table) #note, version 1.9.4 or higher must be installed, otherwise you will have trouble running particular commands. 
library(RPostgreSQL)
library(bit64)
source('required_products_utilities/PSQL_utils.R') #this will give you the tools needed to work with the PSQL database.



tic = function() assign("timer", Sys.time(), envir=.GlobalEnv)
toc = function() print(Sys.time()-timer)
bigtime = Sys.time()

dbsettings = list(
  user     = "bety",             # PSQL username  ###NOTE colin changed the info here to get into the DB @ BU. this works. 
  password = "",                 # PSQL password
  dbname   = "fia5",             # PSQL database name
  host     = "psql-pecan.bu.edu",# PSQL server address (don't change unless server is remote)
  driver   = 'PostgreSQL',       # DB driver (shouldn't need to change)
  write    = FALSE               # Whether to open connection with write access. 
)

#load soils data- 3451 unique profiles. 
soils<- read.csv('FIA_soils/FIAsoil_output_CA.csv')

#Ryan Kelly sets these- this kills everything west of the center of the country. 
lon.bounds = c(-95,999)
lat.bounds = c(-999,999)


# -----------------------------
# Open connection to database
fia.con = db.open(dbsettings)


# ---------- PLOT & COND DATA
# --- Query PLOT
cat("Query PLOT...\n")
query = paste('SELECT 
              cn, statecd, prev_plt_cn, remper
              FROM plot WHERE remper>3 AND remper<9.5 AND designcd=1 AND statecd<=56 AND ',
              'lon>', min(lon.bounds),' AND lon<', max(lon.bounds), ' AND ',
              'lat>', min(lat.bounds),' AND lat<', max(lat.bounds))

tic() # ~10 sec
PLOT = as.data.table(db.query(query, con=fia.con))
setnames(PLOT, toupper(names(PLOT)))
setnames(PLOT,"CN","PLT_CN")
toc()

nrow(merge(soils,PLOT, by="PLT_CN")) #639 site smake it through with these constraints. 



#demonstrate that removing lat/long constraints, remper, and designcd=1 generates matches for all soil profiles.
query = paste('SELECT 
              cn, statecd, prev_plt_cn, remper
              FROM plot ')
tic() # ~10 sec
PLOT = as.data.table(db.query(query, con=fia.con))
setnames(PLOT, toupper(names(PLOT)))
setnames(PLOT,"CN","PLT_CN")
toc()

nrow(merge(soils,PLOT, by="PLT_CN")) #3451 sites match now. 

#demonstrate effect of REMPER constraint. 
query = paste('SELECT 
              cn, statecd, prev_plt_cn, remper
              FROM plot WHERE remper>0 AND remper<100')
tic() # ~10 sec
PLOT = as.data.table(db.query(query, con=fia.con))
setnames(PLOT, toupper(names(PLOT)))
setnames(PLOT,"CN","PLT_CN")
toc()

nrow(merge(soils,PLOT, by="PLT_CN")) #770 sites match now.


#demonstrate effect of deisgncd constraint
query = paste('SELECT 
              cn, statecd, prev_plt_cn, remper,designcd
              FROM plot ') #WHERE designcd>0 AND designcd < 4
tic() # ~10 sec
PLOT = as.data.table(db.query(query, con=fia.con))
setnames(PLOT, toupper(names(PLOT)))
setnames(PLOT,"CN","PLT_CN")
toc()

nrow(merge(soils,PLOT, by="PLT_CN")) #2575 sites match now.

