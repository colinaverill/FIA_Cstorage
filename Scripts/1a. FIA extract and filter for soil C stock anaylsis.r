###GENERATING ALL SITE DATA FOR MATCHING SOIL C TO TREE ABUNDANCE data. 
rm(list=ls())
library(data.table) #note, version 1.9.4 or higher must be installed, otherwise you will have trouble running particular commands. 
library(RPostgreSQL)
library(bit64)
# library(PEcAn.DB)
source('required_products_utilities/PSQL_utils.R') #this will give you the tools needed to work with the PSQL database.

sumNA  = function(x) sum(x,na.rm=T)
meanNA = function(x) mean(x,na.rm=T)
maxNA  = function(x) max(x,na.rm=T)
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


#lon.bounds = c(-95,999)
#lat.bounds = c(-999,999)

file.pft = "required_products_utilities/gcbPFT.csv" #changed to actually find this file within the repo.
# file.pft = "~/rykelly/PhaseSpace/gcbPFT.csv"

file.soil = read.csv("FIA_soils/FIAsoil_output_CA.csv")

file.TA = 'From_T.Andrews/ESMraw.rds' #this file doesn't exist in the repo. 

file.out = "soilC.FIA.out.rds" #changed the name of the output file. 

# -----------------------------
# Open connection to database
fia.con = db.open(dbsettings)

# ---------- PLOT & COND DATA
# --- Query PLOT
#NOTE: CA has modified this query. 
# - All design codes in the soils are equivalent to designcd=1. This constraint has been removed. 
# - All states are allowed. statecd<=56 has been removed. 
cat("Query PLOT...\n")
query = paste('SELECT 
              cn, statecd, prev_plt_cn, remper, lat, lon, elev
              FROM plot')

tic() # ~10 sec
PLOT = as.data.table(db.query(query, con=fia.con))
setnames(PLOT, toupper(names(PLOT)))
setnames(PLOT,"CN","PLT_CN")
toc()

#subset PLOT table so its only observations with soil data. 
PLOT = PLOT[ PLT_CN %in% file.soil$PLT_CN ]


# --- Query COND ~1 minute.
cat("Query COND...\n")
query = paste('SELECT 
              plt_cn, condid, stdorgcd
              FROM cond')

tic() # ~ 15 sec
COND = as.data.table(db.query(query, con=fia.con))
setnames(COND, toupper(names(COND)))
toc()

# Remove all plots with more than 1 condition
COND[, CONmax := maxNA(CONDID), by=PLT_CN]
# *** RK: This is slightly wrong. In a few cases plots have CONDID>1, but still only have a single condition. This would work better:
#     COND[, CONmax2 := .N, by=PLT_CN]
COND = COND[ CONmax==1,]


# --- Merge PLOT and COND- this condition filtering reduces sset form 3451 to 2671 observations
cat("Merge PLOT and COND ...\n")
tic()
PC = merge(COND, PLOT, by="PLT_CN")
toc()



# ---------- RESURVEY DATA ~12 mins. 
# --- Query
cat("Query TREE_GRM_ESTN...\n")
query = paste('SELECT 
              plt_cn, invyr, tpagrow_unadj, dia_begin, dia_end, component, tre_cn, remper, statecd
              FROM tree_grm_estn WHERE ',
              'estn_type=\'AL\' AND land_basis=\'TIMBERLAND\'')

tic()
GRM = as.data.table(db.query(query, con=fia.con))
setnames(GRM, toupper(names(GRM)))
toc()

# --- Filtering
cat("Filtering TREE_GRM_ESTN...\n")

# By plot/cond criteria- 81,736 unique sites - only 1746 unique sites being kept here. This means sites in PC table not in GRM?
GRM = GRM[ PLT_CN %in% PC$PLT_CN ]

# Assign GRM$START + GRM$CUT and restrict to cut==0, start>0
GRM[, START      := INVYR - REMPER                                  ]
GRM[, REMPER := NULL]
GRM[, CUT1TPA    := (COMPONENT=="CUT1") * TPAGROW_UNADJ             ]
GRM[, CUT2TPA    := (COMPONENT=="CUT2") * TPAGROW_UNADJ             ]
GRM[, CUT        := sumNA(CUT2TPA + CUT1TPA), by=PLT_CN             ]
GRM = GRM[ START>0 & CUT==0, ] #only include plots that have not been cut- remove ~230k trees. 
#this reduces GRM table to 71,489 unique sites
#724 when we are using the ones in the soil set. 1000 observations lost here. 
#probably because its getting into REMPER constraint again. 


length(unique(GRM$PLT_CN))
# Assign Reversion/Diversion, and exclude plots with either - this excludes 9 sites. down to 715.  
GRM[, DIVERSION1TPA  := (COMPONENT=="DIVERSION1") * TPAGROW_UNADJ   ]
GRM[, DIVERSION2TPA  := (COMPONENT=="DIVERSION2") * TPAGROW_UNADJ   ]
GRM[, REVERSION1TPA  := (COMPONENT=="REVERSION1") * TPAGROW_UNADJ   ]
GRM[, REVERSION2TPA  := (COMPONENT=="REVERSION2") * TPAGROW_UNADJ   ]
GRM[, REDIV          := sumNA(REVERSION2TPA+REVERSION1TPA+DIVERSION2TPA+DIVERSION1TPA), by=PLT_CN]
GRM = GRM[ REDIV==0, ] #only include plots that have not had diversion/reversion, removes ~50k trees.
#this reduces GRM table to 68,481 sites. 

# Assign SURVIVORTPA, and remove records from any state with <1000 measured trees
# CA note- who cares about states w/ less than 1000 trees, why exclude? Just care at the plot level, no?
GRM[, SURVIVORTPA    := (COMPONENT=="SURVIVOR") * TPAGROW_UNADJ     ]
GRM[, TPATOT         := sumNA(SURVIVORTPA), by=STATECD              ]
GRM = GRM[ TPATOT>1000, ] #this line doesn't actually remove any observations. 


# --- Assign additional variables
cat("Calculating TPA and Diameter...\n")
# Compute TPA
GRM[, INGROWTHTPA    := (COMPONENT=="INGROWTH")   * TPAGROW_UNADJ   ]
GRM[, MORTALITY1TPA  := (COMPONENT=="MORTALITY1") * TPAGROW_UNADJ   ]
GRM[, MORTALITY2TPA  := (COMPONENT=="MORTALITY2") * TPAGROW_UNADJ   ]
GRM[, MORTALITYTPA   := MORTALITY1TPA + MORTALITY2TPA               ]

# Initial number of trees is current survivors plus those that died during the resurvey period.
GRM[, start1tpa      := SURVIVORTPA + MORTALITYTPA                  ]
GRM[, PREVTPAsum     := sumNA(start1tpa), by=PLT_CN                 ]  # "startsumTPA"

# Final number of trees is current survivors plus new trees that cross the 5" threshold
GRM[, end1tpa        := SURVIVORTPA + INGROWTHTPA                   ]
GRM[, TPAsum         := sumNA(end1tpa), by=PLT_CN                   ]  # "endsumTPA"

# Compute plot mean diameters
GRM[, PREVDIAmean    := meanNA(DIA_BEGIN), by=PLT_CN                ]  # "DIAbeginmean"
GRM[, DIAmean        := meanNA(DIA_END),   by=PLT_CN                ]  # "DIAendmean"


# --- Subset for output
GRM.out = GRM[, .(PLT_CN, TRE_CN, PREVTPAsum, TPAsum, PREVDIAmean, DIAmean)]


# ---------- TREE ~13 minutes
cat("Query TREE...\n")
# --- Query
query = paste('SELECT 
              cn, prev_tre_cn, plt_cn, invyr, condid, dia, tpa_unadj, spcd, stocking, statuscd, 
              prevdia, prev_status_cd, p2a_grm_flg, reconcilecd
              FROM tree WHERE 
              (prevdia>5 OR dia>5) AND (statuscd=1 OR prev_status_cd=1) AND p2a_grm_flg!=\'N\'  AND
              statecd IN (', paste(states,collapse=','), ')')

tic() # ~ 10 min
TREE = as.data.table(db.query(query, con=fia.con))
setnames(TREE, toupper(names(TREE)))
toc()

# --- Filter TREE
cat("Filter TREE ...\n")
# By plot/cond criteria
TREE = TREE[ PLT_CN %in% PC$PLT_CN ]

# CONDID ("Remove edge effects" --TA)
TREE[, CONmax := maxNA(CONDID), by=PLT_CN]

# STATUSCD
# *** RK: Next line looks wrong. It's a sum, not max, despite the name. I did rewrite the line but this is equivalent to what Travis had so keeping for now.
TREE[, STATUSCDmax := sumNA(3*as.integer(STATUSCD==3)), by=PLT_CN]

# RECONCILECD
TREE[is.na(RECONCILECD), RECONCILECD :=0] # Set NA values to 0 (unused)

# Filter
TREE = TREE[ CONmax==1 & INVYR<2014 & STATUSCDmax!=3 & STATUSCD!=0 & RECONCILECD<=4 ]


# --- Merge in PFTs and mycorrhizal associations
cat("Merge in PFTs and mycorrhizal associations...\n")
tic() # ~ 1.5 min
MCDPFT = as.data.table(read.csv("required_products_utilities/gcbPFT.csv", header = TRUE)) 
CA_myctype = as.data.table(read.csv("required_products_utilities/mycorrhizal_SPCD_data.csv",header=TRUE)) #colin loads in mycorrhizal associations
CA_myctype = CA_myctype[,c("SPCD","MYCO_ASSO"),with=F] #colin loads in mycorrhizal associations
TREE = merge(TREE, MCDPFT, all.x=T, by = "SPCD")
TREE = merge(TREE, CA_myctype, all.x=T, by = "SPCD") #colin merges in mycorrhizal associations
toc()

# --- Connect PREV_CN for each tree prior to subset
cat("Connect consecutive observations...\n")
tic() # ~20 sec
TREE.prev = TREE[,.(CN, STOCKING, SPCD, TPA_UNADJ, PFT)]
setnames(TREE.prev, paste0("PREV_TRE_",names(TREE.prev))) 
toc()

# Convert PREV_TRE_CN columns to integer64 (have experienced crashes otherwise. memory leak?)  
TREE.prev[, PREV_TRE_CN := as.integer64(PREV_TRE_CN)]
TREE[, PREV_TRE_CN := as.integer64(PREV_TRE_CN)]

tic()
TREE = merge(TREE, TREE.prev, all.x=T, by="PREV_TRE_CN")
setnames(TREE,"CN","TRE_CN")
toc()


# --- Define DIA and STOCKING columns for trees >5"
cat("Calculate DIA and STOCKING...\n")
# DIAmean of DIA>5
TREE[DIA>=5 & STATUSCD==1,                                   DIA5alive := DIA      ]
TREE[, DIA5meanalive     := meanNA(DIA5alive), by=PLT_CN                         ]
TREE[PREVDIA>=5 & PREV_STATUS_CD==1,                     PREVDIA5alive := PREVDIA  ]
TREE[, PREVDIA5meanalive := meanNA(PREVDIA5alive), by=PLT_CN                     ]

#Stocking of plots for trees with DIA>5
TREE[DIA5alive>0, STOCKING5 := STOCKING]
TREE[, STOCKING5mid := sumNA(STOCKING5), by=PLT_CN]
TREE[PREVDIA5alive>0, PREVSTOCKING5 := PREV_TRE_STOCKING]
TREE[, PREVSTOCKING5mid := sumNA(PREVSTOCKING5), by=PLT_CN]


# ---------- MERGE
cat("Final merge...\n")
ALL = merge(GRM, TREE, all.x=T, by='TRE_CN')
ALL[, c("PLT_CN.x","INVYR.x") := list(NULL,NULL)]
setnames(ALL, c("PLT_CN.y","INVYR.y"), c("PLT_CN","INVYR"))
ALL = merge(ALL, PC, by='PLT_CN')
ALL[, c("STATECD.x","CONmax.x") := list(NULL,NULL)]
setnames(ALL, c("STATECD.y","CONmax.y"), c("STATECD","CONmax"))
setnames(ALL, "START", "PREVYR")

#   ALL = merge(GRM, PC, by='PLT_CN')
#     setnames(ALL, "START", "PREVYR")


# --- Save outputs
cat("Save...\n")
tic()
saveRDS(ALL, file = file.out)
toc()


db.close(fia.con)

print(Sys.time()-bigtime)