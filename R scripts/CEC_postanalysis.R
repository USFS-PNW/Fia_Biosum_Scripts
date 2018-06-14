#Make sure you have the Microsoft Access Databse Engine Driver https://www.microsoft.com/en-us/download/confirmation.aspx?id=23734
#and you are using 32-bit R (set in RStudio by going to Tools -> Global Options)

#Load in required packages. Some red warning text is normal.
packages <- c("RODBC", "dplyr", "ggplot2", "gridExtra", "reshape2", "flextable", "R.utils", "officer", "RColorBrewer")

package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, repos="http://cran.r-project.org", dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

options(scipen = 999) #this is important for making sure your stand IDs do not get translated to scientific notation

#Project root location. 
project.location <- "H:/cec_20180529/"
additional.data <- "G:/Dropbox/Carlin/GitHub/Fia_Biosum_Scripts/Additional data" #the local location of the Github Fia_Biosum_Scripts repository
core.scenario.name <- "scenario1" #The name of the core scenario you would like to use to pull in core variables

#Use & set these lines if you aren't in a biosum project directory or if you are using different master/core locations
# project.location <- NA #project.location should be set to NA so the script knows not to try to use it
# prepost.location <- "H:/cec_20170915/fvs/db/PREPOST_FVS_SUMMARY.ACCDB"
# master.location <- "H:/cec_20170915/db/master.mdb"
# core.location <- "H:/cec_20170915/core/scenario6_20180508/db/scenario_results.mdb"
# working.directory <- "H:/cec_20170915/" #Directory where you would like to save outputs

#Get the PREPOST data from PRE_FVS_SUMMARY and POST_FVS_SUMMARY in the fvs/db directory
if (!is.na(project.location)) {
  conn.path <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", file.path(project.location, "fvs", "db", "PREPOST_FVS_SUMMARY.ACCDB"))
} else {
  conn.path <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", file.path(prepost.location))
}
conn <- odbcDriverConnect(conn.path) 
pre <- sqlFetch(conn, "PRE_FVS_SUMMARY", as.is = TRUE) 
post <- sqlFetch(conn, "POST_FVS_SUMMARY", as.is = TRUE) 
odbcCloseAll()

#Connect to the project master database to bring in the condition table and CEC forest type table
if (!is.na(project.location)) {
  conn.path <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", file.path(project.location, "db", "master.mdb"))
} else {
  conn.path <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", file.path(master.location))
}
conn <- odbcDriverConnect(conn.path) #Change the text after "DBH=" to the correct directory for your project
master_cond <- sqlFetch(conn, "cond", as.is = TRUE) 
ftype <- sqlFetch(conn, "CEC_ftype", as.is = TRUE)
odbcCloseAll()

#Crosswalk forest type code for CEC project
names(ftype)[1] <- "fortypcd"
names(ftype)[3] <- "CEC_type"
ftype <- ftype[,c(1,3)]

#Merge CEC forest type with condition table
master_cond<- merge(master_cond, ftype)

###COND DATA###
#The section below connects to the master_cond table and pulls in forest type code and owner code values from master_cond,
#then limits to owncd 11/46/47/48 and to CEC relevant forest type
pattern <- c("biosum_cond_id", "rxpackage", "fortypcd", "owncd", "owngrpcd", "acres", "CEC_type")
cond.relevant.data <- master_cond[,c(which(grepl(paste0(pattern, collapse = "|"), names(master_cond))))]
cond.relevant.data <- cond.relevant.data[cond.relevant.data$owncd %in% c(11,46,47,48),]
cond.relevant.data <- cond.relevant.data[cond.relevant.data$CEC_type %in% c("Douglas-fir", "Pine", "Mixed conifer", "True fir"),]

####PREPOST DATA####
#Calculate mort vol pct, change NA values to 0
pre$MortVol_FOFEM[is.na(pre$MortVol_FOFEM)] <- 0 
pre$MortVolPct_FOFEM <- pre$MortVol_FOFEM/pre$VolSum
pre$MortVolPct_FOFEM[pre$VolSum == 0] <- 0
pre$MortVolPct_FOFEM[is.na(pre$VolSum)] <- 0

post$MortVol_FOFEM[is.na(post$MortVol_FOFEM)] <- 0 
post$MortVolPct_FOFEM <- post$MortVol_FOFEM/post$VolSum
post$MortVolPct_FOFEM[post$VolSum == 0] <- 0
post$MortVolPct_FOFEM[is.na(post$VolSum)] <- 0

#Get relevant columns from PREPOST and core data
pattern <- c("biosum_cond_id", "rxpackage", "rx", "rxcycle", "Year", "fvs_variant", "TCuFt", "MortVol_FOFEM", "SurvVolRatio", "MortVolPct_FOFEM", 
             "RTpa", "RTCuFt", "Acc", "Mort", "Canopy_Density", "CBH", "Torch_Index", "PTorch_Sev", "Surf_Flame_sev", "PERRESBA", "Avg_CBD_Score", 
             "Avg_CBH_Score", "Avg_PERRESBA_Score", "Avg_SurvVolRatio_Score", "Avg_FRS", "Avg_HS_Sev", "Avg_HS_Mod", "Score")
pre.relevant.data <- pre[,c(which(grepl(paste0(pattern, collapse = "|"), names(pre))))]
post.relevant.data <- post[,c(which(grepl(paste0(pattern, collapse = "|"), names(post))))]

all.data <- rbind(pre.relevant.data, post.relevant.data) #combine pre and post data

#merge with cond.relevant.data to bring in ownership, forest type, and acreage from master.cond
all.data <- merge(all.data, cond.relevant.data)

##De-annualize accretion and mort
# Accretion and mortality are expressed in total stem cubic feet in western variants and
# merchantable stem cubic feet of pulpwood in eastern variants. Accretion is growth on
# trees surviving to the end of the cycle
Years <- data.frame(unique(all.data$Year)) #get unique years
names(Years)[1] <- "Year"
Years <- Years[order(Years$Year),] #sort by year
Years <- data.frame("Year" = Years)

for (i in 1:(nrow(Years)-1)) {
  Years$Diff[i] <- Years$Year[i+1] - Years$Year[i]
} #get difference in years from one year cycle to another

all.data <- merge(all.data, Years, by = c("Year")) #add "year difference" to all.data

all.data$Period_Acc <- all.data$Acc * all.data$Diff #multiply accretion by year difference to get period accretion
all.data$Period_Mort <- all.data$Mort * all.data$Diff #multiply mortality by year difference to get period mortality

#Calculate net growth by getting difference in TCuFt from one Year to the next
netgrowth <- melt(all.data, id.vars = c("biosum_cond_id", "rxpackage", "fvs_variant", "Year"),measure.vars = "TCuFt")
netgrowth <- netgrowth[order(netgrowth$biosum_cond_id, netgrowth$rxpackage, netgrowth$Year),] 
netgrowth <- netgrowth %>% dplyr::group_by(biosum_cond_id, rxpackage, fvs_variant) %>% dplyr::mutate(NetGrowth_TCuFt = c(0, diff(value)))

netgrowth$variable <- NULL
netgrowth$value <- NULL

#merge net growth back in with all.data
all.data <- merge(all.data, netgrowth, all.x = TRUE)

#Calculate net growth + harvest by adding net growth and RTCuFt
all.data$NetGrowth_Plus_Harvest_TCuFt <- rowSums(all.data[,which(names(all.data) %in% c("NetGrowth_TCuFt", "RTCuFt"))], na.rm = TRUE)

#sum period accretion and mortality by stand
sums.all.data <- all.data %>% group_by(biosum_cond_id, rxpackage) %>% summarise(Period_Acc_sum = sum(Period_Acc), 
                                                                                Period_Mort_sum = sum(Period_Mort), 
                                                                                Net_Growth_TCuFt_sum = sum(NetGrowth_TCuFt), 
                                                                                NetGrowth_Plus_Harvest_TCuFt_sum = sum(NetGrowth_Plus_Harvest_TCuFt)) 

#add stand initial TCuFt (TCuFt from cycle 1)
initial <- pre.relevant.data[pre.relevant.data$rxcycle == 1,]
initial <- data.frame("biosum_cond_id" = initial$biosum_cond_id, "rxpackage" = initial$rxpackage, "Initial.TCuFt" = initial$TCuFt)

#merge stand level data for inital TCuFt and accretion/mort
packagestand_level_data <- merge(sums.all.data, initial, by = c("biosum_cond_id", "rxpackage"), all = TRUE)

#Separate out package 31 data
package31 <- all.data[all.data$rxpackage == "031",]

#Separate out other packages
all.data2 <- all.data[all.data$rxpackage != "031",]

#Add column with 0 for RTpa values < 0, 1 for RTpva values > 0
all.data2$RTpa_YN <- ifelse(all.data2$RTpa > 0, 1, 0)

#Sum the RTpa_YN values by stand and package
treated_stands <- all.data2 %>% group_by(biosum_cond_id, rxpackage) %>% summarise(RTpa_SUM = sum(RTpa_YN))

#Remove stand-packages where RTpa_YN is 0 (e.g. stands that were never treated)
treated_stands2 <- treated_stands[treated_stands$RTpa_SUM > 0,]

all.data3 <- merge(treated_stands2, all.data2)

#Remove added RTpa_YN column
all.data3$RTpa_YN <- NULL
all.data3$RTpa_SUM <- NULL

#Add package 31 data back in
relevant.data2 <- rbind(all.data3, package31)

#The following calculates package averages for the score values by removing cycle 1
#Remove cycle 1 from PRE
Year1.data <- relevant.data2[relevant.data2$Year == "1",]

#Merge pre and post
NonYear1.data <- relevant.data2[relevant.data2$Year != "1",]

#Average the MortVol_FOFEM and MortVolPct_FOFEM values by stand-package
NonYear1_packagestand_data <- NonYear1.data %>% group_by(biosum_cond_id, rxpackage, rx) %>% summarise(Avg_MortVol_FOFEM = mean(MortVol_FOFEM),
                                                                                                        Avg_MortVolPct_FOFEM = mean(MortVolPct_FOFEM), 
                                                                                                        Avg_CBH = mean(CBH),
                                                                                                        Avg_Canopy_Density = mean(Canopy_Density), 
                                                                                                        Avg_PERRESBA = mean(PERRESBA),
                                                                                                        Avg_Torch_Index = mean(Torch_Index),
                                                                                                        Avg_Surf_Flame_sev = mean(Surf_Flame_sev),
                                                                                                        Avg_SurvVolRatio = mean(SurvVolRatio),
                                                                                                        Avg_HS_Sev = mean(Avg_HS_Sev),
                                                                                                        Avg_FRS = mean(Avg_FRS), 
                                                                                                        Avg_CBD_Score = mean(Avg_CBD_Score),
                                                                                                        Avg_PERRESBA_Score = mean(Avg_PERRESBA_Score), 
                                                                                                        Avg_SurvVolRatio_Score = mean(Avg_SurvVolRatio_Score),
                                                                                                        Avg_CBH_Score = mean(Avg_CBH_Score),
                                                                                                        Avg_Torch_Index_Score = mean(Avg_Torch_Index_Score),
                                                                                                        Avg_Surf_Flame_Sev_Score = mean(Avg_Surf_Flame_Sev_Score),
                                                                                                        Avg_SurvVolRatio_Score = mean(Avg_SurvVolRatio_Score),
                                                                                                        Avg_Ptorch_Sev_Score = mean(Avg_Ptorch_Sev_Score),
                                                                                                      Avg_MortVolPct2_Sev_Score = mean(Avg_MortVolPct2_Sev_Score),
                                                                                                        Sum_RTpa = sum(RTpa), 
                                                                                                        Sum_RTCuFt = sum(RTCuFt)
                                                                                                      )


stand_summary <- merge(NonYear1_packagestand_data, packagestand_level_data)

#####################################################
######CORE DATA. SKIP IF YOU HAVE NOT RUN CORE#######
#####################################################
if (!is.na(project.location)) {
  conn.path <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", file.path(project.location, "core", core.scenario.name, "db", "scenario_results.mdb"))
} else {
  conn.path <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", file.path(core.location))
}
conn <- odbcDriverConnect(conn.path) #Change the text after "DBH=" to the correct directory for your project
net_rev <- sqlFetch(conn, "product_yields_net_rev_costs_summary_by_rxpackage", as.is = TRUE)
effective <- sqlFetch(conn, "cycle1_effective", as.is = TRUE)
valid_combos <- sqlFetch(conn, "validcombos_fvsprepost", as.is = TRUE)
cycle1_best_rx <- sqlFetch(conn, "cycle1_best_rx_summary", as.is = TRUE)
stand_cr <- sqlFetch(conn, "stand_costs_revenue_volume_by_rx", as.is = TRUE)
odbcCloseAll()

#####Get total treated acres####
valid.acres <- merge(cond.relevant.data, valid_combos)
valid.acres.summary <- valid.acres %>% group_by(CEC_type, rxpackage, rxcycle, owngrpcd) %>% summarise(Treatable_Acres = sum(acres))
treated.area <- valid.acres.summary[valid.acres.summary$rxpackage == "031" & valid.acres.summary$rxcycle == "1",]

if (!is.na(project.location)) {
  setwd(project.location) 
} else {
  setwd(working.directory)
}
write.csv(treated.area, paste0("TreatedArea_Master_", format(Sys.Date(), "%Y%m%d"), ".csv"), row.names = FALSE)

pattern <- c("biosum_cond_id", "rxpackage", "merch_yield_gt", "chip_yield_gt", "chip_val_dpa", "merch_val_dpa", "haul_chip_cpa", "haul_merch_cpa","merch_yield_cf", "chip_yield_cf", "harvest_onsite_cpa", "merch_nr_dpa", "max_nr_dpa")
core.data <- net_rev[,c(which(grepl(paste0(pattern, collapse = "|"), names(net_rev))))]

pattern <- c("biosum_cond_id", "rxpackage", "rx", "merch_vol_cf_exp")
stand.cr.relevant.data <- stand_cr[,c(which(grepl(paste0(pattern, collapse = "|"), names(stand_cr))))]
stand.cr.relevant.data <- stand.cr.relevant.data %>% group_by(biosum_cond_id, rxpackage, rx) %>% summarise(stand_sum_merch_vol_cf_exp = sum(merch_vol_cf_exp)) #sum merch_vol_cf_exp by biosum_cond_id and package

core.data <- merge(stand.cr.relevant.data, core.data, by = c("biosum_cond_id", "rxpackage"), all = TRUE) #merge core data

pattern <- c("biosum_cond_id", "rxpackage", "overall_effective_yn", "post_variable1_value", "variable1_change")
effective.relevant.data <- effective[,c(which(grepl(paste0(pattern, collapse = "|"), names(effective))))]

core.data <- merge(effective.relevant.data, core.data, by = c("biosum_cond_id", "rxpackage"), all = TRUE) #merge core data with effective

#create valid column where it equals "Y" if the stand/package combination is in the valid.combos table
core.data$valid[paste(core.data$biosum_cond_id,core.data$rxpackage) %in% paste(valid_combos$biosum_cond_id, valid_combos$rxpackage)] <- "Y"
core.data$valid[!paste(core.data$biosum_cond_id,core.data$rxpackage) %in% paste(valid_combos$biosum_cond_id, valid_combos$rxpackage)] <- "N"

#create valid column where it equals "Y" if the stand/package combination is in the best table
core.data$best[paste(core.data$biosum_cond_id,core.data$rx) %in% paste(cycle1_best_rx$biosum_cond_id, cycle1_best_rx$rx)] <- "Y"
core.data$best[!paste(core.data$biosum_cond_id,core.data$rx) %in% paste(cycle1_best_rx$biosum_cond_id, cycle1_best_rx$rx)] <- "N"

#remove rxcycle from core.data
core.data$rxcycle <- NULL
core.data$rx <- NULL

#merge in the other package-stand data from PREPOST
stand_summary <- merge(stand_summary, core.data, by = c("biosum_cond_id", "rxpackage"), all.x = TRUE)


#####################################################
#####################################################
#####################################################
stand_summary <- merge(stand_summary, cond.relevant.data)

##This table should now contain relevant averages for all non year 1 (pre cycle 1) values only for stands that were treated at least once merged
#with various economic variables from core and forest type code. NA values may indicate the stand was not in all the tables, particularly in 
#core tables, but was in the pre/post summary tables.

#Save a copy of the data as a .csv in the project directory
if (!is.na(project.location)) {
  setwd(project.location) 
} else {
  setwd(working.directory)
}

write.csv(stand_summary, paste0("FigureData_Master_", format(Sys.Date(), "%Y%m%d"), ".csv"), row.names = FALSE)

####GRAPHING DATA####
package_labels <- read.csv(file.path(additional.data, "packagelabels_table.csv")) #bring in package labels for graphs
graph_data <- read.csv(file.path(additional.data, "graph_ref.csv")) #bring in graph parameters 

#The get_data function takes the stand_summary data, limits it to the packages to set as the "packages" parameter,
#limits it to only relevant forest types, limits it to only stands that exist in all packages, 
#puts in categories for volume and stand count, and gets all the data in the format necessary to create the 
#scatter charts. 

#The get_data function below is used to adapt the trimmed PREPOST data above 
get_relevant_stands <- function(data, packages) {
  packages <- c(packages, 31) #add package 31
  numpackages <- length(packages) #get number of packages
  data$rxpackage <- as.character(data$rxpackage) #convert 3 digit package value to integer
  data$rxpackage <- as.numeric(data$rxpackage)
  data$rxpackage <- as.integer(data$rxpackage)
  
  #translate owncd
  
  if (all(data$owncd %in% c(11,46))) {
    data$Owner[data$owncd == 11] <- "NFS"
    data$Owner[data$owncd == 46] <- "Private"
    
  } else if (all(data$owncd %in% c(11,47,48))) {
    data$Owner[data$owncd == 11] <- "NFS"
    data$Owner[data$owncd == 47] <- "Corporate"
    data$Owner[data$owncd == 48] <- "Family"
    
  }
  
  #Limit to relevant packages set by packages paramter with package 31 as reference
  relevant.data <- data[data$rxpackage %in% packages,]
  
  #Limit to relevant forest types
  relevant.data <- relevant.data[relevant.data$CEC_type %in% c("Douglas-fir", "Pine", "Mixed conifer", "True fir"),]
  
  #Count the number of times each stand appears
  relevant.data <- relevant.data %>% group_by(biosum_cond_id) %>% add_tally()
  
  if (max(relevant.data$n) > numpackages) {
    stop("ERROR: package tally incorrect")
  }
  
  #Remove packages that don't appear the same number of times as packages + reference
  relevant.stands <- relevant.data[relevant.data$n == numpackages,]
  
  relevant.stands$n <- NULL
  
  #Create volume classes
  relevant.stands$VolClass <- cut(relevant.stands$Initial.TCuFt, breaks = c(0,3000,5000, (max(relevant.stands$Initial.TCuFt)+1)), labels = c("<3000", "3000-5000", ">5000"), right= FALSE)
  
  #Tally stands by package, volume class, forest type, and owner code to get stand count
  relevant.stands <- relevant.stands %>% group_by(rxpackage, VolClass, CEC_type, Owner) %>% add_tally()
  
  names(relevant.stands)[ncol(relevant.stands)] <- "StandCount"
  
  #Create stand count classes
  relevant.stands$SC_to_print <- cut(relevant.stands$StandCount, breaks = c(0,25, (max(relevant.stands$StandCount)+1)), labels = c("<25", ">25"), right = FALSE)
  
  return(relevant.stands)
}

stands <- get_relevant_stands(stand_summary, c(1,2,3,4))

####PACKAGE GRAPHS####
#This function takes stand level data/averages and calculates package level averages 
#for parameters of interest (MortVol, MortVolPct, CBH, Canopy Density, etc). 

get_package_averages <- function(stand.data) {
  if ("harvest_onsite_cpa" %in% names(stand.data)) {
    
    #if core values are NA, set to 0 (i.e. they were not harvested, so they didn't cost anything)
    stand.data$harvest_onsite_cpa[is.na(stand.data$harvest_onsite_cpa)] <- 0
    stand.data$merch_nr_dpa[is.na(stand.data$merch_nr_dpa)] <- 0
    stand.data$max_nr_dpa[is.na(stand.data$max_nr_dpa)] <- 0
    stand.data$merch_val_dpa[is.na(stand.data$merch_val_dpa)] <- 0
    stand.data$chip_val_dpa[is.na(stand.data$chip_val_dpa)] <- 0
    
    package_average <- stand.data %>% dplyr::group_by(rxpackage, VolClass, CEC_type, Owner, StandCount, SC_to_print) %>% dplyr::summarise(PkgAvg_MortVol = mean(Avg_MortVol_FOFEM), 
                                                                                                                                          PkgAvg_MortVolPct = mean(Avg_MortVolPct_FOFEM), 
                                                                                                                                          PkgAvg_CBH = mean(Avg_CBH), 
                                                                                                                                          PkgAvg_Canopy_Density = mean(Avg_Canopy_Density), 
                                                                                                                                          PkgAvg_HS_Sev = mean(Avg_HS_Sev), PkgAvg_FRS = mean(Avg_FRS),
                                                                                                                                          PkgSum_harvest_onsite_cpa = sum(harvest_onsite_cpa), 
                                                                                                                                          PkgSum_merch_nr_dpa = sum(merch_nr_dpa),
                                                                                                                                          PkgSum_max_nr_dpa = sum(max_nr_dpa),
                                                                                                                                          PkgSum_merch_val_dpa = sum(merch_val_dpa),
                                                                                                                                          PkgSum_chip_val_dpa = sum(chip_val_dpa),
                                                                                                                                          PkgAvg_Initial.TCuFt = mean(Initial.TCuFt), 
                                                                                                                                          PkgSum_Period_Acc = sum(Period_Acc_sum), 
                                                                                                                                          PkgSum_Period_Mort = sum(Period_Mort_sum), 
                                                                                                                                          PkgSum_RTCuFt = sum(Sum_RTCuFt), 
                                                                                                                                          PkgSum_NetGrowth_TCuFt = sum(Net_Growth_TCuFt_sum),
                                                                                                                                          PkgSum_NetGrowth_Plus_Harvest_TCuFt = sum(NetGrowth_Plus_Harvest_TCuFt_sum))
  } else {
    #gets the average values by volclass, CEC_type, Owner, Standcount values
    package_average <- stand.data %>% dplyr::group_by(rxpackage, VolClass, CEC_type, Owner, StandCount, SC_to_print) %>% dplyr::summarise(PkgAvg_MortVol = mean(Avg_MortVol_FOFEM), 
                                                                                                                                          PkgAvg_MortVolPct = mean(Avg_MortVolPct_FOFEM), 
                                                                                                                                          PkgAvg_CBH = mean(Avg_CBH), 
                                                                                                                                          PkgAvg_Canopy_Density = mean(Avg_Canopy_Density), 
                                                                                                                                          PkgAvg_HS_Sev = mean(Avg_HS_Sev), PkgAvg_FRS = mean(Avg_FRS),
                                                                                                                                          PkgAvg_Initial.TCuFt = mean(Initial.TCuFt), 
                                                                                                                                          PkgSum_Period_Acc = sum(Period_Acc_sum), 
                                                                                                                                          PkgSum_Period_Mort = sum(Period_Mort_sum), 
                                                                                                                                          PkgSum_RTCuFt = sum(Sum_RTCuFt),
                                                                                                                                          PkgSum_NetGrowth_TCuFt = sum(Net_Growth_TCuFt_sum),
                                                                                                                                          PkgSum_NetGrowth_Plus_Harvest_TCuFt = sum(NetGrowth_Plus_Harvest_TCuFt_sum))
  }
  
  package_average$rxpackage <- as.factor(package_average$rxpackage)
  
  return(package_average)
}

package_avgs <- get_package_averages(get_relevant_stands(stand_summary, c(1,2,3,4)))

####GRAPH FUNCTION####
#This function takes stand level averages, runs the get_package_averages function (above), 
#then graphs the data based on parameters set in the graph_ref.csv file in the github
#additional data file. You can add variables, change axes names, etc. using that reference.

#The parameters are: 
#data  - this should be stand_summary data (e.g. data with values averaged at the stand level)
#packages - the packages you want to run the analysis for
#graph_variable_name1 - this corresponds to the graph_variable_name column in graph_ref.csv
#graph_variable_name2 - this corresponds to the graph_variable_name column in graph_ref.csv

#The graph_ref.csv defines the graph variable name used for the R function & 
#what the corresponding Y variable is (i.e. for graph_variable_name mortvol, the corresponding Y variable from data is PkgAvg_MortVol). 

#The Modifier column is for if you want the value to be converted for graphical display (i.e. a proportion multiplied by 100 to get percent). 

#YMAX Is if you want to set a manual Y axis maximum, though the script should be pretty good at calculating the best one to use now (so you can leave blank)

#YLABEL sets the Y-axis label for the graph

#Units will put the units in a second line under the YLABEL if you need units
  
graph_package_data <- function(data, packages, graph_variable_name1, graph_variable_name2) {
  package.data <- get_package_averages(get_relevant_stands(data, packages))
  packages <- as.character(unique(package.data$rxpackage))
  numpackages <- length(packages)
  ##PARAMETERS FOR ALL GRAPHS##
  cbPalette <- c("#CC79A7", "#E69F00", "#56B4E9", "#009E73")
  
  custom_theme <- theme_set(theme_bw(base_size = 20)) +
    theme(plot.title = element_text(size = 16, hjust = 0.5),
          axis.text.x=element_text(size=14), 
          axis.text.y=element_text(size=14),
          axis.title = element_text(size = 16),
          strip.text = element_text(size = 14),
          legend.position="right",
          legend.key = element_rect(colour = "gray90"), 
          legend.key.size = unit(2, "line"),
          legend.spacing = unit(2, "line"), 
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 12, face = "bold"))
  
  relevant_packages <- package_labels[package_labels$Package %in% packages,]
  names(relevant_packages) <- c("Sequence", "FVS Style", "BA Threshold", "Max Private \n DBH", "Max Public \n DBH", "Min DBH", "Residual BA \n (less amt)", "Species \n Pressure", "Surface Fuel \n Treatment", "Harvest \n System")
  
  tt <- ttheme_default(base_size = 14)
  t<- tableGrob(relevant_packages, rows = NULL, theme = tt)
  
  layout <- rbind(c(1,1,1,1,1,2,2,2,2,2,2),
                  c(1,1,1,1,1,2,2,2,2,2,2),
                  c(1,1,1,1,1,2,2,2,2,2,2),
                  c(NA,3,3,3,3,3,3,3,3,3,NA))

  final <- NULL
  for (i in 1:numpackages) {
    if (i != numpackages) {
      add <- paste0(packages[i], "v")
      final <- paste0(final, add)
    } else {
      add <- packages[i]
      final <- paste0(final, add)
    }
  }
  
    
  create_variable_graph <- function(graph_variable_name, legend) {
      y <- as.character(graph_data$Y[graph_data$Graph_variable_name == graph_variable_name])
      if(!is.na(graph_data$YMAX[graph_data$Graph_variable_name == graph_variable_name])) {
        ymax <- as.numeric(graph_data$YMAX[graph_data$Graph_variable_name == graph_variable_name])
      } else {
        if (ceiling(max(package.data[,c(which(names(package.data) == y))], na.rm = TRUE)) > 1) {
          ymax <- ceiling(max(package.data[,c(which(names(package.data) == y))], na.rm = TRUE)) + max(package.data[,c(which(names(package.data) == y))], na.rm = TRUE) * 0.1
        } else {
          ymax <- max(package.data[,c(which(names(package.data) == y))], na.rm = TRUE) + max(package.data[,c(which(names(package.data) == y))], na.rm = TRUE) * 0.1
        }
        if (floor(min(package.data[,c(which(names(package.data) == y))], na.rm = TRUE)) >= 0) {
          ymin <- 0
        } else {
          ymin <- min(package.data[,c(which(names(package.data) == y))], na.rm = TRUE) + min(package.data[,c(which(names(package.data) == y))], na.rm = TRUE) * 0.1
        }
      }
      
      if (!is.na(graph_data$Modifier[graph_data$Graph_variable_name == graph_variable_name])){
        y <- paste0(y,graph_data$Modifier[graph_data$Graph_variable_name == graph_variable_name])
        mod.ymax <- parse(text = paste0(ymax, graph_data$Modifier[graph_data$Graph_variable_name == graph_variable_name]))
        mod.ymin <- parse(text = paste0(ymin, graph_data$Modifier[graph_data$Graph_variable_name == graph_variable_name]))
        ymax <- eval(mod.ymax)
        ymin <- eval(mod.ymin)
      }
      if (!graph_data$UNITS[graph_data$Graph_variable_name == graph_variable_name] == ""){
        ylabel <- paste0(as.character(graph_data$YLABEL[graph_data$Graph_variable_name == graph_variable_name]), "\n",
                         "(", as.character(graph_data$UNITS[graph_data$Graph_variable_name == graph_variable_name]), ")")
      } else {
        ylabel <- as.character(graph_data$YLABEL[graph_data$Graph_variable_name == graph_variable_name])
      }
      title <- paste(as.character(graph_data$YLABEL[graph_data$Graph_variable_name == graph_variable_name]), "by Initial Volume")
      yaes <- paste0("package.data$", y)
      
      if (ymin < 0) {
        graph <- ggplot(package.data, aes_string("rxpackage", y)) + 
          geom_hline(yintercept = 0, size = 0.75, color = "gray25") +
          geom_point(aes(color = CEC_type, shape = SC_to_print), size = 4) +
          facet_grid(Owner~VolClass) + 
          scale_shape_manual(values=c(15,17), name = "Stand Count") +
          scale_colour_manual(values=cbPalette, name="Forest Type Group") +
          scale_y_continuous(limits = c(ymin,ymax)) +
          custom_theme +
          labs(x="Silvicultural Sequence", y=ylabel, title=title) + 
          theme(legend.position = legend) 

      } else {
        graph <- ggplot(package.data, aes_string("rxpackage", y)) +
          geom_point(aes(color = CEC_type, shape = SC_to_print), size = 4) +
          facet_grid(Owner~VolClass) + 
          scale_shape_manual(values=c(15,17), name = "Stand Count") +
          scale_colour_manual(values=cbPalette, name="Forest Type Group") +
          scale_y_continuous(limits = c(ymin,ymax)) +
          custom_theme + 
          labs(x="Silvicultural Sequence", y=ylabel, title=title) + 
          theme(legend.position = legend)
      }
      
      # yvals <- eval(parse(text = yaes))
      # 
      # if (any(max(yvals > ymax))) {
      #   stop("package average mortvol outside of graph y axis limits")
      # }
      
      return(graph)
    }
  
  var1 <- create_variable_graph(graph_variable_name1, "none")
  var2 <- create_variable_graph(graph_variable_name2, "right")
  
  graph <- grid.arrange(grobs = list(var1,var2,t), nrow = 2, layout_matrix = layout)
  
  old.dir <- getwd()
  dir <- file.path(getwd(), "graphs")
  dir.create(dir, showWarnings = FALSE)
  dir <- file.path(getwd(), "graphs", paste(format(Sys.Date(), "%Y%m%d"),graph_variable_name1, graph_variable_name2,sep = "_"))
  dir.create(dir, showWarnings = FALSE)
  setwd(dir)
  
  ggsave(filename = paste0(graph_variable_name1, graph_variable_name2,final, ".emf"), plot = graph, device = "emf", width = 13, height = 8.5)
  setwd(old.dir)
  
}

data <- stand_summary #This should have been calculated above

#######CREATE GRAPHICS######
#The lines below create the graphics for each analysis in the CEC report. These are saved to the directory you set above in
#a folder with the name of the two variables, e.g. E:\cec_20180529\cec_20180529\mortvol_mortvolpct

##MORTVOL GRAPHS
packages_1v2v3v4 <- graph_package_data(data, c(1,2,3,4), "mortvol", "mortvolpct")
packages_1v2v3v4 <- graph_package_data(data, c(1,2,3,4), "mortvol", "mortvolpct")
packages_1v2v3v4 <- graph_package_data(data, packages = c(1,2,3,4), "mortvol", "mortvolpct") #comparison of post treatment surface fuel treatments
packages_1v2v12v13 <- graph_package_data(data, packages = c(1,2,12,13), "mortvol", "mortvolpct") #comparison of WTL to CTL
pacakges_1v4 <- graph_package_data(data, packages = c(1,4), "mortvol", "mortvolpct") #CT to 115, rxfire vs lop and scatter
packages_1v4v30 <- graph_package_data(data, packages = c(1,4,32), "mortvol", "mortvolpct") #CT to 115, rxfire vs lop and scatter vs clearcut/lop and scatter
packages_1v20 <- graph_package_data(data, packages = c(1,20), "mortvol", "mortvolpct") #CT/Rxfire ThinDBH vs ThinBBA
packages_4v6 <- graph_package_data(data, packages = c(4,6), "mortvol", "mortvolpct") #target fir vs no fir target 
packages_4v7v17 <- graph_package_data(data, packages = c(4,7,17), "mortvol", "mortvolpct") #NFS diameter cap comparison
packages_4v8v9v11 <- graph_package_data(data, packages = c(4,8,9,11), "mortvol", "mortvolpct") #BA residual 115,135,150,180
packages_4v14 <- graph_package_data(data, packages = c(4,14), "mortvol", "mortvolpct") #min DBH 4 v 9
packages_4v19 <- graph_package_data(data, packages = c(4,9), "mortvol", "mortvolpct") #ThinDBH vs ThinBBA

##HSFRS
packages_1v2v3v4 <- graph_package_data(data, c(1,2,3,4), "hazardscore", "resistancescore")
packages_1v2v3v4 <- graph_package_data(data, packages = c(1,2,3,4), "hazardscore", "resistancescore") #comparison of post treatment surface fuel treatments
packages_1v2v12v13 <- graph_package_data(data, packages = c(1,2,12,13), "hazardscore", "resistancescore") #comparison of WTL to CTL
pacakges_1v4 <- graph_package_data(data, packages = c(1,4), "hazardscore", "resistancescore") #CT to 115, rxfire vs lop and scatter
packages_1v4v30 <- graph_package_data(data, packages = c(1,4,32), "hazardscore", "resistancescore") #CT to 115, rxfire vs lop and scatter vs clearcut/lop and scatter
packages_1v20 <- graph_package_data(data, packages = c(1,20), "hazardscore", "resistancescore") #CT/Rxfire ThinDBH vs ThinBBA
packages_4v6 <- graph_package_data(data, packages = c(4,6), "hazardscore", "resistancescore") #target fir vs no fir target 
packages_4v7v17 <- graph_package_data(data, packages = c(4,7,17), "hazardscore", "resistancescore") #NFS diameter cap comparison
packages_4v8v9v11 <- graph_package_data(data, packages = c(4,8,9,11), "hazardscore", "resistancescore") #BA residual 115,135,150,180
packages_4v14 <- graph_package_data(data, packages = c(4,14), "hazardscore", "resistancescore") #min DBH 4 v 9
packages_4v19 <- graph_package_data(data, packages = c(4,9), "hazardscore", "resistancescore") #ThinDBH vs ThinBBA

##CBD CBH
packages_1v2v3v4 <- graph_package_data(data, c(1,2,3,4), "cbd", "cbh")
packages_1v2v3v4 <- graph_package_data(data, packages = c(1,2,3,4), "cbd", "cbh") #comparison of post treatment surface fuel treatments
packages_1v2v12v13 <- graph_package_data(data, packages = c(1,2,12,13), "cbd", "cbh") #comparison of WTL to CTL
pacakges_1v4 <- graph_package_data(data, packages = c(1,4), "cbd", "cbh") #CT to 115, rxfire vs lop and scatter
packages_1v4v30 <- graph_package_data(data, packages = c(1,4,32), "cbd", "cbh") #CT to 115, rxfire vs lop and scatter vs clearcut/lop and scatter
packages_1v20 <- graph_package_data(data, packages = c(1,20), "cbd", "cbh") #CT/Rxfire ThinDBH vs ThinBBA
packages_4v6 <- graph_package_data(data, packages = c(4,6), "cbd", "cbh") #target fir vs no fir target 
packages_4v7v17 <- graph_package_data(data, packages = c(4,7,17), "cbd", "cbh") #NFS diameter cap comparison
packages_4v8v9v11 <- graph_package_data(data, packages = c(4,8,9,11), "cbd", "cbh") #BA residual 115,135,150,180
packages_4v14 <- graph_package_data(data, packages = c(4,14), "cbd", "cbh") #min DBH 4 v 9
packages_4v19 <- graph_package_data(data, packages = c(4,9), "cbd", "cbh") #ThinDBH vs ThinBBA

##net rev harvest cost
packages_1v2v3v4 <- graph_package_data(data, c(1,2,3,4), "netrev", "harvestcost")
packages_1v2v3v4 <- graph_package_data(data, packages = c(1,2,3,4), "netrev", "harvestcost") #comparison of post treatment surface fuel treatments
packages_1v2v12v13 <- graph_package_data(data, packages = c(1,2,12,13), "netrev", "harvestcost") #comparison of WTL to CTL
pacakges_1v4 <- graph_package_data(data, packages = c(1,4), "netrev", "harvestcost") #CT to 115, rxfire vs lop and scatter
packages_1v4v30 <- graph_package_data(data, packages = c(1,4,32), "netrev", "harvestcost") #CT to 115, rxfire vs lop and scatter vs clearcut/lop and scatter
packages_1v20 <- graph_package_data(data, packages = c(1,20), "netrev", "harvestcost") #CT/Rxfire ThinDBH vs ThinBBA
packages_4v6 <- graph_package_data(data, packages = c(4,6), "netrev", "harvestcost") #target fir vs no fir target 
packages_4v7v17 <- graph_package_data(data, packages = c(4,7,17), "netrev", "harvestcost") #NFS diameter cap comparison
packages_4v8v9v11 <- graph_package_data(data, packages = c(4,8,9,11), "netrev", "harvestcost") #BA residual 115,135,150,180
packages_4v14 <- graph_package_data(data, packages = c(4,14), "netrev", "harvestcost") #min DBH 4 v 9
packages_4v19 <- graph_package_data(data, packages = c(4,9), "netrev", "harvestcost") #ThinDBH vs ThinBBA

##merch chip
packages_1v2v3v4 <- graph_package_data(data, c(1,2,3,4), "merch", "chip")
packages_1v2v3v4 <- graph_package_data(data, packages = c(1,2,3,4), "merch", "chip") #comparison of post treatment surface fuel treatments
packages_1v2v12v13 <- graph_package_data(data, packages = c(1,2,12,13), "merch", "chip") #comparison of WTL to CTL
pacakges_1v4 <- graph_package_data(data, packages = c(1,4), "merch", "chip") #CT to 115, rxfire vs lop and scatter
packages_1v4v30 <- graph_package_data(data, packages = c(1,4,32), "merch", "chip") #CT to 115, rxfire vs lop and scatter vs clearcut/lop and scatter
packages_1v20 <- graph_package_data(data, packages = c(1,20), "merch", "chip") #CT/Rxfire ThinDBH vs ThinBBA
packages_4v6 <- graph_package_data(data, packages = c(4,6), "merch", "chip") #target fir vs no fir target 
packages_4v7v17 <- graph_package_data(data, packages = c(4,7,17), "merch", "chip") #NFS diameter cap comparison
packages_4v8v9v11 <- graph_package_data(data, packages = c(4,8,9,11), "merch", "chip") #BA residual 115,135,150,180
packages_4v14 <- graph_package_data(data, packages = c(4,14), "merch", "chip") #min DBH 4 v 9
packages_4v19 <- graph_package_data(data, packages = c(4,9), "merch", "chip") #ThinDBH vs ThinBBA

##harvest and growth
packages_1v2v3v4 <- graph_package_data(data, c(1,2,3,4), "harvest", "growth")
packages_1v2v3v4 <- graph_package_data(data, packages = c(1,2,3,4), "harvest", "growth") #comparison of post treatment surface fuel treatments
packages_1v2v12v13 <- graph_package_data(data, packages = c(1,2,12,13), "harvest", "growth") #comparison of WTL to CTL
pacakges_1v4 <- graph_package_data(data, packages = c(1,4), "harvest", "growth") #CT to 115, rxfire vs lop and scatter
packages_1v4v30 <- graph_package_data(data, packages = c(1,4,32), "harvest", "growth") #CT to 115, rxfire vs lop and scatter vs clearcut/lop and scatter
packages_1v20 <- graph_package_data(data, packages = c(1,20), "harvest", "growth") #CT/Rxfire ThinDBH vs ThinBBA
packages_4v6 <- graph_package_data(data, packages = c(4,6), "harvest", "growth") #target fir vs no fir target 
packages_4v7v17 <- graph_package_data(data, packages = c(4,7,17), "harvest", "growth") #NFS diameter cap comparison
packages_4v8v9v11 <- graph_package_data(data, packages = c(4,8,9,11), "harvest", "growth") #BA residual 115,135,150,180
packages_4v14 <- graph_package_data(data, packages = c(4,14), "harvest", "growth") #min DBH 4 v 9
packages_4v19 <- graph_package_data(data, packages = c(4,9), "harvest", "growth") #ThinDBH vs ThinBBA

##growth and mortality
packages_1v2v3v4 <- graph_package_data(data, c(1,2,3,4), "growth", "mortality")
packages_1v2v3v4 <- graph_package_data(data, packages = c(1,2,3,4), "growth", "mortality") #comparison of post treatment surface fuel treatments
packages_1v2v12v13 <- graph_package_data(data, packages = c(1,2,12,13), "growth", "mortality") #comparison of WTL to CTL
pacakges_1v4 <- graph_package_data(data, packages = c(1,4), "growth", "mortality") #CT to 115, rxfire vs lop and scatter
packages_1v4v30 <- graph_package_data(data, packages = c(1,4,32), "growth", "mortality") #CT to 115, rxfire vs lop and scatter vs clearcut/lop and scatter
packages_1v20 <- graph_package_data(data, packages = c(1,20), "growth", "mortality") #CT/Rxfire ThinDBH vs ThinBBA
packages_4v6 <- graph_package_data(data, packages = c(4,6), "growth", "mortality") #target fir vs no fir target 
packages_4v7v17 <- graph_package_data(data, packages = c(4,7,17), "growth", "mortality") #NFS diameter cap comparison
packages_4v8v9v11 <- graph_package_data(data, packages = c(4,8,9,11), "growth", "mortality") #BA residual 115,135,150,180
packages_4v14 <- graph_package_data(data, packages = c(4,14), "growth", "mortality") #min DBH 4 v 9
packages_4v19 <- graph_package_data(data, packages = c(4,9), "growth", "mortality") #ThinDBH vs ThinBBA

#net growth & net growth + harvest
packages_1v2v3v4 <- graph_package_data(data, c(1,2,3,4), "netgrowth", "netgrowthharvest")
packages_1v2v3v4 <- graph_package_data(data, packages = c(1,2,3,4), "netgrowth", "netgrowthharvest") #comparison of post treatment surface fuel treatments
packages_1v2v12v13 <- graph_package_data(data, packages = c(1,2,12,13), "netgrowth", "netgrowthharvest") #comparison of WTL to CTL
pacakges_1v4 <- graph_package_data(data, packages = c(1,4), "netgrowth", "netgrowthharvest") #CT to 115, rxfire vs lop and scatter
packages_1v4v30 <- graph_package_data(data, packages = c(1,4,32), "netgrowth", "netgrowthharvest") #CT to 115, rxfire vs lop and scatter vs clearcut/lop and scatter
packages_1v20 <- graph_package_data(data, packages = c(1,20), "netgrowth", "netgrowthharvest") #CT/Rxfire ThinDBH vs ThinBBA
packages_4v6 <- graph_package_data(data, packages = c(4,6), "netgrowth", "netgrowthharvest") #target fir vs no fir target 
packages_4v7v17 <- graph_package_data(data, packages = c(4,7,17), "netgrowth", "netgrowthharvest") #NFS diameter cap comparison
packages_4v8v9v11 <- graph_package_data(data, packages = c(4,8,9,11), "netgrowth", "netgrowthharvest") #BA residual 115,135,150,180
packages_4v14 <- graph_package_data(data, packages = c(4,14), "netgrowth", "netgrowthharvest") #min DBH 4 v 9
packages_4v19 <- graph_package_data(data, packages = c(4,9), "netgrowth", "netgrowthharvest") #ThinDBH vs ThinBBA


#Table 25
packages_5v6v33 <- graph_package_data(data, c(5,6,33), "mortvolpct", "netrev")

####TABLE 6: NUMBER OF UNRESERVED FORESTS CONDITIONS AND ACRES INCALIFORNIA BY OWNERSHIP AND PERCENT OF TOTAL####
#This table originally had ownership separated out by NFS, Other Fed, State Local, Corporate, and All Owners
stand_acres_by_ownership <- master_cond %>% dplyr::group_by(owncd) %>% summarise(Stand_Count = n(), 
                                                                                 Acres = round(sum(acres)), 
                                                                                 Percent = round(sum(acres)/sum(master_cond$acres)*100))
stand_acres_by_ownership[nrow(stand_acres_by_ownership) + 1,] <- c("All Owners", 
                                                                   sum(stand_acres_by_ownership$Stand_Count), 
                                                                   sum(stand_acres_by_ownership$Acres), 
                                                                   sum(stand_acres_by_ownership$Percent))

dir.create(file.path(getwd(), "CEC figures"), showWarnings = FALSE)
write.csv(stand_acres_by_ownership,  "CEC figures/table_6_stand_acres_by_ownership.csv")

####TABLE 7: NUMBER OF CONDITIONS FORESTED ACRES REPRESTENTED BY FIA FOREST TYPE GROUP####
forest_types <- read.csv(file.path(additional.data, "fia_forest_type.csv"))
names(forest_types) <- c("fortypcd", "Forest.Type", "Group")
group_types <- read.csv(file.path(additional.data, "fia_forest_type_groups.csv"))
names(group_types) <- c("Group", "Forest.Type.Group")

forest_groups <- merge(forest_types, group_types)
master_cond2 <- merge(master_cond, forest_groups)

stand_acres_by_forest_type <- master_cond2 %>% dplyr::group_by(Group, Forest.Type.Group) %>% summarise(Stand_Count = n(), 
                                                                                 Acres = round(sum(acres)), 
                                                                                 Percent = round(sum(acres)/sum(master_cond$acres)*100))

write.csv(stand_acres_by_forest_type,  "CEC figures/table_7_stand_acres_by_forest_type.csv")

####TABLE 8: NUMBER OF CONDITIONS FORESTED ACRES BY PROJECT FOREST TYPE####
stand_acres_by_CEC_ftype <- master_cond[master_cond$CEC_type %in% c("Douglas-fir", "Mixed conifer", "Pine", "Redwood", "True fir"),]

stand_acres_by_CEC_ftype <- stand_acres_by_CEC_ftype %>% dplyr::group_by(CEC_type) %>% summarise(Stand_Count = n(), 
                                                                                                       Acres = round(sum(acres)), 
                                                                                                       Percent = round(sum(acres)/sum(stand_acres_by_CEC_ftype$acres)*100))


stand_acres_by_CEC_ftype[nrow(stand_acres_by_CEC_ftype) + 1,] <- c("All Owners", 
                                                                   sum(stand_acres_by_CEC_ftype$Stand_Count), 
                                                                   sum(stand_acres_by_CEC_ftype$Acres), 
                                                                   sum(stand_acres_by_CEC_ftype$Percent))

write.csv(stand_acres_by_forest_type,  "CEC figures/table_8_stand_acres_by_CEC_ftype.csv")

####TABLE 20: TREE SPECIES GROUPS####
conn.path <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", file.path(project.location, "processor", "db", "scenario_processor_rule_definitions.mdb"))
conn <- odbcDriverConnect(conn.path) #Change the text after "DBH=" to the correct directory for your project
group_names <- sqlFetch(conn, "scenario_tree_species_groups_list", as.is = TRUE) 
species_groups <- sqlFetch(conn, "scenario_tree_species_groups", as.is = TRUE)
odbcCloseAll()

processor.scenario.name <- "scenario1"
 
group_names <- group_names[group_names$scenario_id == processor.scenario.name,]
species_groups <- species_groups[species_groups$scenario_id == processor.scenario.name,]

processor.groups <- merge(group_names, species_groups, all = TRUE)
processor.groups <- processor.groups[,c(5,3)]

processor.groups <- regulartable(processor.groups)
processor.groups <- merge_v(processor.groups)

processor.groups <- set_header_labels( processor.groups, species_label = "Species Group", common_name = "Species")

processor.groups

doc <- read_docx()
doc <- body_add_flextable(doc, value = processor.groups)
print(doc, target = file.path(project.location, "CEC figures", "table 20 species groups.docx"))

####TABLE 24: MORTALITY VOLUME PCT AND NET REV PER ACRE BY SLOPE CLASS AND INITIAL STAND VOLUME####
#limit to package 1
stand_summary1 <- stand_summary[stand_summary$rxpackage == "001",]

#get slope
master_slope <- master_cond[,which(names(master_cond) %in% c("slope", "biosum_cond_id"))]

stand_summary1 <- merge(stand_summary1, master_slope)

stand_summary1$VolClass <- cut(stand_summary1$Initial.TCuFt, breaks = c(0,3000,5000, (max(stand_summary1$Initial.TCuFt)+1)), labels = c("<3000", "3000-5000", ">5000"), right= FALSE)
stand_summary1$SlopeClass <- cut(stand_summary1$slope, breaks = c(0, 40, (max(stand_summary1$slope)+1)), labels = c("Gentle", "Steep"), right= FALSE)

mortvol_netrev1 <- stand_summary1 %>% dplyr::group_by(VolClass, SlopeClass, CEC_type) %>% summarise(Avg_MortVolPct_FOFEM = round(mean(Avg_MortVolPct_FOFEM, na.rm = TRUE) *100),
                                                                                          Avg_max_nr_dpa = round(mean(max_nr_dpa, na.rm = TRUE)))

all_vols <- stand_summary1 %>% dplyr::group_by(SlopeClass, CEC_type) %>% summarise(Avg_MortVolPct_FOFEM = round(mean(Avg_MortVolPct_FOFEM, na.rm = TRUE) *100),
                                                                                          Avg_max_nr_dpa = round(mean(max_nr_dpa, na.rm = TRUE)))
all_vols$VolClass <- "All"

mortvol_netrev1 <- merge(all_vols, mortvol_netrev1, all = TRUE)

acres.by.forest.type <- stand_summary1 %>% dplyr::group_by(CEC_type) %>% summarise(Ftype.Acres = round(sum(acres)))
acres.by.slope <- stand_summary1 %>% dplyr::group_by(SlopeClass, CEC_type) %>% summarise(Acres = round(sum(acres)))

acres.by.slope.ftype <- merge(acres.by.forest.type, acres.by.slope)

acres.by.slope.ftype$AcresPct <- round(acres.by.slope.ftype$Acres/acres.by.slope.ftype$Ftype.Acres *100)

write.csv(mortvol_netrev1,  "CEC figures/table_24_mortvol_netrev_slope.csv")
write.csv(acres.by.slope.ftype,  "CEC figures/table_24_acre_pct_by_slope.csv")

####TABLE 27: SEQUENCE SELECTION FREQUENCY UNDER OPTIMIZATION SCENARIO####
#Get "best" treatments 
stand_summary$best <- as.character(stand_summary$best)
optimal_tmts <- stand_summary[stand_summary$best == "Y" & !is.na(stand_summary$best),]

opt_sequence_acres <- optimal_tmts %>% group_by(rxpackage) %>% summarise(Acres = round(sum(acres)))
opt_sequence_acres$AcresPct <- round(opt_sequence_acres$Acres/sum(opt_sequence_acres$Acres)*100)
opt_sequence_acres$rxpackage <- as.integer(as.numeric(opt_sequence_acres$rxpackage))

opt_sequence_acres <- merge(opt_sequence_acres, package_labels, by.x = "rxpackage", by.y = "Package", all.x = TRUE)
opt_sequence_acres$Terrain[opt_sequence_acres$Surface.fuel.method %in% c("Pile/burn", "Masticate") | opt_sequence_acres$Harvest.System == "CTL"] <- "Not steep"
write.csv(opt_sequence_acres,  "CEC figures/table_27_acres_by_sequence.csv")

####TABLE 28: RESULTS FOR IMPROVEMENT IN FIRE HAZARD METRICS AS PERCENTAGE OF GROW ONLY ALTERNATIVE AVERAGED OVER THE 40 YEAR PERIOD####
#NOTE: This is now better described as the percentage difference in average scoring component value for 
#optimal treatments and grow only treatments.
stand_summary$best <- as.character(stand_summary$best)
optimal_tmts <- stand_summary[stand_summary$best == "Y" & !is.na(stand_summary$best),]
optimal_31 <- rbind(optimal_tmts, stand_summary[stand_summary$rxpackage == "031",])
optimal_31$Grow_Only[optimal_31$rxpackage == "031"] <- "Y"
optimal_31$Grow_Only[optimal_31$rxpackage != "031"] <- "N"

fire_improvement <- optimal_31 %>% dplyr::group_by(owncd, Grow_Only) %>% summarise(CBD = mean(Avg_CBD_Score), 
                                                                                   CBH = mean(Avg_CBH_Score),
                                                                                   PERRESBA = mean(Avg_PERRESBA_Score),
                                                                                   SurVolRatio = mean(Avg_SurvVolRatio_Score),
                                                                                   Fire_Resistance = mean(Avg_FRS),
                                                                                   Torch_Index = mean(Avg_Torch_Index_Score),
                                                                                   Ptorch_Sev = mean(Avg_Ptorch_Sev_Score),
                                                                                   Surf_Flame_Sev = mean(Avg_Surf_Flame_Sev_Score),
                                                                                   MortVolPct_Sev = mean(Avg_MortVolPct2_Sev_Score),
                                                                                   Hazard = mean(Avg_HS_Sev)
                                                                                   )

fire_improvement2 <- melt(fire_improvement, id.vars = c("owncd", "Grow_Only"))
fire_improvement2 <- fire_improvement2[with(fire_improvement2, order(owncd, variable, Grow_Only)),]
fire_improvement3 <- aggregate(value ~ owncd + variable, data = fire_improvement2, FUN = diff )
names(fire_improvement3)[3] <- "diff"
fire_improvement4 <- merge(fire_improvement3, fire_improvement2[fire_improvement2$Grow_Only == "Y",])
fire_improvement4$Grow_Only <- NULL
names(fire_improvement4)[which(names(fire_improvement4) == "value")] <- "Grow_Only"
fire_improvement4$diffpct <- abs(round((fire_improvement4$diff/fire_improvement4$Grow_Only)*100))
fire_improvement5 <- dcast(fire_improvement4, variable ~ owncd, value.var = "diffpct")

write.csv(fire_improvement5,  "CEC figures/table_28_fire_score_improvement.csv")

####TABLE 29: AVREAGE COMPOSITE HAZARD SCORE UNDER OPTIMAL AS PCT OF GROW ONLY BY OWNERSHIP AND FOREST TYPE####
stand_summary$best <- as.character(stand_summary$best)
optimal_tmts <- stand_summary[stand_summary$best == "Y" & !is.na(stand_summary$best),]
optimal_31 <- rbind(optimal_tmts, stand_summary[stand_summary$rxpackage == "031",])
optimal_31$Grow_Only[optimal_31$rxpackage == "031"] <- "Y"
optimal_31$Grow_Only[optimal_31$rxpackage != "031"] <- "N"

score_improvement <- optimal_31 %>% dplyr::group_by(owncd, Grow_Only, CEC_type) %>% summarise(Fire_Resistance = mean(Avg_FRS),
                                                                                   Hazard = mean(Avg_HS_Sev)
)

score_improvement2 <- melt(score_improvement, id.vars = c("owncd", "Grow_Only", "CEC_type"))
score_improvement2 <- score_improvement2[with(score_improvement2, order(CEC_type, variable,owncd, Grow_Only)),]
score_improvement3 <- aggregate(value ~ CEC_type + owncd + variable, data = score_improvement2, FUN = diff )
names(score_improvement3)[which(names(score_improvement3) == "value")] <- "diff"
score_improvement4 <- merge(score_improvement3, score_improvement2[score_improvement2$Grow_Only == "Y",])
score_improvement4$Grow_Only <- NULL
names(score_improvement4)[which(names(score_improvement4) == "value")] <- "Grow_Only"
score_improvement4$diffpct <- abs(round((score_improvement4$diff/score_improvement4$Grow_Only)*100))
FRS_ftype <- score_improvement4[score_improvement4$variable == "Fire_Resistance",]
FRS_ftype <- dcast(FRS_ftype, owncd ~ CEC_type, value.var = "diffpct")

HS_ftype <- score_improvement4[score_improvement4$variable == "Hazard",]
HS_ftype <- dcast(HS_ftype, owncd ~ CEC_type, value.var = "diffpct")

write.csv(FRS_ftype,"CEC figures/table_29_FRS_improvement_ftype.csv")
write.csv(HS_ftype, "CEC figures/table_29_HS_improvement_ftype.csv")

####TABLE 30: INITIAL AND AVERAGE MORT VOL PCT OVER 40 YEARS FOR THE OPTIMAL AND GROW-ONLY TREATMENTS####
initial.optimal.growonly <- relevant.data2
initial.optimal.growonly$Year1[initial.optimal.growonly$Year == "1"] <- "Y"
initial.optimal.growonly$Year1[initial.optimal.growonly$Year != "1"] <- "N"

iog <- initial.optimal.growonly[paste(initial.optimal.growonly$biosum_cond_id,initial.optimal.growonly$rx) %in% paste(cycle1_best_rx$biosum_cond_id, cycle1_best_rx$rx),]

iog_31 <-  initial.optimal.growonly[initial.optimal.growonly$rx == "999",]

iog.summary <- iog %>% group_by(Year1, CEC_type) %>% summarise(Avg_MortVolPct_FOFEM = round(mean(MortVolPct_FOFEM)*100))
iog.summary2 <- dcast(iog.summary, CEC_type ~ Year1, value.var = "Avg_MortVolPct_FOFEM")
iog_31.summary <- iog_31 %>% group_by(CEC_type) %>% summarise(Avg_MortVolPct_FOFEM = round(mean(MortVolPct_FOFEM)*100))

iog2 <-  merge(iog.summary2, iog_31.summary)
names(iog2) <- c("Forest Type", "Optimal", "Initial", "Grow-Only")

write.csv(HS_ftype, "CEC figures/table_30_mortvolpct_ftype.csv")

####TABLE 33: ANNUAL AVERAGE ALLOCATION OF GROSS REVENUE####
stand_summary$best <- as.character(stand_summary$best)
optimal_tmts <- stand_summary[stand_summary$best == "Y" & !is.na(stand_summary$best),]

revenue.allocation <- optimal_tmts %>% group_by(owncd) %>% summarise(Harvest.Costs = round(mean(harvest_onsite_cpa)/40),
                                                                     Haul.Chip.Costs = round(mean(haul_chip_cpa)/40),
                                                                     Haul.Merch.Costs = round(mean(haul_merch_cpa)/40),
                                                                     Total.Haul.Costs = round((mean(haul_merch_cpa) + mean(haul_chip_cpa))/40),
                                                                     Net.Revenue = round(mean(max_nr_dpa)/40))
# 
# revenue.allocation <- optimal_tmts %>% group_by(owncd) %>% summarise(Harvest.Costs = round(mean(harvest_onsite_cpa)),
#                                                                      Haul.Costs = round(mean(haul_chip_cpa)),
#                                                                      Net.Revenue = round(mean(max_nr_dpa)))

revenue.allocation[nrow(revenue.allocation) + 1,] <- c("Annual Total", 
                                                       sum(revenue.allocation$Harvest.Costs), 
                                                       sum(revenue.allocation$Haul.Chip.Costs),
                                                       sum(revenue.allocation$Haul.Merch.Costs),
                                                       sum(revenue.allocation$Total.Haul.Costs),
                                                       sum(revenue.allocation$Net.Revenue))

# revenue.allocation[nrow(revenue.allocation) + 1,] <- c("40 Year Total", 
#                                                        sum(as.numeric(revenue.allocation$Harvest.Costs[1:(nrow(revenue.allocation)-1)]))*40, 
#                                                        sum(as.numeric(revenue.allocation$Haul.Costs[1:(nrow(revenue.allocation) -1)]))*40, 
#                                                        sum(as.numeric(revenue.allocation$Net.Revenue[1:(nrow(revenue.allocation) -1)]))*40)


write.csv(revenue.allocation, "CEC figures/table_33_avg_rev_alloc_divided_by_40_years.csv")

####TABLE 36: RATIO OF WOOD OUTPUT FROM OPTIMAL BIOSUM SCENARIO TO 2012 HARVEST LEVELS####
#Note: 2012 harvest levels comes from McIver et al, so this is just biosum raw wood output in cubic ft

stand_summary$best <- as.character(stand_summary$best)
optimal_tmts <- stand_summary[stand_summary$best == "Y" & !is.na(stand_summary$best),]

wood.output <- optimal_tmts %>% group_by(owncd) %>% summarise(mean.Merch.Yield.cf = mean(merch_yield_cf),
                                                                     mean.Chip.Yield.Cf = mean(chip_yield_cf),
                                                                     mean.All.Wood.cf = mean(chip_yield_cf + merch_yield_cf),
                                                                     sum.Merch.Yield.cf = sum(merch_yield_cf),
                                                                     sum.Chip.Yield.Cf = sum(chip_yield_cf),
                                                                     sum.All.Wood.cf = sum(chip_yield_cf + merch_yield_cf))

write.csv(wood.output, "CEC figures/table_36_wood_output_raw.csv")

####Figure 32: Merchantable Volume and woody biomass feedstock as pct of total harvested volume####

conn.path <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", file.path(project.location, "processor", "db", "scenario_processor_rule_definitions.mdb"))
conn <- odbcDriverConnect(conn.path) #Change the text after "DBH=" to the correct directory for your project
diam_groups <- sqlFetch(conn, "scenario_tree_diam_groups", as.is = TRUE) 
odbcCloseAll()

processor.scenario.name <- "scenario1"

conn.path <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", file.path(project.location, "processor", processor.scenario.name, "db", "scenario_results.mdb"))
conn <- odbcDriverConnect(conn.path) #Change the text after "DBH=" to the correct directory for your project
tree_vol_val <- sqlFetch(conn, "tree_vol_val_by_species_diam_groups", as.is = TRUE) 
odbcCloseAll()


#This is for optimal
stand_summary$best <- as.character(stand_summary$best)
optimal_tmts <- stand_summary[stand_summary$best == "Y" & !is.na(stand_summary$best),]
optimal_stands <- optimal_tmts[,which(names(optimal_tmts) %in% c("biosum_cond_id", "rxpackage"))]

opt_vol_val <- merge(optimal_stands, tree_vol_val, by = c("biosum_cond_id", "rxpackage"), all.x = TRUE)

opt_vol_val2 <- opt_vol_val %>% group_by(diam_group) %>% summarise(sum.merch_vol_cf = sum(merch_vol_cf),
                                                                   sum.chip_vol_cf = sum(chip_vol_cf),
                                                                   total.sum.vol.cf = sum(merch_vol_cf, chip_vol_cf) 
                                                                   )

opt_vol_val2 <- opt_vol_val2[-which(opt_vol_val2$diam_group == "999"),]
opt_vol_val3 <- merge(opt_vol_val2, diam_groups[trimws(diam_groups$scenario_id) == processor.scenario.name,], )
opt_vol_val3$merch.pct <- round((opt_vol_val3$sum.merch_vol_cf/opt_vol_val3$total.sum.vol.cf)*100)
opt_vol_val3$chip.pct <- round((opt_vol_val3$sum.chip_vol_cf/opt_vol_val3$total.sum.vol.cf)*100)

opt_vol_val4 <- melt(opt_vol_val3, id.vars = "diam_class", measure.vars = c("merch.pct", "chip.pct"))
opt_vol_val4$diam_class <- as.factor(trimws(opt_vol_val4$diam_class))
opt_vol_val4$diam_class <- factor(opt_vol_val4$diam_class,levels(opt_vol_val4$diam_class)[c(5,3,2,6,4,1)])

custom_theme <- theme_set(theme_bw(base_size = 20))

graph <- ggplot(opt_vol_val4, aes(variable, value, color = diam_class, fill = diam_class)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values =  brewer.pal(6, "Set2"), name = "Diameter Class") +
  scale_color_manual(values =  brewer.pal(6, "Set2"), name = "Diameter Class") + 
  labs(x="", y="Proportion of Total Volume", title="") + 
  theme(legend.position = "right") + 
  scale_x_discrete(labels = c("Merch Total", "Chip Total"))
graph

ggsave("CEC figures/figure32.emf", plot = graph, device = "emf", width = 10, height = 10)

  
r######THE CODE BELOW IS FOR GRAPHS DEVELOPED FOR THE DIADB USERS GROUP MEETING######
#This is not as well documented, but they may not be needed for the CEC report
#I left them in case someone wants to use it for developing additional figures. 

# ####STAND GRAPHS####
# graph_stand_data_4_7_17 <- function(stand.data, variable) {
#   #get only effective
#   stand.data_old <- stand.data
#   
#   #effective tmt test
#   pattern <- c("biosum_cond_id", "rxpackage", "overall_effective_yn")
#   effective.test <- stand.data[,c(which(grepl(paste0(pattern, collapse = "|"), names(stand.data))))]
#   effective.test[,ncol(effective.test) + 1] <- 0
#   names(effective.test)[ncol(effective.test)] <- "binary"
#   effective.test$binary[effective.test$overall_effective_yn == "Y"] <- 1
#   
#   effective.test2 <- effective.test %>% group_by(biosum_cond_id) %>% summarise(binary_SUM = sum(binary))
#   
#   effective.stands <- effective.test2$biosum_cond_id[effective.test2$binary_SUM > 0]
#   
#   effective.stand.data <- stand.data[stand.data$biosum_cond_id %in% effective.stands,]
#   
#   #stands where RTpa for package 7 is greater than package 4
#   pattern <- c("biosum_cond_id", "rxpackage", "RTpa_SUM")
#   rtpa.test <- effective.stand.data[,c(which(grepl(paste0(pattern, collapse = "|"), names(effective.stand.data))))]
#   rtpa.test <- rtpa.test[rtpa.test$rxpackage %in% c(7,4),]
#   
#   rtpa.test2 <- dcast(rtpa.test, biosum_cond_id ~ rxpackage, value.var = "rxpackage")
#   rtpa.test2$test <- rtpa.test2$`7` > rtpa.test2$`4`
#   
#   rtpa.7gthan4 <- rtpa.test2$biosum_cond_id[rtpa.test2$test == TRUE]
#   
#   filtered.stands <- effective.stand.data[effective.stand.data$biosum_cond_id %in% rtpa.7gthan4,]
# 
#   packages <- as.character(unique(stand.data$rxpackage))
#   numpackages <- length(packages)
#   ##PARAMETERS FOR ALL GRAPHS##
#   cbPalette <- c("#CC79A7", "#56B4E9", "#009E73", "#E69F00")
#   
#   custom_theme <- theme_set(theme_bw(base_size = 20)) +
#     theme(plot.title = element_text(size = 16, hjust = 0.5),
#           axis.text.x=element_text(size=14), 
#           axis.text.y=element_text(size=14),
#           axis.title = element_text(size = 16),
#           strip.text = element_text(size = 14),
#           legend.position="right",
#           legend.key = element_rect(colour = "gray90"), 
#           legend.key.size = unit(2, "line"),
#           legend.spacing = unit(2, "line"), 
#           legend.text = element_text(size = 12),
#           legend.title = element_text(size = 12, face = "bold"))
#   
#   relevant_packages <- package_labels[package_labels$Package %in% packages,]
#   names(relevant_packages) <- c("Sequence", "FVS Style", "BA Threshold", "Max Private \n DBH", "Max Public \n DBH", "Min DBH", "Residual BA \n (less amt)", "Species \n Pressure", "Surface Fuel \n Treatment", "Harvest \n System")
#   
#   tt <- ttheme_default(base_size = 14)
#   t<- tableGrob(relevant_packages, rows = NULL, theme = tt)
#   
#   layout <- rbind(c(1,1,1,1,1),
#                   c(1,1,1,1,1),
#                   c(1,1,1,1,1),
#                   c(NA,3,3,3,NA))
#   
#   final <- NULL
#   for (i in 1:numpackages) {
#     if (i != numpackages) {
#       add <- paste0(packages[i], "v")
#       final <- paste0(final, add)
#     } else {
#       add <- packages[i]
#       final <- paste0(final, add)
#     }
#   }
# 
#   pattern <- c("Owner", "rxpackage", variable)
#   
#   data <- filtered.stands[,c(which(grepl(paste0(pattern, collapse = "|"), names(filtered.stands))))]
#   
#   data2 <- melt(data, id.vars = c("Owner", "rxpackage"))
#   data2$rxpackage <- as.factor(data2$rxpackage)
#   graph <- ggplot(data2, aes(rxpackage, value, fill = Owner)) +
#     geom_boxplot() + 
#     scale_fill_manual(values = cbPalette[1:length(unique(data2$Owner))]) +
#     custom_theme +
#     labs(x="RX Package", y=variable) 
#   
#   graph2 <- grid.arrange(grobs = list(graph, t), nrow = 2, layout_matrix = layout)
#   
#   ggsave(filename = paste0(variable, final, ".emf"), plot = graph2, device = "emf", width = 13, height = 8.5)
#   
# }
# 
# avg_cbd_score <- graph_stand_data_4_7_17 (stands, "Avg_CBD_Score")
# avg_cbd <- graph_stand_data_4_7_17 (stands, "Avg_Canopy_Density")
# avg_cbh_score <- graph_stand_data_4_7_17 (stands, "Avg_CBH_Score")
# avg_cbh <- graph_stand_data_4_7_17 (stands, "Avg_CBH")
# avg_PERRESBA_score <- graph_stand_data_4_7_17 (stands, "Avg_PERRESBA_Score")
# avg_PERRESBA_score <- graph_stand_data_4_7_17 (stands, "Avg_PERRESBA")
# avg_SurvVolRatio_score <- graph_stand_data_4_7_17 (stands, "Avg_SurvVolRatio_Score")
# avg_FRS <- graph_stand_data_4_7_17 (stands, "Avg_FRS")
# 
# 
# ####FIA Analysis#####
# best.summary <- stand_summary[stand_summary$best == "Y",] %>% group_by(CEC_type, owngrpcd) %>% summarise(Best_Treatment_Acres = sum(acres), 
#                                                                     Negative_NetRev_Acres = sum(acres[max_nr_dpa < 0]),
#                                                                     Avg_FRS_Treated = mean(post_variable1_value, na.rm= TRUE),
#                                                                     Avg_FRS_Improvement = mean(variable1_change, na.rm= TRUE), 
#                                                                     Avg_MerchYield_GT = mean(merch_yield_gt, na.rm= TRUE), 
#                                                                     Avg_ChipYield_GT = mean(chip_yield_gt, na.rm= TRUE),
#                                                                     Merch_Chip_Ratio_GT = sum(merch_yield_gt/sum(merch_yield_gt, chip_yield_gt)),
#                                                                     Avg_Max_NetRev_dpa = mean(max_nr_dpa), 
#                                                                     Avg_TmtCost_dpa = mean(harvest_onsite_cpa),
#                                                                     Avg_MerchVal_dpa = mean(merch_val_dpa),
#                                                                     Avg_ChipVal_dpa = mean(chip_val_dpa),
#                                                                     Merch_Chip_Ratio_Value = sum(merch_val_dpa/sum(merch_val_dpa, chip_val_dpa)),
#                                                                     Max_NetRev_Acres = sum(max_nr_dpa*acres), 
#                                                                     FRS_Improvement_Acres = sum(variable1_change * acres))
# 
# data.table1 <- merge(treated.area[,c(1,4,5)], best.summary)
# 
# data.table1$Best_Treatment_Pct <- data.table1$Best_Treatment_Acres/data.table1$Treatable_Acres*100
# data.table1$Negative_NetRev_Pct <- data.table1$Negative_NetRev_Acres/data.table1$Treatable_Acres*100
# 
# 
# data.table0 <- data.table1
# data.table0$Filter <- as.factor("0")
# 
# data.table <- rbind(data.table0, data.table200)
# 
# write.csv(data.table, "FIAdbmtg_data2.csv")
# 
# netrev_summary <- data.table %>% group_by(owngrpcd, Filter) %>% summarise(Acres_Treated = sum(Best_Treatment_Acres),
#                                                                           Total_Net_rev = sum(Max_NetRev_Acres), 
#                                                                           Total_FRS_Improvement = sum(FRS_Improvement_Acres))
# 
# write.csv(netrev_summary, "FIAdbmtg_summary2.csv")
# 
# ###MERCH VOL GRAPH###
# pattern <- c("biosum_cond_id", "rxpackage", "rxcycle", "merch_vol_cf_exp", "net_rev_dollars_exp")
# stand_cr2 <- stand_cr[,c(which(grepl(paste0(pattern, collapse = "|"), names(stand_cr))))]
# 
# stand_cr3 <- merge(stand_cr2, cond.relevant.data)
# 
# #Limit to relevant forest types
# stand_cr4 <- stand_cr3[stand_cr3$CEC_type %in% c("Douglas-fir", "Pine", "Mixed conifer", "True fir"),]
# stand_cr4 <- stand_cr4[stand_cr4$owngrpcd %in% c("10", "40"),]
# stand_cr4$profitable[stand_cr4$net_rev_dollars_exp < -200] <- "Not Profitable"
# stand_cr4$profitable[stand_cr4$net_rev_dollars_exp >= -200] <- "Profitable"
# 
# merch_vol_cf_by_cycle <- stand_cr4 %>% group_by(CEC_type, owngrpcd, rxcycle) %>% summarise(all_merch_vol = sum(merch_vol_cf_exp),
#                                                                                            profitable_merch_vol = sum(merch_vol_cf_exp[net_rev_dollars_exp > -200]))
#                                                                                            
# merch_vol_cf_by_cycle$owngrpcd <- as.factor(merch_vol_cf_by_cycle$owngrpcd)
# merch_vol_cf_by_cycle$Owner <- NA
# merch_vol_cf_by_cycle$Owner[merch_vol_cf_by_cycle$owngrpcd == "10"] <- "NFS"
# merch_vol_cf_by_cycle$Owner[merch_vol_cf_by_cycle$owngrpcd == "40"] <- "Private"
# 
# write.csv(merch_vol_cf_by_cycle, "merch_vol_graph_data.csv")
# 
# merch_vol_cf_by_cycle2 <- melt(merch_vol_cf_by_cycle, id.vars = c("rxcycle", "owngrpcd", "CEC_type", "Owner"))
# 
# merch_vol_cf_by_cycle2$variable <- as.character(merch_vol_cf_by_cycle2$variable)
# merch_vol_cf_by_cycle2$variable[merch_vol_cf_by_cycle2$variable == "all_merch_vol"] <- "All"
# merch_vol_cf_by_cycle2$variable[merch_vol_cf_by_cycle2$variable == "profitable_merch_vol"] <- "Profitable"
# 
# 
# custom_theme <- theme_set(theme_bw(base_size = 20)) +
#   theme(plot.title = element_text(size = 16, hjust = 0.5),
#         axis.text.x=element_text(size=14), 
#         axis.text.y=element_text(size=14),
#         axis.title = element_text(size = 16),
#         strip.text = element_text(size = 14),
#         legend.position="right",
#         legend.key = element_rect(colour = "gray90"), 
#         legend.key.size = unit(2, "line"),
#         legend.spacing = unit(2, "line"), 
#         legend.text = element_text(size = 12),
#         legend.title = element_text(size = 12, face = "bold"))
# 
# cbPalette <- c("#CC79A7", "#56B4E9", "#009E73", "#E69F00")
# 
# 
# graph <- ggplot(merch_vol_cf_by_cycle, aes(rxcycle, (all_merch_vol*10^-9), color = Owner, fill = Owner, group = Owner)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   facet_grid(.~ CEC_type) + 
#   custom_theme + 
#   labs(x="RX Cycle", y=paste("Merchantable volume", "(billion cubic feet)", sep = "\n"), title="") +
#   scale_fill_manual(values=cbPalette[1:2], name="Owner") + 
#   scale_color_manual(values=cbPalette[1:2], name="Owner")
# 
# ggsave(filename = "merch_vol_graph.png", plot = graph, device = "png", width = 11, height = 5)
# 
# ###MERCH VOL ONLY STANDS THAT MADE MONEY###
# profitable_merch_vol_cf <- stand_cr4[stand_cr4$net_rev_dollars_exp > -200,]
# 
# profitable_merch_vol_cf_by_cycle <- profitable_merch_vol_cf %>% group_by(CEC_type, owngrpcd, rxcycle) %>% summarise(Cycle_Merch_Vol_CF = sum(merch_vol_cf_exp))
# profitable_merch_vol_cf_by_cycle$owngrpcd <- as.factor(profitable_merch_vol_cf_by_cycle$owngrpcd)
# profitable_merch_vol_cf_by_cycle$Owner <- NA
# profitable_merch_vol_cf_by_cycle$Owner[profitable_merch_vol_cf_by_cycle$owngrpcd == "10"] <- "NFS"
# profitable_merch_vol_cf_by_cycle$Owner[profitable_merch_vol_cf_by_cycle$owngrpcd == "40"] <- "Private"
# 
# graph <- ggplot(profitable_merch_vol_cf_by_cycle, aes(rxcycle, (Cycle_Merch_Vol_CF*10^-9), color = Owner, fill = Owner, group = Owner)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   facet_grid(.~ CEC_type) + 
#   custom_theme + 
#   labs(x="RX Cycle", y=paste("Merchantable volume", "(billion cubic feet)", sep = "/n"), title="") +
#   scale_fill_manual(values=cbPalette[1:2], name="Owner") + 
#   scale_color_manual(values=cbPalette[1:2], name="Owner")
# 
# ggsave(filename = "profitable_merch_vol_graph.emf", plot = graph, device = "emf", width = 9, height = 5)
# 
# 
# #Compare two runs
# data2 <- read.csv("FIAdbmtg_data_nr_gt_neg200.csv", as.is = TRUE )
# data2[,1] <- -200
# names(data2)[1] <- "NetRev_Filter"
# 
# data.table1$NetRev_Filter <- 0
# 
# data3 <- rbind(data2, data.table1)
# 
# data4 <- melt(data3, id.vars = c("CEC_type", "NetRev_Filter", "owngrpcd", "Treatable_Acres"))
# 
# data4$NetRev_Filter <- as.factor(data4$NetRev_Filter)
# data4$owngrpcd <- as.factor(data4$owngrpcd)
# 
# graph <- ggplot(data4, aes(owngrpcd, value, color = NetRev_Filter, fill = NetRev_Filter, group = NetRev_Filter)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   facet_grid(variable ~ CEC_type, scales = "free") + 
#   custom_theme + 
#   labs(x="Owner")
# 
# ggsave(filename = "core_compare.png", plot = graph, device = "png", width = 9, height = 35)
# 
# conn <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=H:/cec_20170915/core/scenario20180418/db/scenario_results.mdb") #Change the text after "DBH=" to the correct directory for your project
# netrev200 <- sqlFetch(conn, "product_yields_net_rev_costs_summary_by_rx", as.is = TRUE)
# cycle1_best_rx200 <- sqlFetch(conn, "cycle1_best_rx_summary", as.is = TRUE)
# odbcCloseAll()
# 
# conn <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=H:/cec_20170915/core/scenario20180418_2/db/scenario_results.mdb") #Change the text after "DBH=" to the correct directory for your project
# netrev0 <- sqlFetch(conn, "product_yields_net_rev_costs_summary_by_rx", as.is = TRUE)
# cycle1_best_rx0 <- sqlFetch(conn, "cycle1_best_rx_summary", as.is = TRUE)
# odbcCloseAll()
# 
# best0 <- cycle1_best_rx0[,c(1,2,3)]
# best200 <- cycle1_best_rx200[,c(1,2,3)]
# # netrev3 <- netrev2 %>% group_by(biosum_cond_id, rx) %>% tally()
# 
# netrev0.2 <- merge(best0, netrev0)
# netrev200.2 <- merge(best200, netrev200)
# netrev0.2$Filter <- as.factor(0)
# netrev200.2$Filter <- as.factor(-200)
# 
# data2 <- rbind(netrev0.2, netrev200.2)
# 
# data3 <- merge(data2, cond.species)
# 
# 
# data3$rxcycle <- as.factor(data3$rxcycle)
# data3$owngrpcd <- as.factor(data3$owngrpcd)
# 
# 
# data4 <- data3[data3$CEC_type %in% c("Douglas-fir", "Pine", "Mixed conifer", "True fir"),]
# 
# graph <- ggplot(data4, aes(rxcycle, max_nr_dpa, fill = owngrpcd)) +
#   geom_boxplot() +
#   facet_grid(Filter~ CEC_type) + 
#   custom_theme
# 
# graph
#   
# ggsave(filename = "rxcyclegraph.png", plot = graph, device = "png", width = 12, height = 12)
# 
