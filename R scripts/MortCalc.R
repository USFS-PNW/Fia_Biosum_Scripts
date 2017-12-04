#This script was created by Carlin Starrs in 2017. 
#It takes the master and ref_master mdb files from a project the 
#all_variants mortality data (created by Terrie Jane using FOFEM), uses the FVS
#Western Species Translator to convert the all_variants species to the FVS species, 
#and assign a mortrate to species by DBH class. 

#Make sure you have the Microsoft Access Databse Engine Driver https://www.microsoft.com/en-us/download/confirmation.aspx?id=23734
#and you are using 32-bit R (set by going to Tools -> Global Options in RStudio)

#setwd("D:/Dropbox/Carlin/Berkeley/biosum/MortCalc")
#D:/cec_20170915/db/ref_master.mdb
setwd("E:/Dropbox/Carlin/Berkeley/biosum/MortCalc")

library("dplyr")
library("RODBC")
options(scipen = 999)
conn <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=F:/BiosumVM/cec_20170915/db/ref_master.mdb")
tree_species_master <- sqlFetch(conn, "tree_species", as.is = TRUE) #import tree_species table from ref_master.mdb
odbcCloseAll()

conn <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=D:/cec_20170915/db/master.mdb")
master_tree <- sqlFetch(conn, "tree", as.is = TRUE) #import tree table from master.mdb
master_cond <- sqlFetch(conn, "cond", as.is = TRUE) #import cond table from master.mdb
master_plot <- sqlFetch(conn, "plot", as.is = TRUE) #import plot table from master_mdb
odbcCloseAll()

#The section below takes the master_tree table and links it with master_cond and master_plot to get the variant data
#and get a complete list of species and variant in the dataset to know what MortCalc values are needed 
master_spcd <- data.frame("biosum_cond_id" = as.character(master_tree$biosum_cond_id), "spcd" = as.character(master_tree$spcd)) #master_spcd initially is just biosum_cond_id and spcd
master_cond <- data.frame("biosum_cond_id" = as.character(master_cond$biosum_cond_id), "biosum_plot_id" = as.character(master_cond$biosum_plot_id)) #get biosum_plot_id and biosum_cond_id
master_plot <- data.frame("biosum_plot_id" = as.character(master_plot$biosum_plot_id), "FVS_VARIANT" = as.character(master_plot$fvs_variant))#get biosum_plot_id and FVS_VARIANT
master_spcd <- merge(master_spcd, master_cond, by = "biosum_cond_id", all = TRUE) #merge tables created above to have spcd and FVS_VARIANT linked
master_spcd <- merge(master_spcd, master_plot, by = "biosum_plot_id", all = TRUE)
master_spcd_nas <- subset(master_spcd, !complete.cases(master_spcd)) #ISSUE: Why do these exist?
master_spcd <- na.omit(master_spcd) #remove NAs
master_spcd$Species <- sprintf("%03d", master_spcd$spcd) #convert spcd to 3 digit "Species" value
master_spcd <- master_spcd[,c(4,5)] #trim table to just Species and FVS_VARIANT
master_spcd <- unique(master_spcd[,c("FVS_VARIANT", "Species")]) #limit to unique FVS_VARIANT & Species combinations

#tree_species_master <- read.csv("tree_species.csv")
#speciesgroups <- read.csv("speciesgroups.csv")
#mortprob <- read.csv("mortprob.csv")

#The section below takes the FVS Western Species Translator data and puts it into 
#a format that allows for linking the USDA_PLANTS_SYMBOL code used by FOFEM and translate
#it to a 2-letter FVS_SPECIES code by variant. OG_FVS_SPECIES is the default FVS species code for 
#that USDA_PLANTS_SYMBOL code, which changes depending on the variant.
species_crosswalk <- read.csv("species_crosswalk.csv")
species_crosswalk <- species_crosswalk[,c(1,2,3,8,15,18,22)] #trim to relevant columns
names(species_crosswalk)[1] <- "USDA_PLANTS_SYMBOL" #rename columns 
names(species_crosswalk)[2] <- "Species"
names(species_crosswalk)[3] <- "OG_FVS_SPECIES"
species_crosswalk$Species <- sprintf("%03d", species_crosswalk$Species) #convert Species to 3-digits

#Convert "variant_mapped_to" columns to a column with variant name and the FVS_SPECIES code
species_crosswalk_CA <- species_crosswalk[,c(1:4)] 
species_crosswalk_CA$FVS_VARIANT <- "CA"
names(species_crosswalk_CA)[4] <- "FVS_SPECIES"

species_crosswalk_NC <- species_crosswalk[,c(1:3,5)]
species_crosswalk_NC$FVS_VARIANT <- "NC"
names(species_crosswalk_NC)[4] <- "FVS_SPECIES"

species_crosswalk_SO <- species_crosswalk[,c(1:3,6)]
species_crosswalk_SO$FVS_VARIANT <- "SO"
names(species_crosswalk_SO)[4] <- "FVS_SPECIES"

species_crosswalk_WS <- species_crosswalk[,c(1:3,7)]
species_crosswalk_WS$FVS_VARIANT <- "WS"
names(species_crosswalk_WS)[4] <- "FVS_SPECIES"

species_crosswalk <- rbind(species_crosswalk_CA, species_crosswalk_NC, species_crosswalk_SO, species_crosswalk_WS) #bind variant tables back together

#The section below imports MortCalc data from the ALL_VARIANTs file and trims it
all_mort <- read.csv("ALL VARIANT_MORTALITY 2 TO 8FTFL.csv")
all_mort <- all_mort[,c(1,2,3,4)]
names(all_mort)[1] <- "FVS_VARIANT"
names(all_mort)[2] <- "USDA_PLANTS_SYMBOL"
names(all_mort)[3] <- "DBH_CLASS"
names(all_mort)[4] <- "MORTRATE"

#The following renames master_spcd to all_species so it can be manipulated without
#losing the original and merges it with the species_crosswalk table to link the
#USDA_PLANTS_SYMBOL values to the list of species in this project
all_species <- master_spcd
all_species_usda <- merge(all_species, species_crosswalk, by = c("Species", "FVS_VARIANT"))
all_species_mortrate<- merge(all_species_usda, all_mort, by = "USDA_PLANTS_SYMBOL", all = TRUE)

names(all_species_mortrate)[3] <- "FVS_VARIANT"
names(all_species_mortrate)[6] <- "ALL_VARIANT"

all_species_mortrate$FVS_VARIANT <- as.character(all_species_mortrate$FVS_VARIANT)
all_species_mortrate$ALL_VARIANT <- as.character(all_species_mortrate$ALL_VARIANT)

assigned <- subset(all_species_mortrate, FVS_VARIANT == ALL_VARIANT)
unassigned <- subset(all_species_mortrate, !(FVS_VARIANT == ALL_VARIANT))

all_mort <- read.csv("ALL VARIANT_MORTALITY 2 TO 8FTFL.csv")
all_mort <- all_mort[,c(1,2,3,4,5,6,7,8)]
names(all_mort)[1] <- "ALL_VARIANT"
names(all_mort)[2] <- "USDA_PLANTS_SYMBOL"
names(all_mort)[3] <- "DBH_CLASS"
names(all_mort)[4] <- "MORTRATE"
unassigned <- merge(unassigned, all_mort, by = c("ALL_VARIANT", "USDA_PLANTS_SYMBOL", "DBH_CLASS", "MORTRATE"))

conn <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=F:/BiosumVM/cec_20170915/db/master.mdb")
master_tree <- sqlFetch(conn, "tree", as.is = TRUE)
master_cond <- sqlFetch(conn, "cond", as.is = TRUE)
master_plot <- sqlFetch(conn, "plot", as.is = TRUE)
odbcCloseAll()

master_tree <- master_tree[,c(1,10,12,14,19)]
master_cond <- master_cond[,c(1,2)]
master_plot <- master_plot[,c(1,11)]

master_dbh <- merge(master_tree, master_cond, by = "biosum_cond_id", all = TRUE)
master_dbh <- merge(master_dbh, master_plot, by = "biosum_plot_id", all = TRUE)
master_dbh <- master_dbh[,c(7,3,4,5,6)]
names(master_dbh)[1] <- "FVS_VARIANT"
names(master_dbh)[2] <- "Species"
names(master_dbh)[3] <- "DBH"
names(master_dbh)[4] <- "HT"
names(master_dbh)[5] <- "CRNRATIO"

master_dbh$Species <- sprintf("%03d", master_dbh$Species)

master_dbh <- master_dbh[complete.cases(master_dbh),]

master_dbh$DBH_CLASS[master_dbh$DBH < 5 & master_dbh$DBH >= 1 ] <- 1
master_dbh$DBH_CLASS[master_dbh$DBH < 10 & master_dbh$DBH >= 5 ] <- 2
master_dbh$DBH_CLASS[master_dbh$DBH < 15 & master_dbh$DBH >= 10 ] <- 3
master_dbh$DBH_CLASS[master_dbh$DBH < 21 & master_dbh$DBH >= 15 ] <- 4
master_dbh$DBH_CLASS[master_dbh$DBH < 30 & master_dbh$DBH >= 21 ] <- 5
master_dbh$DBH_CLASS[master_dbh$DBH < 40 & master_dbh$DBH >= 30 ] <- 6
master_dbh$DBH_CLASS[master_dbh$DBH < 999 & master_dbh$DBH >= 40 ] <- 7

master_dbh_grouped <- group_by(master_dbh, FVS_VARIANT, Species, DBH_CLASS)
master_dbh_summary <- summarise(master_dbh_grouped, AvgDBH = mean(DBH), AvgHT = mean(HT), AvgCRNRATIO = mean(CRNRATIO))

master_dbh_class <- master_dbh_summary
names(master_dbh_class)[1] <- "FVS_VARIANT"
names(master_dbh_class)[2] <- "Species"
names(master_dbh_class)[3] <- "DBH_CLASS"
names(master_dbh_class)[4] <- "DBH"
names(master_dbh_class)[5] <- "HT"
names(master_dbh_class)[6] <- "CRNRATIO"

master_dbh_class$DBH <- round(master_dbh_class$DBH, 1)
master_dbh_class$HT <- round(master_dbh_class$HT, 1)
master_dbh_class$CRNRATIO <- round(master_dbh_class$CRNRATIO, 1)

unassigned$best <- 1
iterations <- nrow(unassigned)
for (i in 1:iterations) {
  matchedrows <- unassigned[unassigned$FVS_VARIANT == unassigned[i,6] &
                              unassigned$FVS_SPECIES == unassigned[i,8] &
                              unassigned$Species == unassigned[i,5] &
                              unassigned$DBH_CLASS == unassigned[i,3],]
  dbh <- master_dbh_class[unassigned[i,6] == master_dbh_class$FVS_VARIANT & 
                            unassigned[i,3] == master_dbh_class$DBH_CLASS & 
                            unassigned[i,5] == master_dbh_class$Species,]
  if (nrow(dbh)> 0) {
    difference <- abs(dbh$CRNRATIO - matchedrows$CRNRATIO)
    matchedrows$difference <- abs(dbh$CRNRATIO - matchedrows$CRNRATIO)
    best <- matchedrows[matchedrows$difference == min(matchedrows$difference),]
    best <- best[1,1]
    unassigned[i,13] <- best
  }
  else {
    unassigned[i,13] <- NA
  }
  
}

unassigned$MORT_VARIANT <- unassigned$best

unassigned_premerge <- unassigned[,c(2,5,6,7,8,1,3,14)]
names(all_mort)[1] <- "MORT_VARIANT"
unassigned_premerge2 <- merge(unassigned_premerge, all_mort, by = c("MORT_VARIANT", "USDA_PLANTS_SYMBOL","DBH_CLASS"), all = TRUE)
unassigned_premerge3 <- unassigned_premerge2[,c(1,5,2,4,6,7,3,9)]
unassigned_premerge4 <- unique(unassigned_premerge3)

assigned$MORT_VARIANT <- assigned$FVS_VARIANT 
assigned2 <- assigned[,c(9,3,1,2,4,5,7,8)]

MORTRATE_SEMIFINAL <- rbind(unassigned_premerge4, assigned2)
MORTRATE_SEMIFINAL2 <- merge(MORTRATE_SEMIFINAL, all_species_usda, by = c("USDA_PLANTS_SYMBOL", "FVS_VARIANT", "Species", "OG_FVS_SPECIES", "FVS_SPECIES"), all = TRUE)
MORTRATE_SEMIFINAL3 <- MORTRATE_SEMIFINAL2[complete.cases(MORTRATE_SEMIFINAL2[,c(2:5)]),]
all_species_mortrate2 <- merge(MORTRATE_SEMIFINAL, all_species_mortrate, by = "USDA_PLANTS_SYMBOL", all = TRUE)

# match to unassigned species + dbhclass
# find crnratio value least different than master_dbh crnratio
# return variant for least different



#PREPARE MASTER TABLES 
speciesgroupnum <- data.frame(table(speciesgroups[,c("VARIANT", "SPGROUP")])) #creates a table with the number of species in each group
speciesgroupnum <- speciesgroupnum[speciesgroupnum$Freq > 0,] #remove 0s
speciesgroups2 <- merge(speciesgroupnum, speciesgroups, by = c("VARIANT", "SPGROUP"))
speciesgroups2 <- transform(speciesgroups2, MORT_SPGROUP = ifelse(speciesgroups2$Freq == 1, as.character(speciesgroups2$FVS_SPCD), as.character(speciesgroups2$SPGROUP)))
names(speciesgroups2)[2] <- "OG_SPGROUP"

#Merges speciesgroups2 and mortprob 
names(mortprob)[2] <- "MORT_SPGROUP"
mortprobgroup <- merge(mortprob, speciesgroups2, by = c("VARIANT", "MORT_SPGROUP"), all = TRUE)
#mortprobgroup <- na.omit(mortprobgroup)
names(mortprobgroup)[1] <- "FVS_VARIANT"
mortprobgroup <- mortprobgroup[,c(1,7,5,2,3,4)]
names(mortprobgroup)[2] <- "FVS_SPECIES"

#Prepare the tree_species table from master database
tree_species_master <- tree_species_master[tree_species_master$FVS_VARIANT %in% c("CA","WS", "NC", "SO"),] #limit to project variants
tree_species_master$FVS_VARIANT <- as.factor(as.character(tree_species_master$FVS_VARIANT))
tree_species_master <- tree_species_master[,c(2,4,5,8,10,11)]

#The following adds the 2 letter SPCD from the tree_species master to the
#mortprobgroup data and renames it to "Species" so it can be compared to the
#FVS_TreeList data.
mortprobgroup_species <- merge(mortprobgroup, tree_species_master, by = c("FVS_VARIANT", "FVS_SPECIES"), all = TRUE)
#mortprobgroup_species <- mortprobgroup_species[,c(1,2,5:7)]
mortprobgroup_species$Species <- sprintf("%03d", mortprobgroup_species$SPCD)
mortprobgroup_species <- mortprobgroup_species[,c(1,2,11,8,9,3,4,5,6)]

mortprobgroup_species <- merge(mortprobgroup_species, master_spcd, by = c("Species", "FVS_VARIANT"), all = TRUE)
write.csv(mortprobgroup_species, "mortprobgroupspecies.csv")


mortprobgroup_problems <- filter(mortprobgroup_species, is.na(MORTRATE))
mortprobgroup_problems <- subset(mortprobgroup_problems, !(OG_SPGROUP %in% c("All")))
mortprobgroup_problems <- mortprobgroup_problems[,c(1,2,3,4,5,6,7)]

all_mort <- all_mort[,c(1,2,3,4)]
names(all_mort)[1] <- "FVS_VARIANT"
names(all_mort)[2] <- "USDA_PLANTS_SYMBOL"
names(all_mort)[3] <- "DBH_CLASS"
names(all_mort)[4] <- "MORTRATE"

species_crosswalk <- species_crosswalk[,c(1,2,3,8,15,18,22)]
names(species_crosswalk)[1] <- "USDA_PLANTS_SYMBOL"
names(species_crosswalk)[2] <- "Species"
names(species_crosswalk)[3] <- "FVS_SPECIES"

species_mort <- merge(species_crosswalk, all_mort, by = "USDA_PLANTS_SYMBOL", all = TRUE)
species_mort$Species <-  sprintf("%03d", species_mort$Species)
test3 <- merge(mortprobgroup_problems, species_mort, by = c("Species", "FVS_SPECIES"), all = TRUE)

test3 <- filter(test3, !is.na(FVS_VARIANT.y))
names(test3)[13] <- "ALL_VARIANTS"
names(test3)[3] <- "FVS_VARIANT"
test4 <- filter(test3, !is.na(FVS_VARIANT))

test4$FVS_VARIANT <- as.character(test4$FVS_VARIANT)
test4$ALL_VARIANTS <- as.character(test4$ALL_VARIANTS)

assigned <- filter(test4, FVS_VARIANT == ALL_VARIANTS)
assigned$double <- paste(assigned$FVS_SPECIES, assigned$FVS_VARIANT)
test4$double <- paste(test4$FVS_SPECIES, test4$FVS_VARIANT)
test5 <- subset(test4, !(assigned$double %in% test4$double))
assigned <- assigned[,c(1,2,3,4,5,6,7,8,14,15)]

mortprobgroup_problems2 <- merge(all_mort, mortprobgroup_problems, by = c("FVS_VARIANT", "DBH_CLASS"), all = TRUE)
write.csv(mortprobgroup_problems, "mortprobgroupproblems.csv")

#EVERYTHING BELOW WILL ITERATE BY PACKAGE

CreateSurvVolRatioTable <- function(directory, variantname) {
  setwd(directory)#sets the working directory to the directory variable
  path <- list.files(path = ".", pattern = glob2rx(paste("FVSOUT_", variantname, "_P0", "*.MDB", sep = ""))) #lists all the files in the directory that begin with FVSOUT_{variantname}_P0 and end with .MDB (case sensitive)
  #path <- list.files(path = ".", pattern = ".accdb")
  #numfiles <- 1
  #i <- 1
  rows <- as.numeric(0)
  numfiles <- nrow(data.frame(path)) #calculates the number of package MDB files based on the path variable above
  for (i in 1:numfiles) {
    file <- file.path(directory, path[i]) #creates a file path using the directory variable and the iteration of the pakage MDB
    x <- paste("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", file, sep = "") #writes the argument for the odbcDriverConnect function below
    conn <- odbcDriverConnect(x) #Connect to access database 
    
    FVS_TreeList <- sqlFetch(conn, "FVS_TreeList", as.is = TRUE)
    FVS_TreeList <- FVS_TreeList[,c(1:3,5:7,11,13,22)]
    
    #assignments must be made manually. 
    FVS_TreeList$DBH_CLASS[FVS_TreeList$DBH < 5 & FVS_TreeList$DBH >= 1 ] <- 1
    FVS_TreeList$DBH_CLASS[FVS_TreeList$DBH < 10 & FVS_TreeList$DBH >= 5 ] <- 2
    FVS_TreeList$DBH_CLASS[FVS_TreeList$DBH < 15 & FVS_TreeList$DBH >= 10 ] <- 3
    FVS_TreeList$DBH_CLASS[FVS_TreeList$DBH < 21 & FVS_TreeList$DBH >= 15 ] <- 4
    FVS_TreeList$DBH_CLASS[FVS_TreeList$DBH < 30 & FVS_TreeList$DBH >= 21 ] <- 5
    FVS_TreeList$DBH_CLASS[FVS_TreeList$DBH < 40 & FVS_TreeList$DBH >= 30 ] <- 6
    FVS_TreeList$DBH_CLASS[FVS_TreeList$DBH < 999 & FVS_TreeList$DBH >= 40 ] <- 7
    
    FVS_TreeList$FVS_VARIANT <- variantname #adds a column for FVS_VARIANT 
    
    #Merges the FVS_TreeList and mortprob tables by DBH Class, Variant, and Species code
    FVS_TreeList <- merge(FVS_TreeList, mortprobgroup_species, by = c("DBH_CLASS", "FVS_VARIANT", "Species"), all = TRUE)
    FVS_TreeList <- FVS_TreeList[complete.cases(FVS_TreeList[,4:5]),] #remove NAs
    #Calculations for Volume and Survival Volume 
    FVS_TreeList$Vol <- FVS_TreeList$TCuFt * FVS_TreeList$TPA
    FVS_TreeList$SurvVol <- (1 - FVS_TreeList$MORTRATE) * FVS_TreeList$TCuFt * FVS_TreeList$TPA
    
    #Move and trim data from FVS_TreeList to new MC_SURV_VOL_TREE table, 
    #remove NA MORTRATE rows, and sum SurvVol by stand and year
    MC_SURV_VOL_TREE <- FVS_TreeList[,c(2,5,3,6,7,1,13,8,9,11,15,16)]
    MC_SURV_VOL_TREE1 <- MC_SURV_VOL_TREE[complete.cases(MC_SURV_VOL_TREE[,7]),]
    MC_SURV_VOL_TREE2 <- group_by(MC_SURV_VOL_TREE1, StandID, Year)
    MC_SURV_VOL_TREE3 <- summarise(MC_SURV_VOL_TREE2, SurvVolSum = sum(SurvVol))
    
    #Sum Volume by stand from FVS_TreeList data into new MC_VOL_ALL table
    MC_VOL_ALL <- group_by(FVS_TreeList, StandID, Year)
    MC_VOL_ALL2 <- summarise(MC_VOL_ALL, VolSum = sum(Vol))
    
    #Merge the Volume by stand data with the MC_SURV_VOL_TREE table 
    MC_VOL_ALL3 <- merge(MC_VOL_ALL2, MC_SURV_VOL_TREE3, by = c("StandID", "Year"))
    
    #Calculate SurvVolRatio. This takes Survival Volume values and divides it by non-zero
    #stand volume sums. If the stand volume is 0, SurvVolRatio is set to 1.
    MC_VOL_ALL3$SurvVolRatio <- ifelse(MC_VOL_ALL3$VolSum > 0, MC_VOL_ALL3$SurvVolSum/MC_VOL_ALL3$VolSum, 1)
    
    #Turn MC_VOL_ALL3 into new SurvVolRatio table and save to access database
    SurvVolRatio <- MC_VOL_ALL3
    SurvVolRatio$MortVol <- SurvVolRatio$VolSum-SurvVolRatio$SurvVolSum
    sqlSave(conn, dat = SurvVolRatio, tablename = "SurvVolRatio", rownames = FALSE)
    
    FVS_Summary <- sqlFetch(conn, "FVS_Summary", as.is = TRUE)
    merge(FVS_Summary, SurvVolRatio, by = )
    
    #add new SurvVolRatio column to FVS_Summary, join on StandID and add SurvVolRatio
    sqlQuery(conn, 'ALTER TABLE FVS_Summary ADD COLUMN SurvVolRatio NUMERIC')
    sqlQuery(conn, 'ALTER TABLE FVS_Summary ADD COLUMN VolSum NUMERIC')
    sqlQuery(conn, 'ALTER TABLE FVS_Summary ADD COLUMN SurvVolSum NUMERIC')
    sqlQuery(conn, 'ALTER TABLE FVS_Summary ADD COLUMN MortVol NUMERIC')
    sqlQuery(conn, 'UPDATE FVS_Summary INNER JOIN SurvVolRatio ON (FVS_Summary.Year = SurvVolRatio.Year) AND (FVS_Summary.StandID = SurvVolRatio.StandID) SET FVS_Summary.SurvVolRatio = [SurvVolRatio].[SurvVolRatio]')
    sqlQuery(conn, 'UPDATE FVS_Summary INNER JOIN SurvVolRatio ON (FVS_Summary.Year = SurvVolRatio.Year) AND (FVS_Summary.StandID = SurvVolRatio.StandID) SET FVS_Summary.VolSum = [SurvVolRatio].[VolSum]')
    sqlQuery(conn, 'UPDATE FVS_Summary INNER JOIN SurvVolRatio ON (FVS_Summary.Year = SurvVolRatio.Year) AND (FVS_Summary.StandID = SurvVolRatio.StandID) SET FVS_Summary.SurvVolSum = [SurvVolRatio].[SurvVolSum]')
    sqlQuery(conn, 'UPDATE FVS_Summary INNER JOIN SurvVolRatio ON (FVS_Summary.Year = SurvVolRatio.Year) AND (FVS_Summary.StandID = SurvVolRatio.StandID) SET FVS_Summary.MortVol = [SurvVolRatio].[MortVol]')
    
    #Close all open connections to access
    odbcCloseAll()
  }
}

directory <- "D:/Dropbox/Carlin/Berkeley/biosum/MortCalc"
variantname <- "WS"
#CreateSurvVolRatioTable("D:/cec_20170915/fvs/data/WS","WS")
CreateSurvVolRatioTable("D:/cec_20170915/fvs/data/CA", "CA")
CreateSurvVolRatioTable("D:/cec_20170915/fvs/data/NC", "NC")
CreateSurvVolRatioTable("D:/cec_20170915/fvs/data/SO", "SO")
CreateSurvVolRatioTable("D:/cec_20170915/fvs/data/WS", "WS")

#Screwed something up? Delete everything! 
DeleteSurvVolRatioTable <- function (directory, variantname) {
  setwd(directory)#sets the working directory to the directory variable
  path <- list.files(path = ".", pattern = glob2rx(paste("FVSOUT_", variantname, "_P0", "*.MDB", sep = ""))) #lists all the files in the directory that begin with FVSOUT_{variantname}_P0 and end with .MDB (case sensitive)
  rows <- as.numeric(0)
  numfiles <- nrow(data.frame(path)) #calculates the number of package MDB files based on the path variable above
#  path <- list.files(path = ".", pattern = ".accdb")
#  numfiles <- 1
#  i <- 1
  for (i in 1:numfiles) {
    file <- file.path(directory, path[i]) #creates a file path using the directory variable and the iteration of the pakage MDB
    x <- paste("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", file, sep = "") #writes the argument for the odbcDriverConnect function below
    conn <- odbcDriverConnect(x) #Connect to the access database 
    sqlQuery(conn, 'ALTER TABLE FVS_Summary DROP SurvVolRatio')
    #sqlQuery(conn, 'ALTER TABLE FVS_Summary DROP VolSum')
    #sqlQuery(conn, 'ALTER TABLE FVS_Summary DROP SurvVolSum')
    #sqlQuery(conn, 'ALTER TABLE FVS_Summary DROP MortVol')
    sqlQuery(conn, 'DROP TABLE SurvVolRatio')
    
    #Close all open connections to access
    odbcCloseAll()
  }
}

DeleteSurvVolRatioTable("D:/cec_20170915/fvs/data/CA", "CA")
DeleteSurvVolRatioTable("D:/cec_20170915/fvs/data/NC", "NC")
DeleteSurvVolRatioTable("D:/cec_20170915/fvs/data/SO", "SO")
DeleteSurvVolRatioTable("D:/cec_20170915/fvs/data/WS", "WS")
