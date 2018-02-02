#This script was created by Carlin Starrs in 2017. 
#It takes the master tree list and pairs it with the all_variants mortality data 
#(created by Terrie Jane using FOFEM) and uses it to calculate Survival Volume 
#data. It has many steps and is currently only set to work with variants CA,
#NC, SO, and WS, but can be used as a framework to adapt for other variants.

#Make sure you have the Microsoft Access Databse Engine Driver https://www.microsoft.com/en-us/download/confirmation.aspx?id=23734
#and you are using 32-bit R (set in RStudio by going to Tools -> Global Options)

#Set the working directory to the location of all_variants.
setwd("G:/Dropbox/Carlin/Berkeley/biosum/MortCalc")

library("dplyr") #if you do not have these packages installed, enter "install.packages("packagename") into the console, then load them using this line.
library("RODBC")
options(scipen = 999) #this is important for making sure your stand IDs do not get translated to scientific notation

#Connect to the master.mdb database. To set to your master.mdb location, change the 
#text following "DBQ=" in the command below (e.g. D:/cec_20170915/db/master.mdb). Make
#sure your slashes are facing the correct direction. 
conn <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=H:/cec_20170915/db/master.mdb")
master_tree <- sqlFetch(conn, "tree", as.is = TRUE) #import tree table from master.mdb
odbcCloseAll()

#Connect to refmaster.mdb usnig the same method as above
conn <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=H:/cec_20170915/db/ref_master.mdb")
FVS_WesternTreeSpeciesTranslator <- sqlFetch(conn, "FVS_WesternTreeSpeciesTranslator", as.is = TRUE) #import species crosswalk table from master.mdb
odbcCloseAll()

#The section below takes the master_tree table and uses the first two characters of fvs_tree_id to determine the variant.
master <- data.frame("biosum_cond_id" = master_tree$biosum_cond_id,
                          "spcd" = master_tree$spcd,
                          "DBH" = master_tree$dia,
                          "HT" = master_tree$ht,
                          "CRNRATIO" = master_tree$cr,
                          "FVS_VARIANT" = substr(master_tree$fvs_tree_id, 0,2))
problem_master_nas <- subset(master, !complete.cases(master)) #these are values with NAs. They will not work for the process below.
master <- na.omit(master) #remove NAs; these either do not have a matching cond_id/plot_id or they have NULL DBH/CR/HT values. 
master$Species <- sprintf("%03d", master$spcd) #convert spcd to 3 digit "Species" value
unique_spcd <- master[,c(6,7)] #trim table to just Species and FVS_VARIANT
unique_spcd <- unique(unique_spcd[,c("FVS_VARIANT", "Species")]) #limit to unique FVS_VARIANT & Species combinations

#The section below takes the FVS Western Species Translator data and puts it into 
#a format that allows for linking the USDA_PLANTS_SYMBOL code used by FOFEM and translate
#it to a 2-letter FVS_SPECIES code by variant. OG_FVS_SPECIES is the default FVS species code for 
#that USDA_PLANTS_SYMBOL code, which changes depending on the variant.
species_crosswalk <- read.csv("species_crosswalk.csv")
species_crosswalk <- FVS_WesternTreeSpeciesTranslator[,c(1,2,3,8,15,18,22)] #trim to relevant columns
species_crosswalk <- subset(species_crosswalk, complete.cases(species_crosswalk[,2])) #remove NA values for FIA_SPCD
names(species_crosswalk)[1] <- "USDA_PLANTS_SYMBOL" #rename columns 
names(species_crosswalk)[2] <- "Species"
names(species_crosswalk)[3] <- "OG_FVS_SPECIES"
species_crosswalk$Species <- as.numeric(species_crosswalk$Species)
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
all_mort <- all_mort[,c(1,2,3,4,6,7,8)]
names(all_mort)[1] <- "FVS_VARIANT"
names(all_mort)[2] <- "USDA_PLANTS_SYMBOL"
names(all_mort)[3] <- "DBH_CLASS"
names(all_mort)[4] <- "MORTRATE"
all_mort$CBH <- round(((100 - all_mort$CRNRATIO) * all_mort$HT/100),1) #calculate CBH

#The following renames unique_spcd to all_species so it can be manipulated without
#losing the original and merges it with the species_crosswalk table to link the
#USDA_PLANTS_SYMBOL values to the list of species in this project
all_species <- unique_spcd
all_species$ID <- 1
all_species_usda <- merge(all_species, species_crosswalk, by = c("Species", "FVS_VARIANT"), all = TRUE) #merge all_species and the species_crosswalk to get USDA_PLANTS_SYMBOL linked with Species
all_species_usda <- subset(all_species_usda, all_species_usda$FVS_VARIANT != "CR")
all_species_usda <- subset(all_species_usda, all_species_usda$ID == 1)
all_species_usda$Species <- as.character(all_species_usda$Species)

all_species_usda$DBH_CLASS <- NA #add DBH_CLASS column
all_species_usda <- all_species_usda[rep(seq_len(nrow(all_species_usda)), each=7),] #repeat each row 7 times
all_species_usda$DBH_CLASS <- 1:7 #populate DBH_CLASS column with 1:7

all_species_usda$OG_USDA_PLANTS_SYMBOL <- all_species_usda$USDA_PLANTS_SYMBOL #store original USDA_PLANTS_SYMBOL as OG_USDA_PLANTS_SYMBOL
all_species_usda$USDA_PLANTS_SYMBOL <- NULL #set USDA_PLANTS_SYMBOL to null
#all_species_usda$USDA_PLANTS_SYMBOL <- as.character(all_species_usda$USDA_PLANTS_SYMBOL) 
all_species_usda$OG_USDA_PLANTS_SYMBOL <- as.character(all_species_usda$OG_USDA_PLANTS_SYMBOL)

#Translate species without USDA_PLANTS_SYMBOL (problem_no_usda_plants_symbol)
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$Species == "313"] <- "ACMA3"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$Species == "511" | all_species_usda$Species == "611"] <- "LIDE3"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$Species == "763"] <- "PREM"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$Species == "997"] <- "ARME"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$Species == "056"] <- "JUOC"

#all_species_usda$OG_USDA_PLANTS_SYMBOL <- ifelse(is.na(all_species_usda$OG_USDA_PLANTS_SYMBOL), all_species_usda$USDA_PLANTS_SYMBOL, all_species_usda$OG_USDA_PLANTS_SYMBOL)

#Translate species that do not appear in ALL_VARIANTS (problem_no_mortrate_for_species)
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$OG_USDA_PLANTS_SYMBOL == "ABPR"] <- "ABMA"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$OG_USDA_PLANTS_SYMBOL == "ABSH"] <- "ABMAS"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$OG_USDA_PLANTS_SYMBOL == "ACGL"] <- "ACMA3"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$OG_USDA_PLANTS_SYMBOL == "AECA"] <- "AEGL"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$OG_USDA_PLANTS_SYMBOL == "CELE3"] <- "CHCHM"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$OG_USDA_PLANTS_SYMBOL == "CHCHC4"] <- "CHCHM"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$OG_USDA_PLANTS_SYMBOL == "CONU4"] <- "CHCHM"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$OG_USDA_PLANTS_SYMBOL == "CUBA"] <- "JUOC"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$OG_USDA_PLANTS_SYMBOL == "CUSA3"] <- "JUOC"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$OG_USDA_PLANTS_SYMBOL == "FRAXI"] <- "BETSPP"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$OG_USDA_PLANTS_SYMBOL == "FRLA"] <- "BETSPP"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$OG_USDA_PLANTS_SYMBOL == "JUCA"] <- "JUOC"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$OG_USDA_PLANTS_SYMBOL == "JUCA7"] <- "JUOC"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$OG_USDA_PLANTS_SYMBOL == "JUOS"] <- "JUOC"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$OG_USDA_PLANTS_SYMBOL == "PIBR"] <- "PISA2"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$OG_USDA_PLANTS_SYMBOL == "PICO3"] <- "PICO"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$OG_USDA_PLANTS_SYMBOL == "PIFL2"] <- "PIFL"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$OG_USDA_PLANTS_SYMBOL == "PILO"] <- "PIMO3"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$OG_USDA_PLANTS_SYMBOL == "PIMO"] <- "PIMO3"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$OG_USDA_PLANTS_SYMBOL == "PIMU"] <- "PIFL"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$OG_USDA_PLANTS_SYMBOL == "PIRA2"] <- "PISA2"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$OG_USDA_PLANTS_SYMBOL == "PIWA"] <- "PIFL"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$OG_USDA_PLANTS_SYMBOL == "PLRA"] <- "ACMA3"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$OG_USDA_PLANTS_SYMBOL == "POBAT"] <- "POTR5"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$OG_USDA_PLANTS_SYMBOL == "POFR2"] <- "POTR5"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$OG_USDA_PLANTS_SYMBOL == "PSMA"] <- "PSME"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$OG_USDA_PLANTS_SYMBOL == "QUAG"] <- "QUWI2"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$OG_USDA_PLANTS_SYMBOL == "QUCH2"] <- "QUWI2"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$OG_USDA_PLANTS_SYMBOL == "QUEN"] <- "QUWI2"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$OG_USDA_PLANTS_SYMBOL == "QULO"] <- "QUKE"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$OG_USDA_PLANTS_SYMBOL == "QUMU"] <- "CHCHM"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$OG_USDA_PLANTS_SYMBOL == "SEGI2"] <- "SEGE3"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$OG_USDA_PLANTS_SYMBOL == "TABR2"] <- "ARME"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$OG_USDA_PLANTS_SYMBOL == "TOCA"] <- "ARME"


all_species_usda$USDA_PLANTS_SYMBOL <- ifelse(is.na(all_species_usda$USDA_PLANTS_SYMBOL), all_species_usda$OG_USDA_PLANTS_SYMBOL, all_species_usda$USDA_PLANTS_SYMBOL)
all_species_usda$USDA_PLANTS_SYMBOL1 <- all_species_usda$USDA_PLANTS_SYMBOL
all_species_usda$USDA_PLANTS_SYMBOL <- NA

#Translate species/dbh class combos that do not appear in ALL_VARIANTS (problem_no_mortrate_for_dbh_class)
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "CA" & all_species_usda$USDA_PLANTS_SYMBOL1 == "ABAM" & all_species_usda$DBH_CLASS == 7] <-  "ABMA"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "CA" & all_species_usda$USDA_PLANTS_SYMBOL1 == "ABLA" & all_species_usda$DBH_CLASS == 7] <- "ABMA"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "CA" & all_species_usda$USDA_PLANTS_SYMBOL1 == "AEGL" & all_species_usda$DBH_CLASS == 3] <- "ARME"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "CA" & all_species_usda$USDA_PLANTS_SYMBOL1 == "AEGL" & all_species_usda$DBH_CLASS == 4] <- "ARME"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "CA" & all_species_usda$USDA_PLANTS_SYMBOL1 == "AEGL" & all_species_usda$DBH_CLASS == 5] <- "ARME"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "CA" & all_species_usda$USDA_PLANTS_SYMBOL1 == "AEGL" & all_species_usda$DBH_CLASS == 6] <- "ARME"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "CA" & all_species_usda$USDA_PLANTS_SYMBOL1 == "AEGL" & all_species_usda$DBH_CLASS == 7] <- "ARME"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "CA" & all_species_usda$USDA_PLANTS_SYMBOL1 == "AIAL" & all_species_usda$DBH_CLASS == 1] <- "CHCHM"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "CA" & all_species_usda$USDA_PLANTS_SYMBOL1 == "AIAL" & all_species_usda$DBH_CLASS == 3] <- "CHCHM"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "CA" & all_species_usda$USDA_PLANTS_SYMBOL1 == "AIAL" & all_species_usda$DBH_CLASS == 4] <- "CHCHM"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "CA" & all_species_usda$USDA_PLANTS_SYMBOL1 == "AIAL" & all_species_usda$DBH_CLASS == 5] <- "CHCHM"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "CA" & all_species_usda$USDA_PLANTS_SYMBOL1 == "AIAL" & all_species_usda$DBH_CLASS == 6] <- "CHCHM"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "CA" & all_species_usda$USDA_PLANTS_SYMBOL1 == "AIAL" & all_species_usda$DBH_CLASS == 7] <- "CHCHM"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "CA" & all_species_usda$USDA_PLANTS_SYMBOL1 == "ALRH2" & all_species_usda$DBH_CLASS == 7] <- "LIDE3"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "CA" & all_species_usda$USDA_PLANTS_SYMBOL1 == "ALRU2" & all_species_usda$DBH_CLASS == 7] <- "LIDE3"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "CA" & all_species_usda$USDA_PLANTS_SYMBOL1 == "BETSPP" & all_species_usda$DBH_CLASS == 4] <- "ALRH2"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "CA" & all_species_usda$USDA_PLANTS_SYMBOL1 == "BETSPP" & all_species_usda$DBH_CLASS == 5] <- "ALRH2"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "CA" & all_species_usda$USDA_PLANTS_SYMBOL1 == "BETSPP" & all_species_usda$DBH_CLASS == 6] <- "ALRH2"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "CA" & all_species_usda$USDA_PLANTS_SYMBOL1 == "BETSPP" & all_species_usda$DBH_CLASS == 7] <- "LIDE3"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "CA" & all_species_usda$USDA_PLANTS_SYMBOL1 == "PISA2" & all_species_usda$DBH_CLASS == 7] <- "PICO"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "CA" & all_species_usda$USDA_PLANTS_SYMBOL1 == "POTR5" & all_species_usda$DBH_CLASS == 6] <- "ALRH2"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "CA" & all_species_usda$USDA_PLANTS_SYMBOL1 == "POTR5" & all_species_usda$DBH_CLASS == 7] <- "ARME"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "CA" & all_species_usda$USDA_PLANTS_SYMBOL1 == "PREM" & all_species_usda$DBH_CLASS == 3] <- "ARME"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "CA" & all_species_usda$USDA_PLANTS_SYMBOL1 == "PREM" & all_species_usda$DBH_CLASS == 4] <- "ARME"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "CA" & all_species_usda$USDA_PLANTS_SYMBOL1 == "PREM" & all_species_usda$DBH_CLASS == 5] <- "ARME"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "CA" & all_species_usda$USDA_PLANTS_SYMBOL1 == "PREM" & all_species_usda$DBH_CLASS == 6] <- "ARME"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "CA" & all_species_usda$USDA_PLANTS_SYMBOL1 == "PREM" & all_species_usda$DBH_CLASS == 7] <- "ARME"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "CA" & all_species_usda$USDA_PLANTS_SYMBOL1 == "QUDO" & all_species_usda$DBH_CLASS == 1] <- "QUKE"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "CA" & all_species_usda$USDA_PLANTS_SYMBOL1 == "QUDO" & all_species_usda$DBH_CLASS == 4] <- "QUKE"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "CA" & all_species_usda$USDA_PLANTS_SYMBOL1 == "QUDO" & all_species_usda$DBH_CLASS == 5] <- "QUKE"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "CA" & all_species_usda$USDA_PLANTS_SYMBOL1 == "QUDO" & all_species_usda$DBH_CLASS == 6] <- "QUKE"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "CA" & all_species_usda$USDA_PLANTS_SYMBOL1 == "QUDO" & all_species_usda$DBH_CLASS == 7] <- "QUKE"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "CA" & all_species_usda$USDA_PLANTS_SYMBOL1 == "QUWI2" & all_species_usda$DBH_CLASS == 6] <- "QUGA4"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "CA" & all_species_usda$USDA_PLANTS_SYMBOL1 == "QUWI2" & all_species_usda$DBH_CLASS == 7] <- "QUGA4"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "CA" & all_species_usda$USDA_PLANTS_SYMBOL1 == "TSME" & all_species_usda$DBH_CLASS == 7] <- "TSHE"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "CA" & all_species_usda$USDA_PLANTS_SYMBOL1 == "UMCA" & all_species_usda$DBH_CLASS == 7] <- "ARME"

all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "NC" & all_species_usda$USDA_PLANTS_SYMBOL1 == "AEGL" & all_species_usda$DBH_CLASS == 3] <- "ARME"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "NC" & all_species_usda$USDA_PLANTS_SYMBOL1 == "AEGL" & all_species_usda$DBH_CLASS == 4] <- "ARME"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "NC" & all_species_usda$USDA_PLANTS_SYMBOL1 == "AEGL" & all_species_usda$DBH_CLASS == 5] <- "ARME"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "NC" & all_species_usda$USDA_PLANTS_SYMBOL1 == "AEGL" & all_species_usda$DBH_CLASS == 6] <- "ARME"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "NC" & all_species_usda$USDA_PLANTS_SYMBOL1 == "AEGL" & all_species_usda$DBH_CLASS == 7] <- "ARME"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "NC" & all_species_usda$USDA_PLANTS_SYMBOL1 == "AIAL" & all_species_usda$DBH_CLASS == 3] <- "CHCHM"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "NC" & all_species_usda$USDA_PLANTS_SYMBOL1 == "AIAL" & all_species_usda$DBH_CLASS == 4] <- "CHCHM"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "NC" & all_species_usda$USDA_PLANTS_SYMBOL1 == "AIAL" & all_species_usda$DBH_CLASS == 5] <- "CHCHM"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "NC" & all_species_usda$USDA_PLANTS_SYMBOL1 == "AIAL" & all_species_usda$DBH_CLASS == 6] <- "CHCHM"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "NC" & all_species_usda$USDA_PLANTS_SYMBOL1 == "AIAL" & all_species_usda$DBH_CLASS == 7] <- "CHCHM"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "NC" & all_species_usda$USDA_PLANTS_SYMBOL1 == "ALRH2" & all_species_usda$DBH_CLASS == 7] <- "LIDE3"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "NC" & all_species_usda$USDA_PLANTS_SYMBOL1 == "ALRU2" & all_species_usda$DBH_CLASS == 7] <- "LIDE3"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "NC" & all_species_usda$USDA_PLANTS_SYMBOL1 == "BETSPP" & all_species_usda$DBH_CLASS == 4] <- "ALRH2"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "NC" & all_species_usda$USDA_PLANTS_SYMBOL1 == "BETSPP" & all_species_usda$DBH_CLASS == 5] <- "ALRH2"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "NC" & all_species_usda$USDA_PLANTS_SYMBOL1 == "BETSPP" & all_species_usda$DBH_CLASS == 6] <- "ALRH2"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "NC" & all_species_usda$USDA_PLANTS_SYMBOL1 == "BETSPP" & all_species_usda$DBH_CLASS == 7] <- "LIDE3"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "NC" & all_species_usda$USDA_PLANTS_SYMBOL1 == "PIFL" & all_species_usda$DBH_CLASS == 6] <- "PISA2"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "NC" & all_species_usda$USDA_PLANTS_SYMBOL1 == "PIFL" & all_species_usda$DBH_CLASS == 7] <- "PIMO3"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "NC" & all_species_usda$USDA_PLANTS_SYMBOL1 == "PISA2" & all_species_usda$DBH_CLASS == 7] <- "PIMO3"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "NC" & all_species_usda$USDA_PLANTS_SYMBOL1 == "PISI" & all_species_usda$DBH_CLASS == 3] <- "PISA2"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "NC" & all_species_usda$USDA_PLANTS_SYMBOL1 == "PISI" & all_species_usda$DBH_CLASS == 5] <- "PISA2"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "NC" & all_species_usda$USDA_PLANTS_SYMBOL1 == "PISI" & all_species_usda$DBH_CLASS == 6] <- "PISA2"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "NC" & all_species_usda$USDA_PLANTS_SYMBOL1 == "PISI" & all_species_usda$DBH_CLASS == 7] <- "PICO"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "NC" & all_species_usda$USDA_PLANTS_SYMBOL1 == "POTR5" & all_species_usda$DBH_CLASS == 6] <- "ALRH2"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "NC" & all_species_usda$USDA_PLANTS_SYMBOL1 == "POTR5" & all_species_usda$DBH_CLASS == 7] <- "ARME"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "NC" & all_species_usda$USDA_PLANTS_SYMBOL1 == "PREM" & all_species_usda$DBH_CLASS == 3] <- "ARME"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "NC" & all_species_usda$USDA_PLANTS_SYMBOL1 == "PREM" & all_species_usda$DBH_CLASS == 4] <- "ARME"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "NC" & all_species_usda$USDA_PLANTS_SYMBOL1 == "PREM" & all_species_usda$DBH_CLASS == 5] <- "ARME"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "NC" & all_species_usda$USDA_PLANTS_SYMBOL1 == "PREM" & all_species_usda$DBH_CLASS == 6] <- "ARME"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "NC" & all_species_usda$USDA_PLANTS_SYMBOL1 == "PREM" & all_species_usda$DBH_CLASS == 7] <- "ARME"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "NC" & all_species_usda$USDA_PLANTS_SYMBOL1 == "QUDO" & all_species_usda$DBH_CLASS == 1] <- "QUKE"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "NC" & all_species_usda$USDA_PLANTS_SYMBOL1 == "QUDO" & all_species_usda$DBH_CLASS == 4] <- "QUKE"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "NC" & all_species_usda$USDA_PLANTS_SYMBOL1 == "QUDO" & all_species_usda$DBH_CLASS == 5] <- "QUKE"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "NC" & all_species_usda$USDA_PLANTS_SYMBOL1 == "QUDO" & all_species_usda$DBH_CLASS == 6] <- "QUKE"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "NC" & all_species_usda$USDA_PLANTS_SYMBOL1 == "QUDO" & all_species_usda$DBH_CLASS == 7] <- "QUKE"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "NC" & all_species_usda$USDA_PLANTS_SYMBOL1 == "QUWI2" & all_species_usda$DBH_CLASS == 6] <- "QUGA4"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "NC" & all_species_usda$USDA_PLANTS_SYMBOL1 == "QUWI2" & all_species_usda$DBH_CLASS == 7] <- "QUGA4"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "NC" & all_species_usda$USDA_PLANTS_SYMBOL1 == "SESE3" & all_species_usda$DBH_CLASS == 2] <- "SEGE3"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "NC" & all_species_usda$USDA_PLANTS_SYMBOL1 == "SESE3" & all_species_usda$DBH_CLASS == 4] <- "SEGE3"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "NC" & all_species_usda$USDA_PLANTS_SYMBOL1 == "SESE3" & all_species_usda$DBH_CLASS == 6] <- "SEGE3"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "NC" & all_species_usda$USDA_PLANTS_SYMBOL1 == "TSME" & all_species_usda$DBH_CLASS == 7] <- "TSHE"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "NC" & all_species_usda$USDA_PLANTS_SYMBOL1 == "UMCA" & all_species_usda$DBH_CLASS == 7] <- "ARME"

all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "SO" & all_species_usda$USDA_PLANTS_SYMBOL1 == "PIAL" & all_species_usda$DBH_CLASS == 5] <- "PISA2"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "SO" & all_species_usda$USDA_PLANTS_SYMBOL1 == "PIAL" & all_species_usda$DBH_CLASS == 6] <- "PISA2"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "SO" & all_species_usda$USDA_PLANTS_SYMBOL1 == "PIAL" & all_species_usda$DBH_CLASS == 7] <- "PIMO3"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "SO" & all_species_usda$USDA_PLANTS_SYMBOL1 == "PIFL" & all_species_usda$DBH_CLASS == 6] <- "PISA2"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "SO" & all_species_usda$USDA_PLANTS_SYMBOL1 == "PIFL" & all_species_usda$DBH_CLASS == 7] <- "PIMO3"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "SO" & all_species_usda$USDA_PLANTS_SYMBOL1 == "POTR5" & all_species_usda$DBH_CLASS == 6] <- "ALRH2"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "SO" & all_species_usda$USDA_PLANTS_SYMBOL1 == "POTR5" & all_species_usda$DBH_CLASS == 7] <- "LIDE3"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "SO" & all_species_usda$USDA_PLANTS_SYMBOL1 == "PREM" & all_species_usda$DBH_CLASS == 3] <- "ARME"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "SO" & all_species_usda$USDA_PLANTS_SYMBOL1 == "PREM" & all_species_usda$DBH_CLASS == 4] <- "ARME"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "SO" & all_species_usda$USDA_PLANTS_SYMBOL1 == "PREM" & all_species_usda$DBH_CLASS == 5] <- "ARME"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "SO" & all_species_usda$USDA_PLANTS_SYMBOL1 == "PREM" & all_species_usda$DBH_CLASS == 6] <- "ARME"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "SO" & all_species_usda$USDA_PLANTS_SYMBOL1 == "PREM" & all_species_usda$DBH_CLASS == 7] <- "ARME"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "SO" & all_species_usda$USDA_PLANTS_SYMBOL1 == "TSME" & all_species_usda$DBH_CLASS == 7] <- "TSHE"

all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "WS" & all_species_usda$USDA_PLANTS_SYMBOL1 == "AEGL" & all_species_usda$DBH_CLASS == 3] <- "ARME"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "WS" & all_species_usda$USDA_PLANTS_SYMBOL1 == "AEGL" & all_species_usda$DBH_CLASS == 4] <- "ARME"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "WS" & all_species_usda$USDA_PLANTS_SYMBOL1 == "AEGL" & all_species_usda$DBH_CLASS == 5] <- "ARME"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "WS" & all_species_usda$USDA_PLANTS_SYMBOL1 == "AEGL" & all_species_usda$DBH_CLASS == 6] <- "ARME"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "WS" & all_species_usda$USDA_PLANTS_SYMBOL1 == "AEGL" & all_species_usda$DBH_CLASS == 7] <- "ARME"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "WS" & all_species_usda$USDA_PLANTS_SYMBOL1 == "ALRH2" & all_species_usda$DBH_CLASS == 7] <- "LIDE3"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "WS" & all_species_usda$USDA_PLANTS_SYMBOL1 == "ALRU2" & all_species_usda$DBH_CLASS == 7] <- "LIDE3"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "WS" & all_species_usda$USDA_PLANTS_SYMBOL1 == "PIAL" & all_species_usda$DBH_CLASS == 7] <- "PIMO3"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "WS" & all_species_usda$USDA_PLANTS_SYMBOL1 == "PIAL" & all_species_usda$DBH_CLASS == 5] <- "PISA2"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "WS" & all_species_usda$USDA_PLANTS_SYMBOL1 == "PIAL" & all_species_usda$DBH_CLASS == 6] <- "PISA2"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "WS" & all_species_usda$USDA_PLANTS_SYMBOL1 == "PIFL" & all_species_usda$DBH_CLASS == 6] <- "PISA2"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "WS" & all_species_usda$USDA_PLANTS_SYMBOL1 == "PIFL" & all_species_usda$DBH_CLASS == 7] <- "PICO"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "WS" & all_species_usda$USDA_PLANTS_SYMBOL1 == "PISA2" & all_species_usda$DBH_CLASS == 7] <- "PIMO3"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "WS" & all_species_usda$USDA_PLANTS_SYMBOL1 == "POTR5" & all_species_usda$DBH_CLASS == 6] <- "ALRH2"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "WS" & all_species_usda$USDA_PLANTS_SYMBOL1 == "POTR5" & all_species_usda$DBH_CLASS == 7] <- "LIDE3"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "WS" & all_species_usda$USDA_PLANTS_SYMBOL1 == "PREM" & all_species_usda$DBH_CLASS == 3] <- "ARME"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "WS" & all_species_usda$USDA_PLANTS_SYMBOL1 == "PREM" & all_species_usda$DBH_CLASS == 4] <- "ARME"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "WS" & all_species_usda$USDA_PLANTS_SYMBOL1 == "PREM" & all_species_usda$DBH_CLASS == 5] <- "ARME"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "WS" & all_species_usda$USDA_PLANTS_SYMBOL1 == "PREM" & all_species_usda$DBH_CLASS == 6] <- "ARME"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "WS" & all_species_usda$USDA_PLANTS_SYMBOL1 == "PREM" & all_species_usda$DBH_CLASS == 7] <- "ARME"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "WS" & all_species_usda$USDA_PLANTS_SYMBOL1 == "QUDO" & all_species_usda$DBH_CLASS == 1] <- "QUKE"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "WS" & all_species_usda$USDA_PLANTS_SYMBOL1 == "QUDO" & all_species_usda$DBH_CLASS == 4] <- "QUKE"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "WS" & all_species_usda$USDA_PLANTS_SYMBOL1 == "QUDO" & all_species_usda$DBH_CLASS == 5] <- "QUKE"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "WS" & all_species_usda$USDA_PLANTS_SYMBOL1 == "QUDO" & all_species_usda$DBH_CLASS == 6] <- "QUKE"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "WS" & all_species_usda$USDA_PLANTS_SYMBOL1 == "QUDO" & all_species_usda$DBH_CLASS == 7] <- "QUKE"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "WS" & all_species_usda$USDA_PLANTS_SYMBOL1 == "QUWI2" & all_species_usda$DBH_CLASS == 6] <- "QUGA4"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "WS" & all_species_usda$USDA_PLANTS_SYMBOL1 == "QUWI2" & all_species_usda$DBH_CLASS == 7] <- "QUGA4"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "WS" & all_species_usda$USDA_PLANTS_SYMBOL1 == "SEGE3" & all_species_usda$DBH_CLASS == 3] <- "SESE3"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "WS" & all_species_usda$USDA_PLANTS_SYMBOL1 == "SEGE3" & all_species_usda$DBH_CLASS == 5] <- "SESE3"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "WS" & all_species_usda$USDA_PLANTS_SYMBOL1 == "SEGE3" & all_species_usda$DBH_CLASS == 7] <- "SESE3"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "WS" & all_species_usda$USDA_PLANTS_SYMBOL1 == "TSME" & all_species_usda$DBH_CLASS == 7] <- "TSHE"
all_species_usda$USDA_PLANTS_SYMBOL[all_species_usda$FVS_VARIANT == "WS" & all_species_usda$USDA_PLANTS_SYMBOL1 == "UMCA" & all_species_usda$DBH_CLASS == 7] <- "ARME"


all_species_usda$USDA_PLANTS_SYMBOL <- ifelse(is.na(all_species_usda$USDA_PLANTS_SYMBOL), all_species_usda$USDA_PLANTS_SYMBOL1, all_species_usda$USDA_PLANTS_SYMBOL)
all_species_usda$USDA_PLANTS_SYMBOL1 <- NULL

#the species_changes table shows all the species in the project that had their USDA_PLANTS_SYMBOL changed. OG_USDA_PLANTS_SYMBOL 
#is the original USDA_PLANTS_SYMBOL, USDA_PLANTS_SYMBOL is the final USDA_PLANTS_SYMBOL used for determining MORTRATE.
species_changes <- subset(all_species_usda, OG_USDA_PLANTS_SYMBOL != USDA_PLANTS_SYMBOL | is.na(OG_USDA_PLANTS_SYMBOL)) 

all_species_usda$OG_USDA_PLANTS_SYMBOL <- NULL

#Merge all_species_usda with mortality data to create all_species_mortrate
all_species_mortrate<- merge(all_species_usda, all_mort, by = c("USDA_PLANTS_SYMBOL", "FVS_VARIANT", "DBH_CLASS"), all = TRUE)
all_species_mortrate <- subset(all_species_mortrate, ID == 1) #remove values that did not appear in all_species_usda. these are not relevant to this project.
all_species_mortrate$ID <- NULL

#The next section separates species with an assigned MortRate and those without. 
#Species without an assigned mortrate will need to be addressed.
assigned <- all_species_mortrate[complete.cases(all_species_mortrate[,7]),]
unassigned <- all_species_mortrate[!complete.cases(all_species_mortrate[,7]),]

#find unassigned values that do not appear in ALL_VARIANTS file/all_mort table
problem_no_mortrate_for_species <- subset(unassigned, !(unassigned$USDA_PLANTS_SYMBOL %in% all_mort$USDA_PLANTS_SYMBOL))
unassigned <- subset(unassigned, (unassigned$USDA_PLANTS_SYMBOL %in% all_mort$USDA_PLANTS_SYMBOL)) #limit unassigned to only those with a USDA_PLANTS_SYMBOL in all_mort

#The steps below are taken to figure out what MORTRATES to use for unassigned values. 
#This process takes the average DBH, height, and crown ratio by DBH CLASS for trees in the 
#master database tree table, calculates crown base height, finds the least different CBH
#value from the other variants that have that USDA_PLANTS_SYMBOL and DBH_CLASS combination, 
#and uses that to select the "best" variant. This is assigned to a new MORT_VARIANT value, 
#which is then used to determine MORTRATE.
master_dbh <- master

master_dbh$DBH_CLASS[master_dbh$DBH < 5 & master_dbh$DBH >= 1 ] <- 1 #assign DBH classes
master_dbh$DBH_CLASS[master_dbh$DBH < 10 & master_dbh$DBH >= 5 ] <- 2
master_dbh$DBH_CLASS[master_dbh$DBH < 15 & master_dbh$DBH >= 10 ] <- 3
master_dbh$DBH_CLASS[master_dbh$DBH < 21 & master_dbh$DBH >= 15 ] <- 4
master_dbh$DBH_CLASS[master_dbh$DBH < 30 & master_dbh$DBH >= 21 ] <- 5
master_dbh$DBH_CLASS[master_dbh$DBH < 40 & master_dbh$DBH >= 30 ] <- 6
master_dbh$DBH_CLASS[master_dbh$DBH < 999 & master_dbh$DBH >= 40 ] <- 7

#get average DBH, HT, and CRNRATIO by DBH_CLASS
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
master_dbh_class$CBH <- round(((100 - master_dbh_class$CRNRATIO) * master_dbh_class$HT/100),1) #create CBH value

all_species_usda2 <- unique(all_species_usda[,c(1,2,6,7)])
master_dbh_class <- merge(master_dbh_class, all_species_usda2, by = c("FVS_VARIANT", "Species", "DBH_CLASS"), all = TRUE)
problem_no_usda_plants_symbol <- subset(master_dbh_class, !complete.cases(master_dbh_class[,7]) & FVS_VARIANT != "CR")

#The process below assigns a "best" variant based on least difference crown ratio value
unassigned <- unassigned[,c(1:7)]
unassigned <- merge(unassigned, master_dbh_class, by = c("FVS_VARIANT", "USDA_PLANTS_SYMBOL", "DBH_CLASS", "Species"), all = TRUE) #merge unassigned with master_dbh_class to get values that need MORTRATE
unassigned$USDA_PLANTS_SYMBOL <- as.character(unassigned$USDA_PLANTS_SYMBOL)
unassigned$FVS_VARIANT <- as.character(unassigned$FVS_VARIANT)
unassigned$best <- as.character(NA) #create a column for "best" value

all_mort$USDA_PLANTS_SYMBOL <- as.character(all_mort$USDA_PLANTS_SYMBOL)

master_dbh_class$FVS_VARIANT <- as.character(master_dbh_class$FVS_VARIANT)

iterations <- nrow(unassigned) 
for (i in 1:iterations) {
  #matchedrows are all rows from the unassigned table that match the row iteration's USDA_PLANT_SYMBOL and DBH_CLASS values. 
  #make sure the columns selected match the selections (they should, but it's good to double check)
  matchedrows <- all_mort[all_mort$USDA_PLANTS_SYMBOL == unassigned[i,2] &
                            all_mort$DBH_CLASS == unassigned[i,3],] 
  if (nrow(matchedrows) > 0) {
    #cbhfinds the master_dbh_class row for that combination of FVS_VARIANT, DBH_CLASS, Species, and USDA_PLANTS_SYMBOL
    cbh <- master_dbh_class[unassigned[i,1] == master_dbh_class$FVS_VARIANT & 
                              unassigned[i,3] == master_dbh_class$DBH_CLASS &
                              unassigned[i,2] == master_dbh_class$USDA_PLANTS_SYMBOL & 
                              unassigned[i,4] == master_dbh_class$Species,]
    if (nrow(cbh) > 0) {
      if (any(is.na(cbh$CBH))) {
        unassigned[i,12] <- as.character(matchedrows$FVS_VARIANT[1]) #if CBH value is null, use whatever variant is first in the matchedrows table.
        #CBH may be null for species with DBH classes that don't exist in master_tree. These are needed in case species grow into higher DBH classes
        #that may not currently exist in master_tree but may appear in FVS_TreeList later on.
      } else {
        difference <- abs(cbh$CBH - matchedrows$CBH) #take the difference between the matchedrows$CBH and the CBH value from that row for master_dbh_class
        matchedrows$difference <- difference #create a column with the difference between master_dbh cbh and matched rows from unassigned crown ratios
        best <- matchedrows[matchedrows$difference == min(matchedrows$difference),] #take the rows where the "difference" column is equal to the minimum and store it as "best"
        best <- best$FVS_VARIANT[1] #returns the ALL_VARIANT value of the first row of best
        unassigned[i,12] <- as.character(best) #assign the "best" value to the original "unassigned" row
      }
    } else {
      if (any((matchedrows$MORT_VARIANT %in% c("CA", "WS", "NC", "SO")))) {
        #if there are no records at all in master_dbh_class with the species/usda_plants_symbol/variant/DBH_CLASS (unlike above where there are
        #records they just don't have a CBH value), this section will search for matchedrows values in "CA","WS","NC", or "SO" variants and use the
        #first record from that variant. If there are no matchedrows in those variants, it takes the first value. 
        matchedrows2 <- subset(matchedrows, MORT_VARIANT %in% c("CA", "WS", "NC", "SO"))[1]
        unassigned[i,12] <- as.character(matchedrows2$FVS_VARIANT[1])
      } else {
        unassigned[i,12] <- as.character(matchedrows$FVS_VARIANT[1])
      }
    }
  } else {
    unassigned[i,12] <- NA
  }
}

names(unassigned)[12] <- "MORT_VARIANT" #create MORT_VARIANT column to show which variant was used for MORTRATE

unassigned_premerge <- unassigned[,c(1:4,6,12)] #get rid of DBH/HT/CRNRATIO/CBH columns
problem_no_mortrate_for_dbh_class <- subset(unassigned, !complete.cases(unassigned[,12]) & unassigned$FVS_VARIANT != "CR")

names(all_mort)[1] <- "MORT_VARIANT"
unassigned2 <- merge(unassigned_premerge, all_mort, by = c("MORT_VARIANT", "USDA_PLANTS_SYMBOL","DBH_CLASS"), all = TRUE)

assigned$MORT_VARIANT <- assigned$FVS_VARIANT
assigned$OG_FVS_SPECIES <- NULL

mortrate_final <-  rbind(assigned, unassigned2)
mortprobgroup_species <- mortrate_final[,c(2:4,6)]

#Use these files below to write the "problem" tables to csv files. If your 
#"problem" tables have 0 rows, congratulations, you don't need to do anything!
#If they have rows, you need to address the issue described. This is done by the manual
#manipulation of all_species_usda above to change USDA_PLANTS_SYMBOL to something that
#exists in all_variants/all_mort and will therefore have a MORTRATE value. 
#write.csv(problem_no_mortrate_for_species, "problem_no_mortrate_for_species.csv")
#write.csv(problem_no_mortrate_for_dbh_class, "problem_no_mortrate_for_dbh_class.csv")
#write.csv(problem_no_usda_plants_symbol, "problem_no_usda_plants_symbol.csv")
#write.csv(problem_master_nas, "problem_master_nas.csv") #Note these may not be of any concern

#Prepare the master_tree table for iteration by package
master_tree_trim <- master_tree[,c(1,10,57)]
master_tree_trim$TreeId <- substr(master_tree_trim$fvs_tree_id,3,9)
master_tree_trim$TreeId <- substr(master_tree_trim$TreeId,regexpr("[^0]",master_tree_trim$TreeId),nchar(master_tree_trim$TreeId))
master_tree_trim$FVS_VARIANT <- substr(master_tree_trim$fvs_tree_id,0,2)
master_tree_trim <- subset(master_tree_trim, complete.cases(master_tree_trim$TreeId))
master_tree_trim$fvs_tree_id <- NULL
names(master_tree_trim)[1] <- "StandID"
master_tree_trim$TreeId <- as.character(master_tree_trim$TreeId)


#The function below will carry out MORTCALC calculations for all packages in the variant
#folder. The only arguments are directory and variantname (demonstrated below). The text
#below creates the function. The next section  after this will run the function.
#NOTE: this is a memory intensive process and R-32bit is finicky. If you get a message
#about not being able to allocate vector of size X, try entering gc() into the console and
#re-running the function for remaining packages. If it still does not work, try restarting R
#in a new session. If it still does not work, you may have to export your access files to csvs
#run the various steps in 64-bit R, and then save them and restart in 32-bit R before you 
#reconnect to the Access database. This should not happen especially often but it might for
#larger variants with intensive treatments.
CreateSurvVolRatioTable <- function(directory, variantname) {
  setwd(directory)#sets the working directory to the directory variable
  path <- list.files(path = ".", pattern = glob2rx(paste("FVSOUT_", variantname, "_P0", "*.MDB", sep = ""))) #lists all the files in the directory that begin with FVSOUT_{variantname}_P0 and end with .MDB (case sensitive)
  master_tree_trim <- subset(master_tree_trim, FVS_VARIANT == variantname)
  rows <- as.numeric(0)
  numfiles <- nrow(data.frame(path)) #calculates the number of package MDB files based on the path variable above
  for (i in 1:numfiles) {
    file <- file.path(directory, path[i]) #creates a file path using the directory variable and the iteration of the pakage MDB
    x <- paste("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", file, sep = "") #writes the argument for the odbcDriverConnect function below
    conn <- odbcDriverConnect(x) #Connect to access database 
    
    #The process below links FVS_TreeList and master_tree_trim to give species codes for species that FVS lumps
    #together (e.g. 298 and 998)
    FVS_TreeList <- sqlFetch(conn, "FVS_TreeList", as.is = TRUE)
    FVS_TreeList <- FVS_TreeList[,c(2:3,5,7,8,11,13,22)] #trim table for efficiency
    FVS_TreeList <- subset(FVS_TreeList, TreeVal != 9) #remove trees with TreeVal = 9
    FVS_TreeList$TreeVal <- NULL
    FVS_TreeList$OG_Species <- FVS_TreeList$Species
    FVS_TreeList$Species <- NULL
    FVS_TreeList$TreeId <- as.character(trimws(FVS_TreeList$TreeId), which = "both")
    FVS_TreeList$FVS_VARIANT <- variantname
    FVS_TreeList <- merge(FVS_TreeList, master_tree_trim, by = c("StandID", "TreeId", "FVS_VARIANT"), all = TRUE)
    FVS_TreeList$spcd2 <- ifelse(is.na(FVS_TreeList$spcd), FVS_TreeList$OG_Species, FVS_TreeList$spcd)
    FVS_TreeList$OG_Species <- NULL
    FVS_TreeList$spcd <- NULL
    FVS_TreeList$spcd2 <- as.numeric(FVS_TreeList$spcd2)
    FVS_TreeList$Species <- sprintf("%03d", FVS_TreeList$spcd2)
    FVS_TreeList$spcd2 <- NULL
    
    #DBH_CLASS assignments must be made manually. 
    FVS_TreeList$DBH_CLASS[FVS_TreeList$DBH < 5 & FVS_TreeList$DBH >= 1 ] <- 1
    FVS_TreeList$DBH_CLASS[FVS_TreeList$DBH < 10 & FVS_TreeList$DBH >= 5 ] <- 2
    FVS_TreeList$DBH_CLASS[FVS_TreeList$DBH < 15 & FVS_TreeList$DBH >= 10 ] <- 3
    FVS_TreeList$DBH_CLASS[FVS_TreeList$DBH < 21 & FVS_TreeList$DBH >= 15 ] <- 4
    FVS_TreeList$DBH_CLASS[FVS_TreeList$DBH < 30 & FVS_TreeList$DBH >= 21 ] <- 5
    FVS_TreeList$DBH_CLASS[FVS_TreeList$DBH < 40 & FVS_TreeList$DBH >= 30 ] <- 6
    FVS_TreeList$DBH_CLASS[FVS_TreeList$DBH < 999 & FVS_TreeList$DBH >= 40 ] <- 7
    
    FVS_TreeList <- subset(FVS_TreeList, complete.cases(FVS_TreeList[,9])) #remove trees below minimum DBH_CLASS
    
    #Merges the FVS_TreeList and mortprob tables by DBH Class, Variant, and Species code
    FVS_TreeList$Species <- as.character(FVS_TreeList$Species)
    mortprobgroup_species$Species <- as.character(mortprobgroup_species$Species)
    mortprobgroup_species <- subset(mortprobgroup_species, FVS_VARIANT == variantname)
    FVS_TreeList <- merge(FVS_TreeList, mortprobgroup_species, by = c("DBH_CLASS", "FVS_VARIANT", "Species"), all = TRUE)
    FVS_TreeList <- FVS_TreeList[complete.cases(FVS_TreeList[,4:10]),] #remove NAs
    
    #Calculations for Volume and Survival Volume 
    FVS_TreeList$Vol <- FVS_TreeList$TCuFt * FVS_TreeList$TPA
    FVS_TreeList$SurvVol <- (1 - FVS_TreeList$MORTRATE) * FVS_TreeList$TCuFt * FVS_TreeList$TPA
    
    #Move and trim data from FVS_TreeList to new MC_SURV_VOL_TREE table, 
    #remove NA MORTRATE rows, and sum SurvVol by stand and year
    #MC_SURV_VOL_TREE <- FVS_TreeList[,c(2,5,3,6,7,1,13,8,9,11,15,16)]
    MC_SURV_VOL_TREE <- FVS_TreeList
    MC_SURV_VOL_TREE1 <- MC_SURV_VOL_TREE[complete.cases(MC_SURV_VOL_TREE[,10]),]
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
    SurvVolRatio$MortVol_FOFEM <- SurvVolRatio$VolSum-SurvVolRatio$SurvVolSum
    sqlSave(conn, dat = SurvVolRatio, tablename = "SurvVolRatio", rownames = FALSE)
    
    #add new SurvVolRatio column to FVS_Summary, join on StandID and add SurvVolRatio
    sqlQuery(conn, 'ALTER TABLE FVS_Summary ADD COLUMN SurvVolRatio NUMERIC')
    sqlQuery(conn, 'ALTER TABLE FVS_Summary ADD COLUMN VolSum NUMERIC')
    sqlQuery(conn, 'ALTER TABLE FVS_Summary ADD COLUMN SurvVolSum NUMERIC')
    sqlQuery(conn, 'ALTER TABLE FVS_Summary ADD COLUMN MortVol_FOFEM NUMERIC')
    sqlQuery(conn, 'UPDATE FVS_Summary INNER JOIN SurvVolRatio ON (FVS_Summary.Year = SurvVolRatio.Year) AND (FVS_Summary.StandID = SurvVolRatio.StandID) SET FVS_Summary.SurvVolRatio = [SurvVolRatio].[SurvVolRatio]')
    sqlQuery(conn, 'UPDATE FVS_Summary INNER JOIN SurvVolRatio ON (FVS_Summary.Year = SurvVolRatio.Year) AND (FVS_Summary.StandID = SurvVolRatio.StandID) SET FVS_Summary.VolSum = [SurvVolRatio].[VolSum]')
    sqlQuery(conn, 'UPDATE FVS_Summary INNER JOIN SurvVolRatio ON (FVS_Summary.Year = SurvVolRatio.Year) AND (FVS_Summary.StandID = SurvVolRatio.StandID) SET FVS_Summary.SurvVolSum = [SurvVolRatio].[SurvVolSum]')
    sqlQuery(conn, 'UPDATE FVS_Summary INNER JOIN SurvVolRatio ON (FVS_Summary.Year = SurvVolRatio.Year) AND (FVS_Summary.StandID = SurvVolRatio.StandID) SET FVS_Summary.MortVol_FOFEM = [SurvVolRatio].[MortVol_FOFEM]')
    
    #Close all open connections to access
    odbcCloseAll()
  }
}

#Run the CreateSurvVol function. Change the directory location and variantname
#as needed.
CreateSurvVolRatioTable(directory = "H:/cec_20170915/fvs/data/CA", variantname = "CA")
CreateSurvVolRatioTable("H:/cec_20170915/fvs/data/NC", "NC")
CreateSurvVolRatioTable("H:/cec_20170915/fvs/data/SO", "SO")
CreateSurvVolRatioTable("H:/cec_20170915/fvs/data/WS", "WS")

#The function below will delete the SurvVolRatio table and all the columns added to FVS_Summary by the 
#CreateSurvVolRatioTable function above. This is useful in case you need to re-run your calculations after making
#adjustments. 
DeleteSurvVolRatioTable <- function (directory, variantname) {
  setwd(directory)#sets the working directory to the directory variable
  path <- list.files(path = ".", pattern = glob2rx(paste("FVSOUT_", variantname, "_P0", "*.MDB", sep = ""))) #lists all the files in the directory that begin with FVSOUT_{variantname}_P0 and end with .MDB (case sensitive)
  rows <- as.numeric(0)
  numfiles <- nrow(data.frame(path)) #calculates the number of package MDB files based on the path variable above
  for (i in 1:numfiles) {
    file <- file.path(directory, path[i]) #creates a file path using the directory variable and the iteration of the pakage MDB
    x <- paste("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", file, sep = "") #writes the argument for the odbcDriverConnect function below
    conn <- odbcDriverConnect(x) #Connect to the access database 
    sqlQuery(conn, 'ALTER TABLE FVS_Summary DROP SurvVolRatio')
    sqlQuery(conn, 'ALTER TABLE FVS_Summary DROP VolSum')
    sqlQuery(conn, 'ALTER TABLE FVS_Summary DROP SurvVolSum')
    sqlQuery(conn, 'ALTER TABLE FVS_Summary DROP MortVol_FOFEM')
    sqlQuery(conn, 'DROP TABLE SurvVolRatio')
    sqlQuery(conn, 'DROP TABLE FVS_ATRTList')
    
    #Close all open connections to access
    odbcCloseAll()
  }
}

DeleteSurvVolRatioTable(directory = "H:/cec_20170915/fvs/data/CA", variantname = "CA")
DeleteSurvVolRatioTable("H:/cec_20170915/fvs/data/NC", "NC")
DeleteSurvVolRatioTable("H:/cec_20170915/fvs/data/SO", "SO")
DeleteSurvVolRatioTable("H:/cec_20170915/fvs/data/WS", "WS")
