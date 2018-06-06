#This script was created by Carlin Starrs in 2018
#The script calculates Fire Resistance Score, Hazard Score, and Canopy Base Height
#using method created by Jeremy Fried/Terri Jaine..

#This script pulls in all the necessary parameters from the various PREPOST tables and then runs the 
#calculations and adds it all to the PREPOST_FVS_SUMMARY database. 

#Make sure you have the Microsoft Access Databse Engine Driver https://www.microsoft.com/en-us/download/confirmation.aspx?id=23734
#and you are using 32-bit R (set in RStudio by going to Tools -> Global Options)
packages <- c("RODBC", "dplyr")

package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, repos="http://cran.r-project.org", dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

options(scipen = 999) #this is important for making sure your stand IDs do not get translated to scientific notation


#Project root location:
project.location <- "H:/cec_20180529"

#Pull in Canopy bulk density from PRE_FVS_POTFIRE and POST_FVS_POTFIRE tables. 
#Start by getting PREPOST_FVS_SUMMARY in 
conn.path <-  paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", file.path(project.location, "fvs", "db", "PREPOST_FVS_SUMMARY.ACCDB"))
conn <- odbcDriverConnect(conn.path)
PRE_FVS_SUMMARY <- sqlFetch(conn, "PRE_FVS_SUMMARY", as.is = TRUE) 
POST_FVS_SUMMARY <- sqlFetch(conn, "POST_FVS_SUMMARY", as.is = TRUE) 
odbcCloseAll()

#Pull in PREPOST_FVS_POTFIRE
conn.path <-  paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", file.path(project.location, "fvs", "db", "PREPOST_FVS_POTFIRE.ACCDB"))
conn <- odbcDriverConnect(conn.path)
PRE_FVS_POTFIRE <- sqlFetch(conn, "PRE_FVS_POTFIRE", as.is = TRUE) 
POST_FVS_POTFIRE <- sqlFetch(conn, "POST_FVS_POTFIRE", as.is = TRUE) 
odbcCloseAll()

#Create new tables with just relevant data from POTFIRE
pre_potfire <- data.frame("biosum_cond_id" = PRE_FVS_POTFIRE$biosum_cond_id, 
                          "rxpackage"= PRE_FVS_POTFIRE$rxpackage, 
                          "rx" =  PRE_FVS_POTFIRE$rx, 
                          "fvs_variant" = PRE_FVS_POTFIRE$fvs_variant, 
                          "rxcycle" = PRE_FVS_POTFIRE$rxcycle, 
                          "Canopy_Density" = PRE_FVS_POTFIRE$Canopy_Density, 
                          "Torch_Index" = PRE_FVS_POTFIRE$Torch_Index,
                          "PTorch_Sev" = PRE_FVS_POTFIRE$PTorch_Sev,
                          "Surf_Flame_sev" = PRE_FVS_POTFIRE$Surf_Flame_Sev,
                          "Mortality_VOL_Sev" = PRE_FVS_POTFIRE$Mortality_VOL_Sev, 
                          "Mortality_VOL_Mod" = PRE_FVS_POTFIRE$Mortality_VOL_Mod)

post_potfire <- data.frame("biosum_cond_id" = POST_FVS_POTFIRE$biosum_cond_id, 
                           "rxpackage" = POST_FVS_POTFIRE$rxpackage, 
                           "rx" =  POST_FVS_POTFIRE$rx, 
                           "fvs_variant" = POST_FVS_POTFIRE$fvs_variant, 
                           "rxcycle" = POST_FVS_POTFIRE$rxcycle, 
                           "Canopy_Density" = POST_FVS_POTFIRE$Canopy_Density,
                           "Torch_Index" = POST_FVS_POTFIRE$Torch_Index,
                           "PTorch_Sev" = POST_FVS_POTFIRE$PTorch_Sev,
                           "Surf_Flame_sev" = POST_FVS_POTFIRE$Surf_Flame_Sev,
                           "Mortality_VOL_Sev" = POST_FVS_POTFIRE$Mortality_VOL_Sev, 
                           "Mortality_VOL_Mod" = POST_FVS_POTFIRE$Mortality_VOL_Mod)

#Merge them with FVS_SUMMARY
PRE_FVS_SUMMARY <- merge(PRE_FVS_SUMMARY, pre_potfire, by = c("biosum_cond_id", "rxpackage", "rx", "fvs_variant", "rxcycle"), all = TRUE)
POST_FVS_SUMMARY <- merge(POST_FVS_SUMMARY, post_potfire, by = c("biosum_cond_id", "rxpackage", "rx", "fvs_variant", "rxcycle"), all = TRUE)

#Pull in Percent Resistant Basal Area from PRE_FVS_COMPUTE and POST_FVS_COMPUTE tables. 
conn.path <-  paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", file.path(project.location, "fvs", "db", "PREPOST_FVS_COMPUTE.ACCDB"))
conn <- odbcDriverConnect(conn.path)
PRE_FVS_COMPUTE <- sqlFetch(conn, "PRE_FVS_COMPUTE", as.is = TRUE) 
POST_FVS_COMPUTE <- sqlFetch(conn, "POST_FVS_COMPUTE", as.is = TRUE) 
odbcCloseAll()


#Create new tables with just relevant data from COMPUTE
pre_compute <- data.frame("biosum_cond_id" = PRE_FVS_COMPUTE$biosum_cond_id, 
                          "rxpackage"= PRE_FVS_COMPUTE$rxpackage, 
                          "rx" =  PRE_FVS_COMPUTE$rx, 
                          "fvs_variant" = PRE_FVS_COMPUTE$fvs_variant, 
                          "rxcycle" = PRE_FVS_COMPUTE$rxcycle, 
                          "PERRESBA" = PRE_FVS_COMPUTE$PERRESBA)

post_compute <- data.frame("biosum_cond_id" = POST_FVS_COMPUTE$biosum_cond_id, 
                           "rxpackage" = POST_FVS_COMPUTE$rxpackage, 
                           "rx" =  POST_FVS_COMPUTE$rx, 
                           "fvs_variant" = POST_FVS_COMPUTE$fvs_variant, 
                           "rxcycle" = POST_FVS_COMPUTE$rxcycle, 
                           "PERRESBA" = POST_FVS_COMPUTE$PERRESBA)

#Merge them with FVS_SUMMARY
PRE_FVS_SUMMARY <- merge(PRE_FVS_SUMMARY, pre_compute, by = c("biosum_cond_id", "rxpackage", "rx", "fvs_variant", "rxcycle"), all = TRUE)
POST_FVS_SUMMARY <- merge(POST_FVS_SUMMARY, post_compute, by = c("biosum_cond_id", "rxpackage", "rx", "fvs_variant", "rxcycle"), all = TRUE)

#SurvVolRatio should already be in FVS_SUMMARY

###Canopy Base Height calculation (formerly canopy_dist.R)
#Pull in the PREPOST_FVS_STRCLASS database from fvs/db
conn.path <-  paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", file.path(project.location, "fvs", "db", "PREPOST_FVS_STRCLASS.ACCDB"))
conn <- odbcDriverConnect(conn.path)
PRE_FVS_STRCLASS <- sqlFetch(conn, "PRE_FVS_STRCLASS", as.is = TRUE)  #pull in tables from PREPOST_FVS_STRCLASS
POST_FVS_STRCLASS <- sqlFetch(conn, "POST_FVS_STRCLASS", as.is = TRUE) 

#Trim tables to just relevant data
pre_STRCLASS <- data.frame("biosum_cond_id" = PRE_FVS_STRCLASS$biosum_cond_id, 
                           "rxpackage"= PRE_FVS_STRCLASS$rxpackage, 
                           "rx" =  PRE_FVS_STRCLASS$rx, 
                           "fvs_variant" = PRE_FVS_STRCLASS$fvs_variant, 
                           "rxcycle" = PRE_FVS_STRCLASS$rxcycle, 
                           "Stratum_1_Nom_Ht" = PRE_FVS_STRCLASS$Stratum_1_Nom_Ht,
                           "Stratum_1_Crown_Base" = PRE_FVS_STRCLASS$Stratum_1_Crown_Base,
                           "Stratum_1_Status_Code" = PRE_FVS_STRCLASS$Stratum_1_Status_Code,
                           "Stratum_2_Nom_Ht" = PRE_FVS_STRCLASS$Stratum_2_Nom_Ht,
                           "Stratum_2_Crown_Base" = PRE_FVS_STRCLASS$Stratum_2_Crown_Base,
                           "Stratum_2_Status_Code" = PRE_FVS_STRCLASS$Stratum_2_Status_Code,
                           "Stratum_3_Nom_Ht" = PRE_FVS_STRCLASS$Stratum_3_Nom_Ht,
                           "Stratum_3_Crown_Base" = PRE_FVS_STRCLASS$Stratum_3_Crown_Base,
                           "Stratum_3_Status_Code" = PRE_FVS_STRCLASS$Stratum_3_Status_Code)

post_STRCLASS <- data.frame("biosum_cond_id" = POST_FVS_STRCLASS$biosum_cond_id, 
                            "rxpackage" = POST_FVS_STRCLASS$rxpackage, 
                            "rx" =  POST_FVS_STRCLASS$rx, 
                            "fvs_variant" = POST_FVS_STRCLASS$fvs_variant, 
                            "rxcycle" = POST_FVS_STRCLASS$rxcycle, 
                            "Stratum_1_Nom_Ht" = POST_FVS_STRCLASS$Stratum_1_Nom_Ht,
                            "Stratum_1_Crown_Base" = POST_FVS_STRCLASS$Stratum_1_Crown_Base,
                            "Stratum_1_Status_Code" = POST_FVS_STRCLASS$Stratum_1_Status_Code,
                            "Stratum_2_Nom_Ht" = POST_FVS_STRCLASS$Stratum_2_Nom_Ht,
                            "Stratum_2_Crown_Base" = POST_FVS_STRCLASS$Stratum_2_Crown_Base,
                            "Stratum_2_Status_Code" = POST_FVS_STRCLASS$Stratum_2_Status_Code,
                            "Stratum_3_Nom_Ht" = POST_FVS_STRCLASS$Stratum_3_Nom_Ht,
                            "Stratum_3_Crown_Base" = POST_FVS_STRCLASS$Stratum_3_Crown_Base,
                            "Stratum_3_Status_Code" = POST_FVS_STRCLASS$Stratum_3_Status_Code)

# #Add dist and CBH columns
pre_STRCLASS$dist1 <- NA
pre_STRCLASS$dist2 <- NA
pre_STRCLASS$dist3 <- NA
pre_STRCLASS$CBH <- NA
post_STRCLASS$dist1 <- NA
post_STRCLASS$dist2 <- NA
post_STRCLASS$dist3 <- NA
post_STRCLASS$CBH<- NA

#Calculate CBH. The section below defines the canopy_dist function. For information on how this was 
#developed, see the documentation in the Additional Data folder on Github (CBH Illustration, Fire resistance score email...)
canopy_dist <- function(data) {
  data$dist1[data$Stratum_1_Status_Code > 0 & data$Stratum_2_Status_Code > 0] <- data$Stratum_1_Crown_Base[data$Stratum_1_Status_Code > 0 & data$Stratum_2_Status_Code > 0] - data$Stratum_2_Nom_Ht[data$Stratum_1_Status_Code > 0 & data$Stratum_2_Status_Code > 0]
  data$dist1[data$Stratum_1_Status_Code > 0 & data$Stratum_2_Status_Code == 0 & data$Stratum_3_Status_Code > 0] <- data$Stratum_1_Crown_Base[data$Stratum_1_Status_Code > 0 & data$Stratum_2_Status_Code == 0 & data$Stratum_3_Status_Code > 0] - data$Stratum_3_Nom_Ht[data$Stratum_1_Status_Code > 0 & data$Stratum_2_Status_Code == 0 & data$Stratum_3_Status_Code > 0]
  data$dist1[data$Stratum_1_Status_Code > 0 & data$Stratum_2_Status_Code == 0 & data$Stratum_3_Status_Code == 0] <- data$Stratum_1_Crown_Base[data$Stratum_1_Status_Code > 0 & data$Stratum_2_Status_Code == 0 & data$Stratum_3_Status_Code == 0]
  data$dist1[data$Stratum_1_Status_Code == 0 & data$Stratum_2_Status_Code > 0 & data$Stratum_3_Status_Code > 0] <- data$Stratum_2_Crown_Base[data$Stratum_1_Status_Code == 0 & data$Stratum_2_Status_Code > 0 & data$Stratum_3_Status_Code > 0] - data$Stratum_3_Nom_Ht[data$Stratum_1_Status_Code == 0 & data$Stratum_2_Status_Code > 0 & data$Stratum_3_Status_Code > 0]
  data$dist1[data$Stratum_1_Status_Code == 0 & data$Stratum_2_Status_Code > 0 & data$Stratum_3_Status_Code == 0] <- data$Stratum_2_Crown_Base[data$Stratum_1_Status_Code == 0 & data$Stratum_2_Status_Code > 0 & data$Stratum_3_Status_Code == 0]
  data$dist1[data$Stratum_1_Status_Code == 0 & data$Stratum_2_Status_Code == 0] <- data$Stratum_3_Crown_Base[data$Stratum_1_Status_Code == 0 & data$Stratum_2_Status_Code == 0]
  
  data$dist2[data$Stratum_1_Status_Code > 0 & data$Stratum_2_Status_Code > 0 & data$Stratum_3_Status_Code > 0] <- data$Stratum_2_Crown_Base[data$Stratum_1_Status_Code > 0 & data$Stratum_2_Status_Code > 0 & data$Stratum_3_Status_Code > 0] - data$Stratum_3_Nom_Ht[data$Stratum_1_Status_Code > 0 & data$Stratum_2_Status_Code > 0 & data$Stratum_3_Status_Code > 0]
  data$dist2[data$Stratum_1_Status_Code > 0 & data$Stratum_2_Status_Code > 0 & data$Stratum_3_Status_Code == 0] <- data$Stratum_2_Crown_Base[data$Stratum_1_Status_Code > 0 & data$Stratum_2_Status_Code > 0 & data$Stratum_3_Status_Code == 0]
  data$dist2[data$Stratum_1_Status_Code > 0 & data$Stratum_2_Status_Code == 0 & data$Stratum_3_Status_Code > 0] <- data$Stratum_3_Crown_Base[data$Stratum_1_Status_Code > 0 & data$Stratum_2_Status_Code == 0 & data$Stratum_3_Status_Code > 0]  
  data$dist2[data$Stratum_1_Status_Code == 0 & data$Stratum_2_Status_Code > 0 & data$Stratum_3_Status_Code > 0] <- data$Stratum_3_Crown_Base[data$Stratum_1_Status_Code == 0 & data$Stratum_2_Status_Code > 0 & data$Stratum_3_Status_Code > 0]
  
  data$dist3[data$Stratum_1_Status_Code > 0 & data$Stratum_2_Status_Code > 0 & data$Stratum_3_Status_Code > 0] <- data$Stratum_3_Crown_Base[data$Stratum_1_Status_Code > 0 & data$Stratum_2_Status_Code > 0 & data$Stratum_3_Status_Code > 0]
  
  data$CBH <- apply(data[,c(15:17)], 1, function(x) max(x, na.rm = TRUE))
  
  return(data)
}


pre_STRCLASS <- canopy_dist(pre_STRCLASS)
post_STRCLASS <- canopy_dist(post_STRCLASS)


#Save to new tables in the PREPOST_FVS_STRCLASS database. This may take a few minutes as well.
sqlSave(conn, dat = pre_STRCLASS, tablename = "PRECBH", rownames = FALSE)
sqlSave(conn, dat = post_STRCLASS, tablename = "POSTCBH", rownames = FALSE)

#Disconnect from database
odbcCloseAll()

PRECBH <- pre_STRCLASS
POSTCBH <- post_STRCLASS

#Pull in Percent Resistant Basal Area from PRE_FVS_COMPUTE and POST_FVS_COMPUTE tables. 
#Create new tables with just relevant data from STRCLASS
pre_CBH <- data.frame("biosum_cond_id" = PRECBH$biosum_cond_id, 
                      "rxpackage"= PRECBH$rxpackage, 
                      "rx" =  PRECBH$rx, 
                      "fvs_variant" = PRECBH$fvs_variant, 
                      "rxcycle" = PRECBH$rxcycle, 
                      "CBH" = PRECBH$CBH)

post_CBH <- data.frame("biosum_cond_id" = POSTCBH$biosum_cond_id, 
                       "rxpackage" = POSTCBH$rxpackage, 
                       "rx" =  POSTCBH$rx, 
                       "fvs_variant" = POSTCBH$fvs_variant, 
                       "rxcycle" = POSTCBH$rxcycle, 
                       "CBH" = POSTCBH$CBH)

#Merge with FVS_SUMMARY
PRE_FVS_SUMMARY <- merge(PRE_FVS_SUMMARY, pre_CBH, by = c("biosum_cond_id", "rxpackage", "rx", "fvs_variant", "rxcycle"), all = TRUE)
POST_FVS_SUMMARY <- merge(POST_FVS_SUMMARY, post_CBH, by = c("biosum_cond_id", "rxpackage", "rx", "fvs_variant", "rxcycle"), all = TRUE)

#manage NA values
NA_fix <- function(data) {
  data$SurvVolRatio[is.na(data$SurvVolRatio)] <- 1
  data$PERRESBA[is.na(data$PERRESBA)] <- 1
  data$CBH[is.na(data$CBH)] <- 31
  
  return(data)
}

PRE_FVS_SUMMARY_NA <- NA_fix(PRE_FVS_SUMMARY)
POST_FVS_SUMMARY_NA <- NA_fix(POST_FVS_SUMMARY)

any(is.na(PRE_FVS_SUMMARY_NA$SurvVolRatio))
any(is.na(POST_FVS_SUMMARY_NA$SurvVolRatio))

score_it <- function(x) {
  #Calculate Fire Resistance Score by scoring each ofthe above 4 variable from 0-3 and then summing the scores. 
  #Canopy Bulk Density: 
  ## > 0.15           | score = 0
  ## > 0.11 & <= 0.15 | score = 1
  ## > 0.05 & <= 0.11 | score = 2
  ##  <= 0.05         | score = 3
  
  #Canopy Base Height: 
  ## <= 7         | score = 0
  ## > 7 & <= 20  | score = 1
  ## > 20 & <= 30 | score = 2
  ## > 30         | score = 3
  
  #Percent Resistant Basal Area: 
  ## <= .25         | score = 0
  ## > .25 & <= .50 | score = 1
  ## > .50 & <= .75 | score = 2
  ## > .75          | score = 3
  
  #Survival Volume Ratio:
  ## <= .02         | score = 0
  ## > .02 & <= .30 | score = 1
  ## > .30 & <= .75 | score = 2
  ## > .75          | score = 3
  
  
  x$CBD_Score <- cut(x$Canopy_Density, breaks = c(-0.1, 0.05, 0.11, 0.15, max(x$Canopy_Density, na.rm = TRUE)), labels = c(3,2,1,0), right = TRUE)
  x$CBH_Score <- cut(x$CBH, breaks = c(-0.1, 7, 20, 30, max(x$CBH, na.rm = TRUE)), labels = c(0,1,2,3), right = TRUE)
  x$PERRESBA_score <- cut(x$PERRESBA, breaks = c(-0.1, 0.25, 0.50, 0.75 ,max(x$PERRESBA, na.rm = TRUE)), labels = c(0,1,2,3), right = TRUE)
  x$SurvVolRatio_score <- cut(x$SurvVolRatio, breaks = c(-0.1,0.02,0.30,0.75,max(x$SurvVolRatio, na.rm = TRUE)), labels = c(0,1,2,3), right = TRUE)
  
  
  x$CBD_Score <- as.numeric(as.character(x$CBD_Score))
  x$CBH_Score <- as.numeric(as.character(x$CBH_Score))
  x$PERRESBA_score <- as.numeric(as.character(x$PERRESBA_score))
  x$SurvVolRatio_score <- as.numeric(as.character(x$SurvVolRatio_score))
  
  columns <- c(which(names(x) == "CBD_Score"), which(names(x) == "CBH_Score"), which(names(x) == "PERRESBA_score"), which(names(x) == "SurvVolRatio_score"))
  
  x$FRS <- rowSums(x[,columns], na.rm = TRUE) 
  
  #Calculate Hazard Score by scoring each ofthe above 4 variable as 0 or 1 and then summing the scores. 
  #This function also calculates MortVolPct2 by taking the Mortality_VOL_Sev and dividing by non-zero TCUft values. It also does this
  #for Mortality_VOL_Mod. This produces to Hazard Scores: one for Severe and one for Moderate. It also adjusts the Torch Index
  #values that are too high or too low. 
  
  #Torch Index: 
  ## < 20  | score = 1
  ## >= 20 | score = 0
  
  #PTorch_Sev: 
  ## > 0.20  | score = 1
  ## <= 0.20 | score = 0
  
  #MortVolPct_Sev: 
  ## > 0.30  | score = 1
  ## <= 0.30 | score = 0
  
  #MortVolPct_Mod: 
  ## > 0.30  | score = 1
  ## <= 0.30 | score = 0
  
  #Surf_Flame_Sev: 
  ## > 4  | score = 1
  ## <= 4 | score = 0
  
  
  
  x$MortVolPct_FOFEM <- x$MortVol_FOFEM/x$VolSum
  x$MortVolPct_FOFEM[x$VolSum == 0] <- 0
  
  x$MortVolPct2_Sev <- x$Mortality_VOL_Sev/x$TCuFt
  x$MortVolPct2_Sev[x$TCuFt == 0] <- 0
  
  x$MortVolPct2_Mod <- x$Mortality_VOL_Mod/x$TCuFt
  x$MortVolPct2_Mod[x$TCuFt == 0] <- 0
  
  x$Torch_Index2 <- x$Torch_Index
  x$Torch_Index2[x$Torch_Index < 0] <- 102
  x$Torch_Index2[x$Torch_Index > 100] <- 101
  
  x$Torch_Index_score <- cut(x$Torch_Index2, breaks = c(-0.1, 20, max(x$Torch_Index2, na.rm = TRUE)), labels = c(1,0), right = TRUE)
  x$PTorch_Sev_Score <- cut(x$PTorch_Sev, breaks = c(-0.1, 0.2, max(x$PTorch_Sev, na.rm = TRUE)), labels = c(1,0), right = TRUE)
  x$Surf_Flame_Sev_score <- cut(x$Surf_Flame_sev, breaks = c(-0.1, 4, max(x$Surf_Flame_sev, na.rm = TRUE)), labels = c(1,0), right = TRUE)
  x$MortVolPct2_Sev_Score <- cut(x$MortVolPct2_Sev, breaks = c(-0.1, 0.3, max(x$MortVolPct2_Sev, na.rm = TRUE)), labels = c(1,0), right = TRUE)
  x$MortVolPct2_Mod_Score <- cut(x$MortVolPct2_Mod, breaks = c(-0.1, 0.3, max(x$MortVolPct2_Mod, na.rm = TRUE)), labels = c(1,0), right = TRUE)
  
  
  x$Torch_Index_score<- as.numeric(as.character(x$Torch_Index_score))
  x$PTorch_Sev_Score <- as.numeric(as.character(x$PTorch_Sev_Score))
  x$Surf_Flame_Sev_score<- as.numeric(as.character(x$Surf_Flame_Sev_score))
  x$MortVolPct2_Sev_Score <- as.numeric(as.character(x$MortVolPct2_Sev_Score))
  x$MortVolPct2_Mod_Score <- as.numeric(as.character(x$MortVolPct2_Mod_Score))
  
  columns_sev <- c(which(names(x) == "Torch_Index_score"), which(names(x) == "PTorch_Sev_Score"), which(names(x) == "Surf_Flame_Sev_score"), which(names(x) == "MortVolPct2_Sev_Score"))
  columns_mod <- c(which(names(x) == "Torch_Index_score"), which(names(x) == "PTorch_Sev_Score"), which(names(x) == "Surf_Flame_Sev_score"), which(names(x) == "MortVolPct2_Mod_Score"))
  
  x$HS_Sev <- rowSums(x[,columns_sev], na.rm = TRUE)
  x$HS_Mod <- rowSums(x[,columns_mod], na.rm = TRUE) 
  
  return(x)
}

pre_scored <- score_it(PRE_FVS_SUMMARY_NA)
post_scored <- score_it(POST_FVS_SUMMARY_NA)

all <- rbind(pre_scored, post_scored)
setwd(project.location)
write.csv(all, paste0("fvs_summary_all_postprocessor", format(Sys.Date(),"%Y%m%d"), ".csv"))

create_avg_table <- function(pre, post) {
  pre.relevant.data <- data.frame("biosum_cond_id" = pre$biosum_cond_id, "rxpackage" = pre$rxpackage, "rx" = pre$rx, "rxcycle" = pre$rxcycle, "Year" = pre$Year,
                                  "CBH_Score" = pre$CBH_Score, "CBD_Score" = pre$CBD_Score, "PERRESBA_score" = pre$PERRESBA_score, "SurvVolRatio_score" = pre$SurvVolRatio_score, "FRS" = pre$FRS,
                                  "Torch_Index_score" = pre$Torch_Index_score, "PTorch_Sev_score" = pre$PTorch_Sev_Score, "MortVolPct2_Sev_Score" = pre$MortVolPct2_Sev_Score, 
                                  "MortVolPct2_Mod_Score" = pre$MortVolPct2_Mod_Score, "Surf_Flame_Sev_Score" = pre$Surf_Flame_Sev_score, "HS_Sev" = pre$HS_Sev, "HS_Mod" = pre$HS_Mod)
  
  post.relevant.data <- data.frame("biosum_cond_id" = post$biosum_cond_id, "rxpackage" = post$rxpackage, "rx" = post$rx, "rxcycle" = post$rxcycle, "Year" = post$Year,
                                   "CBH_Score" = post$CBH_Score, "CBD_Score" = post$CBD_Score, "PERRESBA_score" = post$PERRESBA_score, "SurvVolRatio_score" = post$SurvVolRatio_score, "FRS" = post$FRS,
                                   "Torch_Index_score" = post$Torch_Index_score, "PTorch_Sev_score" = post$PTorch_Sev_Score, "MortVolPct2_Sev_Score" = post$MortVolPct2_Sev_Score, 
                                   "MortVolPct2_Mod_Score" = post$MortVolPct2_Mod_Score, "Surf_Flame_Sev_Score" = post$Surf_Flame_Sev_score, "HS_Sev" = post$HS_Sev, "HS_Mod" = post$HS_Mod)
  
  
  pre.relevant.data2 <- pre.relevant.data[pre.relevant.data$rxcycle != 1,]
  
  relevant.data <- rbind(pre.relevant.data2, post.relevant.data)
  
  relevant.data2 <- relevant.data %>% group_by(biosum_cond_id, rxpackage, rx) %>% summarise(Avg_CBD_Score = mean(CBD_Score, na.rm = TRUE), Avg_CBH_Score = mean(CBH_Score, na.rm = TRUE), 
                                                                                            Avg_PERRESBA_Score = mean(PERRESBA_score, na.rm = TRUE), Avg_SurvVolRatio_Score = mean(SurvVolRatio_score, na.rm = TRUE),
                                                                                            Avg_FRS = mean(FRS, na.rm = TRUE), Avg_Torch_Index_Score = mean(Torch_Index_score, na.rm = TRUE), 
                                                                                            Avg_Ptorch_Sev_Score = mean(PTorch_Sev_score, na.rm = TRUE), Avg_Surf_Flame_Sev_Score = mean(Surf_Flame_Sev_Score, na.rm = TRUE),
                                                                                            Avg_MortVolPct2_Mod_Score = mean(MortVolPct2_Mod_Score, na.rm = TRUE), Avg_MortVolPct2_Sev_Score = mean(MortVolPct2_Sev_Score, na.rm = TRUE), 
                                                                                            Avg_HS_Sev = mean(HS_Sev, na.rm = TRUE), Avg_HS_Mod = mean(HS_Mod, na.rm = TRUE))
  return(relevant.data2)
}


avg_table <- create_avg_table(pre_scored, post_scored) #create average table

PRE_FVS_GROWONLY <- subset(avg_table, rxpackage == "031") #get grow only stand data from avg_table
PRE_FVS_GROWONLY$rxpackage <- NULL
PRE_FVS_GROWONLY$rx <- NULL 

POST_FVS_SUMMARY2 <- merge(POST_FVS_SUMMARY_NA, avg_table, by = c("biosum_cond_id", "rxpackage", "rx")) #merge post summary and average table
PRE_FVS_SUMMARY2 <- merge(PRE_FVS_SUMMARY_NA, PRE_FVS_GROWONLY, by = c("biosum_cond_id")) #merge pre summary and grow only table

####If you get a "cannot allocate vector..." error at this point, you can use the lines below to clear the 
#global environment. 
#################################################################################################
#########WARNING: THIS WILL DELETE ALL OTHER VARIABLES STORED IN THE GLOBAL ENVIRONMENT##########
#################################################################################################
# rm(list=setdiff(ls(), c("PRE_FVS_SUMMARY2", "POST_FVS_SUMMARY2", "project.location", "avg_table)))
# gc()
#################################################################################################

##Add in econ data from core. You must run core once to get the #s and then add max_nr_dpa and re-run it.
conn.path <-  paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", file.path(project.location, "core", "test", "db", "scenario_results.mdb"))
conn <- odbcDriverConnect(conn.path)
products <- sqlFetch(conn, "product_yields_net_rev_costs_summary_by_rxpackage", as.is = TRUE) 
odbcCloseAll()

pattern <- c("biosum_cond_id", "rxpackage", "max_nr_dpa")
netrev.table <- products[,which(grepl(paste0(pattern, collapse = "|"), names(products)))]

pre2 <- merge(PRE_FVS_SUMMARY2, netrev.table, all= TRUE)
post2 <- merge(POST_FVS_SUMMARY2, netrev.table, all = TRUE)

conn.path <-  paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", file.path(project.location, "fvs", "db", "PREPOST_FVS_SUMMARY.ACCDB"))
conn <- odbcDriverConnect(conn.path)

sqlQuery(conn, 'SELECT * INTO PRE_FVS_SUMMARY_OLD FROM PRE_FVS_SUMMARY')
sqlQuery(conn, 'DROP TABLE PRE_FVS_SUMMARY')
sqlSave(conn, dat = pre2, tablename = "PRE_FVS_SUMMARY", rownames = FALSE)

sqlQuery(conn, 'SELECT * INTO POST_FVS_SUMMARY_OLD FROM POST_FVS_SUMMARY')
sqlQuery(conn, 'DROP TABLE POST_FVS_SUMMARY')
sqlSave(conn, dat = post2, tablename = "POST_FVS_SUMMARY", rownames = FALSE)

sqlSave(conn, dat = avg_table, tablename = "avg_table", rownames = FALSE)


odbcCloseAll()


