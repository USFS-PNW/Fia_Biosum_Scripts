#Make sure you have the Microsoft Access Databse Engine Driver https://www.microsoft.com/en-us/download/confirmation.aspx?id=23734
#and you are using 32-bit R (set in RStudio by going to Tools -> Global Options)

library("dplyr")
library("RODBC")
options(scipen = 999)

#Pull in the PREPOST_FVS_STRCLASS database from fvs/db. Set to your directory location by changing what is after DBQ= in the line
#that starts with "conn." Make sure your slashes face the correct direction for R (it's the opposite of what it will be in explorer). 
#This should be all you need to change, the rest will run itself. 

conn <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=H:/cec_20170915/fvs/db/PREPOST_FVS_STRCLASS.ACCDB")
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



#Calculate CBH. The section below defines the canopy_dist function
canopy_dist <- function(data) {
  for (i in 1:nrow(data)) {
    if (data$Stratum_1_Status_Code[i] > 0) {
      if (data$Stratum_2_Status_Code[i] > 0) {
        data$dist1[i] <- data$Stratum_1_Crown_Base[i] - data$Stratum_2_Nom_Ht[i]
        if (data$Stratum_3_Status_Code[i] > 0) {
          data$dist2[i] <- data$Stratum_2_Crown_Base[i] - data$Stratum_3_Nom_Ht[i]
          data$dist3[i] <- data$Stratum_3_Crown_Base[i]
        } else {
          data$dist2[i] <- data$Stratum_2_Crown_Base[i]
        }
      } else if (data$Stratum_3_Status_Code[i] > 0) {
        data$dist1[i] <- data$Stratum_1_Crown_Base[i] - data$Stratum_3_Nom_Ht[i]
        data$dist2[i] <-data$Stratum_3_Crown_Base[i]
      } else {
        data$dist1[i] <- data$Stratum_1_Crown_Base[i]
      }
    } else if (data$Stratum_2_Status_Code[i] > 0) {
      if (data$Stratum_3_Status_Code[i] > 0) {
        data$dist1[i] <- data$Stratum_2_Crown_Base[i] - data$Stratum_3_Nom_Ht[i] 
        data$dist2[i] <- data$Stratum_3_Crown_Base[i]
      } else {
        data$dist1[i] <- data$Stratum_2_Crown_Base[i]
      }
    } else {
      data$dist1[i] <- data$Stratum_3_Crown_Base[i]
    }
    data$CBH[i] <- max(data$dist1[i], data$dist2[i], data$dist3[i], na.rm = TRUE)  
  }
  return(data)
}

#Run the canopy_dist function. This may take a while (it took about 20 minutes for me).
pre_STRCLASS <- canopy_dist(pre_STRCLASS)
post_STRCLASS <- canopy_dist(post_STRCLASS)


#Save to new tables in the PREPOST_FVS_STRCLASS database. This may take a few minutes as well.
sqlSave(conn, dat = pre_STRCLASS, tablename = "PRECBH", rownames = FALSE)
sqlSave(conn, dat = post_STRCLASS, tablename = "POSTCBH", rownames = FALSE)

#Disconnect from database
odbcCloseAll()
