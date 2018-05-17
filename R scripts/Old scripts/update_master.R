#This script was created by Carlin Starrs in 2018
#The script takes a new project and modifies master.mdb to filter
#conditions for reserve code not equal to 1 and land class code equal to 1

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

#Location of additional data files on github:
additional.data <- "G:/Dropbox/Carlin/GitHub/Fia_Biosum_Scripts/Additional data"
master.kcps <-  "G:/Dropbox/Carlin/GitHub/Fia_Biosum_Scripts/CEC Master KCPs"

#Project root location:
project.location <- "H:/cec_20180517"

####PROJECT SETUP####
#This should be run after you have appended the FIA data into your biosum project
#and run through the "Plot FVS Variants" and "FVS Tree Species" options. 
#The script copies over all the master info you will need for the project for quick setup. 
project.setup <- function(additional.data, master.kcps, overwrite){
  #copy over gis data
  file.copy(file.path(additional.data, "plot_near.txt"), file.path(project.location, "gis", "db"))
  file.copy(file.path(additional.data, "gis_travel_times.mdb"), file.path(project.location, "gis", "db"), overwrite = TRUE)
  
  #copy over treatment info
  fvsmasterCEC.location <- file.path(additional.data, "fvs_master_CEC.mdb")
  conn.path <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", fvsmasterCEC.location)
  conn <- odbcDriverConnect(conn.path)
  rx_new <- sqlFetch(conn, "rx", as.is = TRUE)
  rx_harvest_cost_columns_new <- sqlFetch(conn, "rx_harvest_cost_columns", as.is = TRUE)
  rx_package_new <- sqlFetch(conn, "rxpackage", as.is = TRUE)
  PkgLabels_new <- sqlFetch(conn, "PkgLabels", as.is = TRUE)
  odbcCloseAll()
  
  master.location <- file.path(project.location, "db", "fvsmaster.mdb")
  conn.path <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", master.location)
  conn <- odbcDriverConnect(conn.path)
  
  sqlQuery(conn, 'DROP TABLE rx')
  sqlQuery(conn, 'DROP TABLE rx_harvest_cost_columns')
  sqlQuery(conn, 'DROP TABLE rxpackage')
  sqlQuery(conn, 'DROP TABLE PkgLabels')
  
  sqlSave(conn, dat = rx_new, tablename = "rx", rownames = FALSE)
  sqlSave(conn, dat = rx_harvest_cost_columns_new, tablename = "rx_harvest_cost_columns", rownames = FALSE)
  sqlSave(conn, dat = rx_package_new, tablename = "rxpackage", rownames = FALSE)
  sqlSave(conn, dat = PkgLabels_new, tablename = "PkgLabels", rownames = FALSE)
  variants <- list.files(file.path(project.location, "fvs", "data"))
  
  #copy over info into variant folders
  for (i in 1:length(variants)) {
    variant <- variants[i]
    variant.location <- file.path(project.location, "fvs", "data", variant)
    setwd(master.kcps)
    
    cloneKCP <- function(oldvariant, newvariant) {
      end <- length(list.files(path = master.kcps, pattern = glob2rx(paste("FVSOUT_", oldvariant, "_P0*.KCP", sep = ""))))
      for(i in 1:end) {
        x <- readLines(list.files(path = master.kcps, pattern = glob2rx(paste("FVSOUT_", oldvariant, "_P0*.KCP", sep = "")))[i], warn = FALSE) #opens the .KCP file
        y <- gsub(paste("FVSOUT_", oldvariant, substr(list.files(path = master.kcps, pattern = glob2rx(paste("FVSOUT_", oldvariant, "_P0*.KCP", sep = "")))[i], 10, 30), ".MDB", sep = ""), 
                  paste("FVSOUT_", newvariant, substr(list.files(path = master.kcps, pattern = glob2rx(paste("FVSOUT_", oldvariant, "_P0*.KCP", sep = "")))[i], 10, 30), ".MDB", sep = ""), 
                  x) #find and replace the {packagename}.MDB file text in the .KCP file
        cat(y,file=file.path(variant.location, paste("FVSOUT_", newvariant, substr(list.files(path = master.kcps, pattern = glob2rx(paste("FVSOUT_", oldvariant, "_P0*.KCP", sep = "")))[i], 10, 30), ".KCP", sep = "")), sep = "\n")
        # saves the file with the new variant name
      }
    }
    
    #The function is called below by changing the variant names. 
    cloneKCP(oldvariant = "SO", newvariant = variant)
    
    #Copy over variant KCPs
    browser()
    variant.KCPs <- list.files(path = master.kcps, pattern = glob2rx(paste0("*", variant,"*")))
    for (j in 1:length(variant.KCPs)) {
      file.copy(file.path(master.kcps, variant.KCPs[j]), file.path(variant.location), overwrite = overwrite)
    }
    
    file.copy(file.path(master.kcps, "FVSOUT_forBY.KCP"), file.path(variant.location))
    file.copy(file.path(additional.data, "FVSOUT_forBY.mdb"), file.path(variant.location), overwrite = overwrite)
    file.copy(file.path(additional.data, "FVS_SDImax_out.mdb"), file.path(variant.location), overwrite = overwrite)
  }
  
  #Add yarding distance to master.mdb and filter master.cond by reservecd and landclcd
  plot_near <- read.csv(file.path(project.location, "gis", "db", "plot_near.txt"), colClasses = c(rep("numeric",3), "character", rep("numeric", 9), rep("character", 8), "numeric", "character", rep("numeric", 17)))
  yard_dist_2017 <- plot_near[,c(4:6,9:10,14, 17:21,38)]
  yard_dist_2017$gis_yard_d <- yard_dist_2017$NEAR_DIST * 3.28084
  yard_dist_2017$gis_yard_d <- round(yard_dist_2017$gis_yard_d,0)
  yard_dist_2017$gis_yard_d <- ifelse(yard_dist_2017$gis_yard_d < 100, 100, yard_dist_2017$gis_yard_d)
  names(yard_dist_2017)[7] <- "gis_yard_dist"
  names(yard_dist_2017)[1] <- "biosum_plot_id"
  yard_dist_2017$NEAR_DIST <- NULL
  yard_dist_2017$biosum_plot_id <- NULL
  
  #connect to master.mdb
  file <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", file.path(project.location, "db"), "/master.mdb")
  conn <- odbcDriverConnect(file) #set to your project master directory location
  cond <- sqlFetch(conn, "cond", as.is = TRUE)
  plot <- sqlFetch(conn, "plot", as.is = TRUE)
  
  sqlQuery(conn, 'SELECT * INTO cond_backup FROM cond')
  
  conditions.to.remove <- data.frame("biosum_cond_id" = cond$biosum_cond_id[cond$reservcd == 1 | cond$landclcd != 1]) #get condition ids where reservcd is equal to 1 or landclcd is not equal to 1
  
  yard_dist_2017$plot <- as.numeric(yard_dist_2017$plot)
  
  sqlSave(conn, dat = conditions.to.remove, tablename = "conditions_to_remove", rownames = FALSE)
  sqlSave(conn, dat = yard_dist_2017, tablename = "yard_dist", rownames = FALSE)
  
  sqlQuery(conn, 'DELETE cond.*, cond.biosum_cond_id FROM cond WHERE (((cond.biosum_cond_id) In (Select conditions_to_remove.[biosum_cond_id] from conditions_to_remove)));')
  sqlQuery(conn, 'UPDATE plot INNER JOIN yard_dist ON (plot.fvs_variant = yard_dist.fvs_varian) AND (plot.measyear = yard_dist.measyear) AND (plot.invyr = yard_dist.invyr) AND (plot.plot = yard_dist.plot) SET plot.gis_yard_dist = [yard_dist].[gis_yard_dist];')
  
  odbcCloseAll()
}


project.setup(additional.data, master.kcps, FALSE)

#NOTE: you  may get warning for the CR variant; you can ignore it
