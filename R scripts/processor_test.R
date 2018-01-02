
#Make sure you have the Microsoft Access Databse Engine Driver https://www.microsoft.com/en-us/download/confirmation.aspx?id=23734
#and you are using 32-bit R (set in RStudio by going to Tools -> Global Options)

setwd("G:/Dropbox/Carlin/Berkeley/biosum/")

library("dplyr") #if you do not have these packages installed, enter "install.packages("packagename") into the console, then load them using this line.
library("RODBC")
options(scipen = 999) #this is important for making sure your stand IDs do not get translated to scientific notation

#bring in master tree table
conn <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=H:/cec_20170915/db/master.mdb")
master_tree <- sqlFetch(conn, "tree", as.is = TRUE) #import package labels table from fvsmaster.mdb NOTE: this table was manually added earlier
odbcCloseAll()

master_dbh <- master_tree

master_dbh$DBH_BIN <- as.integer(master_dbh$dia)
avg_by_DBH_BIN<- master_dbh[complete.cases(master_dbh$fvs_tree_id, master_dbh$drybiot, master_dbh$voltsgrs),]
avg_by_DBH_BIN <- group_by(avg_by_DBH_BIN, DBH_BIN)
avg_by_DBH_BIN <- summarize(avg_by_DBH_BIN, avg_volcfnet = mean(volcfnet), avg_volcfgrs = mean(volcfnet), avg_drybiom = mean(drybiom), 
                              avg_drybiot = mean(drybiot), avg_voltsgrs = mean(voltsgrs))

directory <- "H:/cec_20170915/fvs/data/"
variantname <- "CA"

processor_test <- function(directory, variantname) {
  biosumcalc <- paste0(directory, variantname, "/BiosumCalc")
  setwd(biosumcalc)#sets the working directory to the directory variable
  path <- list.files(path = ".", pattern = glob2rx(paste(variantname, "_P0", "*.MDB", sep = ""))) #lists all the files in the directory that begin with FVSOUT_{variantname}_P0 and end with .MDB (case sensitive)
  result <- data.frame()
  numfiles <- nrow(data.frame(path)) #calculates the number of package MDB files based on the path variable above
  for (i in 1:numfiles) {
    file <- file.path(biosumcalc, path[i]) #creates a file path using the directory variable and the iteration of the pakage MDB
    x <- paste("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", file, sep = "") #writes the argument for the odbcDriverConnect function below
    conn <- odbcDriverConnect(x) #Connect to access database 
    FVS_Tree<- sqlFetch(conn, "FVS_Tree", as.is = TRUE)
    
    completetest <- function(data) {
      result <- data[!complete.cases(data$biosum_cond_id,
                                     data$rxcycle,
                                     data$rx,
                                     data$rxyear,
                                     data$dbh,
                                     data$tpa,
                                     data$volcfnet,
                                     data$volcfgrs, 
                                     data$drybiom, 
                                     data$drybiot, 
                                     data$fvs_tree_id, 
                                     data$FvsCreatedTree_YN, 
                                     data$voltsgrs),]
      return(result)
    }
    
    completerowtest <- function(data) {
      result <- complete.cases(data$biosum_cond_id,
                               data$rxcycle,
                               data$rx,
                               data$rxyear,
                               data$dbh,
                               data$tpa,
                               data$volcfnet,
                               data$volcfgrs, 
                               data$drybiom, 
                               data$drybiot, 
                               data$fvs_tree_id, 
                               data$FvsCreatedTree_YN, 
                               data$voltsgrs)
      return(result)
    }
    
    if(nrow(completetest(FVS_Tree)) != 0) {
      for (j in 1:nrow(FVS_Tree)) {
        data <- FVS_Tree[j,]
        if (!completerowtest(data)) {#if it fails the complete test
          if (substr(FVS_Tree$fvs_tree_id[j], 3,4) == "ES" | FVS_Tree$dbh[j] <= 5) {
            if (is.na(FVS_Tree$volcfnet[j])) {
              FVS_Tree$volcfnet[j] <- 0.1
            }
            if (is.na(FVS_Tree$volcfgrs[j])) {
              FVS_Tree$volcfgrs[j] <- 0.1
            }
            if (is.na(FVS_Tree$drybiom[j])) {
              FVS_Tree$drybiom[j] <- 0.1
            }
            if (is.na(FVS_Tree$drybiot[j])) {
              FVS_Tree$drybiot[j] <- 0.1
            }
            if (is.na(FVS_Tree$voltsgrs[j])) {
              FVS_Tree$voltsgrs[j] <- 0.1
            }
          }
        }
      }
    }
    
    problems <- completetest(FVS_Tree)
    problems$DBH_BIN <- as.integer(problems$dbh)
    
    for (k in 1:nrow(problems)) {
      if (is.na(problems$volcfnet[k])) {
        problems$volcfnet[k] <- avg_by_DBH_BIN$avg_voltsgrs[avg_by_DBH_BIN$DBH_BIN == problems$DBH_BIN[k]]
      }
      if (is.na(problems$volcfgrs[k])) {
        problems$volcfgrs[k] <- avg_by_DBH_BIN$avg_voltsgrs[avg_by_DBH_BIN$DBH_BIN == problems$DBH_BIN[k]]
      }
      if (is.na(problems$drybiom[k])) {
        problems$drybiom[k] <- avg_by_DBH_BIN$avg_drybiom[avg_by_DBH_BIN$DBH_BIN == problems$DBH_BIN[k]]
      }
      if (is.na(problems$drybiot[k])) {
        problems$drybiot[k] <- avg_by_DBH_BIN$avg_drybiot[avg_by_DBH_BIN$DBH_BIN == problems$DBH_BIN[k]]
      }
      if (is.na(problems$voltsgrs[k])) {
        problems$voltsgrs[k] <- avg_by_DBH_BIN$avg_voltsgrs[avg_by_DBH_BIN$DBH_BIN == problems$DBH_BIN[k]]
      }
    }
    
    for (m in 1:nrow(FVS_Tree)) {
      if (FVS_Tree[m,]$fvs_tree_id %in% problems$fvs_tree_id) {
        problemsrow <- problems[FVS_Tree[m,]$fvs_tree_id == problems$fvs_tree_id,]
        FVS_Tree[m,]$volcfnet <- problemsrow$volcfnet
        FVS_Tree[m,]$volcfgrs <- problemsrow$volcfgrs
        FVS_Tree[m,]$drybiom <- problemsrow$drybiom
        FVS_Tree[m,]$drybiot <- problemsrow$drybiot
        FVS_Tree[m,]$voltsgrs <- problemsrow$voltsgrs
      }
    }
    
    problems2 <- completetest(FVS_Tree)
    
    result <- rbind(result, problems2)
    
    sqlQuery(conn, 'SELECT * INTO FVS_Tree_Old FROM FVS_Tree')
    sqlQuery(conn, 'DROP TABLE FVS_Tree')
    sqlSave(conn, dat = FVS_Tree, tablename = "FVS_Tree", rownames = FALSE)
    
    odbcCloseAll()
  }
  return(result)
}
    
CA_result <- processor_test(directory = "H:/cec_20170915/fvs/data/", variantname = "CA")
WS_result <- processor_test(directory = "H:/cec_20170915/fvs/data/", variantname = "WS")

write.csv(CA_result, "CA_problems_master.csv")
write.csv(WS_result, "WS_problems_master.csv")
    

