#This script was created by Carlin Starrs in 2018
#It is designed to:
#Update County code in FVSIn for each variant to match owncd
#Set inventory year to 2007

#Make sure you have the Microsoft Access Databse Engine Driver https://www.microsoft.com/en-us/download/confirmation.aspx?id=23734
#and you are using 32-bit R (set in RStudio by going to Tools -> Global Options)

packages <- c("RODBC")

package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, repos="http://cran.r-project.org", dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

#Set project directory location
project.location <- "H:/cec_20180529"

fix_FVSIn <- function(project.location) {
  variants <- list.files(file.path(project.location, "fvs", "data"))
  variants <- variants[!variants %in% "CR"]
  
  master <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", file.path(project.location, "db"), "/master.mdb")
  
  conn <- odbcDriverConnect(master)
  cond <- sqlFetch(conn, "cond", as.is = TRUE)
  odbcCloseAll()
  
  for (i in 1:length(variants)) {
    fvsin <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", file.path(project.location, "fvs", "data", variants[i], "FVSIn.accdb"))
    conn <- odbcDriverConnect(fvsin)
    standinit <- sqlFetch(conn, "FVS_StandInit", as.is = TRUE)
    treeinit <- sqlFetch(conn, "FVS_TreeInit", as.is = TRUE)
    
    sqlSave(conn, dat = cond, tablename = "cond", rownames = FALSE)
    
    #Set County to County_Old and set County to cond.owncd
    sqlQuery(conn, 'ALTER TABLE FVS_StandInit ADD COLUMN County_Old NUMERIC')
    sqlQuery(conn, 'UPDATE FVS_StandInit SET FVS_StandInit.County_Old = [FVS_StandInit].[County];')
    sqlQuery(conn, 'UPDATE cond INNER JOIN FVS_StandInit ON cond.biosum_cond_id = FVS_StandInit.Stand_ID SET FVS_StandInit.County = [cond].[owncd];')
    
    #Set inventory year to 2007
    sqlQuery(conn, 'ALTER TABLE FVS_StandInit ADD COLUMN Inv_Year_Old NUMERIC')
    sqlQuery(conn, 'UPDATE FVS_StandInit SET FVS_StandInit.Inv_Year_Old = [FVS_StandInit].[Inv_Year];')
    sqlQuery(conn, 'UPDATE FVS_StandInit SET FVS_StandInit.Inv_Year = 2007;')
    
    odbcCloseAll()
  }
}


fix_FVSIn(project.location)

