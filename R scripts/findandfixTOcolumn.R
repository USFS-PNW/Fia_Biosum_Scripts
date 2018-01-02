#Find and fix"TO" column

setwd("E:/Dropbox/Carlin/Berkeley/biosum/MortCalc")

library("dplyr")
library("RODBC")
options(scipen = 999)

directory <- "H:/cec_20170915/fvs/data/SO"
variantname <- "SO"

Fix_TO_column <- function(directory, variantname) {
  setwd(directory)#sets the working directory to the directory variable
  path <- list.files(path = ".", pattern = glob2rx(paste("FVSOUT_", variantname, "_P0", "*.MDB", sep = ""))) #lists all the files in the directory that begin with FVSOUT_{variantname}_P0 and end with .MDB (case sensitive)
  rows <- as.numeric(0)
  fixed <- as.numeric(0)
  table <- data.frame()
  numfiles <- nrow(data.frame(path)) #calculates the number of package MDB files based on the path variable above
  for (i in 1:numfiles) {
    file <- file.path(directory, path[i]) #creates a file path using the directory variable and the iteration of the pakage MDB
    x <- paste("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", file, sep = "") #writes the argument for the odbcDriverConnect function below
    conn <- odbcDriverConnect(x) #Connect to access database 
    FVS_Compute <- sqlFetch(conn, "FVS_Compute", as.is = TRUE)

    if("TO" %in% colnames(FVS_Compute)) {
      sqlQuery(conn, 'ALTER TABLE FVS_Compute ADD COLUMN _TO NUMERIC')
      sqlQuery(conn, 'UPDATE FVS_Compute SET _TO = TO')
      sqlQuery(conn, 'ALTER TABLE FVS_Compute DROP COLUMN TO')
      fixed <- "fixed"
    } else {
      fixed <- "no fix needed"
    }
    row <- data.frame("filename" = path[i], "fixed" = fixed)
    table <- rbind(table, row)

    #Close all open connections to access
    odbcCloseAll()
  }
  return(table)
}

CA_result <- Fix_TO_column(directory = "H:/cec_20170915/fvs/data/CA", variantname = "CA")
NC_result <- Fix_TO_column(directory = "H:/cec_20170915/fvs/data/NC", variantname = "NC")
SO_result <- Fix_TO_column(directory = "H:/cec_20170915/fvs/data/SO", variantname = "SO")
WS_result <- Fix_TO_column(directory = "H:/cec_20170915/fvs/data/WS", variantname = "WS")
