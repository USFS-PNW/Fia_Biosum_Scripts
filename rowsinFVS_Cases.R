#This function was written by Carlin Starrs in 2017. 
#The purpose is to check FVS output package databases to see if the 
#FVS_Cases table contains a number of rows equivalent to the number of
#stands run through FVS. The number of stands in the variant appears
#in FVS when adding stands. FVS_Cases should have one row for each 
#stand, meaning the number of rows will be equivalent to the number
#of stands run. The function has two arguments, the first is 
#directory, which is the local variant folder path (e.g. in the
#biosum project folder/fvs/data/CA). Note that if you copy the path 
#from file explorer in windows directly, folders will be separated by 
#backslashes. For R to find the directory, the backslashes must be changed
#to forward slashes. An easy way to get this correct initially is to set
#your working directory to your variant folder, and then copy the console
#text. The other argument is variantname, which is the current variant folder. 
#Note that even if the function is successful, you will receive warnings
#stating something about closing unushed RODBC handles. You can ignore this. 

#This function requires the RODBC package. If you do not have it installed, 
#type install.packages("RODBC") into the console. 

#NOTE: YOU MUST USE THE 32-BIT VERSION OF R FOR THIS FUNCTION TO WORK. To 
#change the version of R in R-Studio, go to Tools > Global Options > General
#and change the R version to your system's default 32-bit R version. 

#You must also have the 32-bit Microsoft Access Driver (*.mdb, *.accdb) driver 
#installed in your user DSN list. If you've made it this far, you should already
#have this set up successfully without needing to do more. 

library("RODBC")

#The text below creates the function rowsinFVS_Cases and will not return 
#any output until the function is called below. 

rowsinFVS_Cases <- function(directory, variantname) {
  setwd(directory)#sets the working directory to the directory variable
  path <- list.files(path = ".", pattern = glob2rx(paste("FVSOUT_", variantname, "_P0", "*.MDB", sep = ""))) #lists all the files in the directory that begin with FVSOUT_{variantname}_P0 and end with .MDB (case sensitive)
  rows <- as.numeric(0)
  numfiles <- nrow(data.frame(path)) #calculates the number of package MDB files based on the path variable above
  for (i in 1:numfiles) {
    file <- file.path(directory, path[i]) #creates a file path using the directory variable and the iteration of the pakage MDB
    x <- paste("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", file, sep = "") #writes the argument for the odbcDriverConnect function below
    conn <- odbcDriverConnect(x) #
    table <- sqlFetch(conn, "FVS_Cases")
    rows[i] <- nrow(table)
    on.exit(odbcClose(conn))
  }
  return(data.frame("filename" = path, "FVS_Cases Rows" = rows))
}

#To call the function, enter the directory and variantname arguments 
#separated by a comma. You can store the result in a variable and view 
#it by cliking on it in the environment window or typing View(variable), 
#e.g. View(CA_result). 

CA_result <- rowsinFVS_Cases(directory = "D:/cec_20170915/fvs/data/CA", variantname = "CA")
SO_result <- rowsinFVS_Cases("D:/cec_20170915/fvs/data/SO", "SO")
WS_result <- rowsinFVS_Cases("D:/cec_20170915/fvs/data/WS", "WS")
NC_result <- rowsinFVS_Cases("D:/cec_20170915/fvs/data/NC", "NC")
 