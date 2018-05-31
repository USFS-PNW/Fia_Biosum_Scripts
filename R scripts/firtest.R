#Fir test

library("dplyr") #if you do not have these packages installed, enter "install.packages("packagename") into the console, then load them using this line.
library("RODBC")
options(scipen = 999) #this is important for making sure your stand IDs do not get translated to scientific notation

# #Variables for running step by step instead of in function:
# packagenum <- "004"
# directory <- "H:/cec_20170915/fvs/data/CA"
# variantname <- "CA"
# i <- 1


FIR_check <- function(directory, variantname, packagenum) {
  setwd(directory)#sets the working directory to the directory variable
  path <- list.files(path = ".", pattern = glob2rx(paste("FVSOUT_", variantname, "_P", packagenum, "*.MDB", sep = ""))) #lists all the files in the directory that begin with FVSOUT_{variantname}_P{packagenum} and end with .MDB (case sensitive)
  problem <- data.frame()
  row <- NULL
  FVS_Cases_rows <- as.numeric(0)
  file <- file.path(directory, path) #creates a file path using the directory variable and the iteration of the pakage MDB
  x <- paste("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", file, sep = "") #writes the argument for the odbcDriverConnect function below
  conn <- odbcDriverConnect(x) #Connect to access database 
  FVS_Summary <- sqlFetch(conn, "FVS_Summary", as.is = TRUE)
  FVS_Compute <- sqlFetch(conn, "FVS_Compute", as.is = TRUE)
 # FVS_Treelist <- sqlFetch(conn, "FVS_TreeList", as.is = TRUE)
  odbcCloseAll()
  
  firdata <- data.frame("StandID" = FVS_Compute$StandID, "Year" = FVS_Compute$Year, "CRT" = FVS_Compute$`_CRT`, "OWN" = FVS_Compute$`_OWN`, "BBA" = FVS_Compute$`_BBA`, "FIRBA" = FVS_Compute$`_FIRBA`, "FIRBAover2" = FVS_Compute$`_FIRBA`/2, 
                        "FIRBAMINUSLESSBA" = FVS_Compute$`_FIRBA` - FVS_Compute$`_LESSBA`, "LESSBA" = FVS_Compute$`_LESSBA`, "TFIR" = FVS_Compute$`_TFIR`, 
                        "PREFIRBAMED" = FVS_Compute$`_PRFMD`, "PREFIRBALARGE" = FVS_Compute$`_PRFLG`, "POSTFIRBASMALL" = FVS_Compute$`_PSTFSM`, 
                        "POSTFIRBAMED" = FVS_Compute$`_PSTFMD`, "POSTFIRBALARGE" = FVS_Compute$`_PSTFLG`)
  firdata <- firdata[firdata$CRT != 999,]
  firdata$firmax <- pmax(firdata$FIRBAover2, firdata$FIRBAMINUSLESSBA)
  firdata$tfircheck <- round(firdata$TFIR,3) == round(firdata$firmax,3)
  
  problem_tfir_not_correct <- firdata[firdata$tfircheck == "FALSE",]
  
  years <- data.frame("Year" = unique(FVS_Compute$Year)) #takes unique values for "year" from FVS_Summary
  years <- arrange(years, Year)
  cut_cycles <- c(1,4,7,10) #takes the year #  for the cut cycles. These are defined in the KCP files
  
  cut_years <- data.frame("cut_years" = years[cut_cycles,])
  
  firdata.cutyear <- firdata[firdata$Year %in% cut_years$cut_years,]
  firdata.harvest <- firdata.cutyear[firdata.cutyear$BBA > firdata.cutyear$CRT,]
  firdata.harvest$postmedpluslarge <- firdata.harvest$POSTFIRBAMED + firdata.harvest$POSTFIRBALARGE
  firdata.harvest$smallcheck <- NA
  firdata.harvest$medcheck <- NA
  firdata.harvest$onethird.preBA <- round(firdata.harvest$FIRBA,0) * 0.33
  
  for (i in 1:nrow(firdata.harvest)) {
    if (firdata.harvest$OWN[i] == "11") {
      firdata.harvest$smallcheck[i] <- round(firdata.harvest$POSTFIRBASMALL[i],3) == 0
      firdata.harvest$medcheck[i] <- round(firdata.harvest$POSTFIRBAMED[i],0) %in% c(round(firdata.harvest$TFIR[i],0):round((firdata.harvest$TFIR[i] - firdata.harvest$onethird.preBA[i]),0))
    } else if (firdata.harvest$OWN[i] == "46") {
      firdata.harvest$smallcheck[i] <- round(firdata.harvest$POSTFIRBASMALL[i],3) == 0
      firdata.harvest$medcheck[i] <- (round(firdata.harvest$postmedpluslarge[i],0)) %in% c(round(firdata.harvest$TFIR[i],0):round((firdata.harvest$TFIR[i] - firdata.harvest$onethird.preBA[i]),0))
      
    }
  }
  
  problem_wrong_fir_amt <- firdata.harvest[firdata.harvest$smallcheck == "FALSE" | firdata.harvest$medcheck == "FALSE",]
  
  yearswithcut <- FVS_Summary
  yearswithcut <- data.frame("StandID" = yearswithcut$StandID, "Year" = yearswithcut$Year, "RTPA" = yearswithcut$RTpa)
  problem_years <- merge(yearswithcut, problem_wrong_fir_amt, by = c("StandID", "Year"))
  still_problem_years <- problem_years[problem_years$RTPA >0,]
  
  problem_wrong_fir_amt <- still_problem_years
  
  problem_wrong_fir_amt$tfircheck <- problem_wrong_fir_amt$POSTFIRBAMED > problem_wrong_fir_amt$TFIR
  
  problem_tfir_gt_medfir <- problem_wrong_fir_amt[problem_wrong_fir_amt$tfircheck == TRUE,]
  
  row <- data.frame("filename" = path, "wrong_tfir" = nrow(problem_tfir_not_correct), "wrong_amt" = nrow(problem_wrong_fir_amt), 
                    "tfir_toolow" = nrow(problem_tfir_gt_medfir))
  problem <- rbind(problem, row)
  
  problem2 <- list()
  problem2$problem_tfir_not_correct <- problem_tfir_not_correct
  problem2$problem_wrong_fir_amt <- problem_wrong_fir_amt
  problem2$problem_tfir_gt_medfir <- problem_tfir_gt_medfir
  problem2$problem <- problem
  
  return(problem2)
}


output <- FIR_check("H:/cec_20170915/fvs/data/CA","CA", "008")
output <- FIR_check("H:/cec_20180517/fvs/data/CA","CA", "008")

problem2 <- data.frame(output$problem)
problem_tfir_not_correct <- data.frame(output$problem_tfir_not_correct)
problem_wrong_fir_amt <- data.frame(output$problem_wrong_fir_amt)
problem_tfir_gt_medfir <- data.frame(output$problem_tfir_gt_medfir)

write.csv(problem_wrong_fir_amt, "problem_wrong_fir_amt.csv")



setwd("G:/Dropbox/Carlin/Berkeley/biosum/CEC Master KCPs") #set the working directory
i <- 4
variant <- "SO"
#The text below is for the cloneKCP function and will not return any result until the function is called

OWN_check <- function(variant) {
  end <- length(list.files(path = ".", pattern = glob2rx(paste("FVSOUT_", variant, "_P0*.KCP", sep = ""))))
  n <- as.numeric()
  files <- list.files(path = ".", pattern = glob2rx(paste("FVSOUT_", variant, "_P0*.KCP", sep = "")))
  for(i in 1:end) {
    x <- readLines(files[i]) #opens the .KCP file
    y <- grep(pattern = "OWN = OWNCD", x = x)
    n[i] <- length(y)
  }
  table <- data.frame("filename" = files, "test" = n)
  return(table)
}

#The function is called below by changing the variant names. 
result <- OWN_check(variant = "SO")


