#This script was created by Carlin Starrs in 2017
#The script tests to make sure FVS carried out prescriptions as to be expected based on the KCP files
#for the CEC project dataset. 

#Make sure you have the Microsoft Access Databse Engine Driver https://www.microsoft.com/en-us/download/confirmation.aspx?id=23734
#and you are using 32-bit R (set in RStudio by going to Tools -> Global Options)

setwd("G:/Dropbox/Carlin/Berkeley/biosum/")

library("dplyr") #if you do not have these packages installed, enter "install.packages("packagename") into the console, then load them using this line.
library("RODBC")
options(scipen = 999) #this is important for making sure your stand IDs do not get translated to scientific notation

#create a master_package database that has the BA, less, and QMD thresholds for cutting
conn <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=H:/cec_20170915/db/fvsmaster.mdb")
package_labels <- sqlFetch(conn, "PkgLabels", as.is = TRUE) #import package labels table from fvsmaster.mdb NOTE: this table was manually added earlier
odbcCloseAll()

master_package <- package_labels[,c(1,4,6,7)]

master_package$BA <- substr(master_package$c, 4,7)

master_package$QMD <- NA
master_package$Volume <- NA

master_package[20,4] <- 24
master_package[21,6] <- 30
master_package[27,6] <- 24
master_package[28,5] <- NA
master_package[28,6] <- 14
master_package[28,7] <- 5000
master_package[29,5] <- 75

master_package$ATBA_ReductionPct <- substr(master_package$f, 1, 5)
master_package[17,8] <- 20

master_package$c <- NULL
master_package$f <- NULL

names(master_package)[2] <- "LD"
master_package$BA <- as.numeric(master_package$BA)
master_package$ATBA_ReductionPct <- as.numeric(master_package$ATBA_ReductionPct)

master_package <- master_package[-c(20,21,27,28),]

# The function below checks:
# 1. Prints the number of rows in FVS_Cases (these should all match)
# 2. Checks for cuts being made in a non cut-year (problem_wrong_cut)
# 3. Checks for cut-year cycles where BA was greater than the BA threshold for cutting
#    but there was no harvest in that year (accounting for cut-years where there was a cut
#    in the previous cycle, thus making this cut-year ineligible for cutting) (problem_didnt_cut)
# 4. Checks that the amount of BA cut was approximately equivalent to the starting BA
#    multipled by the less % from master_package for that package within threshold defined
#    by the user (BA_threshold) (problem_wrong_cut_amt)
# 5. Checks that cuts only took place every other cut cycle (20 years between cuts) (problem_wrong_cut_int)
# 6. Checks that SurvVolRatio table exists (i.e. that the MortCalc.R script has been run)

fvs_qa <- function(directory, variantname, BA_threshold) {
  setwd(directory)#sets the working directory to the directory variable
  path <- list.files(path = ".", pattern = glob2rx(paste("FVSOUT_", variantname, "_P0", "*.MDB", sep = ""))) #lists all the files in the directory that begin with FVSOUT_{variantname}_P0 and end with .MDB (case sensitive)
  end <- length(path)
  problem <- data.frame()
  numfiles <- nrow(data.frame(path)) #calculates the number of package MDB files based on the path variable above
  FVS_Cases_rows <- as.numeric(0)
  for (i in 1:numfiles) {
    row <- NULL
    file <- file.path(directory, path[i]) #creates a file path using the directory variable and the iteration of the pakage MDB
    x <- paste("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", file, sep = "") #writes the argument for the odbcDriverConnect function below
    conn <- odbcDriverConnect(x) #Connect to access database 
    FVS_Summary <- sqlFetch(conn, "FVS_Summary", as.is = TRUE)
    FVS_Compute <- sqlFetch(conn, "FVS_Compute", as.is = TRUE)
    FVS_Cases <- sqlFetch(conn, "FVS_Cases")
    
    #check to see if the package has the SurvVolRatio table (this is added using the MortCalc R script)
    if (any(grepl(pattern = "SurvVolRatio", x = sqlTables(conn)))) {
      SurvVolRatio = "YES"
    } else {
      SurvVolRatio = "NO"
    }
    
    odbcCloseAll()
    
    
    #Get # rows in FVS_Cases
    FVS_Cases_rows[i] <- nrow(FVS_Cases)
    
    years <- data.frame("Year" = unique(FVS_Compute$Year)) #takes unique values for "year" from FVS_Summary
    years <- arrange(years, Year)
    cut_cycles <- c(1,4,7,10) #takes the year #  for the cut cycles. These are defined in the KCP files
    
    cut_years <- data.frame("cut_years" = years[cut_cycles,])
    
    #Find rows where RTPA is >0 in a non-cut_year
    problem_wrong_cut <- FVS_Summary[!FVS_Summary$Year %in% cut_years$cut_years & FVS_Summary$RTpa > 0,]
    
    #Check for _CRT column in the package database (CRT is the cut threshold value set by the KCP File. If it is missing you need to change the 
    #KCP file to properly print the CRT variable)
    if(!"_CRT" %in% colnames(FVS_Compute)) {
      problem_no_crt <- "YES"
    } else {
      problem_no_crt <- "NO"
    }
    
    BA_cut <- FVS_Compute[!FVS_Compute$`_CRT` == "999",] #remove stands with CRT = 999, as these are not eligible for harvest
    PkgBA <- unique(BA_cut$`_CRT`) #get package BA
    
    #QMD check
    if (is.na(master_package$QMD[master_package$PkgNum == substr(path[i],12,14)])) { #check if master_package has a QMD value. If the QMD value is not NA, this code runs, otherwise skip to else.
      BA_cutyears <- BA_cut[BA_cut$Year %in% cut_years$cut_years,] #limit to rows in a cut year
      BA_cutyears <- BA_cutyears[BA_cutyears$`_BBA` >= BA_cutyears$`_CRT`,] #find rows where eligible basal area BBA is >= CRT
      FVS_Summary2 <- data.frame("StandID" = FVS_Summary$StandID, "Year" = FVS_Summary$Year, "BA" = FVS_Summary$BA, "TPA" = FVS_Summary$Tpa, "RTpa" = FVS_Summary$RTpa) #get RTpa from FVS_Summary
      BA_cutyears2 <- merge(BA_cutyears, FVS_Summary2, by = c("StandID", "Year")) #add RTpa to BA_cutyears
      
      problem_didnt_cut <- BA_cutyears2[BA_cutyears2$RTpa == 0,] #rows where BBA > CRT in a cut cycle with RTpa = 0
    } else { #process for stands with QMD limit (clearcut packages: note these were removed from the final run of the project)
      BA_cutyears <- BA_cut[BA_cut$Year %in% cut_years$cut_years,] #limit to rows in a cut year
      BA_cutyears <- BA_cutyears[BA_cutyears$`_BBA` >= BA_cutyears$`_CRT` | BA_cutyears$`_BQMD` >= BA_cutyears$`_QCRT`,] #find rows where BBA is >= CRT OR BQMD >= QCRT
      FVS_Summary2 <- data.frame("StandID" = FVS_Summary$StandID, "Year" = FVS_Summary$Year, "BA" = FVS_Summary$BA, "TPA" = FVS_Summary$Tpa, "RTpa" = FVS_Summary$RTpa) #get RTpa from FVS_Summary
      BA_cutyears2 <- merge(BA_cutyears, FVS_Summary2, by = c("StandID", "Year")) #add RTpa and TPA to BA_cutyears
      
      problem_didnt_cut <- BA_cutyears2[BA_cutyears2$RTpa == 0,] #rows where BBA >= CRT OR BQMD >= QCRT in a cut cycle with RTpa = 0
    }
    
    #check to see if previous cycle was cut, making this cycle invalid
    all_cut_years <- FVS_Summary[FVS_Summary$Year %in% cut_years$cut_years,] #get all cut year values
    all_cut_years <- arrange(all_cut_years, StandID, Year) #sort
    problem_didnt_cut_one <- problem_didnt_cut[problem_didnt_cut$Year == 1,] #create table from problem_didnt_cut (calculated above) where Year = 1
    problem_didnt_cut_greater_than_one <- problem_didnt_cut[problem_didnt_cut$Year > 1,] #get all problem_didnt_cut values where Year > 1
    problem_didnt_cut_greater_than_one$previous_cycle <- problem_didnt_cut_greater_than_one$Year- 10 #get the year value for the previous cut cycle by subtracing 10 from the current Year
    previous_cut <- all_cut_years[paste(all_cut_years$StandID, all_cut_years$Year) %in% paste(problem_didnt_cut_greater_than_one$StandID, problem_didnt_cut_greater_than_one$previous_cycle),] #get values from all_cut_years where StandID and Year match the problem_didnt_cut StandID and previous_cycle combination
    previous_cut$harvested_prev <- ifelse(previous_cut$RTpa > 0, "YES", "NO") #take stands from the previous harvest cycle and create new "harvested_prev" row to determine whether they were harvested that cycle or not by looking for RTpa values > 0
    names(previous_cut)[3] <- "previous_cycle" #rename "Year" to "previous_cycle"
    previous_cut <- data.frame("StandID" = previous_cut$StandID, "previous_cycle" = previous_cut$previous_cycle, "harvested_prev" = previous_cut$harvested_prev) #trim the table
    merged <- merge(problem_didnt_cut_greater_than_one, previous_cut, by = c("StandID", "previous_cycle")) #merge the problem_didnt_cut and previous_cut data to add the "harvested" column 
    nonproblem_didnt_cut <- merged[merged$harvested_prev == "YES",] #these values were harvested the previous cycle and thus couldn't be harvested again this cycle despite being over BA 
    still_problem_didnt_cut <- merged[merged$harvested_prev == "NO",] #these were not cut the previous cycle and still not cut
    still_problem_didnt_cut$previous_cycle <- NULL
    still_problem_didnt_cut$harvested_prev <- NULL
    still_problem_didnt_cut <- rbind(still_problem_didnt_cut, problem_didnt_cut_one) #include the Year 1 values
    still_problem_didnt_cut2 <- still_problem_didnt_cut[!still_problem_didnt_cut$`_SBA` == 0,] #remove stands where SBA = 0, which would mean no harvest because there are no trees eligible for harvest based on diameter limits (i.e. all trees in stand are too big or too small)
    problem_didnt_cut_old <- problem_didnt_cut
    problem_didnt_cut <- arrange(still_problem_didnt_cut2, StandID, Year)
    
    #Make sure cut amount was correct
    BA_cutyears2$`_TBA` <- as.numeric(BA_cutyears2$`_TBA`)
    
    #Sometimes LBA was called BLBA (e.g. package 5) and SBA was called something different. The if statements below corrects for these cases
    if("_LBA" %in% colnames(BA_cutyears2)) {
      BA_cutyears2$LBA <- BA_cutyears2$`_LBA`
    } else if ("_BLBA" %in% colnames(BA_cutyears2)) {
      BA_cutyears2$LBA <- BA_cutyears2$`_BLBA`
    } else {
    }
    if("_SBA" %in% colnames(BA_cutyears2)) {
      BA_cutyears2$SBA <- BA_cutyears2$`_SBA`
    } else if ("_BSBA" %in% colnames(BA_cutyears2)) {
      BA_cutyears2$SBA <- BA_cutyears2$`_BSBA`
    } else {
    }
    
    #The section below checks that the harvest amount was as expected based on the prescription set in the KCP file. 
    #It is a rough estimate as the printed variables don't account forall harvested amounts.
    if (!is.na(master_package$QMD[master_package$PkgNum == substr(path[i],12,14)])) { #check if master_package has a QMD value. If the QMD value is not NA, this code runs, otherwise skip to else.
      clearcut <- BA_cutyears2[BA_cutyears2$`_BQMD` >= BA_cutyears2$`_QCRT`,]
      clearcut$check <- ifelse(clearcut$TPA == clearcut$RTpa, "NO", "YES") #Check that TPA = RTpa, i.e. check that all TPA was removed
      problem_wrong_amt_cut <- clearcut[clearcut$check == "YES",]
      
      nonclearcut <- BA_cutyears2[BA_cutyears2$`_BQMD` < BA_cutyears2$`_QCRT`,] #now check nonclearcuts
      nonclearcut$LBA <- as.numeric(nonclearcut$LBA)
      nonclearcut$`_BBA` <- as.numeric(nonclearcut$`_BBA`)
      LBA_GT_TBA <- nonclearcut[nonclearcut$LBA > nonclearcut$`_TBA`,]
      LBA_GT_TBA$POSTCALC <- LBA_GT_TBA$`_BBA` - LBA_GT_TBA$SBA
      LBA_GT_TBA$check <- round(LBA_GT_TBA$PSTBAALL,0) == round(LBA_GT_TBA$POSTCALC, 0)
      problem_LBA_GT_TBA <- LBA_GT_TBA[LBA_GT_TBA$check == FALSE,]
      problem_LBA_GT_TBA <- problem_LBA_GT_TBA[!paste(problem_LBA_GT_TBA$StandID, problem_LBA_GT_TBA$Year) %in% paste(nonproblem_didnt_cut$StandID, nonproblem_didnt_cut$Year),] #remove stands that weren't cut because they were cut the previous cycle
      problem_LBA_GT_TBA$FOURTOFIVE <- problem_LBA_GT_TBA$`_BBA` - problem_LBA_GT_TBA$PREBAALL #get BA for trees in 4-5in DBH
      problem_LBA_GT_TBA$PSTBAFOURTOFIVE <- problem_LBA_GT_TBA$PSTBAALL + (problem_LBA_GT_TBA$FOURTOFIVE) #add 4-5in DBH trees 
      problem_LBA_GT_TBA$check2 <- with(problem_LBA_GT_TBA, problem_LBA_GT_TBA$PSTBAFOURTOFIVE <= problem_LBA_GT_TBA$PSTBAALL + BA_threshold & 
                                          problem_LBA_GT_TBA$PSTBAFOURTOFIVE >= problem_LBA_GT_TBA$PSTBAALL- BA_threshold) #check that package calculated BA is within BA_threshold of ATBA
      problem_LBA_GT_TBA <- problem_LBA_GT_TBA[problem_LBA_GT_TBA$check2 == FALSE,]
      
      LBA_LT_TBA <- nonclearcut[nonclearcut$LBA < nonclearcut$`_TBA`,]
      LBA_LT_TBA$check <- round(LBA_LT_TBA$PSTBAALL,0) == round(LBA_LT_TBA$`_TBA`,0)
      LBA_LT_TBA$POSTCALC <- LBA_LT_TBA$`_TBA`
      problem_LBA_LT_TBA <- LBA_LT_TBA[LBA_LT_TBA$check == FALSE,]
      problem_LBA_LT_TBA <- problem_LBA_LT_TBA[!paste(problem_LBA_LT_TBA$StandID, problem_LBA_LT_TBA$Year) %in% paste(nonproblem_didnt_cut$StandID, nonproblem_didnt_cut$Year),] #remove stands that weren't cut because they were cut the previous cycle
      problem_LBA_LT_TBA$FOURTOFIVE <- problem_LBA_LT_TBA$`_BBA` - problem_LBA_LT_TBA$PREBAALL #get BA for trees in 4-5in DBH
      problem_LBA_LT_TBA$PSTBAFOURTOFIVE <- problem_LBA_LT_TBA$PSTBAALL + (problem_LBA_LT_TBA$FOURTOFIVE*2/3) #add 4-5in DBH trees and multiply by 2/3 to get post-harvest BA
      problem_LBA_LT_TBA$check2 <- with(problem_LBA_LT_TBA, problem_LBA_LT_TBA$PSTBAFOURTOFIVE <= problem_LBA_LT_TBA$`_TBA` + BA_threshold & 
                                          problem_LBA_LT_TBA$PSTBAFOURTOFIVE >= problem_LBA_LT_TBA$`_TBA`- BA_threshold) #check that package calculated BA is within BA_threshold of ATBA
      problem_LBA_LT_TBA <- problem_LBA_LT_TBA[problem_LBA_LT_TBA$check2 == FALSE,]
      
      problem_wrong_amt_cut <- rbind(problem_wrong_amt_cut, problem_LBA_LT_TBA, problem_LBA_GT_TBA)
      
    } else {
      BA_cutyears2$LBA <- as.numeric(BA_cutyears2$LBA)
      BA_cutyears2$`_BBA` <- as.numeric(BA_cutyears2$`_BBA`)
      LBA_GT_TBA <- BA_cutyears2[BA_cutyears2$LBA > BA_cutyears2$`_TBA`,] #Get standyears where LBA > TBA
      LBA_GT_TBA$POSTCALC <- LBA_GT_TBA$`_BBA` - LBA_GT_TBA$SBA #Calculate post harvest BA By subtracting SBA from BBA
      LBA_GT_TBA$check <- round(LBA_GT_TBA$PSTBAALL,0) == round(LBA_GT_TBA$POSTCALC, 0) #Check that the FVS printed variable PSTBAALL is equal to above calculation
      problem_LBA_GT_TBA <- LBA_GT_TBA[LBA_GT_TBA$check == FALSE,] #Label problems where above check is not the case
      problem_LBA_GT_TBA <- problem_LBA_GT_TBA[!paste(problem_LBA_GT_TBA$StandID, problem_LBA_GT_TBA$Year) %in% paste(nonproblem_didnt_cut$StandID, nonproblem_didnt_cut$Year),] #remove stands that weren't cut because they were cut the previous cycle
      problem_LBA_GT_TBA$FOURTOFIVE <- problem_LBA_GT_TBA$`_BBA` - problem_LBA_GT_TBA$PREBAALL #get BA for trees in 4-5in DBH which were in "grey area" as per KCP files as PSTBAALL didn't include them by taking BBA - PREBAALL
      problem_LBA_GT_TBA$PSTBAFOURTOFIVE <- problem_LBA_GT_TBA$PSTBAALL + (problem_LBA_GT_TBA$FOURTOFIVE) #add 4-5in DBH trees 
      problem_LBA_GT_TBA$check2 <- with(problem_LBA_GT_TBA, problem_LBA_GT_TBA$PSTBAFOURTOFIVE <= problem_LBA_GT_TBA$PSTBAALL + BA_threshold & 
                                          problem_LBA_GT_TBA$PSTBAFOURTOFIVE >= problem_LBA_GT_TBA$PSTBAALL- BA_threshold) #check that package calculated BA is within BA_threshold of ATBA
      problem_LBA_GT_TBA <- problem_LBA_GT_TBA[problem_LBA_GT_TBA$check2 == FALSE,] #find rows that still remain a problem even with 4-5in trees added in
      
      LBA_LT_TBA <- BA_cutyears2[BA_cutyears2$LBA < BA_cutyears2$`_TBA`,] #Get standyears where LBA < TBA
      LBA_LT_TBA$check <- round(LBA_LT_TBA$PSTBAALL,0) == round(LBA_LT_TBA$`_TBA`,0) #Check rows where PSTBAALL is not equal to TBA
      LBA_LT_TBA$POSTCALC <- LBA_LT_TBA$`_TBA` #Post harvest BA should be equal to TBA for these stand-years
      problem_LBA_LT_TBA <- LBA_LT_TBA[LBA_LT_TBA$check == FALSE,] #Get rows that failed above check
      problem_LBA_LT_TBA <- problem_LBA_LT_TBA[!paste(problem_LBA_LT_TBA$StandID, problem_LBA_LT_TBA$Year) %in% paste(nonproblem_didnt_cut$StandID, nonproblem_didnt_cut$Year),] #remove stands that weren't cut because they were cut the previous cycle
      problem_LBA_LT_TBA$FOURTOFIVE <- problem_LBA_LT_TBA$`_BBA` - problem_LBA_LT_TBA$PREBAALL #get BA for trees in 4-5in DBH which were in "grey area" as per KCP files as PSTBAALL didn't include them by taking BBA - PREBAALL
      problem_LBA_LT_TBA$PSTBAFOURTOFIVE <- problem_LBA_LT_TBA$PSTBAALL + (problem_LBA_LT_TBA$FOURTOFIVE*2/3) #add 4-5in DBH trees and multiply by 2/3 to get estimated post-harvest BA
      problem_LBA_LT_TBA$check2 <- with(problem_LBA_LT_TBA, problem_LBA_LT_TBA$PSTBAFOURTOFIVE <= problem_LBA_LT_TBA$`_TBA` + BA_threshold & 
                                          problem_LBA_LT_TBA$PSTBAFOURTOFIVE >= problem_LBA_LT_TBA$`_TBA`- BA_threshold) #check that package calculated BA is within BA_threshold of ATBA
      problem_LBA_LT_TBA <- problem_LBA_LT_TBA[problem_LBA_LT_TBA$check2 == FALSE,]
      
      problem_wrong_amt_cut <- rbind(problem_LBA_LT_TBA, problem_LBA_GT_TBA)
    }
    
    
    
    #Make sure cuts are 20 years apart.
    nonzero_rtpa <- FVS_Summary[FVS_Summary$RTpa > 0,] #get all stand-years with cut based on RTpa > 0
    
    test <- function() {
      multiple_cuts <- data.frame(table(nonzero_rtpa$StandID)) #get number of cuts for all stands that were cut
      if (!nrow(multiple_cuts) == 0) {
        multiple_cuts2 <- multiple_cuts[multiple_cuts$Freq == 2,] #for stands that were cut twice
        if (nrow(multiple_cuts2) > 0) {
          multiple_cuts2$Freq <- NULL
          names(multiple_cuts2)[1] <- "StandID"
          multiple_cuts2 <- merge(multiple_cuts2, nonzero_rtpa, by = "StandID")
          multiple_cuts2$check <- NA
          
          multiple_cuts2 <- arrange(multiple_cuts2, StandID, Year) #sort by StandID then Year
          
          multiplerows <- (nrow(multiple_cuts2)-1)
          mult_length <- seq(1,multiplerows, by = 2) #get every other row number
          
          for (i in mult_length) {
            multiple_cuts2$check[i:(i+1)] <- (multiple_cuts2[(i+1),3] - multiple_cuts2[i,3]) >= 20
          }  #check that positive cut values are 20 years apart
          
          problem_mc2 <- multiple_cuts2[multiple_cuts2$check == FALSE,]
        } else {
          
        }
        
        return(problem_mc2)
        
        
        multiple_cuts3 <- multiple_cuts[multiple_cuts$Freq == 3,] #for stands that were cut 3 times
        
        if (nrow(multiple_cuts3 > 0)) {
          multiple_cuts3$Freq <- NULL
          names(multiple_cuts3)[1] <- "StandID"
          multiple_cuts3 <- merge(multiple_cuts3, nonzero_rtpa, by = "StandID")
          multiple_cuts3$check <- NA
          
          multiple_cuts3 <- arrange(multiple_cuts3, StandID, Year) #sort by StandID then Year
          
          multiplerows <- (nrow(multiple_cuts3)-1)
          mult_length <- seq(1,multiplerows, by = 3) #get every 3rd row number
          
          for (i in mult_length) {
            multiple_cuts3$check[i:(i+1)] <- (multiple_cuts3[(i+1),3] - multiple_cuts3[i,3]) >= 20
          }
          problem_mc3 <- multiple_cuts3[multiple_cuts3$check == FALSE,]
          return(problem_mc3)
        } else {
          
        }
        
        multiple_cuts4 <- multiple_cuts[multiple_cuts$Freq == 4,] #for stands that were cut 4 times
        if (nrow(multiple_cuts4 > 0)) {
          multiple_cuts4$Freq <- NULL
          names(multiple_cuts4)[1] <- "StandID"
          multiple_cuts4 <- merge(multiple_cuts, nonzero_rtpa, by = "StandID")
          multiple_cuts4$check <- NA
          
          multiple_cuts4 <- arrange(multiple_cuts4, StandID, Year) #sort by StandID then Year
          
          multiplerows <- (nrow(multiple_cuts4)-1)
          mult_length <- seq(1,multiplerows, by = 4) #get every 4th row number
          
          for (i in mult_length) {
            multiple_cuts4$check[i:(i+1)] <- (multiple_cuts4[(i+1),3] - multiple_cuts4[i,3]) >= 20
          }
          
          problem_mc4 <- multiple_cuts4[multiple_cuts4$check == FALSE,]
          return(problem_mc4)
        } else {
          
        }
        
        check_data <- rbind(problem_mc2, problem_mc3, problem_mc4)
        return(check_data)
      } else {
        
      }
      
    }
    problem_wrong_cut_int <- data.frame(test())
    row <- data.frame("filename" = path[i], "FVS_Cases_rows" = FVS_Cases_rows[i], "problem_no_crt" = problem_no_crt, "wrong_cut" = nrow(problem_wrong_cut), 
                      "didnt_cut" = nrow(problem_didnt_cut), "wrong_amt_cut" = nrow(problem_wrong_amt_cut), "wrong_cut_int" = nrow(problem_wrong_cut_int), 
                      "SurVVolRatio" = SurvVolRatio)
    problem <- rbind(problem, row)
  }
  return(problem)
}

CA_problem <- data.frame(fvs_qa(directory = "H:/cec_20170915_preFVSoutput_preprocessor5.8.0/cec_20170915_20171204 preFVSoutputbackup/cec_20170915/fvs/data/CA", variantname = "CA", BA_threshold = 10))
NC_problem <- data.frame(fvs_qa("H:/cec_20170915_preFVSoutput_preprocessor5.8.0/cec_20170915_20171204 preFVSoutputbackup/cec_20170915/fvs/data/NC","NC",10))
SO_problem <- data.frame(fvs_qa("H:/cec_20170915_preFVSoutput_preprocessor5.8.0/cec_20170915_20171204 preFVSoutputbackup/cec_20170915/fvs/data/SO","SO",10))
WS_problem <- data.frame(fvs_qa("H:/cec_20170915_preFVSoutput_preprocessor5.8.0/cec_20170915_20171204 preFVSoutputbackup/cec_20170915/fvs/data/WS","WS",10))
#BA_threshold is the allowable difference between after treatment BA compared to pre-treatment BA multipled by less %


#The function below allows you to spot check values for a specific package. You need to call the function then 
#separate the output list into separate data frames. These are the same data frames used in the fvs_qa function

fvs_spotcheck <- function(directory, variantname, BA_threshold, packagenum) {
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
  FVS_Cases <- sqlFetch(conn, "FVS_Cases", as.is = TRUE)
  
  #check to see if the package has the SurvVolRatio table (this is added using the MortCalc R script)
  if (any(grepl(pattern = "SurvVolRatio", x = sqlTables(conn)))) {
    SurvVolRatio = "YES"
  } else {
    SurvVolRatio = "NO"
  } 
  
  odbcCloseAll()
  
  
  #Get # rows in FVS_Cases
  FVS_Cases_rows <- nrow(FVS_Cases)
  
  years <- data.frame("Year" = unique(FVS_Compute$Year)) #takes unique values for "year" from FVS_Summary
  years <- arrange(years, Year)
  cut_cycles <- c(1,4,7,10) #takes the year #  for the cut cycles. These are defined in the KCP files
  
  cut_years <- data.frame("cut_years" = years[cut_cycles,])
  
  #Find rows where RTPA is >0 in a non-cut_year
  problem_wrong_cut <- FVS_Summary[!FVS_Summary$Year %in% cut_years$cut_years & FVS_Summary$RTpa > 0,]
  
  #Check for _CRT column in the package database (CRT is the cut threshold value set by the KCP File. If it is missing you need to change the 
  #KCP file to properly print the CRT variable)
  if(!"_CRT" %in% colnames(FVS_Compute)) {
    problem_no_crt <- "YES"
  } else {
    problem_no_crt <- "NO"
  }
  
  BA_cut <- FVS_Compute[!FVS_Compute$`_CRT` == "999",] #remove stands with CRT = 999, as these are not eligible for harvest
  PkgBA <- unique(BA_cut$`_CRT`) #get package BA
  
  #QMD check
  if (is.na(master_package$QMD[master_package$PkgNum == substr(path,12,14)])) { #check if master_package has a QMD value. If the QMD value is not NA, this code runs, otherwise skip to else.
    BA_cutyears <- BA_cut[BA_cut$Year %in% cut_years$cut_years,] #limit to rows in a cut year
    BA_cutyears <- BA_cutyears[BA_cutyears$`_BBA` >= BA_cutyears$`_CRT`,] #find rows where eligible basal area BBA is >= CRT
    FVS_Summary2 <- data.frame("StandID" = FVS_Summary$StandID, "Year" = FVS_Summary$Year, "BA" = FVS_Summary$BA, "TPA" = FVS_Summary$Tpa, "RTpa" = FVS_Summary$RTpa) #get RTpa from FVS_Summary
    BA_cutyears2 <- merge(BA_cutyears, FVS_Summary2, by = c("StandID", "Year")) #add RTpa to BA_cutyears
    
    problem_didnt_cut <- BA_cutyears2[BA_cutyears2$RTpa == 0,] #rows where BBA > CRT in a cut cycle with RTpa = 0
  } else { #process for stands with QMD limit (clearcut packages: note these were removed from the final run of the project)
    BA_cutyears <- BA_cut[BA_cut$Year %in% cut_years$cut_years,] #limit to rows in a cut year
    BA_cutyears <- BA_cutyears[BA_cutyears$`_BBA` >= BA_cutyears$`_CRT` | BA_cutyears$`_BQMD` >= BA_cutyears$`_QCRT`,] #find rows where BBA is >= CRT OR BQMD >= QCRT
    FVS_Summary2 <- data.frame("StandID" = FVS_Summary$StandID, "Year" = FVS_Summary$Year, "BA" = FVS_Summary$BA, "TPA" = FVS_Summary$Tpa, "RTpa" = FVS_Summary$RTpa) #get RTpa from FVS_Summary
    BA_cutyears2 <- merge(BA_cutyears, FVS_Summary2, by = c("StandID", "Year")) #add RTpa and TPA to BA_cutyears
    
    problem_didnt_cut <- BA_cutyears2[BA_cutyears2$RTpa == 0,] #rows where BBA >= CRT OR BQMD >= QCRT in a cut cycle with RTpa = 0
  }
  
  #check to see if previous cycle was cut, making this cycle invalid
  all_cut_years <- FVS_Summary[FVS_Summary$Year %in% cut_years$cut_years,] #get all cut year values
  all_cut_years <- arrange(all_cut_years, StandID, Year) #sort
  problem_didnt_cut_one <- problem_didnt_cut[problem_didnt_cut$Year == 1,] #create table from problem_didnt_cut (calculated above) where Year = 1
  problem_didnt_cut_greater_than_one <- problem_didnt_cut[problem_didnt_cut$Year > 1,] #get all problem_didnt_cut values where Year > 1
  problem_didnt_cut_greater_than_one$previous_cycle <- problem_didnt_cut_greater_than_one$Year- 10 #get the year value for the previous cut cycle by subtracing 10 from the current Year
  previous_cut <- all_cut_years[paste(all_cut_years$StandID, all_cut_years$Year) %in% paste(problem_didnt_cut_greater_than_one$StandID, problem_didnt_cut_greater_than_one$previous_cycle),] #get values from all_cut_years where StandID and Year match the problem_didnt_cut StandID and previous_cycle combination
  previous_cut$harvested_prev <- ifelse(previous_cut$RTpa > 0, "YES", "NO") #take stands from the previous harvest cycle and create new "harvested_prev" row to determine whether they were harvested that cycle or not by looking for RTpa values > 0
  names(previous_cut)[3] <- "previous_cycle" #rename "Year" to "previous_cycle"
  previous_cut <- data.frame("StandID" = previous_cut$StandID, "previous_cycle" = previous_cut$previous_cycle, "harvested_prev" = previous_cut$harvested_prev) #trim the table
  merged <- merge(problem_didnt_cut_greater_than_one, previous_cut, by = c("StandID", "previous_cycle")) #merge the problem_didnt_cut and previous_cut data to add the "harvested" column 
  nonproblem_didnt_cut <- merged[merged$harvested_prev == "YES",] #these values were harvested the previous cycle and thus couldn't be harvested again this cycle despite being over BA 
  still_problem_didnt_cut <- merged[merged$harvested_prev == "NO",] #these were not cut the previous cycle and still not cut
  still_problem_didnt_cut$previous_cycle <- NULL
  still_problem_didnt_cut$harvested_prev <- NULL
  still_problem_didnt_cut <- rbind(still_problem_didnt_cut, problem_didnt_cut_one) #include the Year 1 values
  still_problem_didnt_cut2 <- still_problem_didnt_cut[!still_problem_didnt_cut$`_SBA` == 0,] #remove stands where SBA = 0, which would mean no harvest because there are no trees eligible for harvest based on diameter limits (i.e. all trees in stand are too big or too small)
  problem_didnt_cut_old <- problem_didnt_cut
  problem_didnt_cut <- arrange(still_problem_didnt_cut2, StandID, Year)
  
  #Make sure cut amount was correct
  BA_cutyears2$`_TBA` <- as.numeric(BA_cutyears2$`_TBA`)
  
  #Sometimes LBA was called BLBA (e.g. package 5) and SBA was called something different. The if statements below corrects for these cases
  if("_LBA" %in% colnames(BA_cutyears2)) {
    BA_cutyears2$LBA <- BA_cutyears2$`_LBA`
  } else if ("_BLBA" %in% colnames(BA_cutyears2)) {
    BA_cutyears2$LBA <- BA_cutyears2$`_BLBA`
  } else {
  }
  if("_SBA" %in% colnames(BA_cutyears2)) {
    BA_cutyears2$SBA <- BA_cutyears2$`_SBA`
  } else if ("_BSBA" %in% colnames(BA_cutyears2)) {
    BA_cutyears2$SBA <- BA_cutyears2$`_BSBA`
  } else {
  }
  
  #The section below checks that the harvest amount was as expected based on the prescription set in the KCP file. 
  #It is a rough estimate as the printed variables don't account forall harvested amounts.
  if (!is.na(master_package$QMD[master_package$PkgNum == substr(path,12,14)])) { #check if master_package has a QMD value. If the QMD value is not NA, this code runs, otherwise skip to else.
    clearcut <- BA_cutyears2[BA_cutyears2$`_BQMD` >= BA_cutyears2$`_QCRT`,]
    clearcut$check <- ifelse(clearcut$TPA == clearcut$RTpa, "NO", "YES") #Check that TPA = RTpa, i.e. check that all TPA was removed
    problem_wrong_amt_cut <- clearcut[clearcut$check == "YES",]
    
    nonclearcut <- BA_cutyears2[BA_cutyears2$`_BQMD` < BA_cutyears2$`_QCRT`,] #now check nonclearcuts
    nonclearcut$LBA <- as.numeric(nonclearcut$LBA)
    nonclearcut$`_BBA` <- as.numeric(nonclearcut$`_BBA`)
    LBA_GT_TBA <- nonclearcut[nonclearcut$LBA > nonclearcut$`_TBA`,]
    LBA_GT_TBA$POSTCALC <- LBA_GT_TBA$`_BBA` - LBA_GT_TBA$SBA
    LBA_GT_TBA$check <- round(LBA_GT_TBA$PSTBAALL,0) == round(LBA_GT_TBA$POSTCALC, 0)
    problem_LBA_GT_TBA <- LBA_GT_TBA[LBA_GT_TBA$check == FALSE,]
    problem_LBA_GT_TBA <- problem_LBA_GT_TBA[!paste(problem_LBA_GT_TBA$StandID, problem_LBA_GT_TBA$Year) %in% paste(nonproblem_didnt_cut$StandID, nonproblem_didnt_cut$Year),] #remove stands that weren't cut because they were cut the previous cycle
    problem_LBA_GT_TBA$FOURTOFIVE <- problem_LBA_GT_TBA$`_BBA` - problem_LBA_GT_TBA$PREBAALL #get BA for trees in 4-5in DBH
    problem_LBA_GT_TBA$PSTBAFOURTOFIVE <- problem_LBA_GT_TBA$PSTBAALL + (problem_LBA_GT_TBA$FOURTOFIVE) #add 4-5in DBH trees 
    problem_LBA_GT_TBA$check2 <- with(problem_LBA_GT_TBA, problem_LBA_GT_TBA$PSTBAFOURTOFIVE <= problem_LBA_GT_TBA$PSTBAALL + BA_threshold & 
                                        problem_LBA_GT_TBA$PSTBAFOURTOFIVE >= problem_LBA_GT_TBA$PSTBAALL- BA_threshold) #check that package calculated BA is within BA_threshold of ATBA
    problem_LBA_GT_TBA <- problem_LBA_GT_TBA[problem_LBA_GT_TBA$check2 == FALSE,]
    
    LBA_LT_TBA <- nonclearcut[nonclearcut$LBA < nonclearcut$`_TBA`,]
    LBA_LT_TBA$check <- round(LBA_LT_TBA$PSTBAALL,0) == round(LBA_LT_TBA$`_TBA`,0)
    LBA_LT_TBA$POSTCALC <- LBA_LT_TBA$`_TBA`
    problem_LBA_LT_TBA <- LBA_LT_TBA[LBA_LT_TBA$check == FALSE,]
    problem_LBA_LT_TBA <- problem_LBA_LT_TBA[!paste(problem_LBA_LT_TBA$StandID, problem_LBA_LT_TBA$Year) %in% paste(nonproblem_didnt_cut$StandID, nonproblem_didnt_cut$Year),] #remove stands that weren't cut because they were cut the previous cycle
    problem_LBA_LT_TBA$FOURTOFIVE <- problem_LBA_LT_TBA$`_BBA` - problem_LBA_LT_TBA$PREBAALL #get BA for trees in 4-5in DBH
    problem_LBA_LT_TBA$PSTBAFOURTOFIVE <- problem_LBA_LT_TBA$PSTBAALL + (problem_LBA_LT_TBA$FOURTOFIVE*2/3) #add 4-5in DBH trees and multiply by 2/3 to get post-harvest BA
    problem_LBA_LT_TBA$check2 <- with(problem_LBA_LT_TBA, problem_LBA_LT_TBA$PSTBAFOURTOFIVE <= problem_LBA_LT_TBA$`_TBA` + BA_threshold & 
                                        problem_LBA_LT_TBA$PSTBAFOURTOFIVE >= problem_LBA_LT_TBA$`_TBA`- BA_threshold) #check that package calculated BA is within BA_threshold of ATBA
    problem_LBA_LT_TBA <- problem_LBA_LT_TBA[problem_LBA_LT_TBA$check2 == FALSE,]
    
    problem_wrong_amt_cut <- rbind(problem_wrong_amt_cut, problem_LBA_LT_TBA, problem_LBA_GT_TBA)
    
  } else {
    BA_cutyears2$LBA <- as.numeric(BA_cutyears2$LBA)
    BA_cutyears2$`_BBA` <- as.numeric(BA_cutyears2$`_BBA`)
    LBA_GT_TBA <- BA_cutyears2[BA_cutyears2$LBA > BA_cutyears2$`_TBA`,] #Get standyears where LBA > TBA
    LBA_GT_TBA$POSTCALC <- LBA_GT_TBA$`_BBA` - LBA_GT_TBA$SBA #Calculate post harvest BA By subtracting SBA from BBA
    LBA_GT_TBA$check <- round(LBA_GT_TBA$PSTBAALL,0) == round(LBA_GT_TBA$POSTCALC, 0) #Check that the FVS printed variable PSTBAALL is equal to above calculation
    problem_LBA_GT_TBA <- LBA_GT_TBA[LBA_GT_TBA$check == FALSE,] #Label problems where above check is not the case
    problem_LBA_GT_TBA <- problem_LBA_GT_TBA[!paste(problem_LBA_GT_TBA$StandID, problem_LBA_GT_TBA$Year) %in% paste(nonproblem_didnt_cut$StandID, nonproblem_didnt_cut$Year),] #remove stands that weren't cut because they were cut the previous cycle
    problem_LBA_GT_TBA$FOURTOFIVE <- problem_LBA_GT_TBA$`_BBA` - problem_LBA_GT_TBA$PREBAALL #get BA for trees in 4-5in DBH which were in "grey area" as per KCP files as PSTBAALL didn't include them by taking BBA - PREBAALL
    problem_LBA_GT_TBA$PSTBAFOURTOFIVE <- problem_LBA_GT_TBA$PSTBAALL + (problem_LBA_GT_TBA$FOURTOFIVE) #add 4-5in DBH trees 
    problem_LBA_GT_TBA$check2 <- with(problem_LBA_GT_TBA, problem_LBA_GT_TBA$PSTBAFOURTOFIVE <= problem_LBA_GT_TBA$PSTBAALL + BA_threshold & 
                                        problem_LBA_GT_TBA$PSTBAFOURTOFIVE >= problem_LBA_GT_TBA$PSTBAALL- BA_threshold) #check that package calculated BA is within BA_threshold of ATBA
    problem_LBA_GT_TBA <- problem_LBA_GT_TBA[problem_LBA_GT_TBA$check2 == FALSE,] #find rows that still remain a problem even with 4-5in trees added in
    
    LBA_LT_TBA <- BA_cutyears2[BA_cutyears2$LBA < BA_cutyears2$`_TBA`,] #Get standyears where LBA < TBA
    LBA_LT_TBA$check <- round(LBA_LT_TBA$PSTBAALL,0) == round(LBA_LT_TBA$`_TBA`,0) #Check rows where PSTBAALL is not equal to TBA
    LBA_LT_TBA$POSTCALC <- LBA_LT_TBA$`_TBA` #Post harvest BA should be equal to TBA for these stand-years
    problem_LBA_LT_TBA <- LBA_LT_TBA[LBA_LT_TBA$check == FALSE,] #Get rows that failed above check
    problem_LBA_LT_TBA <- problem_LBA_LT_TBA[!paste(problem_LBA_LT_TBA$StandID, problem_LBA_LT_TBA$Year) %in% paste(nonproblem_didnt_cut$StandID, nonproblem_didnt_cut$Year),] #remove stands that weren't cut because they were cut the previous cycle
    problem_LBA_LT_TBA$FOURTOFIVE <- problem_LBA_LT_TBA$`_BBA` - problem_LBA_LT_TBA$PREBAALL #get BA for trees in 4-5in DBH which were in "grey area" as per KCP files as PSTBAALL didn't include them by taking BBA - PREBAALL
    problem_LBA_LT_TBA$PSTBAFOURTOFIVE <- problem_LBA_LT_TBA$PSTBAALL + (problem_LBA_LT_TBA$FOURTOFIVE*2/3) #add 4-5in DBH trees and multiply by 2/3 to get estimated post-harvest BA
    problem_LBA_LT_TBA$check2 <- with(problem_LBA_LT_TBA, problem_LBA_LT_TBA$PSTBAFOURTOFIVE <= problem_LBA_LT_TBA$`_TBA` + BA_threshold & 
                                        problem_LBA_LT_TBA$PSTBAFOURTOFIVE >= problem_LBA_LT_TBA$`_TBA`- BA_threshold) #check that package calculated BA is within BA_threshold of ATBA
    problem_LBA_LT_TBA <- problem_LBA_LT_TBA[problem_LBA_LT_TBA$check2 == FALSE,]
    
    problem_wrong_amt_cut <- rbind(problem_LBA_LT_TBA, problem_LBA_GT_TBA)
  }
  
  
  
  #Make sure cuts are 20 years apart.
  nonzero_rtpa <- FVS_Summary[FVS_Summary$RTpa > 0,] #get all stand-years with cut based on RTpa > 0
  
  test <- function() {
    multiple_cuts <- data.frame(table(nonzero_rtpa$StandID)) #get number of cuts for all stands that were cut
    if (!nrow(multiple_cuts) == 0) {
      multiple_cuts2 <- multiple_cuts[multiple_cuts$Freq == 2,] #for stands that were cut twice
      if (nrow(multiple_cuts2) > 0) {
        multiple_cuts2$Freq <- NULL
        names(multiple_cuts2)[1] <- "StandID"
        multiple_cuts2 <- merge(multiple_cuts2, nonzero_rtpa, by = "StandID")
        multiple_cuts2$check <- NA
        
        multiple_cuts2 <- arrange(multiple_cuts2, StandID, Year) #sort by StandID then Year
        
        multiplerows <- (nrow(multiple_cuts2)-1)
        mult_length <- seq(1,multiplerows, by = 2) #get every other row number
        
        for (i in mult_length) {
          multiple_cuts2$check[i:(i+1)] <- (multiple_cuts2[(i+1),3] - multiple_cuts2[i,3]) >= 20
        }  #check that positive cut values are 20 years apart
        
        problem_mc2 <- multiple_cuts2[multiple_cuts2$check == FALSE,]
      } else {
        
      }
      
      return(problem_mc2)
      
      
      multiple_cuts3 <- multiple_cuts[multiple_cuts$Freq == 3,] #for stands that were cut 3 times
      
      if (nrow(multiple_cuts3 > 0)) {
        multiple_cuts3$Freq <- NULL
        names(multiple_cuts3)[1] <- "StandID"
        multiple_cuts3 <- merge(multiple_cuts3, nonzero_rtpa, by = "StandID")
        multiple_cuts3$check <- NA
        
        multiple_cuts3 <- arrange(multiple_cuts3, StandID, Year) #sort by StandID then Year
        
        multiplerows <- (nrow(multiple_cuts3)-1)
        mult_length <- seq(1,multiplerows, by = 3) #get every 3rd row number
        
        for (i in mult_length) {
          multiple_cuts3$check[i:(i+1)] <- (multiple_cuts3[(i+1),3] - multiple_cuts3[i,3]) >= 20
        }
        problem_mc3 <- multiple_cuts3[multiple_cuts3$check == FALSE,]
        return(problem_mc3)
      } else {
        
      }
      
      multiple_cuts4 <- multiple_cuts[multiple_cuts$Freq == 4,] #for stands that were cut 4 times
      if (nrow(multiple_cuts4 > 0)) {
        multiple_cuts4$Freq <- NULL
        names(multiple_cuts4)[1] <- "StandID"
        multiple_cuts4 <- merge(multiple_cuts, nonzero_rtpa, by = "StandID")
        multiple_cuts4$check <- NA
        
        multiple_cuts4 <- arrange(multiple_cuts4, StandID, Year) #sort by StandID then Year
        
        multiplerows <- (nrow(multiple_cuts4)-1)
        mult_length <- seq(1,multiplerows, by = 4) #get every 4th row number
        
        for (i in mult_length) {
          multiple_cuts4$check[i:(i+1)] <- (multiple_cuts4[(i+1),3] - multiple_cuts4[i,3]) >= 20
        }
        
        problem_mc4 <- multiple_cuts4[multiple_cuts4$check == FALSE,]
        return(problem_mc4)
      } else {
        
      }
      
      check_data <- rbind(problem_mc2, problem_mc3, problem_mc4)
      return(check_data)
    } else {
    }
    
  }
  
  problem_wrong_cut_int <- data.frame(test())
  
  row <- data.frame("filename" = path, "FVS_Cases_rows" = FVS_Cases_rows, "problem_no_crt" = problem_no_crt, "wrong_cut" = nrow(problem_wrong_cut), 
                    "didnt_cut" = nrow(problem_didnt_cut), "wrong_amt_cut" = nrow(problem_wrong_amt_cut), "wrong_cut_int" = nrow(problem_wrong_cut_int),
                    "SurVVolRatio" = SurvVolRatio)
  problem <- rbind(problem, row)
  
  problem2 <- list()
  problem2$problem_wrong_cut <- problem_wrong_cut
  problem2$problem_didnt_cut <- problem_didnt_cut
  problem2$problem_wrong_amt_cut <- problem_wrong_amt_cut
  problem2$problem_wrong_cut_int <- problem_wrong_cut_int
  problem2$problem <- problem
  
  return(problem2)
}

output <- fvs_spotcheck("H:/cec_20170915/fvs/data/CA","CA", 10, "001")

problem <- data.frame(output$problem)
problem_wrong_cut <- data.frame(output$problem_wrong_cut)
problem_didnt_cut <- data.frame(output$problem_didnt_cut)
problem_wrong_amt_cut <- data.frame(output$problem_wrong_amt_cut)
problem_wrong_cut_int <- data.frame(output$problem_wrong_cut_int)

write.csv(CA_BAonly_problem, "CA_BAonly_problem.csv")
write.csv(problem, "problem.csv")
write.csv(problem_wrong_cut, "problem_wrong_cut.csv")
write.csv(problem_didnt_cut, "problem_didnt_cut.csv")
write.csv(problem_wrong_amt_cut, "problem_wrong_amt_cut.csv")
write.csv(problem_wrong_cut_int, "problem_wrong_cut_int.csv")
