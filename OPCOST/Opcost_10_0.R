#####Initial package loading
#####Automatically install if package missing
packages = ("RODBC")

package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, repos="http://cran.r-project.org", dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

####LOAD DATA FROM BIOSUM####
args=(commandArgs(TRUE))

print(args[1])
con<-odbcConnectAccess2007(args[1])
print("odbc Connection:OK")
m<-data.frame(sqlFetch(con, "opcost_input", as.is=TRUE))
print("m data.frame opcost_input SqlFetch:OK")

odbcCloseAll()

ref2 <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", args[2])

con2 <- odbcDriverConnect(ref2)

opcost_equation_ref<- sqlFetch(con2, "opcost_equation_ref", as.is = TRUE)
names(opcost_equation_ref) <- opcost_equation_ref[1,]
opcost_equation_ref <- opcost_equation_ref[-1,]

opcost_units <- sqlFetch(con2, "opcost_units", as.is = TRUE)
names(opcost_units) <- opcost_units[1,]
opcost_units <- opcost_units[-1,]

opcost_cost_ref <- sqlFetch(con2, "opcost_cost_ref", as.is = TRUE)
names(opcost_cost_ref) <- opcost_cost_ref[1,]
opcost_cost_ref <- opcost_cost_ref[-1,]

opcost_harvestsystem_ref <- sqlFetch(con2, "opcost_harvestsystem_ref", as.is = TRUE)
names(opcost_harvestsystem_ref) <- opcost_harvestsystem_ref[1,]
opcost_harvestsystem_ref <- opcost_harvestsystem_ref[-1,]

opcost_ideal_ref <- sqlFetch(con2, "opcost_ideal_ref", as.is = TRUE)
names(opcost_ideal_ref) <- opcost_ideal_ref[1,]
opcost_ideal_ref <- opcost_ideal_ref[-1,]

odbcCloseAll()

####MANUALLY RUN OPCOST ON A SINGLE OPCOST INPUT FILE####
# opcost.ref.location <- "E:/Dropbox/Carlin/GitHub/Fia_Biosum_Scripts/OPCOST/opcost_ref.accdb" #set the location of the opcost_ref.accdb you'd like to use
# opcost.input.location <- "E:/cec_20180529/cec_20180529/OPCOST/Input/OPCOST_10_0_Input_CA_P001_100_100_100_100_2018-06-05_07_43_09_AM.accdb"
# 
# #Opcost_Input
# conn.path <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", file.path(opcost.input.location))
# conn <- odbcDriverConnect(conn.path)
# m<-data.frame(sqlFetch(conn, "opcost_input", as.is=TRUE))
# 
# odbcCloseAll()
# 
# #Opcost_Ref
# conn.path <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", file.path(opcost.ref.location))
# conn <- odbcDriverConnect(conn.path)
# opcost_equation_ref<- sqlFetch(conn, "opcost_equation_ref", as.is = TRUE)
# names(opcost_equation_ref) <- opcost_equation_ref[1,]
# opcost_equation_ref <- opcost_equation_ref[-1,]
# 
# opcost_units <- sqlFetch(conn, "opcost_units", as.is = TRUE)
# names(opcost_units) <- opcost_units[1,]
# opcost_units <- opcost_units[-1,]
# 
# opcost_cost_ref <- sqlFetch(conn, "opcost_cost_ref", as.is = TRUE)
# names(opcost_cost_ref) <- opcost_cost_ref[1,]
# opcost_cost_ref <- opcost_cost_ref[-1,]
# 
# opcost_harvestsystem_ref <- sqlFetch(conn, "opcost_harvestsystem_ref", as.is = TRUE)
# names(opcost_harvestsystem_ref) <- opcost_harvestsystem_ref[1,]
# opcost_harvestsystem_ref <- opcost_harvestsystem_ref[-1,]
# 
# opcost_ideal_ref <- sqlFetch(conn, "opcost_ideal_ref", as.is = TRUE)
# names(opcost_ideal_ref) <- opcost_ideal_ref[1,]
# opcost_ideal_ref <- opcost_ideal_ref[-1,]
# 
# odbcCloseAll()

#####BRING IN REFERENCE TABLES -- THIS IS FOR IF YOU'RE NOT USING THE ACCESS DATABASE VERSION ABOVE AND NEED TO BRING IN THE CSVS######
# setwd("G:/Dropbox/Carlin/GitHub/Fia_Biosum_Scripts/OPCOST")
# opcost_equation_ref <- read.csv("opcost_equation_ref.csv")
# opcost_units <- read.csv("opcost_units.csv")
# opcost_cost_ref <- read.csv("opcost_cost_ref.csv")
# opcost_harvestsystem_ref <- read.csv("opcost_harvestsystem_ref.csv")
# opcost_ideal_ref <- read.csv("opcost_ideal_ref.csv")

#Convert to Data Frame and set "NaN' to NA
m <- data.frame(m)
m[m == "NaN"] <- NA #convert "NaN" values to NA
pattern <- c("Stand", "Harvesting.System")
m_old_HS <- m[,c(which(grepl(paste0(pattern, collapse = "|"), names(m))))]

#Band aid fixes to run tethered CTL only. This means that the "optimal" and the harvest system for input will always be Tethered CTL 
opcost_harvestsystem_ref <- opcost_harvestsystem_ref[opcost_harvestsystem_ref$Harvesting.System == "Tethered CTL",]
opcost_equation_ref <- opcost_equation_ref[opcost_equation_ref$Equation.ID %in% opcost_harvestsystem_ref$Equation.ID,]
m$Harvesting.System <- "Tethered CTL"

#twitchVol is (Trees Per Acre * Average Tree Volume (ft3)) by size class, divided by total trees per acre to get average volume per tree (ft3/tree)
m$twitchVol <- ((m$Large.log.trees.per.acre * m$Large.log.trees.average.vol.ft3.) + 
               (m$Small.log.trees.per.acre * m$Small.log.trees.average.volume.ft3.) +
               (m$Chip.tree.per.acre * m$Chip.trees.average.volume.ft3.)) / 
               (m$Large.log.trees.per.acre + m$Small.log.trees.per.acre + m$Chip.tree.per.acre)
m$twitchVol[m$Large.log.trees.per.acre == 0 & m$Small.log.trees.per.acre == 0 &  m$Chip.tree.per.acre == 0] <- 0

####TODO - DOUBLE CHECK THAT THIS IS METRIC CONVERSION####
#Convert twitchVol to metric 
m$twitchVolM <- m$twitchVol*0.0283168

####TODO - WHY AREN'T CHIP TREES INCLUDED####
#totalVol is the (Trees Per Acre * Average Tree Volume (ft3)) for small and large trees (ft3/acre)
m$totalVol <- (m$Large.log.trees.per.acre * m$Large.log.trees.average.vol.ft3.) + (m$Small.log.trees.per.acre * m$Small.log.trees.average.volume.ft3.)

####TODO - DOUBLE CHECK THAT THIS IS METRIC CONVERSION####
#Convert totalVol to metric
m$totalVolM <- m$totalVol * 0.0283168

####TODO - DOUBLE CHECK CONVERSION, DOCUMENT COEFFICIENTS - CHECK WITH ROB####
#dbh calculated from twitchVol
m$dbh <- sqrt((m$twitchVol + 8.4166)/.2679)

#dbh in cm
m$dbh.cm <- m$dbh*2.54

#treesRemoved is cut Trees Per Acre for all size classes summed (TPA)
m$treesRemoved <- m$Small.log.trees.per.acre + m$Large.log.trees.per.acre + m$Chip.tree.per.acre

####TODO - SHOULDN'T THIS BE MULTIPLIED??? CHIP PCT VALUES SHOULD BE MULTIPLIED BY VOLUME OR TPA####

#chipTrees varies by harvesting system (TPA)
##Ground-Based Mech WT, Ground-Based Manual WT, Shovel Logging##
#####Small.log.trees.ChipPct_Cat2_4 + Large.log.trees.ChipPct_Cat2 + Chip.tree.per.acre
##Cable Manual WT##
#####Small.log.trees.ChipPct_Cat2_4 + Large.log.trees.ChipPct_Cat1_3_4 + Chip.tree.per.acre
##Cable Manual WT/Log
#####Small.log.trees.ChipPct_Cat5 + Large.log.trees.ChipPct_Cat5 + Chip.tree.per.acre
##Helicopter CTL, Ground-Based CTL, Cable CTL, Ground-Based Manual Log##
#####Small.log.trees.ChipPct_Cat1_3 + Large.log.trees.ChipPct_Cat1_3_4 + Chip.tree.per.acre
##Cable Manual Log, Helicopter Manual WT##
#####Small.log.trees.ChipPct_Cat1_3 + Large.log.trees.ChipPct_Cat1_3_4 + Chip.tree.per.acre

m$chipTrees[m$Harvesting.System == "Ground-Based Mech WT" | m$Harvesting.System == "Ground-Based Manual WT" | m$Harvesting.System == "Shovel Logging"] <- 
  (m$Small.log.trees.ChipPct_Cat2_4[m$Harvesting.System == "Ground-Based Mech WT" | m$Harvesting.System == "Ground-Based Manual WT" | m$Harvesting.System == "Shovel Logging"] +
     m$Large.log.trees.ChipPct_Cat2[m$Harvesting.System == "Ground-Based Mech WT" | m$Harvesting.System == "Ground-Based Manual WT" | m$Harvesting.System == "Shovel Logging"] +
     m$Chip.tree.per.acre[m$Harvesting.System == "Ground-Based Mech WT" | m$Harvesting.System == "Ground-Based Manual WT" | m$Harvesting.System == "Shovel Logging"])

m$chipTrees[m$Harvesting.System == "Cable Manual WT"] <- 
  m$Small.log.trees.ChipPct_Cat2_4[m$Harvesting.System == "Cable Manual WT"] +
  m$Large.log.trees.ChipPct_Cat1_3_4[m$Harvesting.System == "Cable Manual WT"] +
  m$Chip.tree.per.acre[m$Harvesting.System == "Cable Manual WT"]

m$chipTrees[m$Harvesting.System == "Cable Manual WT/Log"] <- 
  m$Small.log.trees.ChipPct_Cat5[m$Harvesting.System == "Cable Manual WT/Log"] +
  m$Large.log.trees.ChipPct_Cat5[m$Harvesting.System == "Cable Manual WT/Log"] +
  m$Chip.tree.per.acre[m$Harvesting.System == "Cable Manual WT/Log"]

m$chipTrees[m$Harvesting.System == "Helicopter CTL" | m$Harvesting.System == "Ground-Based CTL" | m$Harvesting.System == "Cable CTL" | m$Harvesting.System == "Ground-Based Manual Log"] <- 
  m$Small.log.trees.ChipPct_Cat1_3[m$Harvesting.System == "Helicopter CTL" | m$Harvesting.System == "Ground-Based CTL" | m$Harvesting.System == "Cable CTL" | m$Harvesting.System == "Ground-Based Manual Log"] +
  m$Large.log.trees.ChipPct_Cat1_3_4[m$Harvesting.System == "Helicopter CTL" | m$Harvesting.System == "Ground-Based CTL" | m$Harvesting.System == "Cable CTL" | m$Harvesting.System == "Ground-Based Manual Log"] +
  m$Chip.tree.per.acre[m$Harvesting.System == "Helicopter CTL" | m$Harvesting.System == "Ground-Based CTL" | m$Harvesting.System == "Cable CTL" | m$Harvesting.System == "Ground-Based Manual Log"]

m$chipTrees[m$Harvesting.System == "Cable Manual Log" | m$Harvesting.System == "Helicopter Manual WT"] <- 
  m$Small.log.trees.ChipPct_Cat1_3[m$Harvesting.System == "Cable Manual Log" | m$Harvesting.System == "Helicopter Manual WT"] +
  m$Large.log.trees.ChipPct_Cat1_3_4+
  m$Chip.tree.per.acre[m$Harvesting.System == "Cable Manual Log" | m$Harvesting.System == "Helicopter Manual WT"]

####TODO - REVISE TO REFLECT CHANGE FROM PROPORTION TO PERCENT - WEIGHTED AVERAGE OF % BY VOLUME####
#sppgrp gets species group depending on hardwood/softwood percent (if sum of small + large hardwood percent > 1, sppgrp is 0, otherwise = 1)
m$sppgrp <- ifelse((m$Small.log.trees.hardwood.percent+m$Large.log.trees.hardwood.percent) > 1, 0, 1)

####TODO - DOCUMENT COEFFICIENTS/FORMULA####
#distBetween Trees is calculated by taking all size classes trees per acre (feet)
m$distBetweenTrees <- (sqrt((43560/(m$Small.log.trees.per.acre+ m$Large.log.trees.per.acre+ m$Chip.tree.per.acre))/pi))*2 
m$distBetweenTrees[m$Large.log.trees.per.acre == 0 & m$Small.log.trees.per.acre == 0 &  m$Chip.tree.per.acre == 0] <- NA

#twitchWeight is average of non-zero large and small log density values (lbs/ft3) * twitchVol (ft3/tree) to get (lbs/tree) 
m$twitchWeight[m$Large.log.trees.average.density.lbs.ft3. > 0 & m$Small.log.trees.average.density.lbs.ft3. > 0] <- rowMeans(m[m$Large.log.trees.average.density.lbs.ft3. > 0 & m$Small.log.trees.average.density.lbs.ft3. > 0, c(26,18)]) * m$twitchVol[m$Large.log.trees.average.density.lbs.ft3. > 0 & m$Small.log.trees.average.density.lbs.ft3. > 0]
m$twitchWeight[m$Large.log.trees.average.density.lbs.ft3. == 0] <- m$Small.log.trees.average.density.lbs.ft3.[m$Large.log.trees.average.density.lbs.ft3. == 0] * m$twitchVol[m$Large.log.trees.average.density.lbs.ft3. == 0]
m$twitchWeight[m$Small.log.trees.average.density.lbs.ft3. == 0] <- m$Large.log.trees.average.density.lbs.ft3.[m$Small.log.trees.average.density.lbs.ft3. == 0] * m$twitchVol[m$Small.log.trees.average.density.lbs.ft3. == 0]
m$twitchWeight[m$Small.log.trees.average.density.lbs.ft3. == 0 & m$Large.log.trees.average.density.lbs.ft3. == 0] <- 0

#totalWeight is average of non-zero large and small log density values (lbs/ft3) * totalVol (ft3/acre) to get (lbs/acre)
m$totalWeight[m$Large.log.trees.average.density.lbs.ft3. > 0 & m$Small.log.trees.average.density.lbs.ft3. > 0] <- rowMeans(m[m$Large.log.trees.average.density.lbs.ft3. > 0 & m$Small.log.trees.average.density.lbs.ft3. > 0, c(26,18)]) * m$totalVol[m$Large.log.trees.average.density.lbs.ft3. > 0 & m$Small.log.trees.average.density.lbs.ft3. > 0]
m$totalWeight[m$Large.log.trees.average.density.lbs.ft3. == 0] <- m$Small.log.trees.average.density.lbs.ft3.[m$Large.log.trees.average.density.lbs.ft3. == 0] * m$totalVol[m$Large.log.trees.average.density.lbs.ft3. == 0]
m$totalWeight[m$Small.log.trees.average.density.lbs.ft3. == 0] <- m$Large.log.trees.average.density.lbs.ft3.[m$Small.log.trees.average.density.lbs.ft3. == 0] * m$totalVol[m$Small.log.trees.average.density.lbs.ft3. == 0]
m$totalWeight[m$Small.log.trees.average.density.lbs.ft3. == 0 & m$Large.log.trees.average.density.lbs.ft3. == 0] <- 0

####TODO - WHY AREN'T CHIP TREES INCLUDED####
#cordsPerAcre is (Trees Per Acre * Average Tree Volume (ft3)) for small and large trees (ft3/acre) aka totalVol divided by 128
m$cordsPerAcre <- ((m$Large.log.trees.per.acre * m$Large.log.trees.average.vol.ft3.) +
                     (m$Small.log.trees.per.acre * m$Small.log.trees.average.volume.ft3.))/128

###Tethered Harvester equations - see Petitmermet harvest cost model doc for reference

#Tethered total weight in lbm converted to tonnes
# m$TetheredTotalWeight <- (m$Small.log.trees.per.acre * m$Small.log.trees.average.volume.ft3. * m$Small.log.trees.average.density.lbs.ft3.)*0.0005
m$MerchWtLbs_SL <- m$Small.log.trees.per.acre * m$Small.log.trees.average.volume.ft3.*m$Small.log.trees.average.density.lbs.ft3. * (m$Small.log.trees.MerchAsPctOfTotal/100)
m$MerchWtLbs_CT <- m$Chip.tree.per.acre * m$Chip.trees.average.volume.ft3. * m$CHIPS.Average.Density..lbs.ft3. * (m$Chip.trees.MerchAsPctOfTotal/100)
m$MerchWtLbs_Harvester <- m$MerchWtLbs_CT + m$MerchWtLbs_SL
m$MerchWtTonnes_Harvester <- 0.000453592 * m$MerchWtLbs_Harvester

###Tethered Forwarder Equations - see Petitmermet harvest cost model doc for reference
m$MerchWtLbs_LL <- m$Large.log.trees.per.acre * m$Large.log.trees.average.vol.ft3. * m$Large.log.trees.average.density.lbs.ft3. * (m$Large.log.trees.MerchAsPctOfTotal/100)
m$MerchWtLbs_Forwarder <- m$MerchWtLbs_CT + m$MerchWtLbs_SL + m$MerchWtLbs_LL
m$MerchWtTonnes_Forwarder <- 0.000453592 * m$MerchWtLbs_Forwarder

m$TPA_CSL <- m$Small.log.trees.per.acre + m$Large.log.trees.per.acre + m$Chip.tree.per.acre

m$MerchWtLbs_Loader <- m$MerchWtLbs_LL + m$MerchWtLbs_SL
m$MerchWtTonnes_Loader <- m$MerchWtLbs_Loader * 0.000453592

m$FT_wt_CT <- with(m,ifelse(Chip.tree.per.acre > 0, exp(1.0613+0.8841*log(Chip.tree.per.acre)+0.6714*log(MerchWtLbs_CT * 0.000453592/Chip.tree.per.acre)),0))
m$FT_wt_SL <- with(m,ifelse(Small.log.trees.per.acre > 0, exp(1.0613+0.8841*log(Small.log.trees.per.acre)+0.6714*log(MerchWtLbs_CT * 0.000453592/Small.log.trees.per.acre)),0))
m$FT_wt_LL <- with(m, ifelse(Large.log.trees.per.acre > 0, exp(1.0613+0.8841*log(Large.log.trees.per.acre)+0.6714*log(MerchWtLbs_CT * 0.000453592/Large.log.trees.per.acre)),0))


m$ChipFeedstockWeight <- (m$Chip.tree.per.acre * (m$Chip.trees.MerchAsPctOfTotal/100) * m$Chip.trees.average.volume.ft3. * m$CHIPS.Average.Density..lbs.ft3.) + 
                         (m$Small.log.trees.per.acre * (m$Small.log.trees.ChipPct_Cat1_3/100) * m$Small.log.trees.average.volume.ft3. * m$Small.log.trees.average.density.lbs.ft3.) + 
                         (m$Large.log.trees.per.acre * (m$Large.log.trees.ChipPct_Cat1_3_4/100) * m$Large.log.trees.average.vol.ft3. * m$Large.log.trees.average.density.lbs.ft3.)



#####CALCULATE HOURS PER ACRE######
#calculate_hpa calculates harvest time in hours per acre from the imported Opcost_Input table. 
#It incorporates opcost_equation_ref, opcost_modifiers, opcost_units to get a final hours per acre value 
#(as in it includes conversion from the original equation to hours per acre and 
#any other adjustments that get made as per opcost_modifiers). The function parameters are: 

#Arguments:
#data - The opcost input data
#equation.ID - the ID of the equation in the Equation.ID column

#Example: calculate_hpa(data = m, equation.ID = "TETH_01")

calculate_hpa <- function(data, equation.ID) {
  og_data_rows <- nrow(data) #get original number of rows in data
  og_data_columns <- ncol(data) #get original number of columns in data

  ref <- opcost_equation_ref #if a different ref is not specified, use opcost_equation_ref
  units <- opcost_units #if units is not specified, use opcost_units
  
  row <- ref[ref$Equation.ID == equation.ID,] #get the row from ref with the specified equation ID
  
  #stop function if there are duplicates of equation.ID
  if(nrow(row) > 1){
    stop("Duplicate equation ID") #this stops function if the equation ID is not unique
  } 
  
  if(nrow(row) == 0) {
    stop("Reference not found") #this stops the function if the equation.ID is not found in ref
  }
  
  if(row$Units != "") { 
    #convert units if Unit column is populated. This references opcost_units to add the unit conversion to the equation in opcost_equation_ref
    if(!row$Units %in% units$Unit){
      stop("Units not in opcost_units for conversion")
    }
    equation<- gsub("result", paste0("(",row$Equation,")"), units$Hours.Per.Acre.Conversion[units$Unit == as.character(row$Units)]) 
  } else {
    #If units is not populated, use the equation as is from opcost_equation_ref
    equation <- row$Equation
  }
  
  if(row$Limit.Statement != "") {  #for harvest methods with limits
    #Evaluate equation for data subset based on Limit.Statement
    limit.statement <- parse(text = paste0("with(data,", row$Limit.Statement, ")"))
    equation.statement <- parse(text = paste0("with(data[",limit.statement, ",],", equation, ")"))
    data[eval(limit.statement),ncol(data) + 1] <- eval(equation.statement)
  } else {
    equation.statement <- parse(text = paste0("with(data,", equation, ")"))
    data[,ncol(data) + 1] <- eval(equation.statement) #calculate hours per acre value based on equation 
  }
  
  if(ncol(data) > og_data_columns) {
      names(data)[ncol(data)] <- paste0(row$Equation.ID) #add column and name it based on the name/anaylsis/ID with "premod"
  } else {
    stop("Equation not calculated")
  }

  return(data)
  
}

#calculate_hpa1 <- calculate_hpa(data = m, equation.ID = "FB_02")

#####COMPARE HOURS PER ACRE CALCULATIONS BY MACHINE AND GET MEAN######
#compute_harvest_system_equations runs calculate_hpa() for all equations in an machine type (e.g. "Skidder"),
#puts them in a single table, and calculates the mean of all equation results for the machine type. 

#Arguments:
#data - The opcost input data
#harvest_system - Harvesting system to compute using equations specified in opcost_harvestsystem_ref (case sensitive). Note this will only run for equations specified in the "Equation.ID" column
#allCols - TRUE/FALSE - If true, shows original data columns as well as cost and mean values. If FALSE, output is stand ID with equation results and mean
#meansonly - TRUE/FALSE - If true, shows only the calculated mean values and not individual equation values.

#Example: compute_harvest_system_equations(data = m, harvesting_system = "Shovel Logging", allCols = TRUE)

compute_harvest_system_equations <- function(data, harvest_system, allCols, meansonly) {
  #add in default values for function parameters
  if(missing(allCols)) {
    allCols <- FALSE
  }
  
  equation_ref <- opcost_equation_ref
  harvestsystem_ref <- opcost_harvestsystem_ref
  
  data1 <- data #rename data
  a <- ncol(data1) #get number of columns to start
  newdata2 <- data.frame(matrix(NA, nrow = nrow(data), ncol = 1)) #set up empty matrix for output values
  
  harvest.system <- harvestsystem_ref[harvestsystem_ref$Harvesting.System == harvest_system,]
  
  harvest.system$machine.cost2 <- paste0(harvest.system$Machine, ".", harvest.system$Machine.Cost)
  pattern <- c(" ", "-", "/")
  harvest.system$machine.cost2  <- gsub(paste0(pattern, collapse = "|"),".",   harvest.system$machine.cost2)
  
  harvest.system2 <- strsplit(as.character(harvest.system$Equation.ID), split = ",")
  harvest.system2 <- data.frame(Machine.Cost = rep(harvest.system$machine.cost2, sapply(harvest.system2, length)), Equation.ID = trimws(unlist(harvest.system2)))
  
  harvest.equations <- harvest.system2$Equation.ID

  ref <- equation_ref[equation_ref$Equation.ID %in% harvest.equations,] #get rows for relevant machine from opcost_equation_reference (disregarding numbers)

  for (i in 1:nrow(ref)) { #use calculate_hpa to get hours per acre for each equation.ID
    newdata <- calculate_hpa(data = data1, equation.ID = ref$Equation.ID[i])
    newdata <- newdata[,c(1,(ncol(data1)+1):ncol(newdata))] #get the equation hours per acre value
    data <- merge(data, newdata, by = "Stand") #add equation hours per acre value to the original data (merged on Stand values)
  }
  
  data_premean <- data
  
  b <- ncol(data) #get new # of columns for data with analyis hours per acre value columns
  
  cleaned.newdata <- do.call(data.frame,lapply(data[,c(1,(a+1):b)], function(x) replace(x, is.infinite(x),NA))) #replace Inf values with NA
  cleaned.newdata <- do.call(data.frame,lapply(cleaned.newdata, function(x) replace(x, x == 0,NA))) #replace zero values with NA

  data <- merge(data1, cleaned.newdata)

  data2 <- data
  
  machines <- unique(harvest.system2$Machine.Cost) #get unique machine.cost values
  pattern <- c(" ", "-", "/")
  harvest_system <- gsub(paste0(pattern, collapse = "|"),".",   harvest_system)

  for (j in 1:length(machines)) {
    machine.eqs <- harvest.system2$Equation.ID[harvest.system2$Machine.Cost == machines[j]]
    machine.cols <- which(grepl(paste0(machine.eqs, collapse = "|"), names(data2)))
    if(length(machine.cols) > 1) {
      data2[,ncol(data2)+1] <- rowMeans(data2[,machine.cols], na.rm=TRUE) #calculate mean of machine hours per acre values
    } else {
      data2[,ncol(data2)+1] <- data2[,machine.cols]
    }
    names(data2)[ncol(data2)] <- paste0(harvest_system,".mean", ".", machines[j], "_HPA")
  }
  
  mean.cols <- c(1,which(grepl("mean", names(data2))))
  
  data.means <- data2[,mean.cols]
  data.means <- do.call(data.frame,lapply(data.means, function(x) replace(x, is.nan(x),NA))) #replace zero values with NA
  
  data <- merge(data_premean, data.means, all.x = TRUE)
  
  if(allCols != TRUE) { #if allCols = FALSE
    data <- data[,c(1,(a+1):ncol(data))] #remove old data columns (keep Stand column)
  }
  
  if(meansonly == TRUE) {
    data <- data.means
  }
  return(data)
}

tethered <- compute_harvest_system_equations(data = m, harvest_system = "Tethered CTL", allCols = TRUE, meansonly = FALSE)

#####GET MEAN HARVEST HOURS PER ACRE FOR ALL MACHINES######
#all_harvesting_systems runs compute_harvest_system_equations for all analyses and compiles a table
#with mean values for all analyses. It returns a list with each list item as a data frame for a specific
#harvesting system. This function is used in the estimate_cost() function. 

#Arguments:
#data - The opcost input data

#Example: all_harvesting_systems(data = m)

all_harvesting_systems <- function(data) {
  equation_ref <- opcost_equation_ref
  harvestsystem_ref <- opcost_harvestsystem_ref
  
  #get unique harvest system values from harvestsystem_ref
  harvest.system <- unique(harvestsystem_ref$Harvesting.System)
  
  #create empty list to store loop values
  mylist <- vector(mode="list", length=length(harvest.system))
  name.vector <- as.character() #create empty vector for list name values
  for (i in 1:length(harvest.system)) {
   name.vector <- c(name.vector, paste0(harvest.system[i]) ) #get name vector value for this loop iteration
   mylist[[i]] <- list(compute_harvest_system_equations(data, harvest.system[i], allCols = FALSE, meansonly = TRUE)) #run and store compute_harvest_system_equations for each unique machine value
  }
  
  names(mylist) <- name.vector #name each list item 
  
  #all <- Reduce(merge, mylist) #merge list items (i.e. put all machine compute_harvest_system_equations function results in a single data frame)
  
  return(mylist)
}

#all <- all_harvesting_systems(data = m)

#tethered_all <- as.data.frame(all$Tethered) #convert list to data frame

#all$meanChipTime <- chipTime(all) #band aid to account for the random ^0.8 raising if comparing with original opcost values. If you haven't run original opcost this won't work 


#####ESTIMATE COST######
#The estimate_cost function takes reference tables opcost_cost_ref, opcost_harvestsystem_ref, 
#and estimates costs by harvest system for all aspects of the harvest. This function relies on the 
#all_harvesting_systems() and compute_harvest_system_equations() functions. The output is a data frame
#in a list. 

#Arguments:
#harvest_system - the name of the harvest system to estimate costs for. This can be an individual harvest system or the word "ALL" to run all systems.
#data - variable name for Opcost_Input data 
#cost - Cost values the user wants to use to calculate costs. This corresponds to the name of each column in opcost_cost_ref. Defaults to "Default.CPH"

#Example: estimate_cost(harvest_system = "Shovel Logging", data = m, cost = "Default.CPH")
estimate_cost <- function(harvest_system, data, cost) {
  #add in default values for function parameters
  if(missing(cost)){
    cost <- "Default.CPH"
  }
  
  #bring in reference tables
  cost_ref <- opcost_cost_ref
  harvestsystem_ref <- opcost_harvestsystem_ref
  
  
  cost.col <- which(names(cost_ref) == cost) #get cost column specified in function parameter (or use Default.CPH)
  cost_ref2 <- cost_ref[c(1,cost.col)] 
  
  costestimate_ref <- merge(cost_ref2, harvestsystem_ref, all.y = TRUE) #merge cost_ref values with harvest_system_ref
  
  costestimate_ref <- costestimate_ref[costestimate_ref$Harvesting.System != "",] #remove harvest systems where a cost per hour value is not given
  
  if (any(is.na(costestimate_ref$Default.CPH))) {
    warning("A Harvesting.System was removed; cost per hour value missing for a machine")
  }
  
  remove.harvesting.system <- costestimate_ref$Harvesting.System[is.na(costestimate_ref$Default.CPH)] #get harvesting systems where a CPH value is missing
  costestimate_ref <- costestimate_ref[!costestimate_ref$Harvesting.System %in% remove.harvesting.system,]
  
  costestimate_ref$Default.CPH <- parse(text = paste0("with(system,", costestimate_ref$Default.CPH, ")"))
  
  costestimate_ref$full_cost <- ifelse(!is.na(costestimate_ref$Cost.Multiplier), paste0(costestimate_ref$Default.CPH, "*", costestimate_ref$Cost.Multiplier), paste0(costestimate_ref$Default.CPH))
  
  # #calculate "full cost" by taking cost values from cost_ref and multiplying them by Cost.Multiplier values in costestimate_ref
  # costestimate_ref$full_cost <- eval(costestimate_ref$full_cost_eq)
  
  #get machine and harvest system costs in standardized format (remove spaces, dashes, slashes)
  pattern <- c(" ", "-", "/") 
  costestimate_ref$harvest.system2 <- gsub(paste0(pattern, collapse = "|"),".", costestimate_ref$Harvesting.System)
  costestimate_ref$machine2 <- gsub(paste0(pattern, collapse = "|"),".", costestimate_ref$Machine)
  costestimate_ref$machine.cost2 <- gsub(paste0(pattern, collapse = "|"),".", costestimate_ref$Machine.Cost)
  
  #get the corresponding column names for each harvest system, machine, and machine cost row
  costestimate_ref$HPA_col <- paste0(costestimate_ref$harvest.system2, ".mean", ".", costestimate_ref$machine2, ".",costestimate_ref$machine.cost2, "_HPA")
  costestimate_ref$CPA_col <- paste0(costestimate_ref$harvest.system2, ".",costestimate_ref$machine2, ".", costestimate_ref$machine.cost2, "_CPA")

  if (harvest_system == "ALL") {
    all <- all_harvesting_systems(data) #run all_harvesting_systems() function on data to get "all" 
  } else {
    if (!harvest_system %in% costestimate_ref$Harvesting.System) {
      harvest_system <- unique(costestimate_ref$Harvesting.System[costestimate_ref$harvest.system2 == harvest_system])
    }
    all <- vector(mode="list", length= 1)
    all[[1]] <- compute_harvest_system_equations(data, harvest_system, allCols = FALSE, meansonly = TRUE) #run compute_harvesting_system_equations to get "all"
    names(all) <- harvest_system
  }
  
  names.vector <- names(all) #get the data frame names in "all"
  
  pattern <- c(" ", "-", "/") #use data frame names as the harvest system names vector
  names.vector <- gsub(paste0(pattern, collapse = "|"),".", names.vector)
  names(all) <- names.vector
  names.vector.remove <- names.vector[!names.vector %in% costestimate_ref$harvest.system2]
  names.vector <- names.vector[names.vector %in% costestimate_ref$harvest.system2]
  
  mylist <- vector(mode="list", length=length(names.vector))
  
  if(length(names.vector.remove) > 0) {
    for (k in 1:length(names.vector.remove)) {
      all[[names.vector.remove[k]]] <- NULL #remove systems from dataset that are are not in costestimate_ref 
    }
  }
  
  #pull in lowboy calculation parameters
  pattern <- c("Stand", "Move_In_Hours", "Harvest_area_assumed_acres", "Percent.Slope")
  lowboy.data <- data[,c(which(grepl(paste0(pattern, collapse = "|"),names(data))))]

  system.cpa1 <- data.frame()
  
  for(i in 1:length(names.vector)) {
    harvest.system <- names.vector[i]
    system.list <- all[[names.vector[i]]]
    
    if(harvest_system == "ALL") {
      names(system.list) <- ""
    }
    
    system <- data.frame(system.list)
    
    system <- merge(lowboy.data, system, all.y = TRUE)
    
    if(nrow(system) == 0) {
      stop("System not found in list")
    }
    
    system.cpa <- data.frame()
    harvest.cost <- costestimate_ref[costestimate_ref$harvest.system2 == harvest.system,]

    for (j in 1:nrow(harvest.cost)) {
      system_HPA_col <- which(names(system) == harvest.cost$HPA_col[j])
      if(length(system_HPA_col) != 1) {
        stop("Harvest per acre column name not found or duplicated")
      }

      #machine chost per acre calculation
      harvest.cost$full_cost[j] <- parse(text = harvest.cost$full_cost[j])
      system[,ncol(system) + 1] <- eval(harvest.cost$full_cost[j])
      cph.col.name <- paste0(harvest.cost$harvest.system2[j], ".", harvest.cost$machine2[j], ".", harvest.cost$machine.cost2[j],"_CPH")
      names(system)[ncol(system)] <- cph.col.name 
      
      harvest.cost$full_cost[j] <- parse(text = harvest.cost$full_cost[j])
      system[,ncol(system)+1] <- system[,system_HPA_col] * eval(harvest.cost$full_cost[j])
      names(system)[ncol(system)] <- harvest.cost$CPA_col[j]
      
      system[,ncol(system) + 1] <- parse(text = paste0("with(system,",harvest.cost$Move.In.Cost.Multiplier[j], ")"))
      system[,ncol(system) + 1] <- eval(system[,ncol(system)])
      system[,ncol(system) - 1] <- NULL
      mic.col.name <- paste0(harvest.cost$harvest.system2[j], ".", harvest.cost$machine2[j], ".", harvest.cost$machine.cost2[j], "_MIC.Multiplier")
      names(system)[ncol(system)] <- as.character(mic.col.name)
      
      
      system[,ncol(system) + 1] <- system[,c(which(grepl(mic.col.name, names(system))))] * system[,c(which(grepl(cph.col.name, names(system))))]
      names(system)[ncol(system)] <- paste0(harvest.cost$harvest.system2[j], ".", harvest.cost$machine2[j], ".", harvest.cost$machine.cost2[j], "_Move.In.Cost")
    }
    
    #add in and calculate lowboy cost
    #system <- merge(system, lowboy.data, all.x = TRUE)
    
    move.in.cost.lb <- unique(harvest.cost$Move.In.Cost.LB)
    
    if (length(move.in.cost.lb) > 1) {
      stop("Error: multiple lowboy move in costs set for a harvest system")
    }

    system[,ncol(system) + 1] <- as.character(move.in.cost.lb)
    system[,ncol(system) + 1] <- parse(text = paste0("with(system,", system[,ncol(system)],")"))
    system[,ncol(system) - 1] <- NULL
    system[,ncol(system) + 1] <- eval(system[,ncol(system)])
    system[,ncol(system) - 1] <- NULL
    
    names(system)[ncol(system)] <- paste0(harvest.system, "_MIC.Lowboy")
    
    #calculate total machine CPA
    chip.col <- which(grepl("Chipper.Chipper_CPA",names(system)))
    cpa.cols <- which(grepl("_CPA",names(system)))
    cpa.cols.wo.chip <- cpa.cols[!cpa.cols %in% chip.col]
  
    system[,ncol(system) + 1] <- rowSums(system[,c(cpa.cols.wo.chip)], na.rm = TRUE)
    names(system)[ncol(system)] <- paste0(harvest.system,".Total.NoChip.Machine_CPA")
    
    #calculate total non-lowboy move in cost
    system[,ncol(system) + 1] <- rowSums(system[,c(which(grepl("_Move.In.Cost",names(system))))], na.rm = TRUE)
    names(system)[ncol(system)] <- paste0(harvest.system,".Total.Move.In.Cost")
      
    pattern <- c(".Total.NoChip.Machine_CPA", ".Total.Move.In.Cost", "_MIC.Lowboy", "Chipper.Chipper_CPA")
    system[,ncol(system) + 1] <- rowSums(system[,c(which(grepl(paste0(pattern, collapse = "|"),names(system))))], na.rm = TRUE)
    names(system)[ncol(system)] <- paste0(harvest.system,".Total_CPA")
    
    pattern <- c(".Total.NoChip.Machine_CPA", ".Total.Move.In.Cost", "_MIC.Lowboy", ".Total_CPA", "Chipper.Chipper_CPA")
    system.cpa <- system[,c(1,which(grepl(paste0(pattern, collapse = "|"),names(system))))]
    
    if (i == 1) {
      system.cpa1 <- system.cpa
    } else {
      system.cpa1 <- merge(system.cpa1, system.cpa, all.y = TRUE)
    }
    
    
    mylist[[i]] <- system
  }
  
  mylist[[length(names.vector)+1]] <- system.cpa1
  
  names(mylist) <- c(names.vector, "All.Systems_CPA")
  
  return(mylist)
  
}
  
all_cost <- estimate_cost(harvest_system = "ALL", data = m)

#tethered_all_cost <- all_cost[[1]]
#tethered_all_cost_sum <- all_cost[[2]]

#####CALCULATE HARVEST COSTS######
#calculate_costs_for_input runs estimate_cost() for the harvest system specified in the input dataset (via
#the Harvesting.System column) and outputs the total machine cost per acre, the total move in cost per acre, 
#the lowboy cost per acre, and the total cost per acre (i.e. the sum of the other values). The optimal parameter
#allows you to specify whether the data has been run through the optimal_harvesting.system() function yet; if 
#so, setting optimal to TRUE will calculate the cost for the optimal harvest system as well. 

#Arguments:
#data - Input data to be used. 
#optimal - TRUE/FALSE - specify whether the dataset has an Optimal.Harvest.System column or not (i.e. the dataset has been run through optimal_harvesting.system)

#Example: calculate_costs_for_input(data = m, optimal = FALSE)

calculate_costs_for_input <- function(data, optimal) {
  unique.harvesting.systems <- unique(data$Harvesting.System)
  unique.harvesting.systems <- unique.harvesting.systems[!is.na(unique.harvesting.systems)]
  
  mylist <- vector(mode="list", length=length(unique.harvesting.systems))
  
  for(i in 1:length(unique.harvesting.systems)) {
    system <- data[data$Harvesting.System == unique.harvesting.systems[i],]
    system2 <- suppressWarnings(estimate_cost(unique.harvesting.systems[i], system))
    system2 <- system2[[2]] 
    
    pattern <- c("Total.NoChip.Machine_CPA", "Total.Move.In.Cost", "MIC.Lowboy", "Total_CPA", "Chipper.Chipper_CPA")
    
    names(system2)[which(grepl(pattern[1],names(system2)))] <- pattern[1]
    names(system2)[which(grepl(pattern[2],names(system2)))] <- pattern[2]
    names(system2)[which(grepl(pattern[3],names(system2)))] <- pattern[3]
    names(system2)[which(grepl(pattern[4],names(system2)))] <- pattern[4]
    names(system2)[which(grepl(pattern[5],names(system2)))] <- pattern[5]
    
    system <-  merge(system, system2, all.x = TRUE)

    mylist[[i]] <- system
  }
  
  
  data2 <- Reduce(rbind, mylist)
  
  if(optimal == TRUE) {
    optimal.unique.harvesting.systems <- unique(as.character(data$Optimal.Harvest.System))
    optimal.unique.harvesting.systems <- optimal.unique.harvesting.systems[!is.na(optimal.unique.harvesting.systems)]
    
    optimal.list <- vector(mode="list", length=length(optimal.unique.harvesting.systems))
    
    for(j in 1:length(optimal.unique.harvesting.systems)) {
      system <- data[data$Optimal.Harvest.System == optimal.unique.harvesting.systems[j],]
      system <- system[!is.na(system$Optimal.Harvest.System),]
      system$Harvesting.System <- NULL
      names(system)[which(grepl("Optimal.Harvest.System", names(system)))] <- "Harvesting.System"
      system2 <- suppressWarnings(estimate_cost(optimal.unique.harvesting.systems[j], system))
      system2 <- system2[[2]] 
      
      pattern <- c("Total.NoChip.Machine_CPA", "Total.Move.In.Cost", "MIC.Lowboy", "Total_CPA", "Chipper.Chipper_CPA")
      
      names(system2)[which(grepl(pattern[1],names(system2)))] <- pattern[1]
      names(system2)[which(grepl(pattern[2],names(system2)))] <- pattern[2]
      names(system2)[which(grepl(pattern[3],names(system2)))] <- pattern[3]
      names(system2)[which(grepl(pattern[4],names(system2)))] <- pattern[4]
      names(system2)[which(grepl(pattern[5],names(system2)))] <- pattern[5]
      
      
      names(system2)[2:6] <- paste0("Optimal.", names(system2)[2:6])
      
      system <-  merge(system, system2, all.x = TRUE)
      
      optimal.list[[j]] <- system
    }
    
    optimal.data2 <- Reduce(rbind, optimal.list)
    optimal.data2$Harvesting.System <- NULL
    
    data2 <- merge(data2, optimal.data2, all.x = TRUE)
    
  }
  
  return(data2)
}

input_with_cost <- calculate_costs_for_input(m, optimal = FALSE)

#####DETERMINE OPTIMAL HARVEST SYSTEM######
#optimal_harvesting.system takes the output from estimate_cost() with "ALL" as the harvest_system
#and determines the system that yields the minimum total harvesting cost per acre based on limiting parameters 
#set in opcost_ideal_ref

#Arguments:
#data - Input data to be used. 
#all - This should be the output from the estimate_cost() function with harvest_system set to all for the Input data. This can be left blank and it 
#      will run within the function, but it will take significantly longer (if you have already run estimate_cost() you can save processing time by 
#      adding it here)

#Example: optimal_cost(data = m, all = all_cost)

optimal_harvesting.system <- function(data, all) {
  if(missing(all)) {
    all <- estimate_cost("ALL", data)
  }
  ideal_ref <- opcost_ideal_ref
  
  og_all <- all
  og_data <- data
  
  all.harvest.systems <- names(all)
  
  pattern <- c(" ", "-", "/") #use data frame names as the harvest system names vector
  ideal_ref$harvest.system2 <- gsub(paste0(pattern, collapse = "|"),".", ideal_ref$Harvesting.System)
  ideal_ref <- ideal_ref[ideal_ref$harvest.system2 %in% all.harvest.systems,]
  
  all <- all[[length(all)]]
  all <- all[,c(1,which(grepl(".Total_CPA", names(all))))]

  unique.limit.parameters <- unique(ideal_ref$Limiting.Parameter) #get unique limiting paramters from ideal_ref
  
  merge.data <- data[c(1,which(as.character(names(og_data)) %in% as.character(unique.limit.parameters)))] #get Stand and limiting parameter data from the original dataset for merging later
  
  data <- merge(all, merge.data) #merge cost_data with merge data to get cost data plus limiting parameter column
  
  #Combine limiting parameter, limit type, and limiting parameter value to get limit.statement
  ideal_ref$Limit.Statement <- paste0(ideal_ref$Limiting.Parameter, ideal_ref$Limit.Type, ideal_ref$Limiting.Parameter.Value)
  
  unique.ID <- unique(ideal_ref$ID) #get unique ID values from ideal_ref
  #unique.limit.statement <- unique(ideal_ref$Limit.Statement) #get unique limit statements from ideal_ref
  
  for(i in 1:length(unique.ID)) { #Loop for each unique ID in ideal_ref
    mylist.ID <- vector(mode="list", length=length(unique.ID)) #create empty list to store data that is the same length as the number of unique IDs
    
    unique.limit.statement <- unique(ideal_ref$Limit.Statement[ideal_ref$ID == unique.ID[i]]) #get unique limit statements for the current ID iteration
    
    mylist.j <- vector(mode="list", length=length(unique.limit.statement))
    
    for(j in 1:length(unique.limit.statement)) { #Loop for each unique limit statement value in the current ID iteration
      limit.statement <- paste0("with(data,",unique.limit.statement[j],")") #create limit.statement equation from current limit statement iteration
      
      data.limited<- data[eval(parse(text = limit.statement)),] #limit data based on current limit statement iteration
      
      if(nrow(data[eval(parse(text = limit.statement)),]) > 0){
        harvesting.systems <- ideal_ref$harvest.system2[ideal_ref$Limit.Statement == unique.limit.statement[j] & ideal_ref$ID == unique.ID[i]] #get list of harvesting systems for current limit statement and ID iteration
        cols <- which(grepl(paste(harvesting.systems,collapse="|"), names(data.limited))) #get columns from data.limited where the column name contains current harvesting system
        if (length(cols) > 1) {
          data.limited$j <- apply(data.limited[,c(cols)],1,which.min) #get columns number that contains minimum value for columns where column name contains current harvesting system
          data.limited$j <- suppressWarnings(as.numeric(as.character(data.limited$j))) #convert column number to numeric
          data.limited$Optimal.Harvest.System <- gsub(".Total_CPA","",names(data.limited)[cols[data.limited$j]]) #convert column number to column name and remove ".Total_CPA" (so it returns harvest system name)
          data.limited$Optimal.Harvest.System <-  ideal_ref$Harvesting.System[match(data.limited$Optimal.Harvest.System, ideal_ref$harvest.system2)]
        } else {
          data.limited$Optimal.Harvest.System <-  gsub(".Total_CPA", "", names(data.limited)[cols])
          data.limited$Optimal.Harvest.System <-  ideal_ref$Harvesting.System[match(data.limited$Optimal.Harvest.System, ideal_ref$harvest.system2)]
        }
      } else {
        data.limited <- NA
      }
      #The Optimal.Harvest.System column now contains the harvest system name of the column with the minimum cost value according to ideal_ref stipulations/limitations
      mylist.j[[j]] <- data.limited #store for this iteration 
      
      
    }
    
    data1 <- Reduce(rbind, mylist.j) #merge results for each iteration of unique.limit.statement and ID back into single data frame
    data1 <- data1[complete.cases(data1),]
    data1$j <- NULL
    
    if (nrow(data1) < nrow(og_data)) {
      stop("Limit statements do not incorporate entire dataset")
    }
    
    pattern <- c("Stand", "Optimal.Harvest.System")
    data2 <- data1[,c(which(grepl(paste0(pattern, collapse = "|"), names(data1))))]
    data2 <- merge(og_data, data2)
    
    # data2$Harvesting.System2 <- data2$Harvesting.System
    # data2$Harvesting.System <- NULL
    # data2 <-  merge(data2, m_old_HS)
    data2$MatchesOriginalSystem <- data2$Harvesting.System == data2$Optimal.Harvest.System
    
    mylist.ID[[i]] <- data2 #store as data frame for current ideal_ref unique ID iteration
  }
  return(mylist.ID)
}

optimal <- optimal_harvesting.system(m, all_cost)

optimal1 <- optimal[[1]]

optimal_cost <- calculate_costs_for_input(optimal1, optimal = TRUE)

optimal_cost$Harvesting.System <- optimal_cost$Harvesting.System2
optimal_cost$Harvesting.System <- NULL
optimal_cost <- merge(optimal_cost, m_old_HS, by = "Stand")
optimal_cost$MatchesOriginalSystem <- optimal_cost$Harvesting.System == optimal_cost$Optimal.Harvest.System


opcost_output <- data.frame("stand" = optimal_cost$Stand, 
                            "rx_year" = optimal_cost$YearCostCalc, 
                            "harvest_cpa" = optimal_cost$Total_CPA, 
                            "chip_cpa" = optimal_cost$Chipper.Chipper_CPA, 
                            "assumed_movein_cpa" = optimal_cost$Total.Move.In.Cost, 
                            "harvest_system" = optimal_cost$Harvesting.System, 
                            "RxPackage_Rx_RxCycle" = optimal_cost$RxPackage_Rx_RxCycle, 
                            "biosum_cond_id" = optimal_cost$biosum_cond_id, 
                            "RxPackage" = optimal_cost$RxPackage, 
                            "Rx" = optimal_cost$Rx, 
                            "RxCycle" = optimal_cost$RxCycle)

opcost_ideal_output <- data.frame("stand" = optimal_cost$Stand, 
                            "rx_year" = optimal_cost$YearCostCalc, 
                            "harvest_cpa" = optimal_cost$Optimal.Total_CPA,
                            "MatchesOriginalSystem" = optimal_cost$MatchesOriginalSystem,
                            "ideal_assumed_movein_cpa" = optimal_cost$Optimal.Total.Move.In.Cost, 
                            "ideal_chip_cpa" = optimal_cost$Optimal.Chipper.Chipper_CPA,
                            "harvest_system" = optimal_cost$Optimal.Harvest.System, 
                            "RxPackage_Rx_RxCycle" = optimal_cost$RxPackage_Rx_RxCycle, 
                            "biosum_cond_id" = optimal_cost$biosum_cond_id, 
                            "RxPackage" = optimal_cost$RxPackage, 
                            "Rx" = optimal_cost$Rx, 
                            "RxCycle" = optimal_cost$RxCycle)

con<-odbcConnectAccess2007(args)
sqlSave(con, opcost_output, tablename="OpCost_Output", safer=FALSE)
sqlSave(con, opcost_ideal_output, tablename="OpCost_Ideal_Output", safer=FALSE)


odbcCloseAll()


# ###CREATE ANALYSIS GRAPHICS###
# packages <- c("reshape2", "ggplot2", "dplyr")
# 
# package.check <- lapply(packages, FUN = function(x) {
#   if (!require(x, character.only = TRUE)) {
#     install.packages(x, repos="http://cran.r-project.org", dependencies = TRUE)
#     library(x, character.only = TRUE)
#   }
# })


##MAKE SURE YOUR WORKING DIRECTORY IS SET TO WHERE YOU WANT THE GRAPHICS TO SAVE###
#The code below will save it to your project directory in a new folder called "opcost_graphics"
# project.directory <- "E:/cec_20180529/cec_20180529" #change to your project directory
# setwd(project.directory)
# dir.create("opcost_graphics", showWarnings = FALSE)
# setwd(file.path(project.directory, "opcost_graphics"))
# 
# graph_analyses_machine <- function(data) {
#   ref <- opcost_equation_ref
#   ###SUBSET TO ONLY INCLUDE SPECIFIC EQUATIONS/MACHINES###
#   #This is a good spot to subset out certain equations
#   #for example, to remove specific equations, you would change ref to:
#   #ref <- opcost_equation_ref[!opcost_equation_ref$Equation.ID %in% c("FB_06", "FB_04", "FB_01"),]
#   #you can remove additional equations by adding to the list after the c().
#   #You can use the same method to remove machines:
#   #ref <- opcost_equation_ref[!opcost_equation_ref$Machine %in% c("Feller Buncher"),]
#   #This will help avoid corrupting any of the tables as ref is not stored
#   #in the global environment when the function is run
#   unique.machines <- unique(ref$Machine)
#   old.dir <- getwd()
#   dir.create(file.path(getwd(), paste(format(Sys.Date(), "%Y%m%d"), "machine_analysis", sep = "_")), showWarnings = FALSE)
#   setwd(file.path(old.dir, paste(format(Sys.Date(), "%Y%m%d"), "machine_analysis", sep = "_")))
# 
#   for (i in 1:length(unique.machines)) {
#     values <- ref[as.character(ref$Machine) == as.character(unique.machines[i]),]
#     values <- values[values$Equation != "",]
#     mylist <- vector(mode="list", length=nrow(values))
#     name.vector <- as.character()
#     for (j in 1:nrow(values)) {
#       values.data <- calculate_hpa1 <- calculate_hpa(data = data, equation.ID = values$Equation.ID[j])
#       mylist[[j]] <- values.data
#       name.vector[j] <- paste0(values$Equation.ID[j])
#     }
# 
#     values.data <- Reduce(merge, mylist)
#     names(values.data)[(ncol(values.data) + 1 - length(name.vector)):ncol(values.data)] <- name.vector
#     b <- ncol(values.data)
#     a <- ncol(data) + 1
#     df2 <- melt(values.data, id.vars = c(a:b), measure.vars = names(values.data)[a:b])
#     n <- ncol(df2)
#     df2 <- df2[complete.cases(df2[n]),]
#     df3 <- df2[,c(n-1, n)]
#     df4 <- df3 %>% group_by(variable) %>% tally()
#     df2 <- merge(df4, df3, by = "variable")
#     df2$variable <- gsub(unique.machines[i],"",df2$variable)
#     df4$variable <- gsub(unique.machines[i],"",df4$variable)
#     graph <- ggplot(df2, aes(variable, value)) + geom_boxplot() + labs(x=unique.machines[i], y="Hours Per Acre") +
#       scale_x_discrete(labels = paste(df4$variable, df4$n, sep = "\n"))
#     ggsave(filename = paste0(unique.machines[i], ".png"),graph, device = "png", width = ifelse(nrow(df4)*1.3 > 6, nrow(df4)*1.3, 6),)
#   }
# 
#   setwd(old.dir)
# }
# 
# graph_analyses_machine(m)
# 
# graph_analyses_harvest_system <- function(data) {
#   ref <- opcost_harvestsystem_ref
#   unique.harvest.system <- unique(ref$Harvesting.System)
#   old.dir <- getwd()
#   dir.create(file.path(getwd(), paste(format(Sys.Date(), "%Y%m%d"), "harvest_system_analysis", sep = "_")), showWarnings = FALSE)
#   setwd(file.path(old.dir, paste(format(Sys.Date(), "%Y%m%d"), "harvest_system_analysis", sep = "_")))
# 
#   for (i in 1:length(unique.harvest.system)) {
#     pattern <- c(" ", "-", "/") #use data frame names as the harvest system names vector
#     filename1 <- gsub(paste0(pattern, collapse = "|"),".", unique.harvest.system[i])
#     values.data <- compute_harvest_system_equations(data = data, harvest_system = unique.harvest.system[i], meansonly = FALSE)
#     # pattern <- c("TETH_01", "TETH_02", "TETH_03")
#     # values.data$TETH <- rowSums(values.data[,c(which(grepl(paste0(pattern, collapse = "|"), names(values.data))))], na.rm = TRUE)
#     # values.data <- values.data[,-c(which(grepl(paste0(pattern, collapse = "|"), names(values.data))))]
#     values.data <- values.data[,-c(which(grepl("mean", names(values.data))))]
#     b <- ncol(values.data)
#     a <- 2
#     df2 <- melt(values.data, id.vars = c(1), measure.vars = names(values.data)[a:b])
#     n <- ncol(df2)
#     df2 <- df2[complete.cases(df2[n]),]
#     df3 <- df2[,c(n-1, n)]
#     df4 <- df3 %>% group_by(variable) %>% tally()
#     df2 <- merge(df4, df3, by = "variable")
#     # df2$variable <- gsub(unique.machines[i],"",df2$variable)
#     # df4$variable <- gsub(unique.machines[i],"",df4$variable)
#     # ylim1 <- boxplot.stats(df2$value)$stats[c(1, 5)]
#     graph <- ggplot(df2, aes(variable, value)) + geom_boxplot() + labs(x=unique.harvest.system[i], y="Hours Per Acre") +
#       scale_x_discrete(labels = paste(df4$variable, df4$n, sep = "\n"))
#     # coord_cartesian(ylim = ylim1*3)
#     assign("graph", graph, envir = .GlobalEnv)
#     ggsave(filename = paste0(filename1, ".png"),graph, device = "png", width = ifelse(nrow(df4)*1.1 > 6, nrow(df4)*1.1, 6),)
#   }
#   setwd(old.dir)
# }
# 
# graph_analyses_harvest_system(m)
