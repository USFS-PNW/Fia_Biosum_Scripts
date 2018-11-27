#####Initial package loading
#####Automatically install if package missing
packages = ("RODBC")

package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, repos="http://cran.r-project.org", dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})


# ####LOAD DATA FROM BIOSUM####

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

opcost_units <- sqlFetch(con2, "opcost_units", as.is = TRUE)

opcost_cost_ref <- sqlFetch(con2, "opcost_cost_ref", as.is = TRUE)

opcost_harvestequation_ref <- sqlFetch(con2, "opcost_harvestequation_ref", as.is = TRUE)

opcost_harvestsystem_ref <- sqlFetch(con2, "opcost_harvestsystem_ref", as.is = TRUE)

opcost_ideal_ref <- sqlFetch(con2, "opcost_ideal_ref", as.is = TRUE)

odbcCloseAll()


# ####MANUALLY RUN OPCOST ON A SINGLE OPCOST INPUT FILE####
# opcost.ref.location <- "C:/Users/sloreno/Opcost/opcost_ref.accdb" #set the location of the opcost_ref.accdb you'd like to use
# opcost.input.location <- "C:/Users/sloreno/Opcost/OPCOST_10_1_Input_BM_P029_210_210_210_210_2018-10-25_11_35_54_AM.accdb"
# 
# #Opcost_Input
# conn <- odbcConnectAccess2007(opcost.input.location)
# m<-data.frame(sqlFetch(conn, "opcost_input", as.is=TRUE))
# 
# odbcCloseAll()
# 
# #Opcost_Ref
# conn <- odbcConnectAccess2007(opcost.ref.location)
# 
# opcost_equation_ref<- sqlFetch(conn, "opcost_equation_ref", as.is = TRUE)
# 
# opcost_units <- sqlFetch(conn, "opcost_units", as.is = TRUE)
# 
# opcost_cost_ref <- sqlFetch(conn, "opcost_cost_ref", as.is = TRUE)
# 
# opcost_harvestequation_ref <- sqlFetch(conn, "opcost_harvestequation_ref", as.is = TRUE)
# 
# opcost_harvestsystem_ref <- sqlFetch(conn, "opcost_harvestsystem_ref", as.is = TRUE)
# 
# opcost_ideal_ref <- sqlFetch(conn, "opcost_ideal_ref", as.is = TRUE)
# 
# odbcCloseAll()

#####BRING IN REFERENCE TABLES -- THIS IS FOR IF YOU'RE NOT USING THE ACCESS DATABASE VERSION ABOVE AND NEED TO BRING IN THE CSVS######
# setwd("G:/Dropbox/Carlin/GitHub/Fia_Biosum_Scripts/OPCOST")
# opcost_equation_ref <- read.csv("opcost_equation_ref.csv")
# opcost_units <- read.csv("opcost_units.csv")
# opcost_cost_ref <- read.csv("opcost_cost_ref.csv")
# opcost_harvestsystem_ref <- read.csv("opcost_harvestsystem_ref.csv")
# opcost_ideal_ref <- read.csv("opcost_ideal_ref.csv")

#Set "NaN' to NA
m[m == "NaN"] <- NA #convert "NaN" values to NA


####Calculate additional variables

#total twitchVolM
m$twitchVol_ct <- ((m$Chip.trees.MerchAsPctOfTotal/100)*m$Chip.trees.average.volume.ft3.)*.0283168
m$twitchVol_sl <- ((m$Small.log.trees.MerchAsPctOfTotal/100)*m$Small.log.trees.average.volume.ft3.)*.0283168
m$twitchVol_ll <- ((m$Large.log.trees.MerchAsPctOfTotal/100)*m$Large.log.trees.average.vol.ft3.)*.0283168

#totalVol is the (Trees Per Acre * Average Tree Volume (ft3)))
#calculate for all size bins
#convert to metric
m$totalVol_ll <- (m$Large.log.trees.per.acre * m$Large.log.trees.average.vol.ft3.)* 0.0283168
m$totalVol_sl <- (m$Small.log.trees.per.acre * m$Small.log.trees.average.volume.ft3.)* 0.0283168
m$totalVol_ct <- (m$Chip.tree.per.acre * m$Chip.trees.average.volume.ft3.)* 0.0283168
m$totalVol <- (m$totalVol_ll + m$totalVol_sl + m$totalVol_ct)

#dbh calculated from twitchVol
#calculate for chip trees
m$dbh_ct <- sqrt(((m$Chip.trees.average.volume.ft3.) + 8.4166)/.2679)

#distBetween Trees is calculated by taking all size classes trees per acre (feet)
m$distBetweenTrees <- (sqrt((43560/(m$Small.log.trees.per.acre+ m$Large.log.trees.per.acre+ m$Chip.tree.per.acre))/pi))*2 
m$distBetweenTrees_ll <- (sqrt((43560/(m$Large.log.trees.per.acre))/pi))*2 
m$distBetweenTrees[m$Large.log.trees.per.acre == 0 & m$Small.log.trees.per.acre == 0 &  m$Chip.tree.per.acre == 0] <- NA
m$distBetweenTrees_ll[m$Large.log.trees.per.acre == 0] <- NA
                   
#totalWeight is average of non-zero large and small log density values (lbs/ft3) * totalVol (ft3/acre) to get (lbs/acre)
is.na(m$Large.log.trees.average.density.lbs.ft3.) <- m$Large.log.trees.average.density.lbs.ft3.==0
is.na(m$Small.log.trees.average.density.lbs.ft3.) <- m$Small.log.trees.average.density.lbs.ft3.==0

columns <- c("Large.log.trees.average.density.lbs.ft3.", "Small.log.trees.average.density.lbs.ft3.")
m$totalVol_smlg_ft <- (m$Large.log.trees.per.acre * m$Large.log.trees.average.vol.ft3.)+
                  (m$Small.log.trees.per.acre * m$Small.log.trees.average.volume.ft3.)
m$totalWeight<- rowMeans(m[columns], na.rm=TRUE) * m$totalVol_smlg_ft

m$totalVol_sm_ft <- (m$Small.log.trees.per.acre * m$Small.log.trees.average.volume.ft3.)*(m$Small.log.trees.MerchAsPctOfTotal/100)
m$totalVol_ll_ft <- (m$Large.log.trees.per.acre * m$Large.log.trees.average.vol.ft3.)*(m$Large.log.trees.MerchAsPctOfTotal/100)
m$totalVol_ct_ft <- (m$Chip.tree.per.acre * m$Chip.trees.average.volume.ft3.)
m$totalWeight <- (m$totalVol_sm_ft * m$Small.log.trees.average.density.lbs.ft3.)+
  (m$totalVol_sm_ft * m$Large.log.trees.average.density.lbs.ft3.)+(m$totalVol_ct_ft*m$CHIPS.Average.Density..lbs.ft3.)

m[is.na(m)] <- 0

#ChipFeedstockWeight is sum of trees per acre of small logs and large logs assigned to the chip bine times volume and density
m$ChipFeedstockWeight <- (m$Small.log.trees.per.acre * (m$Small.log.trees.ChipPct_Cat1_3/100) * m$Small.log.trees.average.volume.ft3. * m$Small.log.trees.average.density.lbs.ft3.) + 
  (m$Large.log.trees.per.acre * (m$Large.log.trees.ChipPct_Cat1_3_4/100) * m$Large.log.trees.average.vol.ft3. * m$Large.log.trees.average.density.lbs.ft3.)

###Tethered Harvester equations - see Petitmermet harvest cost model doc for reference

#Tethered total weight in lbm converted to tonnes
# m$TetheredTotalWeight <- (m$Small.log.trees.per.acre * m$Small.log.trees.average.volume.ft3. * m$Small.log.trees.average.density.lbs.ft3.)*0.0005
m$MerchWtLbs_SL <- m$Small.log.trees.per.acre * m$Small.log.trees.average.volume.ft3.*m$Small.log.trees.average.density.lbs.ft3. * (m$Small.log.trees.MerchAsPctOfTotal/100)
m$MerchWtLbs_CT <- m$Chip.tree.per.acre * m$Chip.trees.average.volume.ft3. * m$CHIPS.Average.Density..lbs.ft3. * (m$Chip.trees.MerchAsPctOfTotal/100)
m$MerchWtLbs_Harvester <- m$MerchWtLbs_CT + m$MerchWtLbs_SL
m$MerchWtTonnes_Harvester <- 0.000453592 * m$MerchWtLbs_Harvester

###Tethered Forwarder Equations - see Petitmermet harvest cost model doc for reference
m$MerchWtLbs_LL <- m$Large.log.trees.per.acre * m$Large.log.trees.average.vol.ft3. * m$Large.log.trees.average.density.lbs.ft3. * (m$Large.log.trees.MerchAsPctOfTotal/100)
m$MerchWtTonnes_Forwarder<- (m$MerchWtLbs_SL + m$MerchWtLbs_LL + m$MerchWtLbs_CT)* 0.000453592


m$TPA_CSL <- m$Small.log.trees.per.acre + m$Large.log.trees.per.acre + m$Chip.tree.per.acre

m$MerchWtLbs_Loader <- m$MerchWtLbs_LL + m$MerchWtLbs_SL
m$MerchWtTonnes_Loader <- m$MerchWtLbs_Loader * 0.000453592

m$FT_wt_CT <- with(m,ifelse(Chip.tree.per.acre > 0, exp(1.0613+0.8841*log(Chip.tree.per.acre)+0.6714*log(MerchWtLbs_CT * 0.000453592/Chip.tree.per.acre)),0))
m$FT_wt_SL <- with(m,ifelse(Small.log.trees.per.acre > 0, exp(1.0613+0.8841*log(Small.log.trees.per.acre)+0.6714*log(MerchWtLbs_CT * 0.000453592/Small.log.trees.per.acre)),0))
m$FT_wt_LL <- with(m, ifelse(Large.log.trees.per.acre > 0, exp(1.0613+0.8841*log(Large.log.trees.per.acre)+0.6714*log(MerchWtLbs_CT * 0.000453592/Large.log.trees.per.acre)),0))


print("variables calculated")
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
  
  equation_ref <- opcost_equation_ref #if a different ref is not specified, use opcost_equation_ref
  #units <- opcost_units #if units is not specified, use opcost_units
  
  row <- equation_ref[equation_ref$EquationID == equation.ID,] #get the row from equation_ref with the specified equation ID 
  
  #stop function if there are duplicates of equation.ID
  if(nrow(row) > 1){
    stop("Duplicate equation ID") #this stops function if the equation ID is not unique
  } 
  
  if(nrow(row) == 0) {
    stop("Reference not found") #this stops the function if the equation.ID is not found in ref
  }
  
  if(row$Units == "" || is.na(row$Units)) { 
    #If units is not populated, use the equation as is from opcost_equation_ref
    equation <- row$Equation
  } else {
    #convert units if Unit column is populated. This references opcost_units to add the unit conversion to the equation in opcost_equation_ref
    equation<- gsub("result", paste0("(",row$Equation,")"), row$HoursPerAcreConversion)
  }
  
  if(row$LimitStatement == "" || is.na(row$LimitStatement)) {  #for harvest methods with limits
    equation.statement <- parse(text = paste0("with(data,", equation, ")"))
    data[,ncol(data) + 1] <- eval(equation.statement) #calculate hours per acre value based on equation 
  } else {
    #Evaluate equation for data subset based on Limit.Statement
    limit.statement <- parse(text = paste0("with(data,", row$LimitStatement, ")"))
    equation.statement <- parse(text = paste0("with(data[",limit.statement, ",],", equation, ")"))
    data[eval(limit.statement),ncol(data) + 1] <- eval(equation.statement)
  }
  
  if(ncol(data) > og_data_columns) {
    names(data)[ncol(data)] <- paste0(row$EquationID) #add column and name it based on the name/anaylsis/ID with "premod"
  } else {
    stop("Equation not calculated")
  }
  
  return(data)
  
}

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
  harvestequation_ref <- opcost_harvestequation_ref
  
  data1 <- data #rename data
  
  harvest.system <- harvestequation_ref[harvestequation_ref$Method == harvest_system,]
  
  harvest.system$Equations <- paste(harvest.system$BC, harvest.system$Cut, harvest.system$Move, harvest.system$Chip, 
                                    harvest.system$Load, harvest.system$Extra, sep=",")
  
  harvest.system$Equations <- gsub("NA,","",harvest.system$Equations)#remove NA, values
  harvest.system$Equations <- gsub("NA","",harvest.system$Equations)#remove NA values
  
  #create a list of equations
  harvest.system2 <- strsplit(as.character(harvest.system$Equations), split = ",")
  harvest.system2 <- data.frame(Equation.ID = unique(trimws(unlist(harvest.system2))))#create dataframe containing equation IDs 
  
  #attach machine to cost Equation.ID
  harvest.system2 <- merge(equation_ref[, c("EquationID", "Machine")], harvest.system2, by.x="EquationID", by.y="Equation.ID") 
  #rename column to "Machine"
  names(harvest.system2)[names(harvest.system2) =='x'] <- "Machine"
  
  #create list of equations 
  harvest.equations <- harvest.system2$EquationID
  
  #grab the rows from equation_ref  that match the equations in harvest.equations
  ref <- equation_ref[equation_ref$EquationID %in% harvest.equations,] 
  #create list
  ref_list <- ref[,"EquationID"]
  
  #create new dataframe containing stand values of input data
  newdata <- data.frame(data1$Stand)
  #rename column to stand
  names(newdata) <- c("Stand")
  
  for (i in 1:length(ref_list)) { #use calculate_hpa to get hours per acre for each equation.ID
    output <- calculate_hpa(data = data1, equation.ID = ref_list[i])
    newdata <- merge(newdata, output[, c(1, ncol(output))], by = "Stand")
  }
  
  
  
  cleaned.newdata <- do.call(data.frame,lapply(newdata, function(x) replace(x, is.infinite(x),NA))) #replace Inf values with NA
  cleaned.newdata <- do.call(data.frame,lapply(cleaned.newdata, function(x) replace(x, x == 0,NA))) #replace zero values with NA
  cleaned.newdata <- do.call(data.frame,lapply(cleaned.newdata, function(x) replace(x, x == "NaN", NA))) #replace NaN values with NA
  
  data_premean <- cleaned.newdata
  
  data <- merge(data1, cleaned.newdata)
  
  data2 <- data
  
  machines <- unique(equation_ref$Machine) #get unique machine.cost values
  
  for (j in 1:length(machines)) {
    machine.eqs <- harvest.system2$EquationID[harvest.system2$Machine == machines[j] ]
    machine.cols <- which(grepl(paste(machine.eqs, collapse = "|"), colnames(data2)))
    if(length(machine.eqs) == 0) {
      next
    }
    if(length(machine.eqs) > 1) {
      data2[,ncol(data2)+1] <- rowMeans(data2[, machine.cols], na.rm=TRUE) #calculate mean of machine hours per acre values
    } else {
      data2[,ncol(data2)+1] <- data2[,machine.cols]
    }
    names(data2)[ncol(data2)] <- paste0(harvest_system,".mean", ".", machines[j], "_HPA")
  }
  
  means <- sum(grepl('_HPA', data2))
  data.means <- data2[,c(1, (ncol(data)-means+1): ncol(data2))]
  data.means[is.na(data.means)] <- NA
  
  data <- merge(data2, data.means, all.x = TRUE)[, union(names(data2), names(data.means))]
  data[is.na(data)] <- NA
  
  if(allCols == FALSE) { #if allCols = FALSE
    b <- ncol(data)-(ncol(cleaned.newdata)+ncol(data.means))
    data <- data[,c(1, (b):ncol(data))] #remove old data columns (keep Stand column)
  }
  
  if(meansonly == TRUE) {
    data <- data.means
  }
  return(data)
}

#####GET MEAN HARVEST HOURS PER ACRE FOR ALL MACHINES######
#all_harvesting_systems runs compute_harvest_system_equations for all analyses and compiles a table
#with mean values for all analyses. It returns a list with each list item as a data frame for a specific
#harvesting system. This function is used in the estimate_cost() function. 

#Arguments:
#data - The opcost input data

#Example: all_harvesting_systems(data = m)
# 
# all_harvesting_systems <- function(data) {
#   equation_ref <- opcost_equation_ref
#   harvestequation_ref <- opcost_harvestequation_ref
#   
#   #get unique harvest system values from harvestsystem_ref
#   harvest.system <- unique(harvestequation_ref$Method)
#   
#   #create empty list to store loop values
#   mylist <- vector(mode="list", length=length(harvest.system))
#   name.vector <- as.character() #create empty vector for list name values
#   for (i in 1:length(harvest.system)) {
#    name.vector <- c(name.vector, paste0(harvest.system[i]) ) #get name vector value for this loop iteration
#    mylist[[i]] <- list(compute_harvest_system_equations(data, harvest.system[i], allCols = FALSE, meansonly = TRUE)) #run and store compute_harvest_system_equations for each unique machine value
#   }
#   
#   names(mylist) <- name.vector #name each list item 
#   
#   #all <- Reduce(merge, mylist) #merge list items (i.e. put all machine compute_harvest_system_equations function results in a single data frame)
#   
#   return(mylist)
# }
# 
# all <- all_harvesting_systems(data = m)
# 
# GBMWT_all <- as.data.frame(all$"Ground-Based Mech WT") #convert list to data frame

#####ESTIMATE COST######
#The estimate_cost function takes reference tables opcost_cost_ref, opcost_harvestsystem_ref, 
#and estimates costs by harvest system for all aspects of the harvest. This function relies on the 
#all_harvesting_systems() and compute_harvest_system_equations() functions. The output is a data frame
#in a list. 

#Arguments:
#harvest_system - the name of the harvest system to estimate costs for. 
#data - variable name for Opcost_Input data 
#cost - Cost values the user wants to use to calculate costs. This corresponds to the name of each column in opcost_cost_ref. Defaults to "Default.CPH"

#Example: estimate_cost(harvest_system = "Cable Manual Log", data = m, cost = "DefaultCPH")
estimate_cost <- function(harvest_system, data, cost) {
  #add in default values for function parameters
  if(missing(cost)){
    cost <- "DefaultCPH"
  }
  
  #bring in reference tables
  cost_ref <- opcost_cost_ref
  harvestsystem_ref <- opcost_harvestsystem_ref
  
  vars <- c("Machine", cost, "MoveInCostMultiplier", "MoveInCostLowBoy")
  cost_ref2 <- cost_ref[vars]
  names(cost_ref2)[2] <- "Cost"
  
  costestimate_ref <- merge(cost_ref2, harvestsystem_ref, all.y = TRUE) #merge cost_ref values with harvest_system_ref
  
  if (any(is.na(costestimate_ref$DefaultCPH))) {
    warning("A Harvesting.System was removed; cost per hour value missing for a machine")
  }
  
  costestimate_ref <- costestimate_ref[!is.na(costestimate_ref$Cost),]#remove harvest systems where a cost per hour value is not given
  
  costestimate <- compute_harvest_system_equations(data, harvest_system, allCols = FALSE, meansonly = TRUE)
  costestimate[is.na(costestimate)] <- 0
  
  #pull in lowboy calculation parameters
  pattern <- c("Stand", "Move_In_Hours", "Harvest_area_assumed_acres", "Percent.Slope")
  lowboy.data <- m[,c(which(grepl(paste0(pattern, collapse = "|"),names(m))))]#changed data to m
  
  costestimate <- merge(costestimate, lowboy.data, by = "Stand")
  
  system.cpa1 <- data.frame()
  
  harvest.cost <- costestimate_ref[costestimate_ref$HarvestingSystem == harvest_system,]
  
  for (j in 1:nrow(harvest.cost)) {
    system_HPA_col <- which(colnames(costestimate) %in% paste0(harvest.cost$HarvestingSystem, ".mean.", harvest.cost$Machine[j], "_HPA"))
    if(length(system_HPA_col)!= 1) {
      stop("Harvest per acre column name duplicated or missing")
    }
    
    
    #calculate harvest cost _cpa for each machine
    Percent.Slope <- costestimate$Percent.Slope
    costestimate[,ncol(costestimate) + 1] <- costestimate[system_HPA_col] * eval(parse(text=paste0(harvest.cost$Cost[j])))
    names(costestimate)[ncol(costestimate)] <- paste0(harvest.cost$Machine[j], "_CPA")
    cph.col.name <- names(costestimate)[ncol(costestimate)] 
    
    #calculate set up move in costs
    Harvest_area_assumed_acres <- costestimate$Harvest_area_assumed_acres
    costestimate[,ncol(costestimate) + 1] <- eval(parse(text = paste0(harvest.cost$MoveInCostMultiplier[j])))
    mic.col.name <- paste0(harvest.cost$Machine[j], "_MIC.Multiplier")
    names(costestimate)[ncol(costestimate)] <- as.character(mic.col.name)
    
    costestimate[,ncol(costestimate) + 1] <- costestimate[,c(which(grepl(mic.col.name, names(costestimate))))] * costestimate[,c(which(grepl(cph.col.name, names(costestimate))))]
    names(costestimate)[ncol(costestimate)] <- paste0(harvest.cost$Machine[j], "_Set.Up.Cost")
    
    #calculate lowboy move in costs
    Move_In_Hours <- costestimate$Move_In_Hours
    costestimate[,ncol(costestimate) + 1] <- eval(parse(text = paste0(harvest.cost$MoveInCostLowBoy[j])))
    names(costestimate)[ncol(costestimate)] <- paste0(harvest.cost$Machine[j], "_MIC.Lowboy")
  }
  
  #calculate total machine CPA
  chip.col <- which(grepl("Chipper_CPA",names(costestimate)))
  cpa.cols <- which(grepl("_CPA",names(costestimate)))
  cpa.cols.wo.chip <- cpa.cols[!cpa.cols %in% chip.col]
  
  costestimate[,ncol(costestimate) + 1] <- rowSums(costestimate[,c(cpa.cols.wo.chip)], na.rm = TRUE)
  names(costestimate)[ncol(costestimate)] <- paste0("Total.NoChip.Machine_CPA")
  
  #calculate set up move in cost
  costestimate[,ncol(costestimate) + 1] <- rowSums(costestimate[,c(which(grepl("_Set.Up.Cost",names(costestimate))))], na.rm = TRUE)
  names(costestimate)[ncol(costestimate)] <- paste0("Total.Non.LB.Cost")
  #calculate total lowboy cost
  costestimate[,ncol(costestimate) + 1] <- rowSums(costestimate[,c(which(grepl("_MIC.Lowboy",names(costestimate))))], na.rm = TRUE)
  names(costestimate)[ncol(costestimate)] <- paste0("Total.LB.Cost")
  #calculate total move in cost
  costestimate[,ncol(costestimate) + 1] <- rowSums(costestimate[,c("Total.Non.LB.Cost", "Total.LB.Cost")], na.rm = TRUE)
  names(costestimate)[ncol(costestimate)] <- paste0("Total.Move.In.Cost")
  
  pattern <- c("Total.NoChip.Machine_CPA", "Total.Move.In.Cost", "Chipper_CPA")
  costestimate[,ncol(costestimate) + 1] <- rowSums(costestimate[,c(which(grepl(paste0(pattern, collapse = "|"),names(costestimate))))], na.rm = TRUE)
  names(costestimate)[ncol(costestimate)] <- paste0("Total_CPA")
  
  pattern <- c("Total.NoChip.Machine_CPA", "Total.Move.In.Cost", "Total_CPA", "Chipper_CPA")
  system.cpa <- costestimate[,c(1,which(grepl(paste0(pattern, collapse = "|"),names(costestimate))))]

  return(system.cpa)
  
}



#####CALCULATE HARVEST COSTS######
#calculate_costs_for_input runs estimate_cost() for the harvest system specified in the input dataset (via
#the Harvesting.System column) and outputs the total machine cost per acre, the total move in cost per acre, 
#the lowboy cost per acre, and the total cost per acre (i.e. the sum of the other values). The optimal parameter
#allows you to specify whether the data has been run through the optimal_harvesting.system() function yet; if 
#so, setting optimal to TRUE will calculate the cost for the optimal harvest system as well. 

#Arguments:
#data - Input data to be used. 
#optimal - TRUE/FALSE - specify whether the dataset has an Optimal.Harvest.System column or not (i.e. the dataset has been run through optimal_harvesting.system)

#Example: calculate_costs_for_input(data = m)

calculate_costs_for_input <- function(data) {
  unique.harvesting.systems <- unique(data$Harvesting.System)
  unique.harvesting.systems <- unique.harvesting.systems[!is.na(unique.harvesting.systems)]
  
  mylist <- vector(mode="list", length=length(unique.harvesting.systems))
  
  for(i in 1:length(unique.harvesting.systems)) {
    system <- data[data$Harvesting.System == unique.harvesting.systems[i],]
    system2 <- suppressWarnings(estimate_cost(unique.harvesting.systems[i], system))
    mylist[[i]] <- system2
  }
  
  data2 <- Reduce(rbind, mylist)
  data2 <- merge(data2, data[, c("Stand", "Harvesting.System")], by="Stand")
  
  return(data2)
}

output <- calculate_costs_for_input(m)


opcost_output <- data.frame("stand" = output$Stand, 
                            "harvest_cpa" = output$Total_CPA, 
                            "chip_cpa" = output$Chipper_CPA, 
                            "assumed_movein_cpa" = output$Total.Move.In.Cost,
                            "harvest_system" = output$Harvesting.System, 
                            "RxPackage_Rx_RxCycle" = substr(output$Stand, 26, 32), 
                            "biosum_cond_id" = substr(output$Stand, 1, 25),
                            "RxPackage" = substr(output$Stand, 26, 28), 
                            "Rx" = substr(output$Stand, 29, 31), 
                            "RxCycle" = substr(output$Stand, 32, 32)
)
######Use if running opcost through Biosum
con<-odbcConnectAccess2007(args)
sqlSave(con, opcost_output, tablename="OpCost_Output", safer=FALSE)

odbcCloseAll()

# 
# ######Comment out lines 499-504, and uncomment 507-514 if running Opcost outside of BioSum
# ###set the output location database
# opcost.output.location <- "C:/Users/sloreno/Opcost/OPCOST_10_1_Input_BM_P029_210_210_210_210_2018-10-25_11_35_54_AM.accdb"
# 
# #Opcost_Input
# conn <- odbcConnectAccess2007(opcost.output.location)
# sqlSave(conn, opcost_output, tablename="OpCost_Output", safer=FALSE)
# 
# odbcCloseAll()


# ##########################################
# ###CREATE ANALYSIS GRAPHICS###
# packages <- c("reshape2", "ggplot2", "dplyr", "data.table", "plyr")
# 
# package.check <- lapply(packages, FUN = function(x) {
#   if (!require(x, character.only = TRUE)) {
#     install.packages(x, repos="http://cran.r-project.org", dependencies = TRUE)
#     library(x, character.only = TRUE)
#   }
# })
# 
# 
# #MAKE SURE YOUR WORKING DIRECTORY IS SET TO WHERE YOU WANT THE GRAPHICS TO SAVE###
# #The code below will save it to your project directory in a new folder called "opcost_graphics"
# #MAKE SURE YOUR WORKING DIRECTORY IS SET TO WHERE YOU WANT THE GRAPHICS TO SAVE###
# #The code below will save it to your project directory in a new folder called "opcost_graphics"
# project.directory <- "C:/Users/sloreno/Opcost/" #change to your project directory
# setwd(project.directory)
# dir.create("opcost_graphics", showWarnings = FALSE)
# setwd(file.path(project.directory, "opcost_graphics"))
# 
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
#   ref$Machine.size <- paste0(ref$Machine, "_", ref$Size)
#   unique.machines <- unique(ref$Machine.size)
#   old.dir <- getwd()
#   folder <- file.path(old.dir, paste(format(Sys.Date(), "%Y%m%d"), "machine_analysis", sep = "_"))
#   dir.create(folder, showWarnings = FALSE)
#   setwd(folder)
#   
#   
#   for (i in 1:length(unique.machines)) {
#     values <- ref[as.character(ref$Machine.size) == as.character(unique.machines[i]),]
#     values <- values[values$Equation != "",]
#     mylist <- vector(mode="list", length=nrow(values))
#     name.vector <- as.character()
#     for (j in 1:nrow(values)) {
#       values.data <- calculate_hpa1 <-calculate_hpa(data = data, equation.ID = values$EquationID[j])
#       mylist[[j]] <- values.data
#       name.vector[j] <- paste0(values$EquationID[j])
#     }
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
#     df5 <- merge(df2, values[, c("EquationID", "Machine.size")], by.x="variable", by.y="EquationID")#Turn your 'treatment' column into a character vector
#     graph <- ggplot(df5, aes(variable, value, fill =  variable)) + 
#       geom_boxplot() + 
#       labs(x=unique.machines[i], y="Hours Per Acre") +
#       facet_wrap(  ~ Machine.size)
#     ggsave(filename = paste0(unique.machines[i], ".png"),graph, device = "png", width = ifelse(nrow(df4)*1.3 > 6, nrow(df4)*1.3, 6))
#   }
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
