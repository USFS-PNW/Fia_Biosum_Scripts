#####Initial package loading
#####Automatically install if package missing
packages = ("RODBC")

package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, repos="http://cran.r-project.org", dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})


#####Run Data
#Get Opcost_Input table
conn <- odbcConnectAccess2007("H:/cec_20170915/OPCOST/Input/OPCOST_8_7_9_Input_CA_P001_100_100_100_100_2018-01-03_08_32_45_AM.ACCDB") #Change the text after "DBH=" to the correct directory for your project
m <- sqlFetch(conn, "OpCost_Input", as.is = TRUE)
odbcCloseAll()

#Convert to Data Frame and set "NaN' to NA
m <- data.frame(m)
m[m == "NaN"] <- NA #convert "NaN" values to NA

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


####TODO -  PUT BRUSH CUT EQUATIONS IN OPCOST_REF####
#mechBC where BrushCutAvgVol is < 4 is BrushCutTPA/(10*60). Where BrushCutAvgVol is > 4 it is BrushCutTPA/(5*60). Denominator is 
#trees per minute * 60 minutes to get trees/hour. Final units assumed (hours/acre)
m$mechBC[m$BrushCutAvgVol < 4] <- m$BrushCutTPA[m$BrushCutAvgVol < 4]/(10*60)
m$mechBC[m$BrushCutAvgVol > 4] <- m$BrushCutTPA[m$BrushCutAvgVol > 4]/(5*60)

#manBC where BrushCutAvgVol is < 4 is BrushCutTPA/60. Where BrushCutAvgVol is > 4 it is BrushCutTPA/(2*60). Denominator is 
#trees per minute * 60 minutes to get trees/hour. Final units assumed (hours/acre)
m$manBC[m$BrushCutAvgVol < 4] <- m$BrushCutTPA[m$BrushCutAvgVol < 4]/(1*60)
m$manBC[m$BrushCutAvgVol > 4] <- m$BrushCutTPA[m$BrushCutAvgVol > 4]/(2*60)

#####BRING IN REFERENCE TABLES######
setwd("G:/Dropbox/Carlin/Berkeley/biosum/OPCOST")
opcost_ref <- read.csv("opcost_ref.csv")
opcost_ref  <- opcost_ref [-45,] 
opcost_ref  <- opcost_ref [-65,]
opcost_units <- read.csv("opcost_units.csv")
opcost_modifiers <- read.csv("opcost_modifiers.csv")
opcost_cost_ref <- read.csv("opcost_cost_ref.csv")
opcost_costestimate_ref <- read.csv("opcost_costestimate_ref.csv")
opcost_moveincost_ref <- read.csv("opcost_moveincost_ref.csv")
opcost_ideal_ref <- read.csv("opcost_ideal_ref.csv")
opcost_chippingcost2 <- read.csv("opcost_chippingcost2.csv")

#####CALCULATE HOURS PER ACRE######
#calculate_hpa calculates harvest time in hours per acre from the imported Opcost_Input table. 
#It incorporates opcost_ref, opcost_modifiers, opcost_units to get a final hours per acre value 
#(as in it includes conversion from the original equation to hours per acre and 
#any other adjustments that get made as per opcost_modifiers). The function parameters are: 

#Arguments:
#data - The opcost input data
#author - The author of the desired equation to be used in opcost_ref (case sensitive)
#analysis - Analysis column value of desired author/analysis combination in opcost_ref (case sensitive)
#showmod - TRUE/FALSE - If true, show both the original calculated HPA and the values after being modified according to opcost_modifiers. Defaults to FALSE
#ref - Reference equations to use. Defaults to opcost_ref
#mod - Reference modifiers to use. Defaults to opcost_modifiers
#units - Reference unit conversion equations to use. Defaults to opcost_units

#Example: calculate_hpa(data = m, author = "behjou", analysis = "Manual", showmod = TRUE, ref = opcost_ref, mod = opcost_modifiers, units = opcost_units)
####TO DO: CREATE EQUATION INDEXING SYSTEM BASED ON UNIQUE ID TO AVOID ISSUES WITH DUPLICATE AUTHOR/ANALYSIS####
####TO DO: ANAYLSIS ON/OFF SWITCH####
####TO DO: MULTIPLE LIMIT PARAMETERS####
####TO DO: PRE-MOD COLUMN####
####TO DO: ANALYSIS/HARVEST METHODS/HARVEST COMPONENT TO MACHINE####
####TO DO: COST ESTIMATE - EACH HARVEST MACHINE SHOULD HAVE A SEPARATE MOVE IN COST####
####TO DO: COST ESTIMATE - MOVE IN COST NEEDS TO SCALE FOR EFFICIENCY (COST PER ACRE)####
####TO DO: CALCULATE HPA - CHECK FOR 0 DENOMINATORS####
####TO DO: CALCULATE HPA - CHECK THAT NA ROWS WITH BRUSH CUT MOD SHOW BRUSH CUT VALUES####
####TO DO: IDEAL_REF - DIFFERENT SLOPES####


calculate_hpa <- function(data, author, analysis, showmod, ref, mod, units) {
  og_data_rows <- nrow(data) #get original number of rows in data
  og_data_columns <- ncol(data) #get original number of columns in data
  
  if (missing(showmod)) {
    showmod <- FALSE #if showmod is not specified, it is set to false
  }
  
  if (missing(ref)) {
    ref <- opcost_ref[opcost_ref$Name != "" & opcost_ref$Analysis != "",] #if a different ref is not specified, this limits ref to values with both a Name and Analysis value in opcost_ref
  }
  if (missing(units)) {
    units <- opcost_units #if units is not specified, use opcost_units
  }
  if (missing(mod)) {
    mod <- opcost_modifiers #if mod is not specified, use opcost_modifiers
  }
  
  ref$Analysis <- as.character(ref$Analysis) #convert to character
  
  row <- ref[ref$Name == author & ref$Analysis == analysis,] #get the row from opcost_ref with the specified author/analysis combination
  
  #stop function if there are duplicates of name/analysis combination
  if(nrow(row) > 1){
    stop("Duplicate opcost_ref name/analysis combination") #this stops function if there is a duplicate name/analysis combination in ref
  } 
  
  if(nrow(row) == 0) {
    stop("Reference not found") #this stops the function if the name/analysis combination is not found in ref
  }
  
  if(row$Units != "") { 
    #convert units if Unit column is populated. This references opcost_units to add the unit conversion to the equation in opcost_ref
    conversion <- units$TPA_Conversion[units$Unit == as.character(row$Units)]
    conversion2 <- gsub("result", paste0("(",row$Equation,")"), conversion)
    if(!complete.cases(row[,7:9])) { #if there are no limiting values
      equation <- parse(text = paste0("with(data,",conversion2,")"))
    } else { #if there are limiting values
      equation <- parse(text = paste0("with(data.treated,",conversion2,")"))
    }
  } else {
    #If units is not populated, use the equation as is from opcost_ref
    if(!complete.cases(row[,7:9])) { #if there are no limiting values
      equation <- parse(text=paste0("with(data,",row$Equation,")"))
    } else {
      equation <- parse(text=paste0("with(data.treated,",row$Equation,")"))
    }
  }
  
  if(complete.cases(row[,7:9])) {  #for harvest methods with limits
    
    limit.parameter <- row$Limiting.Parameter #get limiting parameter 
    
    column <- which(names(data) == row$Limiting.Parameter) #get column number of limiting parameter
    
    data.na <- data[is.na(data[,column]),] #put rows where limiting parameter = NA into data.na
    data.no.na <- data[!is.na(data[,column]),] #remove rows where limiting parameter = NA from data
    
    #Combine limiting parameter, limit type, and limiting parameter value to get limit.statement
    limit.statement <- parse(text = paste0("with(data.no.na,", row$Limiting.Parameter, row$Limit.Type, row$Limiting.Parameter.Value,")"))
    
    data.nt <- data.no.na[!eval(limit.statement),] #put rows where limit.statement is false into data.nt
    data.treated <- data.no.na[eval(limit.statement),] #remove rows where limit.statement is false from data
    
    #recombine the data
    if(nrow(data.treated) > 0) {
      data.treated[,ncol(data.treated) + 1] <- eval(equation)
      if (nrow(data.nt) > 0) {
        data.nt[,ncol(data.nt) + 1] <- NA
        if (nrow(data.na) > 0) {
          data.na[,ncol(data.na) + 1] <- NA
          data <- rbind(data.treated, data.nt, data.na)
        } else {
          data <- rbind(data.treated, data.nt)
        }
      } else if (nrow(data.na) > 0) {
        data.na[,ncol(data.na) + 1] <- NA
        data <- rbind(data.treated, data.na)
      } else {
        data <- data.treated
      }
    } else if (nrow(data.na) > 0) {
      data.na[,ncol(data.na) + 1] <- NA
      if(nrow(data.nt) > 0) {
        data.nt[,ncol(data.nt) + 1] <- NA
        data <- rbind(data.nt, data.na)
      } else {
        data <- data.na
      }
    } else {
      data.nt[,ncol(data.nt) + 1] <- NA
      data <- data.nt
    }
    
    #make sure you didn't lose any rows
    if(nrow(data) != og_data_rows) {
      stop("Limiting parameter not working properly") #stops the function if something above broke the data
    }
    
  } else {
    #for harvest methods without limits
    data[,ncol(data) + 1] <- eval(equation) #calculate hours per acre value based on equation 
  }
  
  if(ncol(data) > og_data_columns) {
    names(data)[ncol(data)] <- paste0(author,analysis) #add column and name it based on the name/anaylsis 
  } else {
    stop("Equation not calculated")
  }
  
  ##### add in modifiers
  analysis <- as.character(analysis)
  if (analysis %in% mod$Analysis) { #look to see if specified analysis is in the modifier table
    if (any(mod$Harvesting.System[mod$Analysis == analysis] != "All")) { #add adjustment for specific harvesting.systems 
      mod.rows <- mod[mod$Analysis == analysis & mod$Harvesting.System != "All",] #get applicable rows from mod
      
      mod.rows$Comment <- NULL
      mod.rows$Analysis <- NULL
      
      if (any(mod.rows$Harvesting.System %in% data$Harvesting.System)) { #Find any harvesting system values that also exist in the dataset
        mod.rows <- mod.rows[mod.rows$Harvesting.System %in% data$Harvesting.System,] 
        
        mod.rows$Syst.Modifier.Type <- mod.rows$Modifier.Type 
        
        data <- merge(data, mod.rows, by = "Harvesting.System", all = TRUE)
        data$Syst.Adjust <- paste0(data$Modifier) 
        
        data$Modifier <- NULL
        data$Modifier.Type <- NULL
      }
      
    }
    
    if (any(mod$Harvesting.System[mod$Analysis == analysis] == "All")) { #add adjustment for analysis modifier Harvesting.System = "All"
      mod.rows <- mod[mod$Analysis == analysis & mod$Harvesting.System == "All",]
      
      data$All.Modifier.Type <- mod.rows$Modifier.Type
      data$All.Adjust<- paste0(mod.rows$Modifier) #adjustment for "All"
      
    }
    
    calc_column <-  og_data_columns + 1
    data$Final.adjust <- paste0("with(data[i,],data[i,",calc_column,"]",data$All.Modifier.Type, data$All.Adjust, data$Syst.Modifier.Type, data$Syst.Adjust, ")") #combine all adjustments
    data$Final.adjust <- as.character(data$Final.adjust)
    
    # data$new <- NA
    adjust.col <- ncol(data) +1
    
    for (i in 1:nrow(data)) {
      data$Final.adjust[i] <- parse(text = data$Final.adjust[i])
      data[i,(adjust.col)] <- eval(data$Final.adjust[i]) #evaluate adjustment expression by row. This must be done iteratively or else something goes wrong with eval/parse
    } 
    
    #TEST MOD COLUMN NAMING
    names(data)[ncol(data)] <- paste0(author,analysis, "Mod") #get name for "Mod" column (added if showmod = TRUE)
    
    data <- data[,c(1:calc_column, ncol(data))] #remove adjustment columns
  } 
  
  
  ##remove mod column if showmod = FALSE
  if(missing(showmod)) {
    showmod <- FALSE
  }
  
  if (showmod == FALSE) {
    if (analysis %in% mod$Analysis) {
      data[,calc_column] <- data[,ncol(data)]
      data[,ncol(data)] <- NULL
    }
  }
  return(data)
  
}

calculate_hpa1 <- calculate_hpa(data = m, author = "behjou", analysis = "Manual", showmod = FALSE)

calculate_hpa1 <- calculate_hpa(data = m, author = "ghaf", analysis = "Skidder", showmod = TRUE)

#####COMPARE HOURS PER ACRE CALCULATIONS BY ANALYSIS AND GET MEAN######
#compare_harvest_methods runs calculate_hpa() for all equations in an analysis type (e.g. "Skidder"),
#puts them in a single table, and calculates the mean of all equation results for the analysis type. 

#Arguments:
#data - The opcost input data
#analysis - Analysis column value of desired equations to compare from opcost_ref (case sensitive)
#allCols - TRUE/FALSE - If true, shows original data columns as well as cost and mean values. If FALSE, output is stand ID with equation results and mean
#showmod - TRUE/FALSE - If true, show both the original calculated HPA and the values after being modified according to opcost_modifiers. Defaults to FALSE
#ref - Reference equations to use. Defaults to opcost_ref
#mod - Reference modifiers to use. Defaults to opcost_modifiers
#units - Reference unit conversion equations to use. Defaults to opcost_units

#Example: compare_harvest_methods(data = m, analysis = "Manual", allCols = TRUE, showmod = TRUE, ref = opcost_ref, mod = opcost_modifiers, units = opcost_units)

compare_harvest_methods <- function(data, analysis, allCols, showmod, ref, mod, units) {
  #add in default values for function parameters
  if(missing(allCols)) {
    allCols <- FALSE
  }
  if(missing(showmod)) {
    showmod <- FALSE
  }
  if (missing(ref)) {
    ref <- opcost_ref[opcost_ref$Name != "" & opcost_ref$Analysis != "",]
  }
  if (missing(units)) {
    units <- opcost_units
  }
  if (missing(mod)) {
    mod <- opcost_modifiers
  }
  
  data1 <- data #rename data
  a <- ncol(data1) #get number of columns to start
  newdata2 <- data.frame(matrix(NA, nrow = nrow(data), ncol = 1)) #set up empty matrix for output values

  ref <- ref[gsub('[[:digit:]]+',"",ref$Analysis) == gsub('[[:digit:]]+',"",analysis),] #get rows for relevant analysis from opcost_reference (disregarding numbers)

  for (i in 1:nrow(ref)) { #use calculate_hpa to get hours per acre for each author/analysis combination in analysis
    newdata <- calculate_hpa(data = data1, author = ref$Name[i], analysis = ref$Analysis[i])
    newdata <- newdata[,c(1,(ncol(data1)+1):ncol(newdata))] #get the author/analysis hours per acre value
    data <- merge(data, newdata, by = "Stand") #add author/analysis hours per acre value to the original data (merged on Stand values)
  }
  
  b <- ncol(data) #get new # of columns for data with analyis hours per acre value columns
  
  col.diff <- b - a #get difference in # columns
  
  if (col.diff > 1) { #if there are multiple equations for the analysis
    if (showmod != TRUE) { #if showmod is FALSE
      data$meanTime <- rowMeans(data[,(ncol(data) - col.diff + 1):ncol(data)], na.rm=TRUE) #calculate mean of analysis hours per acre values
    } else {
      firstnewcol <- (ncol(data) - col.diff) + 1 #get value where new columns start
      lastnewcol <- ncol(data) #get value where new columns start
      mod.cols <- seq(firstnewcol+1, lastnewcol, by = 2) #get column # for every other column (aka non-mod columns)
      data$meanTime <- rowMeans(data[,c(mod.cols)], na.rm=TRUE) #calculate mean of analysis hours per acre values of non-mod columns
    }
    
  } else { #if there is only one equation for the analysis
    data$meanTime <- data[,ncol(data)]  #meanTime is equal to the single column values (mean of 1)
  }
  
  names(data)[ncol(data)] <- paste0("mean", analysis, "Time") #name the n mean column
  
  if(allCols != TRUE) { #if allCols = FALSE
    data <- data[,c(1,(a+1):(b+1))] #remove old data columns (keep Stand column)
  }
  
  return(data)
}


compare_harvest_methods_Manual <- compare_harvest_methods(data = m, analysis = "Chip", allCols = TRUE)

#####GET MEAN HARVEST HOURS PER ACRE FOR ALL ANALYSIS######
#all_analyses runs compare_harvest_methods for all analyses and compiles a table
#with mean values for all analyses. 

#Arguments:
#data - The opcost input data
#allCols - TRUE/FALSE - If true, shows original data columns as well as cost and mean values. If FALSE, output is stand ID with equation results and mean
#showmod - TRUE/FALSE - If true, show both the original calculated HPA and the values after being modified according to opcost_modifiers. Defaults to FALSE
#meansonly - TRUE/FALSE - If true, shows only the calculated means for each analysis method, and not the values for each equation
#ref - Reference equations to use. Defaults to opcost_ref
#mod - Reference modifiers to use. Defaults to opcost_modifiers
#units - Reference unit conversion equations to use. Defaults to opcost_units

#Example: all_analysis(data = m, allCols = TRUE, showmod = TRUE, meansonly = TRUE, ref = opcost_ref, mod = opcost_modifiers, units = opcost_units)

all_analyses <- function(data, allCols, showmod, meansonly, ref, mod, units) {
  #add in default values for function parameters
  if(missing(allCols)) {
    allCols <- FALSE
  }
  if(missing(showmod)) {
    showmod <- FALSE
  }
  if (missing(ref)) {
    ref <- opcost_ref[opcost_ref$Name != "" & opcost_ref$Analysis != "",]
  }
  if (missing(units)) {
    units <- opcost_units
  }
  if (missing(mod)) {
    mod <- opcost_modifiers
  }

  #get unique author_analysis values from ref (ignoring numeric)
  author_analysis <- ref[ref$Name != "" & ref$Analysis != "",] 
  analysis_values <- unique(gsub('[[:digit:]]+',"",author_analysis$Analysis))
  
  #create empty list to store loop values
  mylist <- vector(mode="list", length=length(analysis_values))
  name.vector <- as.character() #create empty vector for list name values
  
  for (i in 1:length(analysis_values)) {
   name.vector <- c(name.vector, paste0(analysis_values[i],"Time") ) #get name vector value for this loop iteration
   mylist[[i]] <- list(compare_harvest_methods(data, analysis_values[i], allCols)) #run and store compare_harvest_methods for each unique analysis value
  }
  
  names(mylist) <- name.vector #name each list item 
  
  all <- Reduce(merge, mylist) #merge list items (i.e. put all analysis compare_harvest_methods function results in a single data frame)
  all.data <- merge(all, data, all= TRUE) #merge compare_harvest_methods values with original data
  all.diff <- all.data[!all.data$Stand %in% all$Stand,] #get any stand values that did not make it through the merge process (i.e. NA rows)
  all2 <- merge(all.data, all.diff, all = TRUE) #add NA rows back to original data (ensure that all stands are carried through)
  
  if(allCols == FALSE) { #if allCols is false, only return compare_harvest_methods calculations, otherwise include original data rows as well
    all <- all2[,c(1:(ncol(all)))]
  } else {
    all <- all2
  }
  
  if(meansonly == TRUE) { #if meansonly is true, only show means calculated through compare_harvest_methods, otherwise show hours per acre calculations as well
    pattern <- c("mean", "Stand")
    all.means <- all[grepl(paste(pattern,collapse="|"), names(all))]
    if (allCols == TRUE) {
      all <- merge(data, all.means)
    } else {
      all <- all.means
    }
  }
  
  return(all)
}

all <- all_analyses(data = m, allCols = TRUE, meansonly = TRUE)

all$meanChipTime <- chipTime(all) #band aid to account for the random ^0.8 raising if comparing with original opcost values. If you haven't run original opcost this won't work 


#####CREATE HARVEST SYSTEM COST TABLE######
#create_harvest_system_cost table takes reference tables opcost_cost_ref, opcost_costestimate_ref, 
#and opcost_moveincost_ref and compiles them into a single table. This must be done separately since
#the reference tables have parameters that may need to be adjusted (i.e. cost, component multipliers, 
#move in cost, etc)

#Arguments:
#cost - Cost values the user wants to use to calculate costs. This corresponds to the name of each column in opcost_cost_ref. Defaults to "Default.CPH"
#cost_ref - the cost reference table. Defaults to opcost_cost_ref
#costestimate_ref - the cost estimate reference table. Defaults to opcost_costestimate_ref
#moveincost_ref - the move in cost reference table. Defaults to opcost_moveincost_ref

#Example: create_harvest_system_cost_table(cost = "Default.CPH", cost_ref = opcost_cost_ref, costestimate_ref = opcost_costestimate_ref, moveincost_ref = opcost_moveincost_ref)
create_harvest_system_cost_table <- function(cost, cost_ref, costestimate_ref, moveincost_ref) {
  #add in default values for function parameters
  if(missing(cost)){
    cost <- "Default.CPH"
  }
  
  if (missing(cost_ref)) {
    cost_ref <- opcost_cost_ref
  }
  if (missing(costestimate_ref)) {
    costestimate_ref<- opcost_costestimate_ref
  }
  if (missing(moveincost_ref)) {
    moveincost_ref <- opcost_moveincost_ref
  }
  
  cost.col <- which(names(cost_ref) == cost) #get cost column specified in function parameter (or use Default.CPH)
  cost_ref2 <- cost_ref[c(1,cost.col)] 
  
  costestimate_ref2  <- merge(cost_ref2, costestimate_ref) #merge cost column with costestimate_ref
  
  #calculate "full cost" by taking cost values from cost_ref and multiplying them by Cost.Multiplier values in costestimate_ref
  costestimate_ref2$full_cost <- ifelse(!is.na(costestimate_ref2$Cost.Multiplier), costestimate_ref2$Default.CPH * costestimate_ref2$Cost.Multiplier, costestimate_ref2$Default.CPH)
  
  unique.harvest.system <- unique(costestimate_ref2$Harvesting.System) #get unique harvesting.system values from costestimate_ref
  unique.harvest.system <- unique.harvest.system[!is.na(unique.harvest.system) & unique.harvest.system != ""]
  
  harvest.system.cost <- data.frame() #create blank data frame to store harvest.system.cost values
  
  #The loop below creates a "harvest.system.cost" table by compiling values from opcost_moveincost, opcost_cost_ref, opcost_costestimate_ref
  for (i in 1:length(unique.harvest.system)) { #loop for each unique harvest system in costestimate_ref
    harvest.system <- unique.harvest.system[i]
    
    #harvest components equation
    harvest.components <- costestimate_ref2[costestimate_ref2$Harvesting.System == harvest.system,] #get harvest components values from costestimate_ref for this harvest.system iteration
    harvest.components <- harvest.components[c(6,10)] #list harvest components
    
    #create equation to calculate cost of each harvest component by multiplying each by the full_cost value
    harvest.components$cost_equation <- paste0(harvest.components$Harvest.Component,"*", harvest.components$full_cost)
    
    #create equation to sum each harvest component cost
    harvest.components.cost <- paste(collapse = "+", harvest.components$cost_equation)
    
    #move in cost equation
    move.in.cost <- moveincost_ref[as.character(moveincost_ref$Harvesting.System) == as.character(harvest.system),] #get move in cost for harvest system from moveincost_ref
    move.in.cost <- move.in.cost[c(2:3)] #get relevant columns (Move.In.Cost.Component and Move.In.Cost.Multiplier)
    names(move.in.cost)[1] <- "Cost.Component" 
    move.in.cost <- merge(move.in.cost, cost_ref2) #merge move in cost and costestimate_ref by Cost.Component to get the Cost per Hour data moved in
    move.in.cost$move.in.cost <- move.in.cost$Default.CPH * move.in.cost$Move.In.Cost.Multiplier #calculate move in cost by multiplying Default.CPH by Move.In.Cost.Multiplier
    
    #create new data frame row with Harvesting.System iteration, Harvest.Components.Cost, and Move.In.Cost (Move.In.Cost is calculated by summing values calculated in the line above)
    harvest.data <- data.frame("Harvesting.System"  = unique.harvest.system[i], "Harvest.Components.Cost" = harvest.components.cost, "Move.In.Cost" = sum(move.in.cost$move.in.cost))
    
    harvest.system.cost <- rbind(harvest.system.cost, harvest.data) #append to data frame
  }
  
  move.in.cost.lb <- unique(moveincost_ref[,c(1,4)]) #get unique move.in.cost.lb calculation from opcost_moveincost_ref by harvest.system
  
  harvest.system.cost <- merge(harvest.system.cost, move.in.cost.lb) #merge in move.in.cost.lb values to harvest.system.cost by harvest system
}

harvest.system.cost <- create_harvest_system_cost_table()


#####ESTIMATE COST FOR CURRENT HARVEST.SYSTEM VALUE######
#estimate_cost combines the output from all_analyses (all) and the output from
#create_harvest_system_cost_table (harvest.system.cost) and combines them to get the
#cost for the harvest system specified in Harvesting.System

#Arguments:
#data - Input data to be used. This should be an output from the all_analyses() function
#harvest.system.cost - Harvest system cost data to be used. This should be an output from create_harvest_system_cost_table()

#Example: estimate_cost(data = all, harvest.system.cost = harvest.system.cost)

estimate_cost <- function(data, harvest.system.cost) {
  data <- merge(data, harvest.system.cost, all.x = TRUE) #merge harvest.system.cost with original data

  data$Harvesting.Cost <-  parse(text = paste0("with(data[j,],", data$Harvest.Components.Cost, ")")) #turn equations into text (so they can be later evaulated as expressions)
  data$Move.In.Cost.LB <-  parse(text = paste0("with(data[j,],", data$Move.In.Cost.LB, ")")) 
  
  data$Harvesting.Cost2 <- NA #get rid of unnecessary columns
  data$Move.In.Cost.LB2 <- NA
  
  for (j in 1:nrow(data)) { #calculate Harvesting.Cost and Move.In.Cost.LB by row (this breaks if not done iteratively due to strangeness of expressions/parsing)
    data$Harvesting.Cost2[j] <- eval(data$Harvesting.Cost[j])
    data$Move.In.Cost.LB2[j] <- eval(data$Move.In.Cost.LB[j])
  }
  
  data$Harvesting.Cost3 <- as.numeric(as.character(data$Harvesting.Cost2)) #convert to numeric
  
  #clean up unneeded columns
  data$Harvesting.Cost2 <- NULL 
  data$Harvest.Components.Cost <- NULL
  data$Harvesting.Cost <- NULL
  data$Move.In.Cost.LB <- NULL
  
  names(data)[which(grepl("Harvesting.Cost3", names(data)))] <- "Harvesting.Cost" #rename columns
  names(data)[which(grepl("Move.In.Cost.LB2", names(data)))] <- "Move.In.Cost.LB"
  
  pattern <- c("Harvesting.Cost", "Move.In.Cost", "Move.In.Cost.LB", "Total.Cost") #list of columns to sum to get Total.Cost
  data$Total.Cost <- rowSums(data[which(grepl(paste(pattern,collapse="|"), names(data)))]) #sum above columns by row
  
  return(data)
}

estimate.cost <- estimate_cost(all, harvest.system.cost)

#####ESTIMATE COST FOR ALL HARVEST SYSTEM VALUES######
#estimate_all_costs combines the output from all_analyses (all) and the output from
#create_harvest_system_cost_table (harvest.system.cost) and combines them to get the
#cost for ALL unique Harvesting.System values (to be later used to determine the optimal system)

#Arguments:
#data - Input data to be used. This should be an output from the all_analyses() function
#harvest.system.cost - Harvest system cost data to be used. This should be an output from create_harvest_system_cost_table()

#Example: estimate_all_costs(data = all, harvest.system.cost = harvest.system.cost)

estimate_all_costs <- function(data, harvest.system.cost) {
  harvest.system.cost2 <- as.data.frame(t(harvest.system.cost[,c(1:2)]), row.names = FALSE) #transpose harvest.system.cost table
  rownames(harvest.system.cost2) <- NULL #get rid of rownames
  name.vector <- as.character(unlist(c(harvest.system.cost2[1,]))) #save first transposed row as column names
  
  harvest.system.cost2 <- harvest.system.cost2[-1,] #get rid of first transposed row
  names(harvest.system.cost2) <- paste0("Estimate.", name.vector) #change column names to add "Estimate." in front
  
  data2 <- cbind(data, harvest.system.cost2, row.names = NULL) #add estimate columns to data
  
  estimate.cols <- which(grepl("Estimate", names(data2))) #get column #s with "Estimate." in the column name
  
  #Loop for each estimate to run
  for (i in 1:length(estimate.cols)) { #iterate for each estimate column
    data2[,c(estimate.cols[i])] <- parse(text = paste0("with(data[j,],",data2[,c(estimate.cols[i])], ")")) #convert estimate equation to text
    for (j in 1:nrow(data2)) { #for each row of data, run the estimate equation for this iteration
      data2[j,c(estimate.cols[i]+length(estimate.cols))] <- eval(data2[j,c(estimate.cols[i])])
      names(data2)[estimate.cols[i]+length(estimate.cols)] <- paste0(names(data2)[estimate.cols[i]],"V2") #update the name 
    }
    
  }
  
  data3 <- data2[,-c(estimate.cols)] #get rid of the old estimate columns (the equations)
  estimate.cols2 <- which(grepl("V2", names(data3))) #replace and rename with calculated values
  data4 <- data3[,c(1,estimate.cols2)]
  names(data4) <- gsub("V2", "", names(data4))

  return(data4) 
}
  
all_costs <- estimate_all_costs(all, harvest.system.cost)

#####DETERMINE OPTIMAL HARVEST SYSTEM######
#optimal_costs takes the output from estimate_all_costs() and determines the system that
#yields the minimum total harvesting cost based on limiting parameters set in opcost_ideal_ref

#Arguments:
#data - Input data to be used. This should be an output from the all_analyses() function
#ideal_ref - The ideal anaylsis reference to use for harvest system limiters. Defaults to opcost_ideal_ref

#Example: optimal_cost(data = all, ideal_ref = opcost_ideal_ref)

optimal_cost <- function(data, ideal_ref) {
  if(missing(ideal_ref)) {
    ideal_ref <- opcost_ideal_ref
  }
  
  og_data <- data
  
  cost_data <- estimate_all_costs(data, harvest.system.cost) #get cost data by running estimate_all_costs
  
  unique.limit.parameters <- unique(ideal_ref$Limiting.Parameter) #get unique limiting paramters from ideal_ref
  
  merge.data <- data[c(1,which(names(og_data) %in% unique.limit.parameters))] #get Stand and limiting parameter data from the original dataset for merging later
  
  data <- merge(cost_data, merge.data) #merge cost_data with merge data to get cost data plus limiting parameter column
  
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
      
      harvesting.systems <- ideal_ref$Harvesting.System[ideal_ref$Limit.Statement == unique.limit.statement[j] & ideal_ref$ID == unique.ID[i]] #get list of harvesting systems for current limit statement and ID iteration
      cols <- which(grepl(paste(harvesting.systems,collapse="|"), names(data.limited))) #get columns from data.limited where the column name contains current harvesting system
      data.limited$j <- apply(data.limited[,c(cols)],1,which.min) #get columns number that contains minimum value for columns where column name contains current harvesting system
      data.limited$j <- as.numeric(as.character(data.limited$j)) #convert column number to numeric
      data.limited$Optimal.Harvest.System <- gsub("Estimate.","",names(data.limited)[cols[data.limited$j]]) #convert column number to column name and remove "Estimate." (so it returns harvest system name)
      
      #The Optimal.Harvest.System column now contains the harvest system name of the column with the minimum cost value according to ideal_ref stipulations/limitations
      
      mylist.j[[j]] <- data.limited #store for this iteration 
    }
    
    data1 <- Reduce(rbind, mylist.j) #merge results for each iteration of unique.limit.statement and ID back into single data frame
    data1$j <- NULL
    
    if (nrow(data1) < nrow(og_data)) {
      stop("Limit statements do not incorporate entire dataset")
    }
    
    data2 <- data1[,c(1,which(names(data1) == "Optimal.Harvest.System"))] #get just Optimal.Harvesting.System and Stand
    names(data2)[2] <- "Harvesting.System" #change "Optimal.Harvest.System"  column name to just "Harvesting.System" so it can be re-run through estimate_cost
    data3 <- og_data #return to original data
    data3$Harvesting.System <- NULL 
    optimal.analysis <- merge(data2, data3) #go back to original dataset but with Harvesting.System now equal to Optimal.Harvesting.System
    
    
    optimal.cost <- estimate_cost(optimal.analysis, harvest.system.cost) #run cost analysis for optimal.analysis (with Optimal.Harvesting.System)
    pattern <- c("Stand", "Harvesting.System", "Harvesting.Cost", "Move.In.Cost", "Move.In.Cost.LB", "Total.Cost") #cost columns to pull from new estimate_cost table
    optimal.cost2 <- optimal.cost[grepl(paste(pattern,collapse="|"), names(optimal.cost))] #pull new cost columns
    names(optimal.cost2) <- paste0("Optimal.", names(optimal.cost2)) #rename columns to have "Optimal." in front
    
    data4 <- estimate_cost(og_data, harvest.system.cost) #run estimate_cost for original dataset again
    optimal.cost3 <-  merge(data4, optimal.cost2, by.y = "Optimal.Stand", by.x = "Stand", all = TRUE) #merge Optimal costs data with original Costs
    
    #Populate MatchesOriginalSystem column. If original Total.Cost is <= to Optimal.Total.Cost, TRUE, if >, False
    optimal.cost3$MatchesOriginalSystem[round(optimal.cost3$Total.Cost,0) <= round(optimal.cost3$Optimal.Total.Cost,0)] <- "TRUE" 
    optimal.cost3$MatchesOriginalSystem[round(optimal.cost3$Total.Cost,0) > round(optimal.cost3$Optimal.Total.Cost,0)] <- "FALSE"
    
    mylist.ID[[i]] <- optimal.cost3 #store as data frame for current ideal_ref unique ID iteration
    
  }
  
  return(mylist.ID)
}

optimal.cost.data <- optimal_cost(all)

optimal1 <- optimal.cost.data[[1]]

data <- optimal1

#####GET SECONDARY CHIPPING COST######

secondary_chippingcost <- function(data, chip2) {
  if (missing(chip2)) {
    chip2 <- opcost_chippingcost2 #get secondary chipping cost values
  }
  
  data2 <- merge(data, chip2, by = "Harvesting.System", all.x = TRUE) #merge data with chip2 to add in Chipping.Cost.Multiplier2 based on Harvesting.System
  names(data2)[ncol(data2)] <- "Harvesting.Chip.Multiplier2" #rename column so it doesn't get rewritten
  data3 <- merge(data2, chip2, by.x = "Optimal.Harvesting.System", by.y = "Harvesting.System", all.x = TRUE) #merge data with chip2 to add in Chipping.Cost.Multiplier2 based on Optimal.Harvesting.System
  names(data3)[ncol(data3)] <- "Optimal.Harvesting.Chip.Multiplier2" #rename column to Optimal.
  data3$Harvesting.Chipping.Cost2 <- data3$meanChipTime * data3$Harvesting.Chip.Multiplier2 #calculate Chipping.Cost2 for original Harvest method by multiplying meanChipTime * Harvesting.Chip.Multiplier2
  data3$Optimal.Chipping.Cost2 <- data3$meanChipTime * data3$Optimal.Harvesting.Chip.Multiplier2 #calculate Optimal.Chipping.Cost2 for original Harvest method by multiplying meanChipTime * Optimal.Harvesting.Chip.Multiplier2
  
  return(data3)
}

opcost_master <- secondary_chippingcost(optimal1)

pattern <- c("Stand", "YearCostCalc", "Harvesting.Cost", "Harvesting.Chipping.Cost2", "Move.In.Cost.LB", "Move.In.Cost","Harvesting.System", "RxPackage_Rx_RxCycle", "biosum_cond_id",
             "RxPackage", "Rx", "RxCycle") #columns to pull from opcost_master to recreate original Opcost_Output table

opcost_output <- opcost_master[,c(which(names(opcost_master) %in% pattern))] #pull columns

pattern2 <- c("Stand", "YearCostCalc", "Optimal.Harvesting.Cost", "Optimal.Chipping.Cost2", "Optimal.Move.In.Cost.LB", "Optimal.Move.In.Cost", "Optimal.Harvesting.System", "RxPackage_Rx_RxCycle", "biosum_cond_id",
             "RxPackage", "Rx", "RxCycle", "MatchesOriginalSystem") #columns to pull from opcost_master to recreate original Opcost_Ideal_Output table
opcost_ideal_output <- opcost_master[,c(which(names(opcost_master) %in% pattern2))]



#sqlSave(con, exp22, tablename="OpCost_Errors",append = TRUE)

sqlSave(con, m10, tablename="OpCost_Output", safer=FALSE)

ifelse(idealTable==1,sqlSave(con, m16, tablename="OpCost_Ideal_Output", safer=FALSE),print("Ideal Table Not Created or Updated"))

print("m10:OK")

odbcCloseAll()


library("reshape2")
library("ggplot2")
library("dplyr")
graph_analyses <- function(data, ref) {
  analysis_values <- unique(ref$Analysis)

  for (i in 1:length(analysis_values)) {
    values <- ref[gsub('[[:digit:]]+',"",ref$Analysis) == gsub('[[:digit:]]+',"",analysis_values[i]),]
    values.data <- compare_harvest_methods(data = data, analysis = analysis_values[i], allCols = TRUE, ref = values)
    b <- ncol(values.data)
    a <- ncol(data) + 1
    df2 <- melt(values.data, id.vars = c(a:b), measure.vars = names(values.data)[a:b])
    n <- ncol(df2)
    df2 <- df2[complete.cases(df2[n]),]
    df3 <- df2[,c(n-1, n)]
    df4 <- df3 %>% group_by(variable) %>% tally()
    df2 <- merge(df4, df3, by = "variable")
    df2$variable <- gsub(analysis_values[i],"",df2$variable)
    df4$variable <- gsub(analysis_values[i],"",df4$variable)
    graph <- ggplot(df2, aes(variable, value)) + geom_boxplot() + labs(x=analysis_values[i], y="Hours Per Acre") +
      scale_x_discrete(labels = paste(df4$variable, df4$n, sep = "\n"))
    ggsave(filename = paste0(analysis_values[i], ".png"),graph, device = "png", width = ifelse(nrow(df4)*1.1 > 6, nrow(df4)*1.1, 6),)
  }
}

graph_analyses(m, ref = ref)
