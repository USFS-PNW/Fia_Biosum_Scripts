#This script was created by Carlin Starrs in 2018
#The script copies over processor defaults for the CEC project,
#brinds in slope data from master.cond, and calculates additional
#harvest costs based on slope. This script should be run after you
#have opened the processor module once and created a scenario called
#"scenario1"

#Make sure you have the Microsoft Access Database Engine Driver https://www.microsoft.com/en-us/download/confirmation.aspx?id=23734
#and you are using 32-bit R (set in RStudio by going to Tools -> Global Options)
packages <- c("RODBC", "dplyr")

package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, repos="http://cran.r-project.org", dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

options(scipen = 999) #this is important for making sure your stand IDs do not get translated to scientific notation

#Location of additional data files from github:
additional.data <- "G:/Dropbox/Carlin/GitHub/Fia_Biosum_Scripts/Additional data"

#Project root location:
project.location <- "H:/cec_20180529"

processor.setup <- function(additional.data, project.location){
  
  #Get master processor tables from additional data
  processor_master <- file.path(additional.data, "scenario_processor_rule_definitions_master.mdb")
  conn.path <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", processor_master)
  conn <- odbcDriverConnect(conn.path)
  scenario_harvest_method <- sqlFetch(conn, "scenario_harvest_method", as.is = TRUE)
  scenario_move_in_costs <- sqlFetch(conn, "scenario_move_in_costs", as.is = TRUE)
  scenario_tree_species_diam_dollar_values <- sqlFetch(conn, "scenario_tree_species_diam_dollar_values", as.is = TRUE)
  scenario_tree_species_groups <- sqlFetch(conn, "scenario_tree_species_groups", as.is = TRUE)
  scenario_tree_species_groups_list <- sqlFetch(conn, "scenario_tree_species_groups_list", as.is = TRUE)
  odbcCloseAll()
  
  #connect to project processor db
  project_processor <- file.path(project.location, "processor", "db", "scenario_processor_rule_definitions.mdb")
  conn.path <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", project_processor)
  conn <- odbcDriverConnect(conn.path)
  
  sqlQuery(conn, 'SELECT * INTO harvest_method_backup FROM scenario_harvest_method')
  sqlQuery(conn, 'SELECT * INTO move_in_costs_backup FROM scenario_move_in_costs')
  sqlQuery(conn, 'SELECT * INTO diam_dollar_backup FROM scenario_tree_species_diam_dollar_values')
  sqlQuery(conn, 'SELECT * INTO groups_backup FROM scenario_tree_species_groups')
  sqlQuery(conn, 'SELECT * INTO groups_list_backup FROM scenario_tree_species_groups_list')
  
  sqlQuery(conn, 'DROP TABLE scenario_harvest_method')
  sqlQuery(conn, 'DROP TABLE  scenario_move_in_costs')
  sqlQuery(conn, 'DROP TABLE  scenario_tree_species_diam_dollar_values')
  sqlQuery(conn, 'DROP TABLE  scenario_tree_species_groups')
  sqlQuery(conn, 'DROP TABLE  scenario_tree_species_groups_list')
  
  sqlSave(conn, dat = scenario_harvest_method, tablename = "scenario_harvest_method", rownames = FALSE)
  sqlSave(conn, dat = scenario_move_in_costs, tablename = "scenario_move_in_costs", rownames = FALSE)
  sqlSave(conn, dat = scenario_tree_species_diam_dollar_values, tablename = "scenario_tree_species_diam_dollar_values", rownames = FALSE)
  sqlSave(conn, dat = scenario_tree_species_groups, tablename = "scenario_tree_species_groups", rownames = FALSE)
  sqlSave(conn, dat = scenario_tree_species_groups_list, tablename = "scenario_tree_species_groups_list", rownames = FALSE)

  #Run additional harvest costs script
  fvs.master.location <- file.path(project.location, "db", "fvsmaster.mdb")
  conn.path <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", fvs.master.location)
  conn <- odbcDriverConnect(conn.path)
  fvsmaster_PkgLabels <- sqlFetch(conn, "PkgLabels", as.is = TRUE) #This PkgLabels table was added separately from the packagelabels.csv file (see github additional data)
  fvsmaster_rx_pkg_xwalk <- sqlFetch(conn, "rx_pkg_xwalk", as.is = TRUE)
  fvsmaster_rxpackage <- sqlFetch(conn, "rxpackage", as.is = TRUE)
  fvsmaster_rx_harvest_cost_columns <- sqlFetch(conn, "rx_harvest_cost_columns", as.is = TRUE)
  
  rx_addcost <- fvsmaster_PkgLabels[,c(1,9,10)] 
  names(rx_addcost)[1] <- "Pkg"
  names(rx_addcost)[2] <- "ColumnName"
  names(rx_addcost)[3] <- "Type"
  
  rx_addcost$ColumnName[rx_addcost$ColumnName == "Pile/burn"] <- "Pile_burn"
  rx_addcost$ColumnName[rx_addcost$ColumnName == "Lop/scatter"] <- "Lop_and_scatter"
  
  rx_addcost$ColumnName[rx_addcost$ColumnName == "Rx Fire"] <- "RX_burn"
  
  rx_xwalk <- fvsmaster_rx_pkg_xwalk[,c(1,2)]
  
  rx_addcost2 <- merge(rx_addcost, rx_xwalk, by = "Pkg", all = TRUE)
  
  rx_addcost_update <- rx_addcost2[,c(4,2)]
  names(rx_addcost_update)[1] <- "rx"
  names(rx_xwalk)[2] <- "rx"
  
  rx_addcost_update <- rx_addcost_update[is.na(rx_addcost_update$rx) == FALSE,]
  rx_addcost_update$ColumnName[rx_addcost_update$rx == 999] <- "None"
  
  rx_addcost_update$Description <- paste("Cost of", rx_addcost_update$ColumnName, sep = " ")
  
  #backs up the old rx_harvest_cost_columns table and updates with new
  sqlQuery(conn, 'SELECT * INTO rx_harvest_cost_columns_old FROM rx_harvest_cost_columns')
  sqlQuery(conn, 'DROP TABLE rx_harvest_cost_columns') 
  
  sqlSave(conn, dat = rx_addcost_update, tablename = "rx_harvest_cost_columns", rownames = FALSE)
  odbcCloseAll()
  
  #connect to master.mdb
  master.location <- file.path(project.location, "db", "master.mdb")
  conn.path <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", master.location)
  conn <- odbcDriverConnect(conn.path)
  master_cond <- sqlFetch(conn, "cond", as.is = TRUE)
  odbcCloseAll()
  
  #Make sure you have opened the processor module with this project to make sure this table is populated
  project_processor <- file.path(project.location, "processor", "db", "scenario_processor_rule_definitions.mdb")
  conn.path <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", project_processor)
  conn <- odbcDriverConnect(conn.path)
  scenario_additional_harvest_costs <- sqlFetch(conn, "scenario_additional_harvest_costs", as.is = TRUE)
  slope <- master_cond[,c(1,16)] #get slope values for each condition/stand from master_cond
  
  scenario_update <- merge(scenario_additional_harvest_costs, slope, by = "biosum_cond_id") #add slope data to the scenario_additional_harvest_costs table
  names(rx_addcost2)[4] <- "rx"
  scenario_update <- merge(scenario_update, rx_addcost2, by = "rx") #add in additional cost data
  
  #The section below categorizes the additional costs by slope boundaries. e.g., if the package has RX_burn as the additional harvest cost, it 
  #sets the cost value to 75 if slope is <= 40 and 150 if it's over 40
  scenario_update$RX_burn[scenario_update$ColumnName == "RX_burn" & scenario_update$slope <= 40] <- 75
  scenario_update$RX_burn[scenario_update$ColumnName == "RX_burn" & scenario_update$slope > 40] <- 150
  scenario_update$Masticate[scenario_update$ColumnName == "Masticate" & scenario_update$slope <= 40] <- 250
  scenario_update$Lop_and_scatter[scenario_update$ColumnName == "Lop_and_scatter" & scenario_update$slope <= 40 & scenario_update$Type == "WT"] <- 35
  scenario_update$Lop_and_scatter[scenario_update$ColumnName == "Lop_and_scatter" & scenario_update$slope > 40 & scenario_update$Type == "WT"] <- 60
  scenario_update$Lop_and_scatter[scenario_update$ColumnName == "Lop_and_scatter" & scenario_update$slope <= 40 & scenario_update$Type == "CTL"] <- 50
  scenario_update$Lop_and_scatter[scenario_update$ColumnName == "Lop_and_scatter" & scenario_update$slope > 40 & scenario_update$Type == "CTL"] <- 75
  scenario_update$Pile_burn[scenario_update$ColumnName == "Pile_burn" & scenario_update$slope <= 40 & scenario_update$Type == "WT"] <- 125
  scenario_update$Pile_burn[scenario_update$ColumnName == "Pile_burn" & scenario_update$slope <= 40 & scenario_update$Type == "CTL"] <- 250
  
  sqlSave(conn, dat = scenario_update, tablename = "addl_harvest_cost_update", rownames = FALSE)
  
  sqlQuery(conn, 'SELECT * INTO scenario_additional_harvest_costs_old FROM scenario_additional_harvest_costs')
  
  sqlQuery(conn, 'UPDATE addl_harvest_cost_update INNER JOIN scenario_additional_harvest_costs ON (addl_harvest_cost_update.scenario_id = scenario_additional_harvest_costs.scenario_id) AND (addl_harvest_cost_update.biosum_cond_id = scenario_additional_harvest_costs.biosum_cond_id) AND (addl_harvest_cost_update.rx = scenario_additional_harvest_costs.rx) SET scenario_additional_harvest_costs.RX_burn = [addl_harvest_cost_update].[RX_burn];')
  sqlQuery(conn, 'UPDATE addl_harvest_cost_update INNER JOIN scenario_additional_harvest_costs ON (addl_harvest_cost_update.scenario_id = scenario_additional_harvest_costs.scenario_id) AND (addl_harvest_cost_update.biosum_cond_id = scenario_additional_harvest_costs.biosum_cond_id) AND (addl_harvest_cost_update.rx = scenario_additional_harvest_costs.rx) SET scenario_additional_harvest_costs.Pile_burn = [addl_harvest_cost_update].[Pile_burn];')
  sqlQuery(conn, 'UPDATE addl_harvest_cost_update INNER JOIN scenario_additional_harvest_costs ON (addl_harvest_cost_update.scenario_id = scenario_additional_harvest_costs.scenario_id) AND (addl_harvest_cost_update.biosum_cond_id = scenario_additional_harvest_costs.biosum_cond_id) AND (addl_harvest_cost_update.rx = scenario_additional_harvest_costs.rx) SET scenario_additional_harvest_costs.Masticate = [addl_harvest_cost_update].[Masticate];')
  sqlQuery(conn, 'UPDATE addl_harvest_cost_update INNER JOIN scenario_additional_harvest_costs ON (addl_harvest_cost_update.scenario_id = scenario_additional_harvest_costs.scenario_id) AND (addl_harvest_cost_update.biosum_cond_id = scenario_additional_harvest_costs.biosum_cond_id) AND (addl_harvest_cost_update.rx = scenario_additional_harvest_costs.rx) SET scenario_additional_harvest_costs.Lop_and_scatter = [addl_harvest_cost_update].[Lop_and_scatter];')
  sqlQuery(conn, 'UPDATE addl_harvest_cost_update INNER JOIN scenario_additional_harvest_costs ON (addl_harvest_cost_update.scenario_id = scenario_additional_harvest_costs.scenario_id) AND (addl_harvest_cost_update.biosum_cond_id = scenario_additional_harvest_costs.biosum_cond_id) AND (addl_harvest_cost_update.rx = scenario_additional_harvest_costs.rx) SET scenario_additional_harvest_costs.None = [addl_harvest_cost_update].[None];')
  
  odbcCloseAll()
  
}

processor.setup(additional.data, project.location)
