#This script updates the additional harvest costs tables

library("dplyr") #if you do not have these packages installed, enter "install.packages("packagename") into the console, then load them using this line.
library("RODBC")
options(scipen = 999) #this is important for making sure your stand IDs do not get translated to scientific notation

#This takes the rx package information, translates it, and prepares it for 
#updating the scenario_additional_harvest_costs table. 
setwd("H:/cec_20170915/db/")
conn <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=H:/cec_20170915/db/fvsmaster.mdb")
fvsmaster_PkgLabels <- sqlFetch(conn, "PkgLabels", as.is = TRUE)
fvsmaster_rx_pkg_xwalk <- sqlFetch(conn, "rx_pkg_xwalk", as.is = TRUE)
fvsmaster_rxpackage <- sqlFetch(conn, "rxpackage", as.is = TRUE)
fvsmaster_rx_harvest_cost_columns <- sqlFetch(conn, "rx_harvest_cost_columns", as.is = TRUE)

# fvsmaster_rxpackage <- unique(fvsmaster_rxpackage)
# sqlQuery(conn, 'DROP TABLE rxpackage')
# sqlSave(conn, dat = fvsmaster_rxpackage, tablename = "rxpackage", rownames = FALSE)


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


sqlQuery(conn, 'DROP TABLE rx_harvest_cost_columns')

odbcCloseAll()
conn <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=H:/cec_20170915/db/fvsmaster.mdb")
sqlQuery(conn, 'DROP TABLE rx_harvest_cost_columns')
sqlSave(conn, dat = rx_addcost_update, tablename = "rx_harvest_cost_columns", rownames = FALSE)
odbcCloseAll()


conn <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=H:/cec_20170915/db/master.mdb")
master_cond <- sqlFetch(conn, "cond", as.is = TRUE)
odbcCloseAll()

#run once through processor to make sure this table is populated
conn <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=H:/cec_20170915/processor/db/scenario_processor_rule_definitions.mdb")
scenario_additional_harvest_costs <- sqlFetch(conn, "scenario_additional_harvest_costs", as.is = TRUE)


slope <- master_cond[,c(1,16)]

scenario_update <- merge(scenario_additional_harvest_costs, slope, by = "biosum_cond_id")
names(rx_addcost2)[4] <- "rx"
scenario_update <- merge(scenario_update, rx_addcost2, by = "rx")

scenario_update$RX_burn[scenario_update$ColumnName == "RX_burn" & scenario_update$slope <= 40] <- 75
scenario_update$RX_burn[scenario_update$ColumnName == "RX_burn" & scenario_update$slope > 40] <- 150
scenario_update$Masticate[scenario_update$ColumnName == "Masticate" & scenario_update$slope <= 40] <- 250
scenario_update$Lop_and_scatter[scenario_update$ColumnName == "Lop_and_scatter" & scenario_update$slope <= 40] <- 35
scenario_update$Lop_and_scatter[scenario_update$ColumnName == "Lop_and_scatter" & scenario_update$slope > 40] <- 60
scenario_update$Pile_burn[scenario_update$ColumnName == "Pile_burn" & scenario_update$slope <= 40 & scenario_update$Type == "WT"] <- 125
scenario_update$Pile_burn[scenario_update$ColumnName == "Pile_burn" & scenario_update$slope <= 40 & scenario_update$Type == "CTL"] <- 250

sqlQuery(conn, 'DROP TABLE addl_harvest_cost_update')
sqlSave(conn, dat = scenario_update, tablename = "addl_harvest_cost_update", rownames = FALSE)

scenario_additional_harvest_costs <- scenario_update[,c(3,2,1,4,5,6,7,8)]
sqlQuery(conn, 'DROP TABLE scenario_additional_harvest_costs')
sqlSave(conn, dat = scenario_additional_harvest_costs, tablename = "scenario_additional_harvest_costs", rownames = FALSE)

sqlQuery(conn, 'UPDATE addl_harvest_cost_update INNER JOIN scenario_additional_harvest_costs ON (addl_harvest_cost_update.scenario_id = scenario_additional_harvest_costs.scenario_id) AND (addl_harvest_cost_update.biosum_cond_id = scenario_additional_harvest_costs.biosum_cond_id) AND (addl_harvest_cost_update.rx = scenario_additional_harvest_costs.rx) SET scenario_additional_harvest_costs.RX_burn = [addl_harvest_cost_update].[RX_burn];')
sqlQuery(conn, 'UPDATE addl_harvest_cost_update INNER JOIN scenario_additional_harvest_costs ON (addl_harvest_cost_update.scenario_id = scenario_additional_harvest_costs.scenario_id) AND (addl_harvest_cost_update.biosum_cond_id = scenario_additional_harvest_costs.biosum_cond_id) AND (addl_harvest_cost_update.rx = scenario_additional_harvest_costs.rx) SET scenario_additional_harvest_costs.Pile_burn = [addl_harvest_cost_update].[Pile_burn];')
sqlQuery(conn, 'UPDATE addl_harvest_cost_update INNER JOIN scenario_additional_harvest_costs ON (addl_harvest_cost_update.scenario_id = scenario_additional_harvest_costs.scenario_id) AND (addl_harvest_cost_update.biosum_cond_id = scenario_additional_harvest_costs.biosum_cond_id) AND (addl_harvest_cost_update.rx = scenario_additional_harvest_costs.rx) SET scenario_additional_harvest_costs.Masticate = [addl_harvest_cost_update].[Masticate];')
sqlQuery(conn, 'UPDATE addl_harvest_cost_update INNER JOIN scenario_additional_harvest_costs ON (addl_harvest_cost_update.scenario_id = scenario_additional_harvest_costs.scenario_id) AND (addl_harvest_cost_update.biosum_cond_id = scenario_additional_harvest_costs.biosum_cond_id) AND (addl_harvest_cost_update.rx = scenario_additional_harvest_costs.rx) SET scenario_additional_harvest_costs.Lop_and_scatter = [addl_harvest_cost_update].[Lop_and_scatter];')
sqlQuery(conn, 'UPDATE addl_harvest_cost_update INNER JOIN scenario_additional_harvest_costs ON (addl_harvest_cost_update.scenario_id = scenario_additional_harvest_costs.scenario_id) AND (addl_harvest_cost_update.biosum_cond_id = scenario_additional_harvest_costs.biosum_cond_id) AND (addl_harvest_cost_update.rx = scenario_additional_harvest_costs.rx) SET scenario_additional_harvest_costs.None = [addl_harvest_cost_update].[None];')

odbcCloseAll()
#write.csv(scenario_update, "scenario_update.csv")

scenario_problems1 <- subset(scenario_update, ColumnName == "Masticate" & slope > 40)
scenario_problems2 <- subset(scenario_update, ColumnName == "Pile_and_burn" & slope > 40)

scenario_problems <- rbind(scenario_problems1, scenario_problems2)

#write.csv(scenario_problems, "scenario_problems.csv")
