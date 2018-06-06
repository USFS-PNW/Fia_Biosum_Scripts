
library("dplyr") #if you do not have these packages installed, enter "install.packages("packagename") into the console, then load them using this line.
library("RODBC")
options(scipen = 999) #this is important for making sure your stand IDs do not get translated to scientific notation

#bring in master tree table
conn <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=H:/cec_20170915/db/master.mdb")
master_tree <- sqlFetch(conn, "tree", as.is = TRUE) #import package labels table from fvsmaster.mdb NOTE: this table was manually added earlier
master_cond <- sqlFetch(conn, "cond", as.is = TRUE) #import package labels table from fvsmaster.mdb NOTE: this table was manually added earlier
master_plot <- sqlFetch(conn, "plot", as.is = TRUE) #import package labels table from fvsmaster.mdb NOTE: this table was manually added earlier
odbcCloseAll()

master_cond_merge <- data.frame("biosum_cond_id" = master_cond$biosum_cond_id,
                                "biosum_plot_id" = master_cond$biosum_plot_id,
                                "fortypcd" = master_cond$fortypcd, 
                                "slope" = master_cond$slope, 
                                "owncd" = master_cond$owncd)

master_plot_merge <- data.frame("biosum_plot_id" = master_plot$biosum_plot_id, 
                                "fvs_variant" = master_plot$fvs_variant)

master_merge <- merge(master_cond_merge, master_plot_merge, by = "biosum_plot_id")

master_merge$biosum_plot_id <- NULL

conn <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=H:/cec_20170915/processor/scenario1/db/scenario_results.mdb")
harvest_costs <- sqlFetch(conn, "harvest_costs", as.is = TRUE) #import package labels table from fvsmaster.mdb NOTE: this table was manually added earlier
tree_vol_val <- sqlFetch(conn, "tree_vol_val_by_species_diam_groups", as.is = TRUE) #import package labels table from fvsmaster.mdb NOTE: this table was manually added earlier

harvest_costs_master_merge <- merge(harvest_costs, master_merge, by = "biosum_cond_id")
sqlSave(conn, dat = harvest_costs_master_merge, tablename = "harvest_costs_master_merge", rownames = FALSE)

tree_vol_val_master_merge <- merge(tree_vol_val, master_merge, by = "biosum_cond_id")
sqlSave(conn, dat = tree_vol_val_master_merge, tablename = "tree_vol_val_master_merge", rownames = FALSE)

odbcCloseAll()
