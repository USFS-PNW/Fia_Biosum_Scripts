

#This script was created by Carlin Starrs in 2018
#The script takes a new project and modifies master.mdb to filter
#conditions for reserve code not equal to 1 and land class code equal to 1

#Make sure you have the Microsoft Access Databse Engine Driver https://www.microsoft.com/en-us/download/confirmation.aspx?id=23734
#and you are using 32-bit R (set in RStudio by going to Tools -> Global Options)

library("dplyr") #if you do not have these packages installed, enter "install.packages("packagename") into the console, then load them using this line.
library("RODBC")
options(scipen = 999) #this is important for making sure your stand IDs do not get translated to scientific notation

#create a master_package database that has the BA, less, and QMD thresholds for cutting
conn <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=H:/cec_20180515/db/master.mdb") #set to your project master directory location
cond <- sqlFetch(conn, "cond", as.is = TRUE)
plot <- sqlFetch(conn, "plot", as.is = TRUE)
tree <- sqlFetch(conn, "tree", as.is = TRUE)

conditions.to.remove <- cond$biosum_cond_id[cond$reservcd == 1 | cond$landclcd != 1] #get condition ids where reservcd is equal to 1 or landclcd is not equal to 1
plots.to.remove <- cond$biosum_plot_id[cond$reservcd == 1 | cond$landclcd != 1] #get plot ids where reservcd is equal to 1 or landclcd is not equal to 1

cond <- cond[!cond$biosum_cond_id %in% conditions.to.remove,]
plot <- plot[!plot$biosum_plot_id %in% conditions.to.remove,]
tree <- tree[!tree$biosum_cond_id %in% conditions.to.remove,]

sqlQuery(conn, 'SELECT * INTO cond_backup FROM cond')
sqlQuery(conn, 'SELECT * INTO plot_backup FROM plot')
sqlQuery(conn, 'SELECT * INTO tree_backup FROM tree')

sqlQuery(conn, 'DROP TABLE cond')
sqlQuery(conn, 'DROP TABLE plot')
sqlQuery(conn, 'DROP TABLE tree')

sqlSave(conn, dat = cond, tablename = "cond", rownames = FALSE)
sqlSave(conn, dat = plot, tablename = "plot", rownames = FALSE)
sqlSave(conn, dat = tree, tablename = "tree", rownames = FALSE)

odbcCloseAll()
