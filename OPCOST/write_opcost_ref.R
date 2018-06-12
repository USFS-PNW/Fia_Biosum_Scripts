###write opcost_ref.accdb###
setwd("E:/Dropbox/Carlin/GitHub/Fia_Biosum_Scripts/OPCOST")
opcost_equation_ref <- read.csv("opcost_equation_ref.csv")
opcost_units <- read.csv("opcost_units.csv")
opcost_cost_ref <- read.csv("opcost_cost_ref.csv")
opcost_harvestsystem_ref <- read.csv("opcost_harvestsystem_ref.csv")
opcost_ideal_ref <- read.csv("opcost_ideal_ref.csv")

conn <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=E:/Dropbox/Carlin/GitHub/Fia_Biosum_Scripts/OPCOST/opcost_ref.ACCDB") #Change the text after "DBH=" to the correct directory for your project
sqlQuery(conn, 'DROP TABLE opcost_equation_ref')
sqlQuery(conn, 'DROP TABLE opcost_units')
sqlQuery(conn, 'DROP TABLE opcost_cost_ref')
sqlQuery(conn, 'DROP TABLE opcost_harvestsystem_ref')
sqlQuery(conn, 'DROP TABLE opcost_ideal_ref')

sqlSave(conn, opcost_equation_ref, "opcost_equation_ref", colnames = TRUE, rownames = FALSE)
sqlSave(conn, opcost_units, "opcost_units", colnames = TRUE, rownames = FALSE)
sqlSave(conn, opcost_cost_ref, "opcost_cost_ref", colnames = TRUE, rownames = FALSE)
sqlSave(conn, opcost_harvestsystem_ref, "opcost_harvestsystem_ref", colnames = TRUE, rownames = FALSE)
sqlSave(conn, opcost_ideal_ref, "opcost_ideal_ref", colnames = TRUE, rownames = FALSE)

odbcCloseAll()
