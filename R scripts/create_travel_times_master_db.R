library(DBI)
library(RSQLite)
library(tidyverse)

## --------------------- User Input ---------------------

# Cost CSV file you want to add to the database
costcsv <- "cost.csv"

# Plot CSV file you want to add to the database
plotcsv <- "plots.csv"

# Processing site CSV file you want to add to the database
psitecsv <- "processingsites.csv"

# CSV file with move distances from moved_plots geopackage
movedistcsv <- "moved_plots.csv"

# set to TRUE to get rid of records over a certain travel time
# set to FALSE to keep all records
pareflag <- TRUE

# max travel time used to pare down table
maxtime <- 5

## --------------------- End User Input ---------------------

startTime <- Sys.time()

database <- "databases/gis_travel_times_master.db"

# connect to db
con <- dbConnect(RSQLite::SQLite(), dbname = database)
print("database connected")

# create travel_time table
sql <- "CREATE TABLE travel_time (TRAVELTIME_ID INTEGER PRIMARY KEY AUTOINCREMENT, 
        STATECD INTEGER, PLOT INTEGER, PSITE_ID INTEGER, TRAVEL_MODE INTEGER,
        ONE_WAY_HOURS DOUBLE, RAILHEAD_ID INTEGER, COLLECTOR_ID INTEGER)"
dbExecute(con, sql)
print("travel_time table created")

# load data into plot_gis table
df <- read.csv(plotcsv, header=TRUE)
dbWriteTable(con, "plot_gis", df, row.names = FALSE, append = TRUE)
print("data loaded into plot_gis table")

# load data into processing_site table
df <- read.csv(psitecsv, header=TRUE)
dbWriteTable(con, "processing_site", df, row.names = FALSE, append = TRUE)
print("data loaded into processing_site table")

# load data into travel_time table
df <- read.csv(costcsv, row.names = 1)
df_long <- df %>%
  rownames_to_column(var = 'PLOT_CN') %>%
  pivot_longer(-'PLOT_CN', names_to = 'PSITE_ID', values_to = 'ONE_WAY_HOURS')
dbWriteTable(con, "travel_time_temp", df_long, row.names = FALSE, append = TRUE)
dbExecute(con, "UPDATE travel_time_temp SET PSITE_ID = SUBSTR(PSITE_ID, 2) 
           WHERE PSITE_ID LIKE 'X%';")

# pare down database to maxhours if flag is set to true
note <- ""
if (pareflag == TRUE)
{
  sql <- paste("DELETE FROM travel_time_temp WHERE ONE_WAY_HOURS > ", maxtime)
  dbExecute(con, sql)
  note <- paste("database pared down. travel times over", maxtime, "hours deleted and")
}

sql <- "INSERT INTO travel_time (STATECD, PLOT, PSITE_ID, ONE_WAY_HOURS) 
        SELECT PLOT_CN / 100000 AS STATECD, PLOT_CN % 100000 AS PLOT, 
        PSITE_ID, ONE_WAY_HOURS FROM travel_time_temp"
dbExecute(con, sql)
dbExecute(con, "DROP TABLE travel_time_temp")
print(paste(note, "data added to travel_time table"))


# delete any plots not in the travel_time table
sql <- "DELETE FROM plot_gis WHERE PLOT NOT IN (SELECT DISTINCT PLOT FROM travel_time)"
dbExecute(con, sql)
print("plots not in travel_time table deleted")

# delete any psites not in the travel_time table
sql <- "DELETE FROM processing_site WHERE PSITE_ID NOT IN (SELECT DISTINCT PSITE_ID
        FROM travel_time)"
dbExecute(con, sql)
print("psites not in travel_time table deleted")

# add move distances to plot_gis table
df <- read.csv(movedistcsv, header=TRUE)
dbWriteTable(con, "move_dist", df, row.names = FALSE, append = TRUE)
sql <- "ALTER TABLE plot_gis ADD COLUMN MoveDist_ft_NEAR INTEGER"
dbExecute(con, sql)
sql <- "UPDATE plot_gis SET MoveDist_ft_NEAR =3.28084 * (SELECT MOVEDIST FROM move_dist AS m 
        WHERE m.PLOT_CN = plot_gis.PLOT_CN)"
dbExecute(con, sql)
dbExecute(con, "DROP TABLE move_dist")
sql <- "ALTER TABLE plot_gis DROP COLUMN PLOT_CN"
dbExecute(con, sql)
print("move distance values added to plot_gis table")

# add STATECDs to travel_time
sql <- "UPDATE travel_time SET STATECD = (SELECT STATECD FROM plot_gis AS p
        WHERE p.PLOT = travel_time.PLOT)"
dbExecute(con, sql)
print("STATECDs for plots added to travel_time table")

# update any psites with a BIOCD of 4
sql <- "ALTER TABLE processing_site ADD COLUMN NOTES CHAR(100)"
dbExecute(con, sql)
sql <- "UPDATE processing_site SET NOTES = 
        'Other processing site type. Set to merchantable, but could have restrictions' 
        WHERE BIOCD = 4"
dbExecute(con, sql)
sql <- "UPDATE processing_site SET BIOCD = 1 WHERE BIOCD = 4"
dbExecute(con, sql)
print("psites with BIOCD of 4 updated")

# disconnect
dbDisconnect(con)
print("done! database disconnected")

s <- as.numeric(as.character(round(difftime(Sys.time(),startTime,units="secs"),2)))
sUnit <- "second(s)"
if(s > 60) {s <- round(s/60.0, 2); sUnit <- "minute(s)"}
if(s > 60) {s <- round(s/60.0, 2); sUnit <- "hour(s)"}
print(paste("time elapsed: ", s, sUnit))