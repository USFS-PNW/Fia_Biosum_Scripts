## Simple example code for pulling .accdb tables into R, reformatting column data types, and saving the reformated table(s) back into the target .accdb

library(dplyr)
library(RODBC)
library(RSQLite)

#Load NIMS_TREE table, reformat data type, resave in .accdb
con <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=D:/BioSum_Data/wFERS/NIMS_FIRE_2022.accdb") #Connect to .accdb

NIMS_TREE2 <- sqlFetch(con, "NIMS_TREE", as.is = TRUE) #load in the NIMS_TREE table
NIMS_TREE2 <- NIMS_TREE2 %>% 
  mutate_at(c(2:8,11:103,105:114,125,126,128:134,136:164,166:181), as.integer) #convert "decimal" fields into integer

sqlSave(con,NIMS_TREE2, rownames = FALSE)#save reformated NIMS_TREE table
odbcClose(con) #close connection