## Simple example code for pulling .accdb tables into R, reformatting column data types, and saving the reformated table(s) back into the target .accdb

library(dplyr)
library(RODBC)
library(RSQLite)

#Load NIMS_TREE table, reformat data type, resave in .accdb
con <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=X:/FERS_Data/data_source/NIMS_FIRE_2022.accdb") #Connect to .accdb

NIMS_TREE <- sqlFetch(con, "NIMS_TREE", as.is = TRUE) #load in the NIMS_TREE table

NIMS_TREE2 <- NIMS_TREE %>% 
  mutate_at(c(13,21,42,44:55,65:67,79,90,93:103,105:114,147,161), as.numeric) #convert "decimal" fields into double

NIMS_TREE2 <- NIMS_TREE2 %>% 
  mutate_at(c(2:8,11:12,14:20,22:41,43,56:64,68:78,80:89,91,92,125,126,128:134,136:146,148:160,162:164,166:181), as.integer) #convert "decimal" fields into integer


sqlSave(con,NIMS_TREE2, rownames = FALSE)#save reformated NIMS_TREE table
odbcClose(con) #close connection