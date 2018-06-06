#Translate plot data afer running the "Near" function in ArcGIS to get 
#Yarding distance for each plot. This takes the output attribute table, 
#trims to relevant columns, converts from meters to feet, rounds it to 
#an integer, then sets everything below 100 to 100. The final csv can be 
#imported into the master.mdb file and used to update the gis_yard_dist
#value in the plot table by linking plot (NOT biosum_plot_id), measyear, 
#invyr, statecd, and fvs_variant. 
library("dplyr")
library("RODBC")
options(scipen = 999)

setwd("H:/cec_20170915/gis/")

#plot_near.txt is in the additional data on github
plot_near <- read.csv("plot_near.txt", colClasses = c(rep("numeric",3), "character", rep("numeric", 9), rep("character", 8), "numeric", "character", rep("numeric", 17)))
yard_dist_2017 <- plot_near[,c(4:6,9:10,14, 17:21,38)]
yard_dist_2017$gis_yard_d <- yard_dist_2017$NEAR_DIST * 3.28084
yard_dist_2017$gis_yard_d <- round(yard_dist_2017$gis_yard_d,0)
yard_dist_2017$gis_yard_d <- ifelse(yard_dist_2017$gis_yard_d < 100, 100, yard_dist_2017$gis_yard_d)
names(yard_dist_2017)[7] <- "gis_yard_dist"
names(yard_dist_2017)[1] <- "biosum_plot_id"
yard_dist_2017$NEAR_DIST <- NULL

write.csv(yard_dist_2017, "yard_dist_2017.csv")

#then merge this in to the master/plot table to update gis_yard_dist based on the plot_id/plot values (biosum_plot_id gets
#screwed up and isn't correct)