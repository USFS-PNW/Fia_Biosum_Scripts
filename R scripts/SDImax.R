#This script was created by Sara Loreno in 2017. It is designed to:
#Select stands that have a single dominate species (a species must make up >80% of the total BA)
#Group stands by the dominant species
#Calculate summary statistics for each species group, including SDI percentiles 
#(max, 98%, 95%, 90%, and 85%, and the number of stands in each group)
#Remove groups with less than 100 stands
#Subset by variant
#Create SDImax keywords using the 85 percentile as the maximum SDI, 55% of max 
#density as when when mortality is introduced, and 75% of max density as when actual max density is reached. 

setwd("G:/projects/FSC_CaseStudies_2016/Data/Work/AllAreas/SDImax") #Set your working directory to the location where your FVS tables were exported

#load packages. If packages are not already installed, type "install.packges("{packagename}") into the console below for each package name
#For example: install.packages("reshape2")

library(reshape2)
library(sqldf)
library(tcltk2)
library(gdata)
library(plyr)

#read in FVS_Cases table. Make sure the tables you exported from access have the same name as below. If they have
#a .txt extension, change the extension file name to txt (but not the function), e.g. read.csv("FVS_Cases.txt")

cases = read.csv("FVS_Cases.csv")  

#read in FVS_Compute table
compute = read.csv("FVS_Compute.csv")

#read in FVS_StrClass table
structure = read.csv("FVS_StrClass.csv") 

#read in FVS_Summary table
summary = read.csv("FVS_Summary.csv") 

#SQL query to select variables of interest from each table into dataframe compile.  Only select stands that have a dominant
#species with >80% of total basal area
compile<-sqldf("SELECT compute.Year, compute.PERBA1, structure.Stratum_1_Species_1, summary.SDI, cases.Variant
                FROM ((compute INNER JOIN structure ON (compute.Year = structure.Year) AND (compute.StandID = structure.StandID)) 
                INNER JOIN summary ON (structure.Year = summary.Year) AND (structure.StandID = summary.StandID)) 
                INNER JOIN cases ON compute.CaseID = cases.CaseID
                WHERE (((compute.Year)=2014) AND ((compute.PERBA1)>0.8));")

#group by variant and species type
groupColumns = c("Variant","Stratum_1_Species_1")

#calculate summary stats: 98%, 95%, and 90%, count of records in group
stats<-ddply(compile, groupColumns, summarise, Max = max(SDI), SDI98 = quantile(SDI, .98), SDI95 = quantile(SDI, .95),
             SDI90 = quantile(SDI, .90),  SDI85=round(SDI95*.85), freq=length(Variant))

#subset data to only species with 100+ counts and remove -- species
stats100<-subset(stats, stats$freq>=100 & stats$Stratum_1_Species_1!="--")

#create variant specific tables. Make sure to change "WC" to whatever variant you are running. 
#For example, if you are running the CA variant: CA_stats<-subset(stats100, stats100$Variant=="CA")

WC_stats<-subset(stats100, stats100$Variant=="WC")

#function to write variant KCP SDImax files
f <- function(x, output) {
  species <- x[2]
  SDI85 <- x[7]
  print(paste("SDIMAX            ", species, "       ", SDI85, "                           55.        75", sep=""))
  cat(paste("SDIMAX            ", species, "       ", SDI85, "                           55.        75", sep=""), 
      file= output, append = T, fill = T)
}

#write SDImax KCP files. Make sure you change the variant name to whatever variant you are running, and that the
#"WC_stats" matches your variable you named above. For example, if you are running the CA variant: 
#apply(CA_stats, 1, f, output = 'CA_SDImax.KCP')
apply(WC_stats, 1, f, output = 'WC_SDImax.KCP')

