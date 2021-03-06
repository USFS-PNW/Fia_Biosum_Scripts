#This script was created by Sara Loreno in 2017 and modified by Carlin Starrs in 2018
#It is designed to:
#Select stands that have a single dominate species (a species must make up >80% of the total BA)
#Group stands by the dominant species
#Calculate summary statistics for each species group, including SDI percentiles 
#(max, 98%, 95%, 90%, and 85%, and the number of stands in each group)
#Remove groups with less than 100 stands
#Subset by variant
#Create SDImax keywords using the 85 percentile as the maximum SDI, 55% of max 
#density as when when mortality is introduced, and 75% of max density as when actual max density is reached. 


#Make sure you have the Microsoft Access Databse Engine Driver https://www.microsoft.com/en-us/download/confirmation.aspx?id=23734
#and you are using 32-bit R (set in RStudio by going to Tools -> Global Options)

#install & load missing packages

packages <- c("RODBC", "dplyr")

package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, repos="http://cran.r-project.org", dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})


options(scipen = 999) #this is important for making sure your stand IDs do not get translated to scientific notation

#Project root location:
project.location <- "H:/cec_20180529"

create_SDImax <- function(project.location) {
  variants <- list.files(file.path(project.location, "fvs", "data"))
  variants <- variants[!variants %in% "CR"]
  for(i in 1:length(variants)) {
    #read in tables from FVS_SDImax_out
    setwd(file.path(project.location, "fvs", "data", variants[i]))
    file <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", file.path(project.location, "fvs", "data", variants[i]), "/FVS_SDImax_out.mdb")
    conn <- odbcDriverConnect(file)
    cases <- sqlFetch(conn, "FVS_Cases", as.is = TRUE)
    compute <- sqlFetch(conn, "FVS_Compute", as.is = TRUE)
    structure <- sqlFetch(conn, "FVS_StrClass", as.is = TRUE)
    summary <- sqlFetch(conn, "FVS_Summary", as.is = TRUE)
    odbcCloseAll()
    
    #select variables of interest from each table into dataframe compile.  Only select stands that have a dominant
    #species with >80% of total basal area
    mylist <- list(mode = "vector", length = 4)
    mylist[[1]] <- cases[,c(which(names(cases) %in% c("StandID", "Variant")))]
    mylist[[2]] <- compute[,c(which(names(compute) %in% c("StandID", "Year", "PERBA1")))]
    mylist[[3]] <- structure[,c(which(names(structure) %in% c("StandID", "Year", "Stratum_1_Species_1")))]
    mylist[[4]] <- summary[,c(which(names(summary) %in% c("StandID", "Year", "SDI")))]
    
    all <- Reduce(merge, mylist)
    compile <- all[all$Year == 2014 & all$PERBA1 > 0.8,]
    
    #calculate summary stats: 98%, 95%, and 90%, count of records in group
    stats <- compile %>% dplyr::group_by(Variant, Stratum_1_Species_1) %>% dplyr::summarise(max = max(SDI),
                                                                           SDI98 = quantile(SDI, .98), 
                                                                           SDI95 = quantile(SDI, .95), 
                                                                           SDI90 = quantile(SDI, .90),
                                                                           SDI85 = round(quantile(SDI, .95) * 0.85),
                                                                           freq = n()) 
    
    #subset data to only species with 100+ counts and remove -- species
    stats100 <- stats[stats$freq>=100 & stats$Stratum_1_Species_1!="--",]
    
    #create variant specific tables. Make sure to change "WC" to whatever variant you are running. 
    #For example, if you are running the CA variant: CA_stats<-subset(stats100, stats100$Variant=="CA")
    
    stats2<- stats100[stats100$Variant==variants[i],]
    
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
    filename <- paste0(variants[i], "_SDImax.KCP")
    apply(stats2, 1, f, output = filename)
  }
}

create_SDImax(project.location)

