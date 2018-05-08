#Package Figures
library("dplyr")
library("RODBC")
library("ggplot2")
library("gridExtra")
library("reshape2")
options(scipen = 999)


#####################################################################################################################
#######################IF USING THE MortVolFigureData_Master.csv FILE SKIP DIRECTLY TO LINE 120#####################
#####################################################################################################################


#Get the PREPOST data from PRE_FVS_SUMMARY and POST_FVS_SUMMARY in the fvs/db directory
conn <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=H:/cec_20170915/fvs/db/PREPOST_FVS_SUMMARY.ACCDB") #Change the text after "DBH=" to the correct directory for your project
pre <- sqlFetch(conn, "PRE_FVS_SUMMARY", as.is = TRUE) 
post <- sqlFetch(conn, "POST_FVS_SUMMARY", as.is = TRUE) 
odbcCloseAll()

conn <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=H:/cec_20170915/db/master.mdb") #Change the text after "DBH=" to the correct directory for your project
master_cond <- sqlFetch(conn, "cond", as.is = TRUE) 
odbcCloseAll()

#Crosswalk forest type code, Set your working directory by updating the directory in the line below or by going to 
#"Session" -> "Set working directory" in the menu at the top. If you do that, do not run the line starting with 
#"setwd" below. The working directory should be set to wherever the CEC-ftype.csv file is. This is also where any
#outputs will be saved to.
setwd("G:/Dropbox/Carlin/Berkeley/biosum")
ftype <- read.csv("CEC_ftype.csv") #This is downloaded separately

names(ftype)[1] <- "fortypcd"
names(ftype)[3] <- "CEC_type"
ftype <- ftype[,c(1,3)]

master_cond<- merge(master_cond, ftype)

conn <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=H:/cec_20170915/core/scenario6_20180508/db/scenario_results.mdb") #Change the text after "DBH=" to the correct directory for your project
net_rev <- sqlFetch(conn, "product_yields_net_rev_costs_summary_by_rxpackage", as.is = TRUE) 
effective <- sqlFetch(conn, "cycle1_effective", as.is = TRUE)
valid_combos <- sqlFetch(conn, "validcombos_fvsprepost", as.is = TRUE)
cycle1_best_rx <- sqlFetch(conn, "cycle1_best_rx_summary", as.is = TRUE)
stand_cr <- sqlFetch(conn, "stand_costs_revenue_volume_by_rx", as.is = TRUE)
odbcCloseAll()

###COND DATA###
#The section below connects to the master_cond table and pulls in forest type code and owner code values from master_cond,
#then limits to owncd 11 and 46 and to CEC relevant forest types. 
pattern <- c("biosum_cond_id", "rxpackage", "fortypcd", "owncd", "owngrpcd", "acres", "CEC_type")
cond.relevant.data <- master_cond[,c(which(grepl(paste0(pattern, collapse = "|"), names(master_cond))))]
cond.relevant.data <- cond.relevant.data[cond.relevant.data$owncd == 11 |  cond.relevant.data$owncd == 46,]
cond.relevant.data <- cond.relevant.data[cond.relevant.data$CEC_type %in% c("Douglas-fir", "Pine", "Mixed conifer", "True fir"),]

#####Get total treated acres####
valid.acres <- merge(cond.relevant.data, valid_combos)
valid.acres.summary <- valid.acres %>% group_by(CEC_type, rxpackage, rxcycle, owngrpcd) %>% summarise(Treatable_Acres = sum(acres))
treated.area <- valid.acres.summary[valid.acres.summary$rxpackage == "031" & valid.acres.summary$rxcycle == "1",]
write.csv(treated.area, paste0("TreatedArea_Master_", format(Sys.Date(), "%Y%m%d"), ".csv"), row.names = FALSE)

####PREPOST DATA####
#Calculate mort vol pct, change NA values to 0
pre$MortVol_FOFEM[pre$MortVol_FOFEM == 0] <- 0
pre$MortVolPct_FOFEM <- pre$MortVol_FOFEM/pre$VolSum
pre$MortVolPct_FOFEM[pre$VolSum == 0] <- 0

post$MortVol_FOFEM[post$MortVol_FOFEM == 0] <- 0
post$MortVolPct_FOFEM <- post$MortVol_FOFEM/post$VolSum
post$MortVolPct_FOFEM[post$VolSum == 0] <- 0

#Get relevant columns from PREPOST and core data
pattern <- c("biosum_cond_id", "rxpackage", "rx", "rxcycle", "Year", "fvs_variant", "TCuFt", "MortVol_FOFEM", "MortVolPct_FOFEM", "RTpa", "RTCuFt", "Acc",
             "Mort", "Canopy_Density", "CBH", "Avg_CBD_Score", "Avg_CBH_Score", "Avg_PERRESBA_Score", "Avg_SurvVolRatio_Score", "Avg_FRS", "Avg_HS_Sev", "Avg_HS_Mod")
pre.relevant.data <- pre[,c(which(grepl(paste0(pattern, collapse = "|"), names(pre))))]
post.relevant.data <- post[,c(which(grepl(paste0(pattern, collapse = "|"), names(post))))]

all.data <- rbind(pre.relevant.data, post.relevant.data) #combine pre and post data

#merge with cond.relevant.data. This 
all.data <- merge(all.data, cond.relevant.data)

##De-annualize accretion and mort
# Accretion and mortality are expressed in total stem cubic feet in western variants and
# merchantable stem cubic feet of pulpwood in eastern variants. Accretion is growth on
# trees surviving to the end of the cycle
Years <- data.frame(unique(all.data$Year)) #get unique years
names(Years)[1] <- "Year"
Years <- Years[order(Years$Year),] #sort by year
Years <- data.frame("Year" = Years)

for (i in 1:(nrow(Years)-1)) {
  Years$Diff[i] <- Years$Year[i+1] - Years$Year[i]
} #get difference in years from one year cycle to another

all.data <- merge(all.data, Years, by = c("Year")) #add "year difference" to all.data

all.data$Period_Acc <- all.data$Acc * all.data$Diff #multiply accretion by year difference to get period accretion
all.data$Period_Mort <- all.data$Mort * all.data$Diff #multiply mortality by year difference to get period mortality

# all.data$Period_TCuFt <- all.data$TCuFt * all.data$Diff 
# all.data$Period_RTCuFt <- all.data$RTCuFt * all.data$Diff 

#sum period accretion and mortality by stand
sums.all.data <- all.data %>% group_by(biosum_cond_id, rxpackage) %>% summarise(Period_Acc_sum = sum(Period_Acc), Period_Mort_sum = sum(Period_Mort)) 

#add stand initial TCuFt (TCuFt from cycle 1)
initial <- pre.relevant.data[pre.relevant.data$rxcycle == 1,]
initial <- data.frame("biosum_cond_id" = initial$biosum_cond_id, "rxpackage" = initial$rxpackage, "Initial.TCuFt" = initial$TCuFt)

#merge stand level data for inital TCuFt and accretion/mort
packagestand_level_data <- merge(sums.all.data, initial, by = c("biosum_cond_id", "rxpackage"), all = TRUE)

#Separate out package 31 data
package31 <- all.data[all.data$rxpackage == "031",]

#Separate out other packages
all.data2 <- all.data[all.data$rxpackage != "031",]

#Add column with 0 for RTpa values < 0, 1 for RTpva values > 0
all.data2$RTpa_YN <- ifelse(all.data2$RTpa > 0, 1, 0)

#Sum the RTpa_YN values by stand and package
treated_stands <- all.data2 %>% group_by(biosum_cond_id, rxpackage) %>% summarise(RTpa_SUM = sum(RTpa_YN))

#Remove stand-packages where RTpa_YN is 0 (e.g. stands that were never treated)
treated_stands2 <- treated_stands[treated_stands$RTpa_SUM > 0,]

all.data3 <- merge(treated_stands2, all.data2)

#Remove added RTpa_YN column
all.data3$RTpa_YN <- NULL
all.data3$RTpa_SUM <- NULL

#Add package 31 data back in
relevant.data2 <- rbind(all.data3, package31)

#The following calculates package averages for the score values by removing cycle 1
#Remove cycle 1 from PRE
Year1.data <- relevant.data2[relevant.data2$Year == "1",]

#Merge pre and post
NonYear1.data <- relevant.data2[relevant.data2$Year != "1",]

#Average the MortVol_FOFEM and MortVolPct_FOFEM values by stand-package
NonYear1_packagestand_data <- NonYear1.data %>% group_by(biosum_cond_id, rxpackage, rx) %>% summarise(Avg_MortVol_FOFEM = mean(MortVol_FOFEM),
                                                                                                        Avg_MortVolPct_FOFEM = mean(MortVolPct_FOFEM), 
                                                                                                        Avg_CBH = mean(CBH),
                                                                                                        Avg_Canopy_Density = mean(Canopy_Density), 
                                                                                                        Avg_HS_Sev = mean(Avg_HS_Sev),
                                                                                                        Avg_FRS = mean(Avg_FRS), 
                                                                                                        Avg_CBD_Score = mean(Avg_CBD_Score),
                                                                                                        Avg_PERRESBA_Score = mean(Avg_PERRESBA_Score), 
                                                                                                        Avg_SurvVolRatio_Score = mean(Avg_SurvVolRatio_Score),
                                                                                                        Avg_CBH_Score = mean(Avg_CBH_Score), 
                                                                                                        Sum_RTpa = sum(RTpa), 
                                                                                                        Sum_RTCuFt = sum(RTCuFt)
                                                                                                      )


PREPOST_stand <- merge(NonYear1_packagestand_data, packagestand_level_data)

###CORE DATA####
pattern <- c("biosum_cond_id", "rxpackage", "merch_yield_gt", "chip_yield_gt", "chip_val_dpa", "merch_val_dpa", "harvest_onsite_cpa", "merch_nr_dpa", "max_nr_dpa")
core.data <- net_rev[,c(which(grepl(paste0(pattern, collapse = "|"), names(net_rev))))]

pattern <- c("biosum_cond_id", "rxpackage", "rx", "merch_vol_cf_exp")
stand.cr.relevant.data <- stand_cr[,c(which(grepl(paste0(pattern, collapse = "|"), names(stand_cr))))]
stand.cr.relevant.data <- stand.cr.relevant.data %>% group_by(biosum_cond_id, rxpackage, rx) %>% summarise(stand_sum_merch_vol_cf_exp = sum(merch_vol_cf_exp))

core.data <- merge(stand.cr.relevant.data, core.data, by = c("biosum_cond_id", "rxpackage"), all = TRUE) #merge core data

pattern <- c("biosum_cond_id", "rxpackage", "overall_effective_yn", "post_variable1_value", "variable1_change")
effective.relevant.data <- effective[,c(which(grepl(paste0(pattern, collapse = "|"), names(effective))))]

core.data <- merge(effective.relevant.data, core.data, by = c("biosum_cond_id", "rxpackage"), all = TRUE) #merge core data

#create valid column where it equals "Y" if the stand/package combination is in the valid.combos table
core.data$valid[paste(core.data$biosum_cond_id,core.data$rxpackage) %in% paste(valid_combos$biosum_cond_id, valid_combos$rxpackage)] <- "Y"
core.data$valid[!paste(core.data$biosum_cond_id,core.data$rxpackage) %in% paste(valid_combos$biosum_cond_id, valid_combos$rxpackage)] <- "N"

core.data$best[paste(core.data$biosum_cond_id,core.data$rx) %in% paste(cycle1_best_rx$biosum_cond_id, cycle1_best_rx$rx)] <- "Y"
core.data$best[!paste(core.data$biosum_cond_id,core.data$rx) %in% paste(cycle1_best_rx$biosum_cond_id, cycle1_best_rx$rx)] <- "N"

#remove rxcycle from core.data
core.data$rxcycle <- NULL
core.data$rx <- NULL

#merge in the other package-stand data from PREPOST
stand_summary <- merge(PREPOST_stand, core.data, by = c("biosum_cond_id", "rxpackage"), all.x = TRUE)
stand_summary <- merge(stand_summary, cond.relevant.data)
  
##This table should now contain relevant averages for all non year 1 (pre cycle 1) values only for stands that were treated at least once merged
#with various economic variables from core and forest type code.
write.csv(stand_summary, paste0("FigureData_Master_", format(Sys.Date(), "%Y%m%d"), ".csv"), row.names = FALSE)

####GRAPHING DATA####
package_labels <- read.csv("packagelabels.csv")
graph_data <- read.csv("graph_ref.csv")

#The get_data function takes the packagestand_data3 data, limits it to the packages to set as the "packages" parameter
#in line 150, limits it to only relevant forest types, limits it to only stands that exist in all packages, 
#puts in categories for volume and stand count, and gets all the data in the format necessary to create the 
#scatter charts. 

#The get_data function below is used to adapt the trimmed PREPOST data above 
get_relevant_stands <- function(data, packages) {
  packages <- c(packages, 31) #add package 31
  numpackages <- length(packages) #get number of packages
  data$rxpackage <- as.character(data$rxpackage) #convert 3 digit package value to integer
  data$rxpackage <- as.numeric(data$rxpackage)
  data$rxpackage <- as.integer(data$rxpackage)
  
  #translate owncd
  
  if (all(data$owncd %in% c(11,46))) {
    data$Owner[data$owncd == 11] <- "NFS"
    data$Owner[data$owncd == 46] <- "Private"
    
  } else if (all(data$owncd %in% c(11,47,48))) {
    data$Owner[data$owncd == 11] <- "NFS"
    data$Owner[data$owncd == 47] <- "Corporate"
    data$Owner[data$owncd == 48] <- "Family"
    
  }
  
  #Limit to relevant packages set by packages paramter with package 31 as reference
  relevant.data <- data[data$rxpackage %in% packages,]
  
  #Limit to relevant forest types
  relevant.data <- relevant.data[relevant.data$CEC_type %in% c("Douglas-fir", "Pine", "Mixed conifer", "True fir"),]
  
  #Count the number of times each stand appears
  relevant.data <- relevant.data %>% group_by(biosum_cond_id) %>% add_tally()
  
  if (max(relevant.data$n) > numpackages) {
    stop("ERROR: package tally incorrect")
  }
  
  #Remove packages that don't appear the same number of times as packages + reference
  relevant.stands <- relevant.data[relevant.data$n == numpackages,]
  
  relevant.stands$n <- NULL
  
  #Create volume classes
  relevant.stands$VolClass <- cut(relevant.stands$Initial.TCuFt, breaks = c(0,3000,5000, (max(relevant.stands$Initial.TCuFt)+1)), labels = c("<3000", "3000-5000", ">5000"), right= FALSE)
  
  #Tally stands by package, volume class, forest type, and owner code to get stand count
  relevant.stands <- relevant.stands %>% group_by(rxpackage, VolClass, CEC_type, Owner) %>% add_tally()
  
  names(relevant.stands)[ncol(relevant.stands)] <- "StandCount"
  
  #Create stand count classes
  relevant.stands$SC_to_print <- cut(relevant.stands$StandCount, breaks = c(0,25, (max(relevant.stands$StandCount)+1)), labels = c("<25", ">25"), right = FALSE)
  
  return(relevant.stands)
}

#stands <- get_relevant_stands(figure.data, c(4,7,17))

stands <- get_relevant_stands(stand_summary, c(4,7,17))

####PACKAGE GRAPHS####
get_package_averages <- function(stand.data) {
  stand.data$harvest_onsite_cpa[is.na(stand.data$harvest_onsite_cpa)] <- 0
  stand.data$merch_nr_dpa[is.na(stand.data$merch_nr_dpa)] <- 0
  stand.data$max_nr_dpa[is.na(stand.data$max_nr_dpa)] <- 0
  stand.data$merch_val_dpa[is.na(stand.data$merch_val_dpa)] <- 0
  stand.data$chip_val_dpa[is.na(stand.data$chip_val_dpa)] <- 0
  #gets the average values by volclass, CEC_type, Owner, Standcount values
  package_average <- stand.data %>% group_by(rxpackage, VolClass, CEC_type, Owner, StandCount, SC_to_print) %>% summarise(PkgAvg_MortVol = mean(Avg_MortVol_FOFEM), 
                                                                                                                          PkgAvg_MortVolPct = mean(Avg_MortVolPct_FOFEM), 
                                                                                                                          PkgAvg_CBH = mean(Avg_CBH), 
                                                                                                                          PkgAvg_Canopy_Density = mean(Avg_Canopy_Density), 
                                                                                                                          PkgAvg_HS_Sev = mean(Avg_HS_Sev), PkgAvg_FRS = mean(Avg_FRS),
                                                                                                                          PkgSum_harvest_onsite_cpa = sum(harvest_onsite_cpa), 
                                                                                                                          PkgSum_merch_nr_dpa = sum(merch_nr_dpa),
                                                                                                                          PkgSum_max_nr_dpa = sum(max_nr_dpa),
                                                                                                                          PkgSum_merch_val_dpa = sum(merch_val_dpa),
                                                                                                                          PkgSum_chip_val_dpa = sum(chip_val_dpa),
                                                                                                                          PkgAvg_Initial.TCuFt = mean(Initial.TCuFt), 
                                                                                                                          PkgSum_Period_Acc = sum(Period_Acc_sum), 
                                                                                                                          PkgSum_Period_Mort = sum(Period_Mort_sum), 
                                                                                                                          PkgSum_RTCuFt = sum(Sum_RTCuFt))
  
  package_average$rxpackage <- as.factor(package_average$rxpackage)
  
  return(package_average)
}

package_avgs <- get_package_averages(stands)
  
graph_package_data <- function(data, packages, graph_variable_name1, graph_variable_name2) {
  package.data <- get_package_averages(get_relevant_stands(data, packages))
  packages <- as.character(unique(package.data$rxpackage))
  numpackages <- length(packages)
  ##PARAMETERS FOR ALL GRAPHS##
  cbPalette <- c("#CC79A7", "#E69F00", "#56B4E9", "#009E73")
  
  custom_theme <- theme_set(theme_bw(base_size = 20)) +
    theme(plot.title = element_text(size = 16, hjust = 0.5),
          axis.text.x=element_text(size=14), 
          axis.text.y=element_text(size=14),
          axis.title = element_text(size = 16),
          strip.text = element_text(size = 14),
          legend.position="right",
          legend.key = element_rect(colour = "gray90"), 
          legend.key.size = unit(2, "line"),
          legend.spacing = unit(2, "line"), 
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 12, face = "bold"))
  
  relevant_packages <- package_labels[package_labels$Package %in% packages,]
  names(relevant_packages) <- c("Sequence", "FVS Style", "BA Threshold", "Max Private \n DBH", "Max Public \n DBH", "Min DBH", "Residual BA \n (less amt)", "Species \n Pressure", "Surface Fuel \n Treatment", "Harvest \n System")
  
  tt <- ttheme_default(base_size = 14)
  t<- tableGrob(relevant_packages, rows = NULL, theme = tt)
  
  layout <- rbind(c(1,1,1,1,1,2,2,2,2,2,2),
                  c(1,1,1,1,1,2,2,2,2,2,2),
                  c(1,1,1,1,1,2,2,2,2,2,2),
                  c(NA,3,3,3,3,3,3,3,3,3,NA))

  final <- NULL
  for (i in 1:numpackages) {
    if (i != numpackages) {
      add <- paste0(packages[i], "v")
      final <- paste0(final, add)
    } else {
      add <- packages[i]
      final <- paste0(final, add)
    }
  }
    
  create_variable_graph <- function(graph_variable_name, legend) {
      y <- as.character(graph_data$Y[graph_data$Graph_variable_name == graph_variable_name])
      if(!is.na(graph_data$YMAX[graph_data$Graph_variable_name == graph_variable_name])) {
        ymax <- as.numeric(graph_data$YMAX[graph_data$Graph_variable_name == graph_variable_name])
      } else {
        if (ceiling(max(package.data[,c(which(names(package.data) == y))])) > 1) {
        ymax <- ceiling(max(package.data[,c(which(names(package.data) == y))])) + max(package.data[,c(which(names(package.data) == y))]) * 0.1
        } else {
          ymax <- max(package.data[,c(which(names(package.data) == y))]) + max(package.data[,c(which(names(package.data) == y))]) * 0.1
        }
      }
      
      if (!is.na(graph_data$Modifier[graph_data$Graph_variable_name == graph_variable_name])){
        y <- paste0(y,graph_data$Modifier[graph_data$Graph_variable_name == graph_variable_name])
        mod <- parse(text = paste0(ymax, graph_data$Modifier[graph_data$Graph_variable_name == graph_variable_name]))
        ymax <- eval(mod)
      }
      if (!graph_data$UNITS[graph_data$Graph_variable_name == graph_variable_name] == ""){
        ylabel <- paste0(as.character(graph_data$YLABEL[graph_data$Graph_variable_name == graph_variable_name]), "\n",
                         "(", as.character(graph_data$UNITS[graph_data$Graph_variable_name == graph_variable_name]), ")")
      } else {
        ylabel <- as.character(graph_data$YLABEL[graph_data$Graph_variable_name == graph_variable_name])
      }
      title <- paste(as.character(graph_data$YLABEL[graph_data$Graph_variable_name == graph_variable_name]), "by Initial Volume")
      yaes <- paste0("package.data$", y)
      
      graph <- ggplot(package.data, aes_string("rxpackage", y)) +
        geom_point(aes(color = CEC_type, shape = SC_to_print), size = 4) +
        facet_grid(Owner~VolClass) + 
        scale_shape_manual(values=c(15,17), name = "Stand Count") +
        scale_colour_manual(values=cbPalette, name="Forest Type Group") +
        scale_y_continuous(limits = c(0,ymax)) +
        custom_theme +
        labs(x="RX Package", y=ylabel, title=title) + 
        theme(legend.position = legend) 
      
      # yvals <- eval(parse(text = yaes))
      # 
      # if (any(max(yvals > ymax))) {
      #   stop("package average mortvol outside of graph y axis limits")
      # }
      
      return(graph)
    }
  
  var1 <- create_variable_graph(graph_variable_name1, "none")
  var2 <- create_variable_graph(graph_variable_name2, "right")
  
  graph <- grid.arrange(grobs = list(var1,var2,t), nrow = 2, layout_matrix = layout)
  
  old.dir <- getwd()
  dir <- file.path(getwd(), paste(graph_variable_name1, graph_variable_name2, sep = "_"))
  dir.create(dir, showWarnings = FALSE)
  setwd(dir)
  
  ggsave(filename = paste0(graph_variable_name1, graph_variable_name2,final, ".emf"), plot = graph, device = "emf", width = 13, height = 8.5)
  setwd(old.dir)
  
}

data <- stand_summary
##MORTVOL GRAPHS
packages_1v2v3v4 <- graph_package_data(data, c(1,2,3,4), "mortvol", "mortvolpct")
packages_1v2v3v4 <- graph_package_data(data, packages = c(1,2,3,4), "mortvol", "mortvolpct") #comparison of post treatment surface fuel treatments
packages_1v2v12v13 <- graph_package_data(data, packages = c(1,2,12,13), "mortvol", "mortvolpct") #comparison of WTL to CTL
pacakges_1v4 <- graph_package_data(data, packages = c(1,4), "mortvol", "mortvolpct") #CT to 115, rxfire vs lop and scatter
packages_1v4v30 <- graph_package_data(data, packages = c(1,4,32), "mortvol", "mortvolpct") #CT to 115, rxfire vs lop and scatter vs clearcut/lop and scatter
packages_1v20 <- graph_package_data(data, packages = c(1,20), "mortvol", "mortvolpct") #CT/Rxfire ThinDBH vs ThinBBA
packages_4v6 <- graph_package_data(data, packages = c(4,6), "mortvol", "mortvolpct") #target fir vs no fir target 
packages_4v7v17 <- graph_package_data(data, packages = c(4,7,17), "mortvol", "mortvolpct") #NFS diameter cap comparison
packages_4v8v9v11 <- graph_package_data(data, packages = c(4,8,9,11), "mortvol", "mortvolpct") #BA residual 115,135,150,180
packages_4v14 <- graph_package_data(data, packages = c(4,14), "mortvol", "mortvolpct") #min DBH 4 v 9
packages_4v19 <- graph_package_data(data, packages = c(4,9), "mortvol", "mortvolpct") #ThinDBH vs ThinBBA

##HSFRS
packages_1v2v3v4 <- graph_package_data(data, c(1,2,3,4), "hazardscore", "resistancescore")
packages_1v2v3v4 <- graph_package_data(data, packages = c(1,2,3,4), "hazardscore", "resistancescore") #comparison of post treatment surface fuel treatments
packages_1v2v12v13 <- graph_package_data(data, packages = c(1,2,12,13), "hazardscore", "resistancescore") #comparison of WTL to CTL
pacakges_1v4 <- graph_package_data(data, packages = c(1,4), "hazardscore", "resistancescore") #CT to 115, rxfire vs lop and scatter
packages_1v4v30 <- graph_package_data(data, packages = c(1,4,32), "hazardscore", "resistancescore") #CT to 115, rxfire vs lop and scatter vs clearcut/lop and scatter
packages_1v20 <- graph_package_data(data, packages = c(1,20), "hazardscore", "resistancescore") #CT/Rxfire ThinDBH vs ThinBBA
packages_4v6 <- graph_package_data(data, packages = c(4,6), "hazardscore", "resistancescore") #target fir vs no fir target 
packages_4v7v17 <- graph_package_data(data, packages = c(4,7,17), "hazardscore", "resistancescore") #NFS diameter cap comparison
packages_4v8v9v11 <- graph_package_data(data, packages = c(4,8,9,11), "hazardscore", "resistancescore") #BA residual 115,135,150,180
packages_4v14 <- graph_package_data(data, packages = c(4,14), "hazardscore", "resistancescore") #min DBH 4 v 9
packages_4v19 <- graph_package_data(data, packages = c(4,9), "hazardscore", "resistancescore") #ThinDBH vs ThinBBA

##CBD CBH
packages_1v2v3v4 <- graph_package_data(data, c(1,2,3,4), "cbd", "cbh")
packages_1v2v3v4 <- graph_package_data(data, packages = c(1,2,3,4), "cbd", "cbh") #comparison of post treatment surface fuel treatments
packages_1v2v12v13 <- graph_package_data(data, packages = c(1,2,12,13), "cbd", "cbh") #comparison of WTL to CTL
pacakges_1v4 <- graph_package_data(data, packages = c(1,4), "cbd", "cbh") #CT to 115, rxfire vs lop and scatter
packages_1v4v30 <- graph_package_data(data, packages = c(1,4,32), "cbd", "cbh") #CT to 115, rxfire vs lop and scatter vs clearcut/lop and scatter
packages_1v20 <- graph_package_data(data, packages = c(1,20), "cbd", "cbh") #CT/Rxfire ThinDBH vs ThinBBA
packages_4v6 <- graph_package_data(data, packages = c(4,6), "cbd", "cbh") #target fir vs no fir target 
packages_4v7v17 <- graph_package_data(data, packages = c(4,7,17), "cbd", "cbh") #NFS diameter cap comparison
packages_4v8v9v11 <- graph_package_data(data, packages = c(4,8,9,11), "cbd", "cbh") #BA residual 115,135,150,180
packages_4v14 <- graph_package_data(data, packages = c(4,14), "cbd", "cbh") #min DBH 4 v 9
packages_4v19 <- graph_package_data(data, packages = c(4,9), "cbd", "cbh") #ThinDBH vs ThinBBA

##net rev harvest cost
packages_1v2v3v4 <- graph_package_data(data, c(1,2,3,4), "netrev", "harvestcost")
packages_1v2v3v4 <- graph_package_data(data, packages = c(1,2,3,4), "netrev", "harvestcost") #comparison of post treatment surface fuel treatments
packages_1v2v12v13 <- graph_package_data(data, packages = c(1,2,12,13), "netrev", "harvestcost") #comparison of WTL to CTL
pacakges_1v4 <- graph_package_data(data, packages = c(1,4), "netrev", "harvestcost") #CT to 115, rxfire vs lop and scatter
packages_1v4v30 <- graph_package_data(data, packages = c(1,4,32), "netrev", "harvestcost") #CT to 115, rxfire vs lop and scatter vs clearcut/lop and scatter
packages_1v20 <- graph_package_data(data, packages = c(1,20), "netrev", "harvestcost") #CT/Rxfire ThinDBH vs ThinBBA
packages_4v6 <- graph_package_data(data, packages = c(4,6), "netrev", "harvestcost") #target fir vs no fir target 
packages_4v7v17 <- graph_package_data(data, packages = c(4,7,17), "netrev", "harvestcost") #NFS diameter cap comparison
packages_4v8v9v11 <- graph_package_data(data, packages = c(4,8,9,11), "netrev", "harvestcost") #BA residual 115,135,150,180
packages_4v14 <- graph_package_data(data, packages = c(4,14), "netrev", "harvestcost") #min DBH 4 v 9
packages_4v19 <- graph_package_data(data, packages = c(4,9), "netrev", "harvestcost") #ThinDBH vs ThinBBA

##merch chip
packages_1v2v3v4 <- graph_package_data(data, c(1,2,3,4), "merch", "chip")
packages_1v2v3v4 <- graph_package_data(data, packages = c(1,2,3,4), "merch", "chip") #comparison of post treatment surface fuel treatments
packages_1v2v12v13 <- graph_package_data(data, packages = c(1,2,12,13), "merch", "chip") #comparison of WTL to CTL
pacakges_1v4 <- graph_package_data(data, packages = c(1,4), "merch", "chip") #CT to 115, rxfire vs lop and scatter
packages_1v4v30 <- graph_package_data(data, packages = c(1,4,32), "merch", "chip") #CT to 115, rxfire vs lop and scatter vs clearcut/lop and scatter
packages_1v20 <- graph_package_data(data, packages = c(1,20), "merch", "chip") #CT/Rxfire ThinDBH vs ThinBBA
packages_4v6 <- graph_package_data(data, packages = c(4,6), "merch", "chip") #target fir vs no fir target 
packages_4v7v17 <- graph_package_data(data, packages = c(4,7,17), "merch", "chip") #NFS diameter cap comparison
packages_4v8v9v11 <- graph_package_data(data, packages = c(4,8,9,11), "merch", "chip") #BA residual 115,135,150,180
packages_4v14 <- graph_package_data(data, packages = c(4,14), "merch", "chip") #min DBH 4 v 9
packages_4v19 <- graph_package_data(data, packages = c(4,9), "merch", "chip") #ThinDBH vs ThinBBA

##harvest and growth
packages_1v2v3v4 <- graph_package_data(data, c(1,2,3,4), "harvest", "growth")
packages_1v2v3v4 <- graph_package_data(data, packages = c(1,2,3,4), "harvest", "growth") #comparison of post treatment surface fuel treatments
packages_1v2v12v13 <- graph_package_data(data, packages = c(1,2,12,13), "harvest", "growth") #comparison of WTL to CTL
pacakges_1v4 <- graph_package_data(data, packages = c(1,4), "harvest", "growth") #CT to 115, rxfire vs lop and scatter
packages_1v4v30 <- graph_package_data(data, packages = c(1,4,32), "harvest", "growth") #CT to 115, rxfire vs lop and scatter vs clearcut/lop and scatter
packages_1v20 <- graph_package_data(data, packages = c(1,20), "harvest", "growth") #CT/Rxfire ThinDBH vs ThinBBA
packages_4v6 <- graph_package_data(data, packages = c(4,6), "harvest", "growth") #target fir vs no fir target 
packages_4v7v17 <- graph_package_data(data, packages = c(4,7,17), "harvest", "growth") #NFS diameter cap comparison
packages_4v8v9v11 <- graph_package_data(data, packages = c(4,8,9,11), "harvest", "growth") #BA residual 115,135,150,180
packages_4v14 <- graph_package_data(data, packages = c(4,14), "harvest", "growth") #min DBH 4 v 9
packages_4v19 <- graph_package_data(data, packages = c(4,9), "harvest", "growth") #ThinDBH vs ThinBBA

##growth and mortality
packages_1v2v3v4 <- graph_package_data(data, c(1,2,3,4), "growth", "mortality")
packages_1v2v3v4 <- graph_package_data(data, packages = c(1,2,3,4), "growth", "mortality") #comparison of post treatment surface fuel treatments
packages_1v2v12v13 <- graph_package_data(data, packages = c(1,2,12,13), "growth", "mortality") #comparison of WTL to CTL
pacakges_1v4 <- graph_package_data(data, packages = c(1,4), "growth", "mortality") #CT to 115, rxfire vs lop and scatter
packages_1v4v30 <- graph_package_data(data, packages = c(1,4,32), "growth", "mortality") #CT to 115, rxfire vs lop and scatter vs clearcut/lop and scatter
packages_1v20 <- graph_package_data(data, packages = c(1,20), "growth", "mortality") #CT/Rxfire ThinDBH vs ThinBBA
packages_4v6 <- graph_package_data(data, packages = c(4,6), "growth", "mortality") #target fir vs no fir target 
packages_4v7v17 <- graph_package_data(data, packages = c(4,7,17), "growth", "mortality") #NFS diameter cap comparison
packages_4v8v9v11 <- graph_package_data(data, packages = c(4,8,9,11), "growth", "mortality") #BA residual 115,135,150,180
packages_4v14 <- graph_package_data(data, packages = c(4,14), "growth", "mortality") #min DBH 4 v 9
packages_4v19 <- graph_package_data(data, packages = c(4,9), "growth", "mortality") #ThinDBH vs ThinBBA

# && net growth        net growth + harvest             = diff(TCuFt-prev.cycle)/#years  - net growth + diff(RTCuft-prev.cycle)/#years


####STAND GRAPHS####
graph_stand_data_4_7_17 <- function(stand.data, variable) {
  #get only effective
  stand.data_old <- stand.data
  
  #effective tmt test
  pattern <- c("biosum_cond_id", "rxpackage", "overall_effective_yn")
  effective.test <- stand.data[,c(which(grepl(paste0(pattern, collapse = "|"), names(stand.data))))]
  effective.test[,ncol(effective.test) + 1] <- 0
  names(effective.test)[ncol(effective.test)] <- "binary"
  effective.test$binary[effective.test$overall_effective_yn == "Y"] <- 1
  
  effective.test2 <- effective.test %>% group_by(biosum_cond_id) %>% summarise(binary_SUM = sum(binary))
  
  effective.stands <- effective.test2$biosum_cond_id[effective.test2$binary_SUM > 0]
  
  effective.stand.data <- stand.data[stand.data$biosum_cond_id %in% effective.stands,]
  
  #stands where RTpa for package 7 is greater than package 4
  pattern <- c("biosum_cond_id", "rxpackage", "RTpa_SUM")
  rtpa.test <- effective.stand.data[,c(which(grepl(paste0(pattern, collapse = "|"), names(effective.stand.data))))]
  rtpa.test <- rtpa.test[rtpa.test$rxpackage %in% c(7,4),]
  
  rtpa.test2 <- dcast(rtpa.test, biosum_cond_id ~ rxpackage, value.var = "rxpackage")
  rtpa.test2$test <- rtpa.test2$`7` > rtpa.test2$`4`
  
  rtpa.7gthan4 <- rtpa.test2$biosum_cond_id[rtpa.test2$test == TRUE]
  
  filtered.stands <- effective.stand.data[effective.stand.data$biosum_cond_id %in% rtpa.7gthan4,]

  packages <- as.character(unique(stand.data$rxpackage))
  numpackages <- length(packages)
  ##PARAMETERS FOR ALL GRAPHS##
  cbPalette <- c("#CC79A7", "#56B4E9", "#009E73", "#E69F00")
  
  custom_theme <- theme_set(theme_bw(base_size = 20)) +
    theme(plot.title = element_text(size = 16, hjust = 0.5),
          axis.text.x=element_text(size=14), 
          axis.text.y=element_text(size=14),
          axis.title = element_text(size = 16),
          strip.text = element_text(size = 14),
          legend.position="right",
          legend.key = element_rect(colour = "gray90"), 
          legend.key.size = unit(2, "line"),
          legend.spacing = unit(2, "line"), 
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 12, face = "bold"))
  
  relevant_packages <- package_labels[package_labels$Package %in% packages,]
  names(relevant_packages) <- c("Sequence", "FVS Style", "BA Threshold", "Max Private \n DBH", "Max Public \n DBH", "Min DBH", "Residual BA \n (less amt)", "Species \n Pressure", "Surface Fuel \n Treatment", "Harvest \n System")
  
  tt <- ttheme_default(base_size = 14)
  t<- tableGrob(relevant_packages, rows = NULL, theme = tt)
  
  layout <- rbind(c(1,1,1,1,1),
                  c(1,1,1,1,1),
                  c(1,1,1,1,1),
                  c(NA,3,3,3,NA))
  
  final <- NULL
  for (i in 1:numpackages) {
    if (i != numpackages) {
      add <- paste0(packages[i], "v")
      final <- paste0(final, add)
    } else {
      add <- packages[i]
      final <- paste0(final, add)
    }
  }

  pattern <- c("Owner", "rxpackage", variable)
  
  data <- filtered.stands[,c(which(grepl(paste0(pattern, collapse = "|"), names(filtered.stands))))]
  
  data2 <- melt(data, id.vars = c("Owner", "rxpackage"))
  data2$rxpackage <- as.factor(data2$rxpackage)
  graph <- ggplot(data2, aes(rxpackage, value, fill = Owner)) +
    geom_boxplot() + 
    scale_fill_manual(values = cbPalette[1:length(unique(data2$Owner))]) +
    custom_theme +
    labs(x="RX Package", y=variable) 
  
  graph2 <- grid.arrange(grobs = list(graph, t), nrow = 2, layout_matrix = layout)
  
  ggsave(filename = paste0(variable, final, ".emf"), plot = graph2, device = "emf", width = 13, height = 8.5)
  
}

avg_cbd_score <- graph_stand_data_4_7_17 (stands, "Avg_CBD_Score")
avg_cbd <- graph_stand_data_4_7_17 (stands, "Avg_Canopy_Density")
avg_cbh_score <- graph_stand_data_4_7_17 (stands, "Avg_CBH_Score")
avg_cbh <- graph_stand_data_4_7_17 (stands, "Avg_CBH")
avg_PERRESBA_score <- graph_stand_data_4_7_17 (stands, "Avg_PERRESBA_Score")
avg_PERRESBA_score <- graph_stand_data_4_7_17 (stands, "Avg_PERRESBA")
avg_SurvVolRatio_score <- graph_stand_data_4_7_17 (stands, "Avg_SurvVolRatio_Score")
avg_FRS <- graph_stand_data_4_7_17 (stands, "Avg_FRS")


####FIA Analysis#####
best.summary <- stand_summary[stand_summary$best == "Y",] %>% group_by(CEC_type, owngrpcd) %>% summarise(Best_Treatment_Acres = sum(acres), 
                                                                    Negative_NetRev_Acres = sum(acres[max_nr_dpa < 0]),
                                                                    Avg_FRS_Treated = mean(post_variable1_value, na.rm= TRUE),
                                                                    Avg_FRS_Improvement = mean(variable1_change, na.rm= TRUE), 
                                                                    Avg_MerchYield_GT = mean(merch_yield_gt, na.rm= TRUE), 
                                                                    Avg_ChipYield_GT = mean(chip_yield_gt, na.rm= TRUE),
                                                                    Merch_Chip_Ratio_GT = sum(merch_yield_gt/sum(merch_yield_gt, chip_yield_gt)),
                                                                    Avg_Max_NetRev_dpa = mean(max_nr_dpa), 
                                                                    Avg_TmtCost_dpa = mean(harvest_onsite_cpa),
                                                                    Avg_MerchVal_dpa = mean(merch_val_dpa),
                                                                    Avg_ChipVal_dpa = mean(chip_val_dpa),
                                                                    Merch_Chip_Ratio_Value = sum(merch_val_dpa/sum(merch_val_dpa, chip_val_dpa)),
                                                                    Max_NetRev_Acres = sum(max_nr_dpa*acres), 
                                                                    FRS_Improvement_Acres = sum(variable1_change * acres))

data.table1 <- merge(treated.area[,c(1,4,5)], best.summary)

data.table1$Best_Treatment_Pct <- data.table1$Best_Treatment_Acres/data.table1$Treatable_Acres*100
data.table1$Negative_NetRev_Pct <- data.table1$Negative_NetRev_Acres/data.table1$Treatable_Acres*100


data.table0 <- data.table1
data.table0$Filter <- as.factor("0")

data.table <- rbind(data.table0, data.table200)

write.csv(data.table, "FIAdbmtg_data2.csv")

netrev_summary <- data.table %>% group_by(owngrpcd, Filter) %>% summarise(Acres_Treated = sum(Best_Treatment_Acres),
                                                                          Total_Net_rev = sum(Max_NetRev_Acres), 
                                                                          Total_FRS_Improvement = sum(FRS_Improvement_Acres))

write.csv(netrev_summary, "FIAdbmtg_summary2.csv")

###MERCH VOL GRAPH###
pattern <- c("biosum_cond_id", "rxpackage", "rxcycle", "merch_vol_cf_exp", "net_rev_dollars_exp")
stand_cr2 <- stand_cr[,c(which(grepl(paste0(pattern, collapse = "|"), names(stand_cr))))]

stand_cr3 <- merge(stand_cr2, cond.relevant.data)

#Limit to relevant forest types
stand_cr4 <- stand_cr3[stand_cr3$CEC_type %in% c("Douglas-fir", "Pine", "Mixed conifer", "True fir"),]
stand_cr4 <- stand_cr4[stand_cr4$owngrpcd %in% c("10", "40"),]
stand_cr4$profitable[stand_cr4$net_rev_dollars_exp < -200] <- "Not Profitable"
stand_cr4$profitable[stand_cr4$net_rev_dollars_exp >= -200] <- "Profitable"

merch_vol_cf_by_cycle <- stand_cr4 %>% group_by(CEC_type, owngrpcd, rxcycle) %>% summarise(all_merch_vol = sum(merch_vol_cf_exp),
                                                                                           profitable_merch_vol = sum(merch_vol_cf_exp[net_rev_dollars_exp > -200]))
                                                                                           
merch_vol_cf_by_cycle$owngrpcd <- as.factor(merch_vol_cf_by_cycle$owngrpcd)
merch_vol_cf_by_cycle$Owner <- NA
merch_vol_cf_by_cycle$Owner[merch_vol_cf_by_cycle$owngrpcd == "10"] <- "NFS"
merch_vol_cf_by_cycle$Owner[merch_vol_cf_by_cycle$owngrpcd == "40"] <- "Private"

write.csv(merch_vol_cf_by_cycle, "merch_vol_graph_data.csv")

merch_vol_cf_by_cycle2 <- melt(merch_vol_cf_by_cycle, id.vars = c("rxcycle", "owngrpcd", "CEC_type", "Owner"))

merch_vol_cf_by_cycle2$variable <- as.character(merch_vol_cf_by_cycle2$variable)
merch_vol_cf_by_cycle2$variable[merch_vol_cf_by_cycle2$variable == "all_merch_vol"] <- "All"
merch_vol_cf_by_cycle2$variable[merch_vol_cf_by_cycle2$variable == "profitable_merch_vol"] <- "Profitable"


custom_theme <- theme_set(theme_bw(base_size = 20)) +
  theme(plot.title = element_text(size = 16, hjust = 0.5),
        axis.text.x=element_text(size=14), 
        axis.text.y=element_text(size=14),
        axis.title = element_text(size = 16),
        strip.text = element_text(size = 14),
        legend.position="right",
        legend.key = element_rect(colour = "gray90"), 
        legend.key.size = unit(2, "line"),
        legend.spacing = unit(2, "line"), 
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12, face = "bold"))

cbPalette <- c("#CC79A7", "#56B4E9", "#009E73", "#E69F00")


graph <- ggplot(merch_vol_cf_by_cycle, aes(rxcycle, (all_merch_vol*10^-9), color = Owner, fill = Owner, group = Owner)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(.~ CEC_type) + 
  custom_theme + 
  labs(x="RX Cycle", y=paste("Merchantable volume", "(billion cubic feet)", sep = "\n"), title="") +
  scale_fill_manual(values=cbPalette[1:2], name="Owner") + 
  scale_color_manual(values=cbPalette[1:2], name="Owner")

ggsave(filename = "merch_vol_graph.png", plot = graph, device = "png", width = 11, height = 5)

###MERCH VOL ONLY STANDS THAT MADE MONEY###
profitable_merch_vol_cf <- stand_cr4[stand_cr4$net_rev_dollars_exp > -200,]

profitable_merch_vol_cf_by_cycle <- profitable_merch_vol_cf %>% group_by(CEC_type, owngrpcd, rxcycle) %>% summarise(Cycle_Merch_Vol_CF = sum(merch_vol_cf_exp))
profitable_merch_vol_cf_by_cycle$owngrpcd <- as.factor(profitable_merch_vol_cf_by_cycle$owngrpcd)
profitable_merch_vol_cf_by_cycle$Owner <- NA
profitable_merch_vol_cf_by_cycle$Owner[profitable_merch_vol_cf_by_cycle$owngrpcd == "10"] <- "NFS"
profitable_merch_vol_cf_by_cycle$Owner[profitable_merch_vol_cf_by_cycle$owngrpcd == "40"] <- "Private"

graph <- ggplot(profitable_merch_vol_cf_by_cycle, aes(rxcycle, (Cycle_Merch_Vol_CF*10^-9), color = Owner, fill = Owner, group = Owner)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(.~ CEC_type) + 
  custom_theme + 
  labs(x="RX Cycle", y=paste("Merchantable volume", "(billion cubic feet)", sep = "/n"), title="") +
  scale_fill_manual(values=cbPalette[1:2], name="Owner") + 
  scale_color_manual(values=cbPalette[1:2], name="Owner")

ggsave(filename = "profitable_merch_vol_graph.emf", plot = graph, device = "emf", width = 9, height = 5)


#Compare two runs
data2 <- read.csv("FIAdbmtg_data_nr_gt_neg200.csv", as.is = TRUE )
data2[,1] <- -200
names(data2)[1] <- "NetRev_Filter"

data.table1$NetRev_Filter <- 0

data3 <- rbind(data2, data.table1)

data4 <- melt(data3, id.vars = c("CEC_type", "NetRev_Filter", "owngrpcd", "Treatable_Acres"))

data4$NetRev_Filter <- as.factor(data4$NetRev_Filter)
data4$owngrpcd <- as.factor(data4$owngrpcd)

graph <- ggplot(data4, aes(owngrpcd, value, color = NetRev_Filter, fill = NetRev_Filter, group = NetRev_Filter)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(variable ~ CEC_type, scales = "free") + 
  custom_theme + 
  labs(x="Owner")

ggsave(filename = "core_compare.png", plot = graph, device = "png", width = 9, height = 35)

conn <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=H:/cec_20170915/core/scenario20180418/db/scenario_results.mdb") #Change the text after "DBH=" to the correct directory for your project
netrev200 <- sqlFetch(conn, "product_yields_net_rev_costs_summary_by_rx", as.is = TRUE)
cycle1_best_rx200 <- sqlFetch(conn, "cycle1_best_rx_summary", as.is = TRUE)
odbcCloseAll()

conn <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=H:/cec_20170915/core/scenario20180418_2/db/scenario_results.mdb") #Change the text after "DBH=" to the correct directory for your project
netrev0 <- sqlFetch(conn, "product_yields_net_rev_costs_summary_by_rx", as.is = TRUE)
cycle1_best_rx0 <- sqlFetch(conn, "cycle1_best_rx_summary", as.is = TRUE)
odbcCloseAll()

best0 <- cycle1_best_rx0[,c(1,2,3)]
best200 <- cycle1_best_rx200[,c(1,2,3)]
# netrev3 <- netrev2 %>% group_by(biosum_cond_id, rx) %>% tally()

netrev0.2 <- merge(best0, netrev0)
netrev200.2 <- merge(best200, netrev200)
netrev0.2$Filter <- as.factor(0)
netrev200.2$Filter <- as.factor(-200)

data2 <- rbind(netrev0.2, netrev200.2)

data3 <- merge(data2, cond.species)


data3$rxcycle <- as.factor(data3$rxcycle)
data3$owngrpcd <- as.factor(data3$owngrpcd)


data4 <- data3[data3$CEC_type %in% c("Douglas-fir", "Pine", "Mixed conifer", "True fir"),]

graph <- ggplot(data4, aes(rxcycle, max_nr_dpa, fill = owngrpcd)) +
  geom_boxplot() +
  facet_grid(Filter~ CEC_type) + 
  custom_theme

graph
  
ggsave(filename = "rxcyclegraph.png", plot = graph, device = "png", width = 12, height = 12)

