packages <- c("RODBC", "dplyr", "reshape2", "ggplot2", "ggthemes", "grid")

package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, repos="http://cran.r-project.org", dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

options(scipen = 999) #this is important for making sure your stand IDs do not get translated to scientific notation

project1.location <- "H:/cec_20180517"
# project2.location <- "H:/cec_20180517"
project2.location <- "H:/cec_20170915"
variantname <- "CA"
comparison <- "package"
packagenum <- 27

setwd(file.path(project1.location, "fvs", "data", variantname))#sets the working directory to the project1.location variable

create_sensitivity_graphs <- function(comparison, project1.location, project2.location, variantname, packagenum, specific.variable, specific.sensitivity){
  if (comparison == "package"){
    path1 <- list.files(path = file.path(project1.location, "fvs", "data", variantname), pattern = glob2rx(paste("FVSOUT_", variantname, "_P0", "*.MDB", sep = ""))) #lists all the files in the directory that begin with FVSOUT_{variantname}_P0 and end with .MDB (case sensitive)
    path2 <- list.files(path = file.path(project1.location, "fvs", "data", variantname), pattern = glob2rx(paste("FVSOUT_", variantname, "_P0", "*.MDB", sep = ""))) #lists all the files in the directory that begin with FVSOUT_{variantname}_P0 and end with .MDB (case sensitive)
    path <- path1[path1 %in% path2]
    
    path <- c(path1[8], path1[10])
  }
    
    if (missing(packagenum)) {
      packagenum <- NA
    }
    
    if(!is.na(packagenum)) {
      path <- path[which(grepl(sprintf("%03d", packagenum), path))]
    }
    
  } 
  
  if (comparison == "master") {
    path1 <- file.path(project1.location, "db", "master.mdb")
    path2 <- file.path(project2.location, "db", "master.mdb")
    path <- "master"
    
  }
  if (comparison == "SDImax") {
    path1 <- list.files(path = file.path(project1.location, "fvs", "data", variantname), pattern = glob2rx("FVS_SDImax*")) 
    path2 <- list.files(path = file.path(project2.location, "fvs", "data", variantname), pattern = glob2rx("FVS_SDImax*"))
    
    path <- "FVS_SDImax_out"
  }
  
  if (comparison == "FVSin") {
    path1 <- list.files(path = file.path(project1.location, "fvs", "data", variantname), pattern = glob2rx("FVSIn*")) 
    path2 <- list.files(path = file.path(project2.location, "fvs", "data", variantname), pattern = glob2rx("Predispose.mdb"))
    
    path <- "FVSin"
  }
  
  for (i in 1:length(path)) {
    pathname <- gsub(".MDB", "", path[i])
    if (comparison == "package"){
      
      conn.path1 <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", file.path(project1.location, "fvs", "data", variantname), "/", path[i])
      conn.path2 <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", file.path(project2.location, "fvs", "data", variantname),"/",  path[i])
      
      conn <- odbcDriverConnect(conn.path1)
      new_summary <- sqlFetch(conn, "FVS_Summary", as.is = TRUE)
      #new_trees<- sqlFetch(conn, "FVS_TreeList", as.is = TRUE)
      odbcCloseAll()
      
      conn <- odbcDriverConnect(conn.path2)
      old_summary <- sqlFetch(conn, "FVS_Summary", as.is = TRUE)
      #old_trees<- sqlFetch(conn, "FVS_TreeList", as.is = TRUE)
      odbcCloseAll()
      
    
      
      # new_trees$Status <- "new"
      # old_trees$Status <- "old"

      # old_trees$TreeId2[nchar(trimws(old_trees$TreeId)) == 4] <- substr(old_trees$TreeId[nchar(trimws(old_trees$TreeId)) == 4], 1,4)
      # old_trees$TreeId2[nchar(trimws(old_trees$TreeId)) == 5] <- substr(old_trees$TreeId[nchar(trimws(old_trees$TreeId)) == 5], 2,5)
      # old_trees$TreeId2[nchar(trimws(old_trees$TreeId)) == 6] <- substr(old_trees$TreeId[nchar(trimws(old_trees$TreeId)) == 6], 3,6)
      # old_trees$TreeId2[nchar(trimws(old_trees$TreeId)) == 7] <- substr(old_trees$TreeId[nchar(trimws(old_trees$TreeId)) == 7], 4,7)
      # 
      # old_trees$TreeId <- old_trees$TreeId2
      # old_trees$TreeId2 <- NULL
      # old_trees$TreeId <- as.integer(old_trees$TreeId)
      # 
      # all_trees <- rbind(new_trees, old_trees)
      # all_trees_vars <- all_trees[all_trees$StandID %in% vars.with.diff1$StandID,]
      # all_trees_vars1 <- all_trees_vars[all_trees_vars$Year == 1,]
      
    } else if (comparison == "master") {
      conn.path1 <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", file.path(project1.location, "db", "master.mdb"))
      conn.path2 <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", file.path(project2.location, "db", "master.mdb"))
      
      conn <- odbcDriverConnect(conn.path1)
      new_summary <- sqlFetch(conn, "tree", as.is = TRUE)
      # tree1 <- sqlFetch(conn, "tree", as.is = TRUE)
      cond1 <- sqlFetch(conn, "cond", as.is = TRUE)
      odbcCloseAll()
      
      conn <- odbcDriverConnect(conn.path2)
      old_summary <- sqlFetch(conn, "tree", as.is = TRUE)
      # tree2 <- sqlFetch(conn, "tree", as.is = TRUE)
      # cond2 <- sqlFetch(conn, "cond", as.is = TRUE)
      odbcCloseAll()
      
      # test <- old_summary[!old_summary$biosum_cond_id %in% new_summary$biosum_cond_id,]
      # test2 <- new_summary[!new_summary$biosum_cond_id %in% old_summary$biosum_cond_id,]
      
      new.both <- new_summary %>% group_by(biosum_cond_id, tree, subp) %>% tally()
      old.both <- old_summary %>% group_by(biosum_cond_id, tree, subp) %>% tally()
      new.both$n <- NULL
      old.both$n <- NULL
      
      all.both <- merge(new.both, old.both)
      missing.in.new <- anti_join(new.both, old.both)
      new.missing <- merge(new_summary, missing)
      new.missing <- new.missing[new.missing$biosum_cond_id %in% cond1$biosum_cond_id,]
      
      missing.in.old <- anti_join(old.both, new.both)
      old.missing <- merge(old_summary, missing)
      
      new_summary <- merge(new_summary, all.both)
      old_summary <- merge(old_summary, all.both)
      
      
      # new_summary <- new_summary[new_summary$biosum_cond_id %in% old_summary$biosum_cond_id,]
      # old_standtree_not_in_new <- old_summary[!paste0(old_summary$biosum_cond_id, old_summary$tree) %in% paste0(new_summary$biosum_cond_id, new_summary$tree),]
      # old_summary <- old_summary[paste0(old_summary$biosum_cond_id, old_summary$tree) %in% paste0(new_summary$biosum_cond_id, new_summary$tree),]
      # 
      # test <- merge(old_summary, new_summary, by = c("biosum_cond_id"), all = TRUE)
      # test2 <- test[ , order(names(test))]
      # length(unique(paste0(old_summary$biosum_cond_id, old_summary$tree)))
      # length(unique(paste0(new_summary$biosum_cond_id, new_summary$tree)))
      
    } else if (comparison == "SDImax") {
      conn.path1 <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", file.path(project1.location, "fvs", "data", variantname), "/", path1)
      conn.path2 <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", file.path(project2.location, "fvs", "data", variantname),"/",  path2)
      
      conn <- odbcDriverConnect(conn.path1)
      # new_summary <- sqlFetch(conn, "FVS_Summary", as.is = TRUE)
      new_summary <- sqlFetch(conn, "FVS_Compute", as.is = TRUE)
      odbcCloseAll()
      
      conn <- odbcDriverConnect(conn.path2)
      old_summary <- sqlFetch(conn, "FVS_Compute", as.is = TRUE)
      odbcCloseAll()
    } else if (comparison == "FVSin") {
      conn.path1 <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", file.path(project1.location, "fvs", "data", variantname), "/", path1)
      conn.path2 <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", file.path(project2.location, "fvs", "data", variantname),"/",  path2)
      
      conn <- odbcDriverConnect(conn.path1)
      new_summary <- sqlFetch(conn, "FVS_TreeInit", as.is = TRUE)
      # new_summary$Inv_Year_Old <- NULL
      # new_summary$County_Old <- NULL
      odbcCloseAll()
      
      conn <- odbcDriverConnect(conn.path2)
      old_summary <- sqlFetch(conn, "FVS_TreeInit", as.is = TRUE)
      old_summary$DBH <- old_summary$Diameter
      old_summary$Plot_ID <- old_summary$PLOT_ID
      old_summary$Tree_ID <- old_summary$TREE_ID
      odbcCloseAll()
    }
    
    
    old_summary$Status <- "old"
    new_summary$Status <- "new"
    if (length(setdiff(names(new_summary), names(old_summary))) > 0){
      new_summary <- new_summary[,-c(which(names(new_summary) %in% setdiff(names(new_summary), names(old_summary))))]
      if (length(setdiff(names(old_summary), names(new_summary))) > 0) {
        old_summary <- old_summary[,-c(which(names(old_summary) %in% setdiff(names(old_summary), names(new_summary))))]
      }
    }
    
    # old_summary2 <- old_summary[,which(grepl(paste0(names(new_summary), collapse = "|"), names(old_summary)))]
    names(old_summary)[which(grepl("MortVol", names(old_summary)))] <- "MortVol_FOFEM"
    
    # new_summary <- old_summary[,which(grepl(paste0(names(old_summary), collapse = "|"), names(new_summary)))]
    
    all <- data.frame(rbind(new_summary, old_summary))
    
    # all$FM_BY <- as.numeric(as.character(all$FM_BY))
    # all$StandID <- trimws(all$Stand_ID)
    
    
    if (comparison == "master") {# #code for master
      master.cols <- c("dia", "ht", "actualht", "cr", "stocking", "tpacurr", "vol", "dry", "tpa_unadj","Status")
      all <- all[,c(1:3,7,which(grepl(paste0(master.cols, collapse = "|"), names(all))))]
      names(all)[1] <- "StandID"
      all$diahtcd <- NULL
      all$htcd <- NULL
      #all %>% select(which(sapply(.,is.character)))
    }
    
    start <- 5
    end <- length(all) - 1
    
    if(missing(specific.variable)) {
      specific.variable <- NA
    }
    
    
    if (!is.na(specific.variable)) {
      start <- which(names(all) == specific.variable)
      end <- start
    } 
    
    mylist <- vector(mode = "list", length = length(start:end))
    
    m <- 1
    for (n in start:end) {
      variable <- names(all)[n]
      if (comparison != "master") {
        if (comparison == "FVSin") {
          statement <- parse(text = paste0("all %>% dplyr::group_by(Status, StandID) %>%  dplyr::summarise(","mean", variable, " = ", "mean(", variable, "))"))
        } else {
          statement <- parse(text = paste0("all[all$Year == 1,] %>% dplyr::group_by(Status, StandID) %>%  dplyr::summarise(","mean", variable, " = ", "mean(", variable, "))"))
        }
      } else {
        statement <- parse(text = paste0("all %>% dplyr::group_by(Status, StandID) %>%  dplyr::summarise(","mean", variable, " = ", "mean(", variable, "))"))
      }
      assign("test", eval(statement), envir = .GlobalEnv)
      mylist[[m]] <- test
      names(mylist)[[m]] <- variable
      m <- m + 1
    }
    
    if(missing(specific.sensitivity)) {
      specific.sensitivity <- 0.05
    }
    sensitivity <- specific.sensitivity
    
    result <- data.frame()
    test.sensitivity <- function(sensitivity, specific.variable){
      
      if(missing(specific.variable)) {
        specific.variable <- NA
      }
      for (j in 1:length(mylist)){
        test2 <- suppressMessages(dcast(mylist[[j]], StandID ~ Status))
        test2$diff <- abs(test2$new - test2$old)
        test2$diffpct <- test2$diff > ifelse(test2$new  > test2$old, (test2$new * sensitivity), (test2$old * sensitivity))
        test2$means <- (rowMeans(test2[,c(2:3)])*sensitivity)
        test2$diffpct <- test2$diff > test2$means
        number.different <- sum(test2$diffpct, na.rm = TRUE)
        row <- data.frame("var" = names(mylist)[j],"num" = number.different)
        result <- rbind(result, row)
      }
      # 
      # ones <- test2[test2$diffpct == TRUE & test2$diff == 1,]
      # trues <- test2[test2$diffpct == TRUE,]
      # 
      # nrow(ones)/nrow(trues)
      
      names(result)[1] <- "variable"
      names(result)[2] <- sensitivity
      
      if (!is.na(specific.variable)) {
        return(test2)
      } else {
        return(result)
      }
    }
    
    one <- test.sensitivity(specific.sensitivity, specific.variable)
    
    if(is.na(specific.variable)) {
      sensitivity.array <- seq(0.05,1, by = 0.05)
      
      list2 <- vector(mode = "list", length = length(sensitivity.array))
      for (k in 1:length(sensitivity.array)) {
        list2[[k]] <- test.sensitivity(sensitivity.array[k])
      }
      
      all2 <- Reduce(merge, list2)
      if (any(all2$variable == "ForTyp")) {
        all3 <- all2[-which(all2$variable == "ForTyp"),]
      } else {
        all3 <- all2
      }
      
      all4 <- melt(all3, id.vars = "variable")
      names(all4)[2] <- "var"
      
      acc.true.stands <- acc.test2[acc.test2$diffpct == TRUE,]
      mort.true.stands <- mort.test2[mort.test2$diffpct == TRUE,]
      
      
      acc.true.summary <- rbind(new_summary[new_summary$StandID %in% acc.true.stands$StandID,], old_summary[old_summary$StandID %in% acc.true.stands$StandID,])
      mort.true.summary <- rbind(new_summary[new_summary$StandID %in% mort.true.stands$StandID,], old_summary[old_summary$StandID %in% mort.true.stands$StandID,])
      
      conn.path1 <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", file.path(project1.location, "fvs", "data", variantname), "/", path[i])
      conn.path2 <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", file.path(project2.location, "fvs", "data", variantname),"/",  path[i])
      
      conn <- odbcDriverConnect(conn.path1)
      new_trees<- sqlFetch(conn, "FVS_TreeList", as.is = TRUE)
      odbcCloseAll()
      
      conn <- odbcDriverConnect(conn.path2)
      old_trees<- sqlFetch(conn, "FVS_TreeList", as.is = TRUE)
      odbcCloseAll()
      
      new_trees$Status <- "new"
      old_trees$Status <- "old"
      
      all <- rbind(new_trees, old_trees)
      
      all <- all[all$Year == 1,]
      
      acc.trees <- all[all$StandID %in% acc.true.stands$StandID,]
      mort.trees <- all[all$StandID %in% mort.true.stands$StandID,]
      
      acc.trees2 <- melt(acc.trees, id.vars = c("StandID", "CaseID", "Year", "PrdLen", "TreeId", "TreeIndex", "TreeVal", "Status", "DBH", "Ht", "Species"))
      mort.trees2 <- melt(mort.trees, id.vars = c("StandID", "CaseID", "Year", "PrdLen", "TreeId", "TreeIndex", "TreeVal", "Status", "DBH", "Ht", "Species"))
      
      acc.trees3 <- aggregate(value ~ StandID + DBH + Ht + Species + TreeVal + variable, data = acc.trees2, FUN = diff)
      acc.trees3$value <- as.numeric(as.character(acc.trees3$value))
      acc.trees3$value <- abs(acc.trees3$value)
      
      mort.trees3 <- aggregate(value ~ StandID + DBH + Ht + Species + TreeVal + variable, data = mort.trees2, FUN = diff)
      mort.trees3$value <- as.numeric(as.character(mort.trees3$value))
      mort.trees3$value <- abs(mort.trees3$value)
      
      write.csv(acc.trees3, "acc.diff.fvs.treelist.year.1.csv")
      write.csv(mort.trees3, "mort.diff.fvs.treelist.year.1.csv")
      
      acc.variable.diff.tally <- acc.trees3[acc.trees3$value > 0,] %>% group_by(variable) %>% tally()
      names(acc.variable.diff.tally)[2] <- "Acc.tally"
      mort.variable.diff.tally <- mort.trees3[mort.trees3$value > 0,] %>% group_by(variable) %>% tally()
      names(mort.variable.diff.tally)[2] <- "Mort.tally"
      
      tally.var <- merge(acc.variable.diff.tally, mort.variable.diff.tally)
      
      write.csv(tally.var, "tally.csv")

      
      rel_trees <- all[all$biosum_cond_id %in% acc.vars.with.diff1$StandID | all$biosum_cond_id %in% mort.vars.with.diff1,]
      rel_trees2 <- melt(rel_trees[,c(which(names(rel_trees) %in% c("biosum_cond_id", "invyr", "countycd", "subp", "tree", "spcd", "dia",
                                                                    "ht", "cr", "stocking", "tpacurr", "tpa_unadj", "Status")))],
                         id.vars = c("biosum_cond_id", "countycd", "subp", "tree", "invyr", "Status"))

      rel_trees_test <- rel_trees2 %>% dplyr::group_by(biosum_cond_id, invyr, countycd, subp, tree, variable) %>% tally()
      rel_trees2$value <- as.numeric(rel_trees2$value)

      agg.trees <- aggregate(value ~ biosum_cond_id + invyr + countycd + subp + tree + variable, data = rel_trees2, FUN = diff)
      agg.trees$value <- as.numeric(as.character(agg.trees$value))
      #acc.trees$value <- as.numeric(acc.trees$value)
      agg.trees <- aggregate(value ~ StandID + Year + variable + Species + DBH, data = acc.trees, FUN = diff)

      ######
      rel_trees <- all_trees[all_trees$StandID %in% acc.vars.with.diff1$StandID | all_trees$StandID %in% mort.vars.with.diff1,]
      rel_trees2 <- melt(rel_trees[,c(which(names(rel_trees) %in% c("StandID", "Year", "TreeId", "TreeIndex", "Species",
                                                                    "TPA", "MortPA", "DBH", "DG", "Ht", "HtG", "PctCr", "CrWidth", "MistCD",
                                                                    "BAPctile", "PtBAL", "TCuFt", "MCuFt", "BdFt", "ActPt", "Ht2TDCF", "Ht2TDBF",
                                                                    "Status")))],
                         id.vars = c("StandID", "Year", "TreeId", "TreeIndex", "Species", "Status"))

      rel_trees_test <- rel_trees2 %>% dplyr::group_by(StandID, Year, TreeId, TreeIndex, Species, Status) %>% tally()
      rel_trees2$value <- as.numeric(rel_trees2$value)

      agg.trees <- aggregate(value ~ StandID + Year + variable + Status, data = rel_trees2, FUN = sum)
      agg.trees1<- agg.trees[agg.trees$Year == 1,]

      sumdiff <- aggregate(value ~ StandID + Year + variable, data = agg.trees1, FUN = diff)
      sumdiff$value <- abs(sumdiff$value)
      agg.trees$value <- as.numeric(as.character(agg.trees$value))
      #acc.trees$value <- as.numeric(acc.trees$value)
      agg.trees <- aggregate(value ~ StandID + Year + variable + Species + DBH, data = acc.trees, FUN = diff)

      all4 <- melt(acc.true.summary, id.vars = c("StandID", "CaseID", "Year", "Age", "Status"))
      all4 <- melt(mort.true.summary, id.vars = c("StandID", "CaseID", "Year", "Age", "Status"))
      all5 <- aggregate(value ~ StandID + Year + variable, data = all4, FUN = diff)

      vars.with.diff <- all5[all5$value > 0,]
      vars.with.diff1 <- vars.with.diff[vars.with.diff$Year == 1,]

      # graph <- ggplot(all4, aes(variable, value, group = Status, color = Status, fill = Status)) +
      #   geom_bar(stat = "identity", position = "dodge") + facet_wrap(~ StandID)
      # 
      # ggsave(filename, plot = graph, device = "png", width = 30, height = 30)

      # graph <- ggplot(all4, aes(Year, value, group = variable, color = variable)) +
      #   geom_line(size = 1) +
      #   geom_text(data = subset(all4, Year == 1), aes(label = variable, colour = variable, y = (value + 10)), hjust = .1) +
      #   labs(x = "Percent Difference", y = paste0("Number of stands", paste0("(total stands = ", length(unique(all$StandID)),")"), sep = "\n"), title = paste0("Number of stands where % difference is greater than set % for ", comparison))
      # 
      # graph
      ####
      
      graph <- ggplot(all4, aes(var, value, group = variable, color = variable)) + 
        geom_line(size = 1) + 
        geom_text(data = subset(all4, var == 0.1), aes(label = variable, colour = variable, y = (value + 10)), hjust = .1) + 
        labs(x = "Percent Difference", y = paste0("Number of stands", paste0("(total stands = ", length(unique(all$StandID)),")"), sep = "\n"), title = paste0("Number of stands where % difference is greater than set % for ", comparison))
      
      graph
      
      filename <- paste0(pathname, ".png")
      
      old_wd <- getwd()
      suppressWarnings(dir.create(file.path(getwd(), "compare_result", comparison)))
      setwd(file.path(getwd(), "compare_result", comparison))
      
      ggsave(filename, plot = graph, device = "png", width = 12, height = 8)
      
      setwd(old_wd)
    } else {
      return(one)
    }
  }
}

create_sensitivity_graphs(comparison, project1.location, project2.location, variantname)
#comparison, project1.location, project2.location, variantname, packagenum, specific.variable, specific.sensitivity
package8 <- create_sensitivity_graphs("package", project1.location, project2.location, "CA", 8)

tree1$standtree <- paste0(tree1$biosum_cond_id, tree1$tree)
tree1$status <- "new"
tree2$status <- "old"

tree1.in.both <- tree1[tree1$standtree %in% tree2$standtree,]
tree1.not.in.both <- tree1[!tree1$standtree %in% tree2$standtree,]
tree1.not.in.both <- tree1.not.in.both[tree1.not.in.both$biosum_cond_id %in% cond1$biosum_cond_id,]


tree2.in.both <- tree2[tree2$standtree %in% tree1$standtree,]
tree2.not.in.both <- tree2[!(tree2$standtree %in% tree1$standtree),]
tree2.not.in.both <- tree2.not.in.both[tree2.not.in.both$biosum_cond_id %in% cond2$biosum_cond_id,]

write.csv(tree2.not.in.both, "trees from earlier project not in current.csv")

conn.path1 <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", file.path(project1.location, "fvs", "data", variantname), "/", path1)
conn.path2 <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", file.path(project2.location, "fvs", "data", variantname),"/",  path2)

conn <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=H:/CAaccdb/CA.accdb")
CA_tree <- sqlFetch(conn, "TREE", as.is = TRUE)
odbcCloseAll()

tree2.not.in.both$plot <- substr(tree2.not.in.both$biosum_cond_id, 17,21)
tree2.not.in.both$plottree <- paste0(tree2.not.in.both$plot, tree2.not.in.both$tree)
CA_tree$plottree <- paste0(CA_tree$PLOT, CA_tree$TREE)
tree2.not.in.both.in.CA.tree <- tree2.not.in.both[tree2.not.in.both$plottree %in% CA_tree$plottree,]
