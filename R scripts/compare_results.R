packages <- c("RODBC", "dplyr", "reshape2", "ggplot2")

package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, repos="http://cran.r-project.org", dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

options(scipen = 999) #this is important for making sure your stand IDs do not get translated to scientific notation


#compare CA 01
conn <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=H:/cec_20180517/fvs/data/CA/FVSOUT_CA_P001-100-100-100-100.mdb")
new_summary <- sqlFetch(conn, "FVS_Summary", as.is = TRUE)
odbcCloseAll()

conn <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=H:/cec_20170915/fvs/data/CA/FVSOUT_CA_P001-100-100-100-100.mdb")
old_summary <- sqlFetch(conn, "FVS_Summary", as.is = TRUE)
odbcCloseAll()


old_summary$Status <- "old"
new_summary$Status <- "new"

old_summary2 <- old_summary[,which(grepl(paste0(names(new_summary), collapse = "|"), names(old_summary)))]
old_summary2$MortVol <- NULL

all <- data.frame(rbind(new_summary, old_summary2))
start <- 5
end <- length(all) - 1
mylist <- vector(mode = "list", length = length(start:end))
i <- 1
for (n in start:end) {
  variable <- names(all)[n]
  statement <- parse(text = paste0("all %>% group_by(Status, StandID) %>%  summarise(","mean", variable, " = ", "mean(", variable, "))"))
  assign("test", eval(statement), envir = .GlobalEnv)
  mylist[[i]] <- test
  names(mylist)[[i]] <- variable
  i <- i + 1
}

sensitivity <- 0.10
result <- data.frame()
test.sensitivity <- function(sensitivity){
  for (j in 1:length(mylist)){
    test2 <- suppressMessages(dcast(mylist[[j]], StandID ~ Status))
    test2$diff <- test2$new - test2$old
    test2$diffpct <- test2$diff > ifelse(test2$new  > test2$old, (test2$new * sensitivity), (test2$old * sensitivity))
    number.different <- sum(test2$diffpct, na.rm = TRUE)
    row <- data.frame("var" = names(mylist)[j],"num" = number.different)
    result <- rbind(result, row)
  }
  
  names(result)[1] <- "variable"
  names(result)[2] <- sensitivity
  return(result)
}

one <- test.sensitivity(0.1)

sensitivity.array <- seq(0.05,1, by = 0.05)

list2 <- vector(mode = "list", length = length(sensitivity.array))
for (k in 1:length(sensitivity.array)) {
  list2[[k]] <- test.sensitivity(sensitivity.array[k])
}

all <- Reduce(merge, list2)

all2 <- all[-which(all$variable == "ForTyp"),]

all3 <- melt(all2, id.vars = "variable")

names(all3)[2] <- "var"
graph <- ggplot(all3, aes(var, value, group = variable, color = variable)) + 
  geom_line(size = 1) + 
  geom_text(data = subset(all3, var == 0.55), aes(label = variable, colour = variable, y = (value + 10)), hjust = .1)
package <- "CA_01"
filename <- paste0(package, ".png")
ggsave(filename, plot = graph, device = "png")

