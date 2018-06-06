#This script was created by Carlin Starrs in 2017. 
#The purpose of this script is to find all the package KCPs in a directory, 
#open the file and find and replace text that reads
#FVSOUT_{variantname}_{packagename}.MDB with a new variant name. This 
#allows you to clone KCP files for a new variant, i.e., if you have created
#KCP files and assured they are correct for one variant, you can clone them
#for a new variant. The arguments for the function are "oldvariant", which
#is the variant for the KCP files you wish to clone, and "newvariant", which 
#is the variant you wish to create KCP files for. This script only works with
#package KCP files and should not be used for other KCPs. The new files are 
#saved to the same directory as the old, and should be cut and copied into the 
#newvariant directory. 


setwd("G:/Dropbox/Carlin/Berkeley/biosum/CEC Master KCPs") #set the working directory

#The text below is for the cloneKCP function and will not return any result until the function is called
cloneKCP <- function(oldvariant, newvariant) {
  end <- length(list.files(path = ".", pattern = glob2rx(paste("FVSOUT_", oldvariant, "_P0*.KCP", sep = ""))))
  for(i in 1:end) {
  x <- readLines(list.files(path = ".", pattern = glob2rx(paste("FVSOUT_", oldvariant, "_P0*.KCP", sep = "")))[i]) #opens the .KCP file
  y <- gsub(paste("FVSOUT_", oldvariant, substr(list.files(path = ".", pattern = glob2rx(paste("FVSOUT_", oldvariant, "_P0*.KCP", sep = "")))[i], 10, 30), ".MDB", sep = ""), 
            paste("FVSOUT_", newvariant, substr(list.files(path = ".", pattern = glob2rx(paste("FVSOUT_", oldvariant, "_P0*.KCP", sep = "")))[i], 10, 30), ".MDB", sep = ""), 
            x) #find and replace the {packagename}.MDB file text in the .KCP file
  cat(y,file=paste("FVSOUT_", newvariant, substr(list.files(path = ".", pattern = glob2rx(paste("FVSOUT_", oldvariant, "_P0*.KCP", sep = "")))[i], 10, 30), ".KCP", sep = ""), sep = "\n") 
  # saves the file with the new variant name
  }
}

#The function is called below by changing the variant names. 
cloneKCP(oldvariant = "SO", newvariant = "WS")


