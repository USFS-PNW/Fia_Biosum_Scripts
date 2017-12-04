#This script was created by Carlin Starrs in 2017. 
#It takes batch file names and then lists them using the "call" 
#function in a new batch file. This means that the batch files listed
#in the new batch file will be run sequentially. If your computer has the 
#processing power to run multiple files at the same time, you can alter the
#batchsize so it will split the files by the # of batches you want, and then
#you can open each at the same time so they run simultaneously. i.e., if you have
#30 files, and your computer can run 2 at a time, set batch size = to 15. The 
#script will create two bat files: runall1 and runall2. Then double click each to 
#run them simultaneously.

#The number of sets of files you can run simultaneously will depend on your computer
#processing power. The upper limit will be the number of cores of your processor, 
#but you may not be able to run that many without bogging down your CPU. The # 
#to run is best determined by experimenting with running multiple .bat files at a time
#(before you run this script) and using Task Manager to determine the CPU usage. 
#You want your CPU usage to remain below 90%-ish. Once you have figured out how many
#can run without getting your CPU to 90%, put the bat files you've already run in a 
#separate directory, then run this script. 

setwd("H:/cec_20170915/fvs/data/NC")

batchfiles <- function(batchsize) {
  files <- length(list.files(path = ".", pattern = glob2rx("FVSOUT_*_P0*.bat"))) #get # of files named {package}.bat
  div <- ceiling(files/batchsize) 
  string <- 0:div
  for (i in 1:div) {
    j <- (1+(batchsize*string[i])):(1*(batchsize*string[(i+1)]))
    for (x in 1:length(j)) {
      y <- paste("call", (list.files(path = ".", pattern = glob2rx("FVSOUT_*_P0*.bat"))[j[x]]))
      cat(y, file = paste("runall",i,".bat", sep = ""), sep = "\n", append = TRUE)
    }
  }
}

batchfiles(6)
