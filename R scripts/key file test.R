#This script was created by Carlin Starrs in 2017
#The script tests to make sure the package KCP file used to create each FVS key
#file in FVS Suppose uses the correct corresponding package KCP. 

#Project root location:
project.location <- "H:/cec_20180529"

#NOTE: Before running, make sure all the variant KCP files follow the same name format. They should begin with the 
#variant name then end with ".kcp" (the extension MUST be lowercase). For example, "CA_SDImax.kcp" will work, but 
#CA_SDImax.KCP and SDImax_CA.kcp will not work. The only argument for the function is the variant name.

test <- function(project.location, variantname) {
  setwd(file.path(project.location, "fvs", "data", variantname)) #sets the working project.location to the project.location variable
  numfiles <- nrow(data.frame(list.files(path = ".", pattern = ".key")))#calculate the number of .KEY files in the project.location
  numadd <- nrow(data.frame(list.files(path = ".", pattern = glob2rx(paste(variantname, "*.kcp", sep = "")))))#calculate the number of additional variant KCPS (file names must start with the variant name, e.g. "CA.kcp")
  numadd2 <- nrow(data.frame(list.files(path = ".", pattern = glob2rx(paste(variantname, "*.KCP", sep = "")))))#calculate the number of additional variant KCPS (file names must start with the variant name, e.g. "CA.kcp")
  numadd <- sum(numadd, numadd2)
  rows <- as.numeric(0)
  final <- matrix(,numfiles,(numadd + 1))
  filename <- list()
  rowrow <- as.numeric(0)
  for(i in 1:numfiles) {
    name <- gsub(".key", ".KCP", list.files(path = ".", pattern = ".key")[i])#changes the file name from .KEY to .KCP
    x <- data.frame(grep(name, readLines(list.files(path = ".", pattern = ".key")[i]), value = TRUE)) #checks for lines within the .KEY file that have {keyfilename}.KCP file name
    rows <- nrow(x)#saves the number of times the .KCP file name appears
    test2 <- function() {
      addfile <- list.files(path = ".", pattern = glob2rx(paste(variantname, "*.kcp", sep = ""))) #lists the variant KCPs
      addfile2 <- list.files(path = ".", pattern = glob2rx(paste(variantname, "*.KCP", sep = ""))) #lists the variant KCPs
      addfile <- c(addfile, addfile2)
      rows2 <- as.numeric(0)
      filenames2 <- as.numeric(0)
      for (j in 1:length(addfile)) {
        addfile3 <- addfile[j] #lists the variant KCPs
        a <- data.frame(grep(addfile3, readLines(list.files(path = ".", pattern = ".key")[i]), value = TRUE)) #calculates the # of times the variant KCP name appears in the .key file
        rows2[j] <- nrow(a)
      }
      return(rows2)
    }
    rowrow <- c(rows, test2())
    final[i,] <- rowrow
    filename[i] <- list.files(path = ".", pattern = ".key")[i]
  }
  testresult <- data.frame(unlist(filename), final)
  addfilenames <- list.files(path = ".", pattern = glob2rx(paste(variantname, "*.kcp", sep = "")))
  addfilenames2 <- list.files(path = ".", pattern = glob2rx(paste(variantname, "*.KCP", sep = "")))
  addfilenames <- c(addfilenames, addfilenames2)
  colnames(testresult) <- c("key file", "package KCP", addfilenames)
  return(testresult)
}

CA_result <- test(project.location, variantname = "CA")
NC_result <- test(project.location, variantname = "NC")
SO_result <- test(project.location, variantname = "SO")
WS_result <- test(project.location, variantname = "WS")

