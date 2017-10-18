#########################  Overview and description
# 
#   1. defining all names of files
#         
#   2. loading all files
#  
#


###########################################################################
######################### 1.
###########################################################################
#
filenameMake <- "make.csv"
filenameModel <- "model.csv"
filenameZip <- "de_postal_codes.txt"
filenameColurIdLabel <- "colourIdLabel.csv"


###########################################################################
######################### 2.
###########################################################################
#
makeCSV <- read.csv(paste0("../data/",filenameMake), sep = "\t")
modelCSV <- read.csv(paste0("../data/",filenameModel), sep = "\t")
zipCodes <- read.table(paste0("../data/", filenameZip), sep = "\t", header= TRUE)
colourLabelID <- read.csv(paste0("../data/",filenameColurIdLabel), sep = ";")





