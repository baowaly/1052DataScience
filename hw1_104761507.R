########################
# homework1
# author: 104761507
########################

#install packages
list.of.packages <- c("tools")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos="http://cran.rstudio.com/")

#attaching packages
library(tools)

#processing commandline arguments
args = commandArgs(trailingOnly=TRUE)
#print(args)

i_f <- ""
o_f <- ""

test <- c("-files", "-out") %in% args
if (FALSE %in% test | length(args) < 4 ) {
  stop("USAGE: Rscript hw1_104761507.R -files test.1.csv -out result.csv", call.=FALSE)
} else {
  if(args[1]=="-files"){
    i_f <- args[2]
  }
  if(args[3]=="-out"){
    o_f <- args[4]
  }
  
  if(args[1]=="-out"){
    o_f <- args[2]
  }
  if(args[3]=="-files"){
    i_f <- args[4]
  }
  
}

#check input and output files
if(!file.exists(i_f)){
  stop("Input file does not exist!!", call.=FALSE)
}

if(o_f == ""){
  stop("No output filename is found!!", call.=FALSE)
}

#read input file
inputData <- read.csv(file=i_f, header=TRUE, sep=",")
max_weight <- round(max(inputData$weight),2)
max_height <- round(max(inputData$height),2)

#process data
file_without_ext <- file_path_sans_ext(basename(i_f))
result <- cbind(set=file_without_ext, weight=max_weight, height=max_height)

#save result
print(result)
if(!file.exists(o_f)){
  file.create(o_f)
}
write.csv(result, file=o_f, quote = FALSE, row.names = FALSE)
