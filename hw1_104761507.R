########################
# homework1 
# author: 104761507
########################
args = commandArgs(trailingOnly=TRUE)

#print(args)

i_f <- ""
o_f <- ""

if (length(args) < 4 ) {
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

if(!file.exists(i_f)){
  stop("Input file does not exist!!", call.=FALSE)
}

if(o_f == ""){
  stop("No output filename is found!!", call.=FALSE)
}

inputData <- read.csv(file=i_f, header=TRUE, sep=",")
max_weight <- round(max(inputData$weight),2)
max_height <- round(max(inputData$height),2)

file_without_ext <- tools::file_path_sans_ext(basename(i_f))
row <- cbind(set=file_without_ext, weight=max_weight, height=max_height)
if(!file.exists(o_f)){
  file.create(o_f)
}

write.csv(row, file=o_f, quote = FALSE, row.names = FALSE)
