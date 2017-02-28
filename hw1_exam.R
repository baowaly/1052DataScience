########################
# homework1 example
########################
args = commandArgs(trailingOnly=TRUE)
if (length(args) < 2) {
  stop("USAGE: Rscript hw1_exam.R input", call.=FALSE)
} else {
  i_f <- args[1]
  o_f <- args[2]
}

#print(i_f)
#print(o_f)

inputData <- read.csv(file=i_f, header=TRUE, sep=",")

max_weight <- round(max(inputData$weight),2)
max_height <- round(max(inputData$height),2)

file_without_ext <- tools::file_path_sans_ext(basename(i_f))
row <- cbind(set=file_without_ext, weight=max_weight, height=max_height)
if(!file.exists(o_f)){
  file.create(o_f)
}

write.csv(row, file=o_f, quote = FALSE, row.names = FALSE)