########################
# homework1
# author: 104761507
########################

#install packages
list.of.packages <- c("ROCR", "caret")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos="http://cran.rstudio.com/")

library(caret)
library(ROCR)


query_func<-function(target, i)
{
  if(target == "male"){
    which.min(i)
  }
  else if (target == "female") {
    which.max(i)
  } else {
    stop(paste("ERROR: unknown target function", target))
  }
}

# read parameters
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("USAGE: Rscript hw1.R -query min|max -files file1 file2 ... filen –out out.csv", call.=FALSE)
}

# parse parameters
i<-1 
while(i < length(args))
{
  if(args[i] == "--target"){
    target<-args[i+1]
    i<-i+1
  }else if(args[i] == "--files"){
    j<-grep("-", c(args[(i+1):length(args)], "-"))[1]
    files<-args[(i+1):(i+j-1)]
    i<-i+j-1
  }else if(args[i] == "--out"){
    out_f<-args[i+1]
    i<-i+1
  }else{
    stop(paste("Unknown flag", args[i]), call.=FALSE)
  }
  i<-i+1
}

print("PROCESS")
print(paste("target :", target))
print(paste("output file:", out_f))
print(paste("files      :", files))

# read files
names<-c()
weis<-c()
heis<-c()
for(file in files)
{
  name<-gsub(".csv", "", basename(file))
  d<-read.table(file, header=T,sep=",")
  weis<-c(weis, d$weight[query_func(target, d$weight)])
  heis<-c(heis, d$height[query_func(target, d$height)])
  names<-c(names,name)
}
out_data<-data.frame(set=names, wei=weis, hei=heis, stringsAsFactors = F)
index<-sapply(out_data[,c("wei","hei")], query_func, target=target)

# output file
out_data<-rbind(out_data,c(target,names[index]))
write.table(out_data, file=out_f, row.names = F, quote = F)
