########################
# homework2
# author: 104761507
########################

#install packages
list.of.packages <- c("ROCR", "tools")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos="http://cran.rstudio.com/")

library(ROCR)
library(tools)

# read parameters
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("USAGE: Rscript hw2_104761507.R --target male --files methods/method1.csv methods/method2.csv --out result.csv", call.=FALSE)
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

#check target
if(!(target %in% c("male", "female")))
  stop(paste("Unknown target", target), call.=FALSE)

if(!exists("out_f"))
  stop(paste("Output file not found"), call.=FALSE)

#print("PROCESS")
print(paste("target :", target))
print(paste("output file:", out_f))
print(paste("files      :", files))

results <- NULL
method <- c()
sensitivity <- c()
specificity <- c()
f1_score <- c()
auc_score <- c()

for(input_file in files)
{
  #read input file
  #input_file <- "methods/method1.csv"
  inputData <- read.csv(file=input_file, header=TRUE, sep=",")
  method <- c(method, file_path_sans_ext(basename(input_file)))
  
  predicted <- as.factor(inputData$prediction)
  expected <- as.factor(inputData$reference)
  
  #(conf_matrx <- confusionMatrix(data=predicted, reference=expected, positive=target, mode = "everything"))
  
  #confusion matrix
  resultframe <- data.frame(Reference=expected, Prediction=predicted)
  CM <- table(resultframe)
  
  #print(conf_matrx)
  
  if(target == "male"){
    TP <- CM[2,2]
    TN <- CM[1,1]
    FP <- CM[1,2]
    FN <- CM[2,1]
  }
  
  if(target == "female"){
    TP <- CM[1,1]
    TN <- CM[2,2]
    FP <- CM[2,1]
    FN <- CM[1,2]    
  }
  
  pre <- TP/(TP+FP)
  rec <- TP/(TP+FN)
  sen <- rec
  spe <- TN/(TN+FP)
  f1 <- (2*pre*rec)/(pre+rec)
  
  sensitivity <- c(sensitivity, round(sen,2))
  specificity <- c(specificity, round(spe,2))
  f1_score <- c(f1_score, round(f1,2))
  
  #AUC
  pred_vector <- as.numeric(inputData$pred.score) #make numeric
  if(target == "female")
    pred_vector <- 1 - pred_vector
  
  ref_labels <- as.factor(ifelse(inputData$reference== target, 1 , 0))
  
  auc_pred <- prediction(predictions = pred_vector, labels = ref_labels)
  auc_perf <- performance(auc_pred,"auc");
  auc_score <- c(auc_score, round(as.numeric(auc_perf@y.values),2))

}

#make dataframe
results <- data.frame(method=method, sensitivity = sensitivity, specificity = specificity, F1=f1_score, AUC = auc_score, stringsAsFactors = F)

#find max data
max_sensitivity <- results[which.max(results$sensitivity),]$method
max_specificity <- results[which.max(results$specificity),]$method
max_F1 <- results[which.max(results$F1),]$method
max_AUC <- results[which.max(results$AUC),]$method

#add max row
row <- cbind(method="highest", sensitivity = max_sensitivity, specificity = max_specificity, F1=max_F1, AUC = max_AUC)
results <- rbind(results, row)

#print(paste("Results:", results))

#save result
if(!file.exists(out_f)){
  file.create(out_f)
}
write.csv(results, file=out_f, quote = FALSE, row.names = FALSE)

print("--DONE--")