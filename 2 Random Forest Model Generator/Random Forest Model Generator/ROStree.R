#change your directory first!

library(tree)
library(randomForest)

ROS <- read.csv("ROS.csv") #change the name of the file you imported.
train = which(ROS$ReportYearNumber != "2012")
#ROS <- ROS[which(ROS$MarketArea == "Ford - Great Lake"),]  #Han uncomment this line
ROS <- ROS[,c(1:(dim(ROS)[2]-2))] #remove the last column related to the region name.
set.seed(1)

for (i in 1:10 ){
  #sample 1/2 of the entire dataset as traing set, return index.
  tree.ROS = tree(ROS ~., ROS, subset = train)
  
  rf.ROS = randomForest(ROS~., data = ROS, subset = train, mtry = ceiling((ncol(ROS)-1)/3) , importance = TRUE)
  importance(rf.ROS)
  
  out <- capture.output(importance(rf.ROS), file = NULL, append = FALSE)
  filename = paste("ImportanceTableROS", i,".txt", sep = "")
  cat(out, file=filename, sep="\n", append=TRUE)
  
  out <- capture.output(print(rf.ROS), file = NULL, append = FALSE)
  filename = paste("InfoROS", i,".txt", sep = "")
  cat(out, file=filename, sep="\n", append=TRUE)
  
  yhat.rf = predict(rf.ROS,newdata= ROS[c(train),])
  MSE.ROS.train <- mean ((yhat.rf - ROS[c(train),]$ROS)^2)
  
  
  cat(paste("MSE of training set:",MSE.ROS.train,sep = " "), file=filename, sep="\n", append=TRUE)
  
  r2.ROS.train <- 1 - sum((ROS[c(train),]$ROS - yhat.rf)^2)/sum((ROS[c(train),]$ROS - mean(ROS[c(train),]$ROS))^2)
  
  cat(paste("R square of training set:",r2.ROS.train,sep = " "), file=filename, sep="\n", append=TRUE)
  
  
  # testing set analysis
  
  yhat.rf = predict(rf.ROS,newdata= ROS[-c(train),])
  MSE.ROS.test <- mean ((yhat.rf - ROS[-c(train),]$ROS)^2)
  
  
  cat(paste("MSE of testing set:",MSE.ROS.test,sep = " "), file=filename, sep="\n", append=TRUE)
  
  r2.ROS.test <- 1 - sum((ROS[-c(train),]$ROS - yhat.rf)^2)/sum((ROS[-c(train),]$ROS - mean(ROS[-c(train),]$ROS))^2)
  
  cat(paste("R square of testing set:",r2.ROS.test,sep = " "), file=filename, sep="\n", append=TRUE)
  
}


#varImpPlot(rf.ROS)


#execute seperately for multilinear regression. 
#The predictors should be changed based on your selection.


ml.ROS = ROS[c(train),]
ROSfit = lm(formula = ROS ~ KPI0004 + KPI3226 + KPI0003 + KPI0031 + KPI0008 + KPI0008 + KPI0122 + KPI0012 + KPI0030 + KPI0514 + KPI0013 + KPI0020, data = ml.ROS)
summary(ROSfit)

#out <- capture.output(summary(ROSfit), file = NULL, append = FALSE)
#cat(out, file="ROSfit.txt", sep="\n", append=TRUE)

