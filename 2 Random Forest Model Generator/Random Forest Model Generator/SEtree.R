#change your directory first!

library(tree)
library(randomForest)


#do the hierarchical clustering.
SE <- read.csv("SE.csv")

yhc.complete = hclust(dist(SE$SE), method = "complete")
tmp <- cutree (yhc.complete , 5)

for (k in 1:5) {
  
  SE <- read.csv("SE.csv")
  SE <- cbind(SE,tmp)
  train = which(SE$ReportYearNumber != "2012" & SE$tmp == toString(k))
  test = which(SE$ReportYearNumber == "2012" & SE$tmp == toString(k))
  #SE <- SE[which(SE$MarketArea == "Ford - Great Lake"),]  #Yunyan uncomment this line.
  SE <- SE[,c(1:(dim(SE)[2]-3))]
  set.seed(1)
  
  for (i in 1:10){
    
    #sample 1/2 of the entire dataset as traing set, return index.
    tree.SE = tree(SE~., SE, subset = train)
    
    rf.SE = randomForest(SE~., data = SE, subset = train, mtry = ceiling((ncol(SE)-1)/3) , importance = TRUE)
    importance(rf.SE)
    
    out <- capture.output(importance(rf.SE), file = NULL, append = FALSE)
    filename = paste("ImportanceTableSE","Group",k,"Trail", i,".txt", sep = "")
    cat(out, file=filename, sep="\n", append=TRUE)
    
    out <- capture.output(print(rf.SE), file = NULL, append = FALSE)
    filename = paste("InfoSE","Group",k,"Trail", i,".txt", sep = "")
    cat(out, file=filename, sep="\n", append=TRUE)
    
    yhat.rf = predict(rf.SE,newdata= SE[c(train),])
    MSE.SE.train <- mean ((yhat.rf - SE[c(train),]$SE)^2)
    
    
    cat(paste("MSE of training set:",MSE.SE.train,sep = " "), file=filename, sep="\n", append=TRUE)
    
    r2.SE.train <- 1 - sum((SE[c(train),]$SE - yhat.rf)^2)/sum((SE[c(train),]$SE - mean(SE[c(train),]$SE))^2)
    
    cat(paste("R square of training set:",r2.SE.train,sep = " "), file=filename, sep="\n", append=TRUE)
    
    
    # testing set analysis
    
    yhat.rf = predict(rf.SE,newdata= SE[c(test),])
    MSE.SE.test <- mean ((yhat.rf - SE[c(test),]$SE)^2)
    
    
    cat(paste("MSE of testing set:",MSE.SE.test,sep = " "), file=filename, sep="\n", append=TRUE)
    
    r2.SE.test <- 1 - sum((SE[c(test),]$SE - yhat.rf)^2)/sum((SE[c(test),]$SE - mean(SE[c(test),]$SE))^2)
    
    cat(paste("R square of testing set:",r2.SE.test,sep = " "), file=filename, sep="\n", append=TRUE)
    
  }
  
  
}


#execute seperately for multilinear regression. 
#The predictors should be changed based on your selection.

#ml.SE_19var = SE[c(train),]
#SEfit = lm(formula = SE  ~ NewKPI2 + KPI0116 + KPI3226 + NewKPI1 + NewKPI3 + KPI0068 + KPI0017 + KPI0012 + KPI0077 + KPI0759,  data = ml.SE_19var)
#summary(SEfit)
#out <- capture.output(summary(SEfit), file = NULL, append = FALSE)
#cat(out, file="SEfit.txt", sep="\n", append=TRUE)

