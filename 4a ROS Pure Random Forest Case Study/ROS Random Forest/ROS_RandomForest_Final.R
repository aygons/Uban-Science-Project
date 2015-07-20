# ===== START OF SETUP =====
# Setup requires initialization of working directory, loading of Urban Science data,
# and loading of randomForest model

# Load Library
library(randomForest)

# Set working directory
setwd("/Users/hanlinyeo/Dropbox/Winter 2014/IOE 424/Effects of ROS")

# Load RandomForest model (.RData File) and generate importance list
load(file.choose())
importance.ROS<-importance(rf.ROS)

# Load ROS dealership level data provided by Urban Science
rawData.ROS <- read.csv(file.choose())

# Function to generate plots for KPIXXXX based on a dealership record in testing set
# dealerYear used to select 2012 data (i.e. testing set)
# dealerNumber = dealerCode, dealerYear = recordYear, kpiNumber = KPIXXXX
ROSvsKPI <- function(rawData.ROS, kpiNumber, dealerNumber, dealerYear){
  
  # Extract dealership record from testing set
  dealership <- subset(rawData.ROS, (DealerCode == dealerNumber) & (ReportYearNumber == dealerYear))
  futureKPI <- dealership[[kpiNumber]]
  
  # Remove testing set
  rawData.ROS <- subset(rawData.ROS, ReportYearNumber != dealerYear)
  
  # Find values at every percentile (1-99) for KPIXXXX in training set
  percentiles <- seq(0.01,0.99,0.01)
  percentiles.ROS <- quantile(rawData.ROS[[kpiNumber]], percentiles, na.rm=TRUE, names=TRUE)
  medianKPI <- median(rawData.ROS[[kpiNumber]])
  
  # Create matrix of 2 columns (percentile, KPIXXXX at that percentile)
  output <- cbind(percentiles.ROS, seq(1:length(percentiles)))
  colnames(output) <- c(kpiNumber, "ROS")
  
  # Sub 1st - 99th percentile values for KPIXXXX into dealership record and predict ROS 
  for (i in 1:length(percentiles)){
    dealership[[kpiNumber]] <- percentiles.ROS[i]
    output[i,2] <- predict(rf.ROS, dealership)
  }
  
  # Print Scatter plot
  output <- as.data.frame(output)
  png(filename=paste("ROS - ", kpiNumber, ".png", sep=""))
  plot(output[[kpiNumber]], output[["ROS"]], main=paste("Predicted ROS against",kpiNumber), xlab=kpiNumber, ylab="Predicted ROS", ylim = c(0, 0.055), pch=16)
  
  # Show actual KPI value and ROS as red dot
  points(futureKPI, dealership[["ROS"]], col="red", pch=16)
  dev.off()

  return(output)
}
# ===== END OF SETUP =====

# ===== IMPLEMENTATION =====
# Implementation step prints a list of 12 most important KPIs for ROS, tables of
# predicted ROS for each KPI percentile, and sensitivity plots for dealership record
# Required: Input dealerCode and reportYear

# Create text file that lists important KPIs (by rank)
importance.ROS <- importance.ROS[order(-importance.ROS[,1]),]
topKPIs.ROS <- names(importance.ROS[1:12,1])
write.table(topKPIs.ROS, file = "ROS - TopKPIs.txt", sep = "\t")

# Print Plots for top KPIs, given dealerCode (e.g. 1002099) and reportYear (e.g. 2012)
for (i in 1:length(topKPIs.ROS)){
  
  # Change dealerCode and reportYear to analyze other dealership records
  output <- ROSvsKPI(rawData.ROS, topKPIs.ROS[i], 1002099, 2012)
  
  # Print table of predicted ROS at each KPI percentile
  write.table(output, file = paste("ROS - ", topKPIs.ROS[i], ".txt", sep = ""), sep="\t")
}
