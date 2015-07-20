# ===== START OF SETUP =====
# Setup requires initialization of working directory, loading of Urban Science data,
# and loading of randomForest model

# Load Library
library(randomForest)

# Set working directory
setwd("/Users/hanlinyeo/Dropbox/Winter 2014/IOE 424/Effects of SE")

# Load RandomForest model (.RData File) and generate importance list
load(file.choose())
importance.SE<-importance(rf.SE)

# Load SE dealership level data provided by Urban Science
rawData.SE <- read.csv(file.choose())

# Function to generate plots for KPIXXXX based on a dealership record in testing set
# dealerYear used to select 2012 data (i.e. testing set)
# dealerNumber = dealerCode, dealerYear = recordYear, kpiNumber = KPIXXXX
SEvsKPI <- function(rawData.SE, kpiNumber, dealerNumber, dealerYear){
  
  # Extract dealership record from testing set
  dealership <- subset(rawData.SE, (DealerCode == dealerNumber) & (ReportYearNumber == dealerYear))
  futureKPI <- dealership[[kpiNumber]]
  
  # Remove testing set
  rawData.SE <- subset(rawData.SE, ReportYearNumber != dealerYear)
  
  # Find values at every percentile (1-99) for KPIXXXX in training set
  percentiles <- seq(0.01,0.99,0.01)
  percentiles.SE <- quantile(rawData.SE[[kpiNumber]], percentiles, na.rm=TRUE, names=TRUE)
  medianKPI <- median(rawData.SE[[kpiNumber]])
  
  # Create matrix of 2 columns (percentile, KPIXXXX at that percentile)
  output <- cbind(percentiles.SE, seq(1:length(percentiles)))
  colnames(output) <- c(kpiNumber, "SE")
  
  # Sub 1st - 99th percentile values for KPIXXXX into dealership record and predict SE 
  for (i in 1:length(percentiles)){
    dealership[[kpiNumber]] <- percentiles.SE[i]
    output[i,2] <- predict(rf.SE, dealership)
  }
  
  # Print Scatter plot
  output <- as.data.frame(output)
  png(filename=paste("SE - ", kpiNumber, ".png", sep=""))
  plot(output[[kpiNumber]], output[["SE"]], main=paste("Predicted SE against",kpiNumber), xlab=kpiNumber, ylab="Predicted SE", ylim = c(1.1, 2.2), pch=16)
  
  # Show actual KPI value and SE as red dot
  points(futureKPI, dealership[["SE"]], col = "red", pch = 16)
  dev.off()
  
  return(output)
}
# ===== END OF SETUP =====

# ===== IMPLEMENTATION =====
# Implementation step prints a list of 8 most important KPIs for SE, tables of
# predicted SE for each KPI percentile, and sensitivity plots for dealership record
# Required: Input dealerCode and reportYear

# Create text file that lists important KPIs (by rank)
importance.SE <- importance.SE[order(-importance.SE[,1]),]
topKPIs.SE <- names(importance.SE[1:8,1])
write.table(topKPIs.SE, file = "SE - TopKPIs.txt", sep = "\t")

# Print Plots for top KPIs, given dealerCode (e.g. 1000010) and reportYear (e.g. 2012)
for (i in 1:length(topKPIs.SE)){
  
  # Change dealerCode and reportYear to analyze other dealership records
  output <- SEvsKPI(rawData.SE, topKPIs.SE[i], 1000010, 2012)
  
  # Print table of predicted SE at each KPI percentile
  write.table(output, file = paste("SE - ", topKPIs.SE[i], ".txt", sep = ""), sep="\t")
}
