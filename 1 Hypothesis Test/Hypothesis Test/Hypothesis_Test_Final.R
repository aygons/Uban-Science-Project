library(Hmisc)

# Draw Density Plot
DensityPlot <- function(data, kpiName, kpiLabel, line1, line2, response){
  xLabel <- paste(kpiName, " ", "(", kpiLabel, ")", sep="")
  mainLabel <- paste("Density Scatter Plot of", response, "against", xLabel)
  smoothScatter(data[[kpiName]], 
                data[[response]], 
                nbin=300, 
                main=mainLabel,
                xlab=xLabel,
                ylab=response)
  abline(v=line1,col=4,lty=2)
  abline(v=line2,col=4,lty=2)
}

# Perform Wilcoxon and T-Test
HypoTest <- function(data, kpiName, lower, upper, response){
  # Divide into within threshold and outside threshold
  data.in<-subset(data, (data[[kpiName]] >= lower) & (data[[kpiName]] <= upper))
  data.out<-subset(data, (data[[kpiName]] < lower) | (data[[kpiName]] > upper))
  
  # SET KPI number for ROS, state alt. hypothesis, show confidence interval
    print(wilcox.test(data.in[[response]], data.out[[response]], alternative="greater", paired=FALSE, conf.int=TRUE, exact=FALSE, correct=FALSE))
    print(t.test(data.in[[response]], data.out[[response]], alternative="greater", conf.level=0.95))
}

# Bin
BinData <- function(data, numBins, kpiName, kpiLabel, response){
  data$Group <- cut2(data[[kpiName]], g=numBins)
  mainLabel <- paste("Box Plots of ", response, " against binned ", kpiName, " (", kpiLabel, ")", sep="")
  xLabel <- paste("Binned Ranges for ", kpiName, " (", kpiLabel, ")", sep="")
  
  # Plot Boxplot
  boxplot(data[[response]] ~ data$Group,
          main=mainLabel,
          xlab=xLabel,
          ylab=response,
          outline=F)
  means <- tapply(data[[response]], data$Group, mean)
  points(means, col="red", pch=18)
}

# Load SE Data (Restrict to records with SE between 0 and 5)
data.SE <- read.csv(file.choose())
data.SE <- subset(data.SE, SE >= 0 & SE <= 5)
data.SE <- subset(data.SE, ReportYearNumber==2012)

# Load ROS Data
data.ROS <- read.csv(file.choose())
data.ROS <- subset(data.ROS, ReportYearNumber==2012)

# KPI = 3079
# Guide = 10 - 12
# Range = 10 - 12
kpiName <- "KPI3079"
kpiLabel <- "Ave Month New/Used Veh Sales per Salesperson (Units)"
lower <- 10
upper <- 12
numBins <- 10
DensityPlot(data.SE, kpiName, kpiLabel, lower, upper, "SE")
HypoTest(data.SE, kpiName, lower, upper, "SE")
BinData(data.SE, numBins, kpiName, kpiLabel, "SE")

DensityPlot(data.ROS, kpiName, kpiLabel, lower, upper, "ROS")
HypoTest(data.ROS, kpiName, lower, upper, "ROS")
BinData(data.ROS, numBins, kpiName, kpiLabel, "ROS")

# KPI = 122
# Guide = 1.25
# Range = 1.1875 - 1.3125
kpiName <- "KPI0122"
kpiLabel <- "Used to New Ratio: Truck for Department Financial"
lower <- 1.1875
upper <- 1.3125
numBins <- 20
DensityPlot(data.SE, kpiName, kpiLabel, lower, upper, "SE")
HypoTest(data.SE, kpiName, lower, upper, "SE")
BinData(data.SE, numBins, kpiName, kpiLabel, "SE")

DensityPlot(data.ROS, kpiName, kpiLabel, lower, upper, "ROS")
HypoTest(data.ROS, kpiName, lower, upper, "ROS")
BinData(data.ROS, numBins, kpiName, kpiLabel, "ROS")

# KPI = 540
# Guide = 1.25
# Range = 1.1875 - 1.3125
kpiName <- "KPI0540"
kpiLabel <- "Used to New Ratio: Car for the Used Department Financial"
lower <- 1.1875
upper <- 1.3125
numBins <- 20
DensityPlot(subset(data.SE, data.SE$KPI0540 >= 0 & data.SE$KPI0540 <= 50), kpiName, kpiLabel, lower, upper, "SE") # Ratio cannot be negative. Larger than 50 seems to be unusual.
HypoTest(subset(data.SE, data.SE$KPI0540 >= 0 & data.SE$KPI0540 <= 50), kpiName, lower, upper, "SE")
BinData(subset(data.SE, data.SE$KPI0540 >= 0 & data.SE$KPI0540 <= 50), numBins, kpiName, kpiLabel, "SE")

DensityPlot(subset(data.ROS, data.ROS$KPI0540 >= 0 & data.ROS$KPI0540 <= 50), kpiName, kpiLabel, lower, upper, "ROS")
HypoTest(subset(data.ROS, data.ROS$KPI0540 >= 0 & data.ROS$KPI0540 <= 50), kpiName, lower, upper, "ROS")
BinData(subset(data.ROS, data.ROS$KPI0540 >= 0 & data.ROS$KPI0540 <= 50), numBins, kpiName, kpiLabel, "ROS")

# KPI = 539
# Guide = 30
# Range = 28.5 - 31.5
kpiName <- "KPI0539"
kpiLabel <- "Truck Day Supply for the Used Department Financial"
lower <- 28.5
upper <- 31.5
numBins <- 20
DensityPlot(subset(data.SE, data.SE$KPI0539 <= 2000), kpiName, kpiLabel, lower, upper, "SE")
HypoTest(subset(data.SE, data.SE$KPI0539 <= 2000), kpiName, lower, upper, "SE")
BinData(subset(data.SE, data.SE$KPI0539 <= 2000), numBins, kpiName, kpiLabel, "SE")

DensityPlot(subset(data.ROS, data.ROS$KPI0539 <= 2000), kpiName, kpiLabel, lower, upper, "ROS")
HypoTest(subset(data.ROS, data.ROS$KPI0539 <= 2000), kpiName, lower, upper, "ROS")
BinData(subset(data.ROS, data.ROS$KPI0539 <= 2000), numBins, kpiName, kpiLabel, "ROS")

# KPI = 1039
# Guide = 39,000 - 45,000
# Range = 39,000 - 45,000
kpiName <- "KPI1039"
kpiLabel <- "Total Sales per Management and Salespeople for the Parts Department"
lower <- 39000
upper <- 45000
numBins <- 10
DensityPlot(data.SE, kpiName, kpiLabel, lower, upper, "SE")
HypoTest(data.SE, kpiName, lower, upper, "SE")
BinData(data.SE, numBins, kpiName, kpiLabel, "SE")

DensityPlot(data.ROS, kpiName, kpiLabel, lower, upper, "ROS")
HypoTest(data.ROS, kpiName, lower, upper, "ROS")
BinData(data.ROS, numBins, kpiName, kpiLabel, "ROS")

# KPI = 1021
# Guide = 1.5
# Range = 1.425 - 1.575
kpiName <- "KPI1021"
kpiLabel <- "Parts Inventory Months Supply for the Parts Department"
lower <- 1.425
upper <- 1.575
numBins <- 20
DensityPlot(subset(data.SE, data.SE$KPI1021 <= 50), kpiName, kpiLabel, lower, upper, "SE")
HypoTest(subset(data.SE, data.SE$KPI1021 <= 50), kpiName, lower, upper, "SE")
BinData(subset(data.SE, data.SE$KPI1021 <= 50), numBins, kpiName, kpiLabel, "SE")

DensityPlot(subset(data.ROS, data.ROS$KPI1021 <= 50), kpiName, kpiLabel, lower, upper, "ROS")
HypoTest(subset(data.ROS, data.ROS$KPI1021 <= 50), kpiName, lower, upper, "ROS")
BinData(subset(data.ROS, data.ROS$KPI1021 <= 50), numBins, kpiName, kpiLabel, "ROS")

# KPI = 913
# Guide = 0.38
# Range = 0.361 - 0.399
kpiName <- "KPI0913"
kpiLabel <- "Gross as % Sales for the Parts Department"
lower <- 0.361
upper <- 0.399
numBins <- 20
DensityPlot(data.SE, kpiName, kpiLabel, lower, upper, "SE")
HypoTest(data.SE, kpiName, lower, upper, "SE")
BinData(data.SE, numBins, kpiName, kpiLabel, "SE")

DensityPlot(data.ROS, kpiName, kpiLabel, lower, upper, "ROS")
HypoTest(data.ROS, kpiName, lower, upper, "ROS")
BinData(data.ROS, numBins, kpiName, kpiLabel, "ROS")
