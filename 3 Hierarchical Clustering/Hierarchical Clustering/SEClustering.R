SE <- read.csv("SE_interaction.csv")

#SE.large <- read.csv("SE.csv")

#x.kpi = matrix(nrow= nrow(SE),ncol= (ncol(SE)-3))
#for (i in 1:(ncol(SE)-3)){
#  x.kpi[1:nrow(x.kpi),i] = scale(SE[1:nrow(SE),i+1])
#}

y.kpi = matrix(nrow = nrow(SE),ncol = 1)
y.kpi[1:nrow(y.kpi),1] = scale(SE[1:nrow(SE),1])

#z.kpi = matrix(nrow = nrow(SE),ncol = 10)
#for (i in 1:10){
#  z.kpi[1:nrow(z.kpi),i] = SE[1:nrow(SE),i]
#}

for (k in 1:10){
  yhc.complete = hclust(dist(y.kpi), method = "complete")
  a <- cutree (yhc.complete , k)
  
  out <- capture.output(yhc.complete, file = NULL, append = FALSE)
  cat(out, file=paste("Clustering",k,".txt", sep=""), sep="\n", append=FALSE)
  
  
  out <- capture.output(table(a,a), file = NULL, append = FALSE)
  
  
  cat(out, file=paste("ClusteringTable",k,".txt", sep=""), sep="\n", append=FALSE)
  
  #xkm.out = kmeans(x.kpi,5,nstart=20)
  #ykm.out = kmeans(y.kpi,5,nstart=20)
  #zkm.out = kmeans(z.kpi,5,nstart=20)
  
  
  for (j in 1:k){
    train = which(a == toString(j))
    train.x = which(SE[c(train),]$ReportYearNumber != "2012")
    
    ml.SE_19var = SE[c(train.x),]
    SEfit = lm(formula = SE  ~ NewKPI2 + KPI0116 + KPI3226 + NewKPI1 + NewKPI3 + KPI0068 + KPI0017 + KPI0012 + KPI0077 + TS + SI + IT + TSI,  data = ml.SE_19var)
    summary(SEfit)
    
    out <- capture.output(summary(SEfit), file = NULL, append = FALSE)
    cat(out, file=paste("SEfit",k,".txt", sep=""), sep="\n", append=TRUE)
  }
  
}

