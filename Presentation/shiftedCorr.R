shiftedCorr <- function(dataFrame1,dataFrame2,colName1,colName2,yearDelta,stateConv) {
  dataFrame1$State <- as.character(dataFrame1$State)
  dataFrame2$State <- as.character(dataFrame2$State)
  dataFrame1$Year=dataFrame1$Year+yearDelta
  mergeDF<-merge(dataFrame1,dataFrame2,by=c('Year', 'State'))
  stateLIST <- unique(mergeDF$State)
  numStates <- length(stateLIST)
  leadCrimeCorr <- NULL
  for ( s in 1:numStates ) {
    theState <- stateLIST[s]
    subDF <- mergeDF[ mergeDF$State == theState, ]
    subDF2 <- stateConv[ stateConv$State == tolower(theState), ]
    subDF2$Corr <-cor(subDF[,colName1], subDF[,colName2], use="complete.obs")
    leadCrimeCorr <- rbind(leadCrimeCorr,subDF2)
  }
  return(leadCrimeCorr)
}


