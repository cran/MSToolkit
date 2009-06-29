test.analyseData <- function(){
# generateData(20, subjects = 50, treatDoses = c(0, 15, 30), 
#   genParNames = "E0,ED50,EMAX", genParMean = c(0, 50, 100), genParVCov = diag(3),
#   genParBtwNames = "E0,ED50,EMAX", genParBtwVCov = diag(3), genParErrStruc="Additive",
#   respEqn = "E0 + (EMAX * DOSE) / (ED50 + DOSE)", interimSubj = c(.3, .6, 1))
  
# analysisCode <- function(data) {
#    doses <- sort(unique(data$DOSE))
#   outDf <- data.frame(DOSE=doses)
#   outDf$MEAN <- outDf$SE <- outDf$LOWER <- outDf$UPPER <- rep(1, nrow(outDf))
#   tabDose <- table(data$DOSE)
#   outDf$N <- table(data$DOSE)[as.character(doses)]
#   outDf
# }
# macroCode <- function(data) data.frame(SUCCESS=1)
 #interimCode <- function(data) list(STOP = FALSE)
# analyzeData(1:5, analysisCode, macroCode, interimCode )
 
  checkTrue( TRUE )
  
}
