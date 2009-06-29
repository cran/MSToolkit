
# MSToolkit will create ReplicateData, MicroData and MacroData directories 
# under the current working directory
getwd()

generateData( replicateN = 5, subjects = 100, treatDoses = c(0, 5, 10, 50, 100), 
  genParNames = "E0,ED50,EMAX", genParMean = c(2,50,10), genParVCov = c(.5,30,10), 
  respEqn = "E0 + ((DOSE * EMAX)/(DOSE + ED50))",  respVCov = 2, 
  interimSubj = ".3,.7" 
  )

emax.fit<- function (y, dose, iparm = c("ed50", "e0", "emax"), ed50cutoff = 2.5 * max(doselev)) 
{
    dm <- tapply(y, dose, mean)
    dsd <- sqrt(tapply(y, dose, var))
    intercept <- rep(1, length(dose))
    doselev <- sort(unique(dose))
    est3 <- rep(NA, 3)
    estA <- rep(NA, 2)

        e0 <- min(dm)
        emax <- max(dm) - e0
		ed50 <- max(doselev)/2
        Sparm <- c(ed50 = ed50, e0 = e0, emax = emax)

        fit <- try(nls(y ~ e0 + (emax * dose)/(dose + ed50), 
            start = Sparm, control = nls.control(maxiter = 100), 
            trace = F), silent = T)

    AcceptConv <- F
    if (class(fit) == "nls") {
        est3 <- coef(fit)
        if (coef(fit)[1] > 0) {
            if (coef(fit)[1] <= ed50cutoff) {
                vc <- as.vector(vcov(fit))
                seout <- SeEmax3(fit, doselev)
                fitpred <- seout$fitpred
                sdpred <- seout$sdpred
				sddif <- seout$sddif
                AcceptConv <- T
            }
        }
    }
    if (!AcceptConv) {
        fitL <- lm(y ~ dose)
        sigL <- summary(fitL)$sigma
        predobj <- predict(fitL, data.frame(dose = doselev), se.fit = T)
        fitpred <- predobj$fit
        sdpred <- predobj$se.fit
		sddif <- doselev * sqrt(vcov(fitL)[2, 2])
    }
    return(list(fitpred = fitpred, 
        sdpred = sdpred, sddif = sddif))
}

SeEmax3<-function (fit, doselev) 
{
    E0<-coef(fit)[2]
    ED50<-coef(fit)[1]
    EMAX<-coef(fit)[3]
    fitpred <- E0+(EMAX*doselev)/(ED50+doselev)
    fitdif <- fitpred - E0
    vfcov <- vcov(fit)
    sddif <- sqrt((fitdif^2) * (vfcov[3, 3]/coef(fit)[3]^2 + 
        vfcov[1, 1]/(coef(fit)[1] + doselev)^2 - (2 * vfcov[1, 
        3])/(coef(fit)[3] * (coef(fit)[1] + doselev))))
    covint <- -((doselev * coef(fit)[3])/(coef(fit)[1] + doselev)^2) * 
        vfcov[2, 1] + (doselev/(coef(fit)[1] + doselev)) * vfcov[2, 
        3]
    sdpred <- sqrt(vfcov[2, 2] + sddif^2 + 2 * covint)
    return(list(fitpred = fitpred, sdpred = sdpred, fitdif = fitdif, 
        sddif = sddif))
}

emaxCode <- function(data){
    uniDoses <- sort( unique(data$DOSE)) 
	obsMean <- tapply(data$RESP, list(data$DOSE), mean)
	obsSD <- tapply(data$RESP, list(data$DOSE), sd)                                                                   
    eFit <- emax.fit( data$RESP, data$DOSE )
    outDf <- data.frame( DOSE = uniDoses, 
      MEAN = eFit$fitpred, 
      SE = eFit$sdpred,
	  SDDIF = eFit$sddif)
    outDf$LOWER <- outDf$MEAN - 1.96*outDf$SE
    outDf$UPPER <- outDf$MEAN + 1.96*outDf$SE
    outDf$N     <- table(data$DOSE)
	outDf$OBSMEAN <- obsMean
	outDf$OBSSD <- obsSD
    outDf 
}                                                                                                                   
             
macroCode <- function(data) {
  # Is effect at highest dose significant?
  success <- data$LOWER[data$INTERIM==max(data$INTERIM) & data$DOSE==max(data$DOSE)] > 7
  data.frame( SUCCESS = success )
}
  
interimCode <- function( data ){
  # DROP any doses where the lower bound of the difference from placebo is negative
  dropdose  <- with( data , DOSE [ LOWER < 0 & DOSE != 0] )
  outList <- list()
  if( length(dropdose) > 0 ) outList$DROP <- dropdose
  outList$STOP <- length(dropdose) == nrow(data)-1
  outList
}
   
analyzeData(analysisCode = emaxCode, macroCode = macroCode, 
  interimCode = interimCode )

