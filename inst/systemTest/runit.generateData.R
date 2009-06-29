
if( !exists("systemTestPath")) systemTestPath <- "."
covariatesDataFile <- file.path( systemTestPath, "data", "testCovariates.csv")
parametersDataFile <- file.path( systemTestPath, "data", "testParam.csv")

test.generateData.call1 <- function() {

  ## set up a temp directory as the workingPath
  dir.create( workPath <- tempfile() )
  
  # Set up elements of the run
  respFun <- "E0 + (EMAX * D) / (ED50 + D)"
  seqMat <- cbind(c(0, 15, 30, 45), c(45, 15, 30, 0) ,c(45, 30, 15, 0))
  dFun <- function(data, prop) ifelse(data$HOUR <= 0, 0, sample(0:1, nrow(data), TRUE, c(1-prop, prop)))

  # Create a dummy file (to check if it is overwritten)
  dir.create( file.path(workPath, "ReplicateData"), showWarnings = FALSE)
  write.csv(data.frame(X=1:3, Y=1:3), file.path( workPath, "ReplicateData", "replicate0003.csv") )

  # Execute call 1
  genCall1 <- try(generateData(replicateN = 2, subjects = 500, treatPeriod = c(0, 1:3, 5), treatSeq = seqMat, treatProp = c(0.2, 0.2, 0.6), 
	  conCovNames = c("ConCov1","ConCov2","ConCov3"), conCovMean = rep(0, 3), conCovVCov=diag(3), conCovCrit = "ConCov1 > 0",
	  disCovNames = "DisCov1,DisCov2,DisCov3", disCovVals=list(0:1,1:2,1:3), disCovProb = ".5,.5#.5,.5#.3,.3,.4",
	  genParNames = c("E0","ED50","EMAX"), genParMean = c(0, 50, 10), genParVCov=diag(c(5, 10, 0)), genParCrit = "E0 > 0",
	  genParBtwNames = c("E0", "EMAX"), genParBtwVCov = diag(2), genParBtwCrit = "E0 > 0", genParErrStruc = "Additive",
	  respEqn = respFun, respCrit = c("MyResponse < 6","MyResponse > 0"), respVCov = 1, respName = "MyResponse", 
    idCol="SUBJ", doseCol="D", timeCol="HOUR", trtCol="T", mcarProp=.05, 
    dropFun = dFun, dropFunExtraArgs = list(prop = .05), interimSubj = c(.3, .6, .8, 1), 
    missingFlag = "MISSFLAG", parOmitFlag = "PARFLAG", respOmitFlag = "RESPFLAG", interimCol = "INT", seed=1, 
    workingPath = workPath, mcarRule = "HOUR > 0" ))

  # Check basics
  checkTrue(class(genCall1) != "try-error", msg = "Check the call was successful")
  checkTrue( file.exists( file.path(workPath, "ReplicateData") ), msg = "Check ReplicateData directory has been created")
  checkTrue( file.exists( file.path(workPath, "ReplicateData", "replicate0001.csv")), msg = "Check Replicate Data replicate0001.csv was created")
  checkTrue( file.exists( file.path(workPath, "ReplicateData", "replicate0002.csv")), msg = "Check Replicate Data replicate0002.csv was created" )
  checkTrue(!file.exists( file.path(workPath, "ReplicateData", "replicate0003.csv")), msg = "Check Replicate Data replicate0003.csv was removed")
  
  # Import the data
  x <- lapply(1:2, readData, dataType="Replicate", workingPath = workPath )
  
  # Check variables exist in the data
  checkTrue(all(c("SUBJ", "T", "HOUR", "D") %in% names(x[[1]])), msg = "Check dosing variables are in the data")
  checkTrue(all(c("ConCov1", "ConCov2", "ConCov3", "DisCov1", "DisCov2", "DisCov3") %in% names(x[[1]])), msg = "Check covariate variables are in the data")
  checkTrue(all(c("E0", "ED50", "EMAX", "PARFLAG") %in% names(x[[1]])), msg = "Check parameter variables are in the data")
  checkTrue(all(c("MyResponse", "RESPFLAG", "MISSFLAG", "INT") %in% names(x[[1]])), msg = "Check response variables are in the data")
  
  # Check dosing regimes set up
  theTreats <- unique(x[[1]][,c("T", "HOUR", "D")])
  theTreats <- theTreats[order(theTreats$T, theTreats$HOUR, theTreats$D),]
  checkTrue(nrow(theTreats) == 15 && all(theTreats$T == rep(1:3, each=5)), msg = "Check treatment column (T)")
  checkTrue(nrow(theTreats) == 15 && all(theTreats$HOUR == rep(c(0, 1:3, 5), 3)), msg = "Check time column (HOUR)")
  checkTrue(nrow(theTreats) == 15 && all(theTreats$D == as.vector(rbind(0, seqMat))), msg = "Check dose column (D)")

  # Check treatment allocation
  x1 <- unique(x[[1]][,c("SUBJ", "T")])
  x2 <- unique(x[[2]][,c("SUBJ", "T")])
  x2$SUBJ <- x2$SUBJ + 500
  treatAlloc <- rbind(x1, x2)
  checkTrue(nrow(treatAlloc) == 1000, msg = "Check all subjects returned and allocated")
  checkTrue(binom.test(table(treatAlloc$T != 1), p=.2)$p.value > 0.05, msg = "Check correct proportional allocation of treatment 1")
  #checkTrue(binom.test(table(treatAlloc$T != 2), p=.2)$p.value > 0.05, msg = "Check correct proportional allocation of treatment 2")
  checkTrue(binom.test(table(treatAlloc$T != 3), p=.6)$p.value > 0.05, msg = "Check correct proportional allocation of treatment 3")
  checkTrue(!all(x1$T == x2$T), msg = "Check continuous treatments differ between replicates")
  
  # Check continuous covariates
  conData1 <- x[[1]][!duplicated(x[[1]]$SUBJ),c("ConCov1", "ConCov2", "ConCov3")]
  conData2 <- x[[2]][!duplicated(x[[1]]$SUBJ),c("ConCov1", "ConCov2", "ConCov3")]
  checkTrue(!all(conData1$ConCov1 == conData2$ConCov1), msg = "Check covariates differ between replicates")
  checkTrue(all(conData1$ConCov1 >= 0), msg = "Check all of covariate 1 are greater than 0 (as specified in the criteria)")
  checkTrue(t.test(conData1$ConCov2)$p.value > 0.05, msg = "Check distribution of covariate 2")
  checkTrue(t.test(conData1$ConCov3)$p.value > 0.05, msg = "Check distribution of covariate 3")
  checkTrue(cor.test(conData1$ConCov2, conData1$ConCov3)$p.value > 0.05, msg = "Check for (lack of) correlation between covariates")
  
  # Check discrete covariates: disCovVals=list(0:1,1:2,1:3), disCovProb = ".5,.5#.5,.5#.3,.3,.4",
  disData1 <- x[[1]][!duplicated(x[[1]]$SUBJ),c("DisCov1", "DisCov2", "DisCov3")]
  disData2 <- x[[2]][!duplicated(x[[1]]$SUBJ),c("DisCov1", "DisCov2", "DisCov3")]
  checkTrue(!all(disData1$DisCov1 == disData2$DisCov1), msg = "Check discrete covariates differ between replicates")
  checkTrue(all(disData1$DisCov1 %in% 0:1), msg = "Check values for discrete covariate 1")
  checkTrue(all(disData1$DisCov2 %in% 1:2), msg = "Check values for discrete covariate 2")
  checkTrue(all(disData1$DisCov3 %in% 1:3), msg = "Check values for discrete covariate 3")
  checkTrue(all(binom.test(table(disData1$DisCov1 != 1), p=.5)$p.value >.05), msg = "Check proportion for discrete covariate 1")
  checkTrue(all(binom.test(table(disData1$DisCov2 != 1), p=.5)$p.value >.05), msg = "Check proportion for discrete covariate 1")
  checkTrue(all(binom.test(table(disData1$DisCov3 != 1), p=.3)$p.value >.05), msg = "Check proportion for discrete covariate 1 (value 1)")
  checkTrue(all(binom.test(table(disData1$DisCov3 != 3), p=.4)$p.value >.05), msg = "Check proportion for discrete covariate 1 (value 3)")

  # Check parameters
  parData <- x[[1]][,c("SUBJ", "E0", "ED50", "EMAX")]
  checkTrue(all(parData$ED50 == parData$ED50[1]), msg = "Check all ED50s are the same (no random effects)")
  checkTrue(!all(parData$E0 == parData$E0[1]), msg = "Check E0 effects differ between subject")
  checkTrue(!all(parData$EMAX == parData$EMAX[1]), msg = "Check EMAX effects differ between subject")
  checkTrue(all(tapply(parData$EMAX, parData$SUBJ, function(x) all(x == x[1]))), msg = "Check effects are the same within subject")
  parData <- parData[!duplicated(parData$SUBJ),]
  checkTrue(t.test(parData$EMAX, mu = 10)$p.value > 0.05, msg = "Check distribution of EMAX")

  # Check response
  respData <- x[[1]][,c("E0", "ED50", "EMAX", "D", "MyResponse", "RESPFLAG")]
  respData$PRED <- with(respData, E0 + (EMAX * D) / (ED50 + D))
  respData$RES <- respData$MyResponse - respData$PRED
  checkTrue(t.test(respData$RES[1:500])$p.value > .05, msg = "Check response variable has been created correctly")
  respTab <- table(respData$RESPFLAG, cut(respData$MyResponse, c(-100, 0, 6, 100)))
  checkTrue(nrow(respTab) == 2 & ncol(respTab) == 3 & sum(respTab) == nrow(respData), msg = "Check values have been generated outside the range")
  checkTrue(respTab[1,1] == 0 & respTab[2,2] == 0 & respTab[1,3] == 0, msg = "Check values outside response range have been flagged as omitted")
  
  # Check missings
  missTab <- table(x[[1]]$HOUR, x[[1]]$MISSFLAG)
  expectMCAR <- c(0, rep(.05*500, 4))
  expectDrop <- 0:4 * 0.05 * 475
  missTab <- cbind(missTab, MCAR = expectMCAR, DROP = expectDrop, EXPECT = expectMCAR + expectDrop )
  checkTrue(all(apply(missTab, 1, function(x) binom.test(x[2:1], p=x[5]/500)$p.value) > .05), msg = "Checking actual vs expected missing proportions")
  
  # Check interim data
  intData <- x[[1]][,c("SUBJ", "INT")]
  checkTrue(all(tapply(intData$INT, intData$SUBJ, function(x) all(x == x[1]))), msg = "Check interims allocated are the same within subject")
  intData <- intData[!duplicated(intData$SUBJ),]
  checkTrue(all(intData$INT %in% 1:4), msg = "Check values of interims allocated")
  checkTrue(binom.test(table(intData$INT != 1), p = .3)$p.value > 0.05, msg = "Check proportions of interims allocated (interim 1)")
  checkTrue(binom.test(table(intData$INT != 2), p = .3)$p.value > 0.05, msg = "Check proportions of interims allocated (interim 2)")
  checkTrue(binom.test(table(intData$INT != 3), p = .2)$p.value > 0.05, msg = "Check proportions of interims allocated (interim 3)")
  checkTrue(binom.test(table(intData$INT != 4), p = .2)$p.value > 0.05, msg = "Check proportions of interims allocated (interim 4)")
  
  
  unlink(workPath, recursive = TRUE)
  invisible(NULL)
}

test.generateData.call2 <- function() {

  ## set up a temp directory as the workingPath
  dir.create( workPath <- tempfile() )

  # Set up elements of the run
  respFun <- "E0 + (EMAX * DOSE) / (ED50 + DOSE)"
  
  # Create a dummy file (to check if it is overwritten)
  dir.create( file.path(workPath, "ReplicateData") , showWarnings = FALSE)
  write.csv(data.frame(X=1:3, Y=1:3), file.path( workPath, "ReplicateData", "replicate0003.csv") )

  # Execute call 2
  genCall2 <- try(generateData(replicateN = 2, subjects = 500, treatDoses = c(0, 15, 30), treatPeriod = 0:3, 
    disCovNames = "DisCov1,DisCov2", disCovVals="1,2#1,2,3", disCovProbArray = rbind(c(.1, .1, .3), c(.3, .1, .1)),
	  genParNames = c("E0","ED50","EMAX"), genParMean = c(0, 50, 10), genParVCov=diag(c(1, 0, 0)), 
	  genParBtwNames = c("E0", "EMAX"), genParBtwVCov = diag(2), genParBtwCrit = "E0 > 0", genParErrStruc = "None",
	  respEqn = respFun, covDiff = FALSE, treatDiff = FALSE, seed=1, parBtwSuffix=".Extra", deleteDirs = FALSE, 
    workingPath = workPath))
	  
  # Check basics
  checkTrue(class(genCall2) != "try-error", msg = "Check the call was successful")
  checkTrue(file.exists( file.path( workPath, "ReplicateData")), msg = "Check ReplicateData directory has been created")
  checkTrue(file.exists( file.path( workPath, "ReplicateData", "replicate0001.csv" ) ), msg = "Check Replicate Data replicate0001.csv was created")
  checkTrue(file.exists( file.path( workPath, "ReplicateData", "replicate0002.csv" ) ), msg = "Check Replicate Data replicate0002.csv was created" )
  checkTrue(file.exists( file.path( workPath, "ReplicateData", "replicate0003.csv" ) ), msg = "Check (dummy) Replicate Data replicate0003.csv still exists" )
  
  # Import the data
  x <- lapply(1:2, readData, dataType="Replicate", workingPath = workPath )
  
  # Check variables exist in the data
  checkTrue(all(c("SUBJ", "TIME", "DOSE", "TRT") %in% names(x[[1]])), msg = "Check dosing variables are in the data")
  checkTrue(all(c("DisCov1", "DisCov2") %in% names(x[[1]])), msg = "Check covariate variables are in the data")
  checkTrue(all(c("E0", "ED50", "EMAX", "E0.Extra", "EMAX.Extra", "RESP") %in% names(x[[1]])), msg = "Check parameter variables are in the data")
  
  # Check dosing regimes set up
  theTreats <- unique(x[[1]][,c("TRT", "TIME", "DOSE")])
  theTreats <- theTreats[order(theTreats$TRT, theTreats$TIME, theTreats$DOSE),]
  checkTrue(nrow(theTreats) == 12 && all(theTreats$TRT == rep(1:3, each=4)), msg = "Check treatment column (TRT)")
  checkTrue(nrow(theTreats) == 12 && all(theTreats$TIME == rep(0:3, 3)), msg = "Check time column (TIME)")
  expectDose <- c(rep(0, 5), rep(15, 3), 0, rep(30, 3))
  checkTrue(nrow(theTreats) == 12 && all(theTreats$DOSE == expectDose), msg = "Check dose column (D)")
  
  # Check treatment allocation
  x1 <- unique(x[[1]][,c("SUBJ", "TRT")])
  x2 <- unique(x[[2]][,c("SUBJ", "TRT")])
  checkTrue(all(x1$T == x2$T), msg = "Check continuous treatments differ between replicates")
  
  # Check discrete covariates
  disData1 <- x[[1]][!duplicated(x[[1]]$SUBJ),c("DisCov1", "DisCov2")]
  disData2 <- x[[2]][!duplicated(x[[1]]$SUBJ),c("DisCov1", "DisCov2")]
  checkTrue(all(disData1$DisCov1 == disData2$DisCov1), msg = "Check discrete covariates are the same between replicates")
  checkTrue(all(disData1$DisCov1 %in% 1:2), msg = "Check values for discrete covariate 1")
  checkTrue(all(disData1$DisCov2 %in% 1:3), msg = "Check values for discrete covariate 2")   
  checkTrue(binom.test(table(disData1$DisCov1 != 1), p = .5)$p.value > .05, msg = "Check overall proportion for covariate 1")
  checkTrue(binom.test(table(disData1$DisCov2 != 1), p = .4)$p.value > .05, msg = "Check overall proportion for covariate 2")
  disTest1 <- disData1$DisCov1 == 1 & disData1$DisCov2 == 1
  disTest2 <- disData1$DisCov1 == 2 & disData1$DisCov2 == 1
  checkTrue(binom.test(table(!disTest1), p = .1)$p.value > .05, msg = "Check multinomial proportion (1,1)")
  checkTrue(binom.test(table(!disTest2), p = .3)$p.value > .05, msg = "Check multinomial proportion (2,1)")
  
  # Check parameters
  parData <- x[[1]][,c("SUBJ", "E0", "ED50", "EMAX", "E0.Extra", "EMAX.Extra")]
  checkTrue(all(parData$ED50 == 50), msg = "Check all ED50s are the same")
  checkTrue(all(parData$E0 == parData$E0[1]), msg = "Check all E0s are the same")
  checkTrue(all(parData$EMAX == 10), msg = "Check all EMAXs are the same")  
  checkTrue(!all(parData$E0.Extra == parData$E0.Extra[1]), msg = "Check all E0.Extras are different")
  checkTrue(!all(parData$EMAX.Extra == parData$EMAX.Extra[1]), msg = "Check all EMAX.Extras are different")
  checkTrue(abs(parData$E0[1]) < 4, msg = "Check E0 is in expected range")
  checkTrue(all(tapply(parData$EMAX.Extra, parData$SUBJ, function(x) all(x == x[1]))), msg = "Check effects are the same within subject")
  parData <- parData[!duplicated(parData$SUBJ), c("E0.Extra", "EMAX.Extra")]
  checkTrue(all(parData$E0.Extra >= 0), msg = "Check all E0.Extra are greater than 0")
  checkTrue(t.test(parData$EMAX.Extra)$p.value > 0.05, msg = "Check distribution of EMAX.Extra")
  
  # Check response
  respData <- x[[1]][,c("E0", "ED50", "EMAX", "DOSE", "RESP")]
  respData$PRED <- with(respData, E0 + (EMAX * DOSE) / (ED50 + DOSE))
  respData$RES <- respData$RESP - respData$PRED
  checkTrue(all(round(respData$RES, 3) == 0), msg = "Check response variable has been created correctly")
  
  unlink(workPath, recursive = TRUE)
  invisible(NULL)
}

test.generateData.call3 <- function() {
  
  dir.create( td3 <- tempfile() )
  file.copy(covariatesDataFile, td3 )
  file.copy(parametersDataFile, td3 )
  
  # Set up elements of the run
  respFun <- "X1 + X2 + X3"
  
  # Execute call 3
  genCall3 <- try(generateData(replicateN = 1, subjects = 10, treatDoses = 1, 
    extCovNames = "X1,X2,X3", extCovFile = "testCovariates.csv", extCovSubset = "X2 > 0", 
    extCovSameRow = TRUE, extCovDataId = "ID", extCovRefCol = "ID", 
    extParNames = "E0,ED50,EMAX", extParBtwNames = "B1,B2", extParBtwNums = c(1, 3), 
    extParErrStruc = "None", extParDataId = "ID",
    extParFile = "testParam.csv", extParSubset = "B1 > 0",
    respEqn = respFun, covDiff = FALSE, treatDiff = FALSE, seed=1, 
    parBtwSuffix=".Extra", deleteDirs = FALSE, workingPath = td3))  
  x3 <- readData(1, dataType="Replicate", workingPath = td3)
  
  # Execute call 4
  genCall4 <- try(generateData(replicateN = 1, subjects = 10, treatDoses = 1, 
    extCovNames = "X1,X2,X3", extCovFile = "testCovariates.csv", extCovSubset = "X2 > 0", extCovSameRow = FALSE, 
    extCovDataId = "ID", 
    extParNames = "E0,ED50,EMAX", extParBtwNames = "B1,B2", extParBtwNums = c(1, 3), extParErrStruc = "Additive", 
    extParFile = "testParam.csv", extParSubset = "B1 > 0",  extParDataId = "ID",
    respEqn = respFun, covDiff = FALSE, treatDiff = FALSE, seed=1, parBtwSuffix=".Extra", deleteDirs = FALSE, workingPath = td3))  
  x4 <- readData(1, dataType="Replicate", workingPath = td3)

  # Execute call 5
  genCall5 <- try(generateData(replicateN = 1, subjects = 10, treatDoses = 1, 
    extCovNames = "X1,X2,X3", extCovFile = "testCovariates.csv", extCovSubset = "X2 > 0", extCovSameRow = TRUE, extCovDataId = "ID", extCovRefCol = "ID",
    extParNames = "E0,ED50,EMAX", extParBtwNames = "B1,B2", extParBtwNums = c(1, 3), extParErrStruc = "None", 
    extParFile = "testParam.csv", extParRefColData = "ID", extParRefColName = "ID", extParDataId = "ID",
    respEqn = respFun, covDiff = FALSE, treatDiff = FALSE, seed=1, parBtwSuffix=".Extra", deleteDirs = FALSE, workingPath = td3))  
  x5 <- readData(1, dataType="Replicate", workingPath = td3)
  
  # Check basics
  checkTrue(class(genCall3) != "try-error", msg = "Check call 3 was successful")
  checkTrue(class(genCall4) != "try-error", msg = "Check call 4 was successful")
  
  # Check covariates
  checkTrue(all(x3$X3 %in% 1:10), msg = "Check imported covariate values")
  checkTrue(all(x3$X1 == x3$X2), msg = "Check all covariates taken from same line")
  checkTrue(!all(x4$X1 == x4$X2), msg = "Check all covariates not taken from same line")
  checkTrue(all(x3$X1 == x3$ID.refCol), msg = "Check reference column correctly created")  
  checkTrue(all(x4$X2 >= 0), msg = "Check covariate subset works")
                                
  # Check Parameters
  checkTrue(all(c("B1", "B2") %in% names(x3)), msg = "Check between subject variables imported correctly")
  checkTrue(!any(c("B1", "B2") %in% names(x4)), msg = "Check between subject variables removed correctly")
  checkTrue(all(round(x3$E0 + x3$B1 - x4$E0, 3) == 0), msg = "Check random effects correctly applied")
  checkTrue(all(x3$B1 >= 0), msg = "Check between subject subset applied correctly")
 
  # Check refCol in parameters
  importPars <- read.csv(parametersDataFile, sep=",", header=TRUE)
  importPars <- importPars[!duplicated(importPars$ID), c("ID", "B1", "B2")]
  names(importPars) <- c("ID", "Orig1", "Orig2")
  importPars <- merge(importPars, x5[,c("ID.refCol", "B1", "B2")], by.x="ID", by.y="ID.refCol")
  checkTrue(all(round(importPars$B1 - importPars$Orig1, 3) == 0), msg = "Check reference column functionality")  
  
  try( unlink(td3, recursive = TRUE) )
  invisible(NULL)                                                                   
}

### change request 29aug07
### Wed Aug 29 11:04:59 BST 2007 @461 /Internet Time/
# testing that the default covariance works for generateData call
test.changeRequest19aug07 <- function( ){
  
  ## set up a temp directory as the workingPath
  dir.create( workPath <- tempfile() )

  # Set up elements of the run
  respFun <- "E0 + (EMAX * DOSE) / (ED50 + DOSE)"
  
  # Create a dummy file (to check if it is overwritten)
  dir.create( file.path(workPath, "ReplicateData") , showWarnings = FALSE)

  # Execute call 2
  genCall1 <- try(generateData(replicateN = 2, subjects = 500, treatDoses = c(0, 15, 30), treatPeriod = 0:3, 
    disCovNames = "DisCov1,DisCov2", disCovVals="1,2#1,2,3", disCovProbArray = rbind(c(.1, .1, .3), c(.3, .1, .1)),
	  genParNames = c("E0","ED50","EMAX"), genParMean = c(0, 50, 100) , genParErrStruc = "None",
	  respEqn = respFun, covDiff = FALSE, treatDiff = FALSE, seed=1, parBtwSuffix=".Extra", deleteDirs = FALSE, 
    workingPath = workPath))
	  
  checkTrue(class(genCall1) != "try-error", msg = "Check call was successful")
         
  x <- do.call( rbind, lapply(1:2, readData, dataType="Replicate", workingPath = workPath ) )
    
  checkTrue( all( x[, "E0"] == 0), msg = "check default vcov matrix to 0 ()" )
  checkTrue( all( x[, "ED50"] == 50),msg = "check default vcov matrix to 0 ()" )
  checkTrue( all( x[, "EMAX"] == 100),msg = "check default vcov matrix to 0 ()" )
  
}
