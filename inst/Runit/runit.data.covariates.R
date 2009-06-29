if( !exists("unitTestPath")) unitTestPath <- "."
covariates.datapath <- file.path( unitTestPath , "data", "createCovariates" )
cat("covariates.datapath: ", covariates.datapath , "\n")

test.data.covariates.cont <- function(){
  
  # wrong mean 
  checkException( createContinuousCovariates( 10, mean = "a,b"  ) )
 
  # wrong subjects 
  checkException( createContinuousCovariates( -10, mean = "0,1"  ) )

  # wrong covariance
  checkException( createContinuousCovariates( 10, mean = "0,1", covariance = "1,1,1,1,1,1"  ) )
  
  # dimension problem
  checkException( createContinuousCovariates( 10, mean = "0,1", names = "b" ) )
  
  # duplicated names
  checkException( createContinuousCovariates( 10, mean = "0,1", names = c("X", "X")  ) )
  
  # wrong names
  checkException( createContinuousCovariates( 10, mean = "0,1", names = c("X", ".23")  ) )
  
  # digits > 0
  checkException( createContinuousCovariates( 10, mean = "0,1", names = c("X", "Y"), digits = -1  ) )
  
  # maxDraws > 0
  checkException( createContinuousCovariates( 10, mean = "0,1", names = c("X", "Y"), maxDraws = -100  ) )
  
  # wrong `idCol`
  checkException( createContinuousCovariates( 10, mean = "0,1", names = c("X", "Y"), idCol = ".534"  ) )
  
  dat <- createContinuousCovariates( 10, mean = "0,1", names = c("X", "Y")  )
  checkEquals( nrow(dat), 10 )
  checkEquals( ncol(dat), 3  ) # SUBJ, X, Y
  checkEquals( names(dat), c("SUBJ", "X", "Y")  ) # SUBJ, X, Y    
    
}

test.data.covariates.disc <- function(){
  
  dat <- createDiscreteCovariates( 10 , names = "X", probs = ".1,.9", values = "1,2")
  checkEquals( nrow(dat), 10 )
  checkTrue( all( dat[,2] %in% 1:2 ) )
  
  # wrong names
  checkException( createDiscreteCovariates( 10 , probs = ".1,.9", values = "1,2", names = "43") )
  checkException( createDiscreteCovariates( 10 , probs = ".1,.9", values = "1,2", idCol = "43") )
  
  # dimension problem
  checkException( createDiscreteCovariates( 100 , probs = ".1,.9#.3,.3,.4", values = "1,2#1,3" ) )
  checkException( createDiscreteCovariates( 100 , probs = ".1,.9#.3,4", values = "1,2#1,3", names = c("F1", "F2")) ) 
  checkException( createDiscreteCovariates( 100 , probs = ".1,.9#1#1", values = "1,2#1,3", names = c("F1", "F2")) ) 
  
  pa <- data.frame( F1 = rep(0:1, 3), F2 = rep(1:3, each = 2), PROB = c(.1,.2,.1,.2,.2,8) )
  checkException( createDiscreteCovariates( 100 , probArray = pa) ) 
  
  # testing the probArray thing
  padf <- data.frame( 
    F1 = rep(0:1, 3), 
    F2 = rep(1:3, each = 2), 
    PROB = c(.1,.2,.1,.2,.2,.2) )
  paArr <- rbind( c(.1,.1,.2), c(.2,.2,.2) )
  outDf  <- createDiscreteCovariates( 100 , probArray = padf, seed = 10 )  
  outArr <- createDiscreteCovariates( 100 , values = list(0:1, 1:3), probArray = paArr,
    names = "F1,F2", seed = 10)  
  checkEquals( outDf, outArr, 
    msg = "checking the prob array handling")  
    
  checkException( 
    createDiscreteCovariates(5, names="D1,D2", 
    values = "1,2#1,2,3", 
    probArray = rbind(c(.1, .1, .3), c(.3, 8, .1))), 
    msg = "check reject proba array as an array and does not sum up to one" )

  out <- createDiscreteCovariates(5, names="D1,D2", 
    values = "1,2#1,2,3", probArray = rbind(c(0,0,0), c(0,0,1)))
    
  checkTrue( all(out$D1 == 2 & out$D3 == 3), 
    msg = "checking 0 probabilities" )
}

test.data.covariates.ext <- function(){
   
  checkException( createExternalCovariates( 20, names = "X", file = "thisDoesNotExists.csv" ), 
    msg = "Unexisting file generates error" )
    
  checkException( createExternalCovariates( 20, names = "X1,X2,X3", 
    file =  "wrongTestCovariates.csv", workingPath = covariates.datapath ),
    msg = "Not correctly formatted csv file generates error" )
    
  testFile <- "testCovariates.csv"  
  checkException( createExternalCovariates( 20, names = "YY", 
    file = testFile, workingPath = covariates.datapath ), 
    msg = "Unfound variables in the file generates error" )
  
  checkException( createExternalCovariates( 20, names = "X1",
    file = testFile, dataId = "SUBJECTS", workingPath = covariates.datapath ), 
    msg = "Unfound `dataId` in the file generates error" )
    
  checkException( createExternalCovariates( 20, names = "X1", 
  file = testFile, refCol = "SUBJECTS", workingPath = covariates.datapath ), 
    msg = "Unfound `refCol` in the file generates error" )

  checkException( createExternalCovariates( 20, names = ".25352" ), 
    msg = "Invalid `names` generates an error" )
    
  checkException( createExternalCovariates( 20, names = "X1,X1" ), 
    msg = "Duplicated `names` generates an error" )
   
  checkException( createExternalCovariates( 20, names = "X1", dataId = ".43gt4e" ), 
    msg = "Wrong `dataId` generates an error" )
    
  checkException( createExternalCovariates( 20, names = "X1", dataId = ".43gt4e" ), 
    msg = "Wrong `refCol` generates an error" )

  checkException( createExternalCovariates( 20, names = "X1", idCol = ".43fewfgt4e" ), 
    msg = "Wrong `dataId` generates an error" )
      
    
  ## subset checks  
  checkException( createExternalCovariates( 20, names = "X1", subset = "1<X1<2<4" ), 
    msg = "Incorrect subset code (Too many comparators) generates error" )
    
  checkException( createExternalCovariates( 20, names = "X1", subset = "X1" ), 
    msg = "Incorrect subset code (Too few comparators) generates error" )
  
  checkException( createExternalCovariates( 20, names = "X1", subset = "-1202@{} > 1" ), 
    msg = "Incorrect subset code generates error" )
    
  checkException( createExternalCovariates( 20, names = "X1", subset = "X1 >" ), 
    msg = "Incorrect subset code (Empty side) generates error" )
      
  checkException( 
    createExternalCovariates( 20, names = "X1", 
      file = testFile, subset = "YY > 4", dataId = "ID", workingPath = covariates.datapath ), 
    msg = "subset on unexisting variables generates an error" )
    
  checkException( 
    createExternalCovariates( 20, names = "X1", subset = "X1 > 100", dataId="ID",
      file = testFile, workingPath = covariates.datapath ), 
    msg = "percent must be lower than 100" )
  
  checkTrue( 
    all( createExternalCovariates( 20, names = "X1", subset = "X1 > 0",
      file = testFile, dataId = "ID", workingPath = covariates.datapath )$X1 > 0 ), 
    msg = "subset correctly applied" )
   
  dat <- createExternalCovariates( 20, names = "X1", subset = ".7 < X1 < .8", dataId = "ID",
      file = testFile, workingPath = covariates.datapath )$X1
  checkTrue( all( dat > .7 & dat < .8), 
    msg = "subset correctly applied" )
    
  dat <- createExternalCovariates( 20, names = "X1", dataId = "ID", 
    subset = c(".7 < X1 < .8", "-1 <= X2 <= 1"),  
    file = testFile, workingPath = covariates.datapath )
  checkTrue( all( dat$X1 > .7 & dat$X1 < .8 & dat$X2 >= -1 & dat$X2 <= 1), 
    msg = "subset correctly applied" )

  checkException( createExternalCovariates( 20, names = "X1", sameRow = FALSE, refCol = "ID",  
    dataId = "ID", file = testFile, workingPath = covariates.datapath ) , 
    msg = "checking incompatibility between refCol and sameRow")
    
  # about the percent argument
  checkException( 
    createExternalCovariates( 20, names = "X1", dataId="ID",
      file = testFile, workingPath = covariates.datapath, percent = "x" ), 
    msg = "percent can't be converted to a number" )
  
  checkException( 
    createExternalCovariates( 20, names = "X1", dataId="ID",
      file = testFile, workingPath = covariates.datapath, percent = "10,20" ), 
    msg = "percent must be of length 1" )
    
  checkException( 
    createExternalCovariates( 20, names = "X1", dataId="ID",
      file = testFile, workingPath = covariates.datapath, percent = "-10" ), 
    msg = "percent must be greater than 0" )
    
  checkException( 
    createExternalCovariates( 20, names = "X1", dataId="ID",
      file = testFile, workingPath = covariates.datapath, percent = "1910" ), 
    msg = "percent must be lower than 100" )

  testSameRowFile <- "testSameRow.csv" 
  # the imported dataset has two columns X1 and X2 which have the same values
  dataSameRow <- createExternalCovariates( 20, names = "X,Y",dataId="ID",
    file = testSameRowFile, sameRow = TRUE, workingPath = covariates.datapath )[,-1] 
  checkTrue( all( apply( dataSameRow, 1, diff ) == 0 ), 
    msg = "checking the sameRow functionality")

  # checks on the outputs
  out <- createExternalCovariates( 50, names = "X1,X2", dataId="ID",
      file = testFile, workingPath = covariates.datapath )
  checkEquals( nrow(out), 50, 
    msg = "Checking the number of rows of the output")
  
  checkTrue( all( names(out) %in% c("X1","X2","SUBJ")   ), 
    msg = "Checking the naming of columns" )    
  
  out <- createExternalCovariates( 50, names = "X1,X2", 
      file = testFile, workingPath = covariates.datapath,
      idCol = "SUB", dataId = "ID")  
  checkTrue( all( names(out) %in% c("X1","X2","SUB")   ), 
    msg = "Checking the naming of columns" )    
  
  out <- createExternalCovariates( 50, names = "X1,X2", 
      file = testFile, workingPath = covariates.datapath, 
      idCol = "SUB", dataId = "ID", refCol = "ID")  
  checkTrue( all( names(out) %in% c("X1","X2","SUB", "ID.refCol")   ), 
    msg = "Checking the naming of columns" )    
    
  out <- createExternalCovariates( 50, names = "X1,X2", 
      file = testFile, workingPath = covariates.datapath, 
      idCol = "SUB", dataId = "ID", refCol = "ID", refColSuffix = "")  
  checkTrue( all( names(out) %in% c("X1","X2","SUB", "ID.")   ), 
    msg = "Checking the naming of columns" )    
   
  # test the seed
  out1 <- createExternalCovariates( 50, names = "X1,X2", dataId="ID",
      file = testFile, workingPath = covariates.datapath , seed = 10)
  rnorm( 1002 )
  out2 <- createExternalCovariates( 50, names = "X1,X2", dataId="ID",
      file = testFile, workingPath = covariates.datapath , seed = 10)
  checkTrue( identical( out1, out2 ) )

  
}


test.data.covariates.wrapper <- function(){
    testFile <- "testCovariates.csv"

  checkException( createCovariates( subjects = -3 ), 
    msg = "wrong subjects")

  checkException( createCovariates( subjects = 10, idCol = "ID, SUB" ), 
    msg = "id too long")
    
  checkException( createCovariates( subjects = 10, idCol = "4542" ), 
    msg = "invalid ID")
  
  checkTrue( all(createCovariates( subjects = 100 ) == 1:100 ), 
    msg= "test when no covariates")
    
  checkEquals( names( createCovariates( subjects = 100 , idCol = "SUB" )) , "SUB", 
    msg= "test idCol")
    

  # names incompatibility
  checkException( createCovariates( 30, conNames = "X1,X2", extNames = "X2, X3", disNames = "X4, X5" ), 
    msg = "incompatibility in names" )  
    
  # the continuous alone
  d1 <- createCovariates( 30, conNames = "X,Y", conMean = "0,0" , seed = 10)  
  d2 <- createContinuousCovariates( 30, names = "X,Y", mean = "0,0", seed = 10 )
  checkEquals( d1, d2 , 
    msg = "simple check only continuous covariates")

  d1 <- createCovariates( 30, conNames = "X,Y", conMean = "0,0" , conCov = "1,0,1", seed = 10)  
  d2 <- createContinuousCovariates( 30, names = "X,Y", mean = "0,0", covariance = "1,0,1", seed = 10 )
  checkEquals( d1, d2 , 
    msg = "simple check only continuous covariates, using cov matrix")
  
  d1 <- createCovariates( 30, conNames = "X,Y", conMean = "0,0" , conCov = "1,0,1", seed = 10, conRange = "-1<X<1")  
  d2 <- createContinuousCovariates( 30, names = "X,Y", mean = "0,0", covariance = "1,0,1", seed = 10 , range = "-1<X<1")
  checkEquals( d1, d2 , 
    msg = "simple check only continuous covariates, with range")
    
  # discrete alone
  d1 <- createCovariates( 70, disNames = "P1,P2", disValues = "1,2#3,5,6" , disProbs = ".5,.5#.3,.3,.4" , seed = 10)  
  d2 <- createDiscreteCovariates( 70, names = "P1,P2", values = "1,2#3,5,6" , probs = ".5,.5#.3,.3,.4" , seed = 10)
  checkEquals( d1, d2 , 
    msg = "simple check only discrete covariates")
  
  # external alone
  d1 <- createExternalCovariates( 80, names = "X1", dataId="ID",
    subset = c(".7 < X1 < .8", "-1 <= X2 <= 1"),  seed = 3, 
    file = testFile, workingPath = covariates.datapath )  
  d2 <- createCovariates( 80, extNames = "X1", extSubset = c(".7 < X1 < .8", "-1 <= X2 <= 1"), 
    extFile = testFile , extDataId="ID", workingPath = covariates.datapath , seed = 3)  
  checkEquals( d1, d2 , 
    msg = "simple check only external covariates")
    
  # test altogether
  dAll <- createCovariates( 30, conNames = "X,Y", conMean = "0,0" , conCov = "1,0,1", 
    seed = 10, conRange = "-1<X<1", disNames = "P1,P2", disValues = "1,2#3,5,6" , 
    disProbs = ".5,.5#.3,.3,.4", extNames = "X1",
    extDataId="ID", extFile = testFile, workingPath = covariates.datapath )  
  dCon <- createContinuousCovariates( 30, names = "X,Y", mean = "0,0", covariance = "1,0,1", seed = 10 , range = "-1<X<1")
  dDis <- createDiscreteCovariates( 30, names = "P1,P2", values = "1,2#3,5,6" , probs = ".5,.5#.3,.3,.4" , seed = 10)
  dExt <- createExternalCovariates( 30, names = "X1", dataId="ID",
    file = testFile, workingPath = covariates.datapath, seed = 10 )  
  checkTrue( identical( dAll[, c("SUBJ", "X", "Y")], dCon ), 
    msg = "check altogether 1" )  
  checkTrue( identical( dAll[, c("SUBJ", "P1", "P2")], dDis ), 
    msg = "check altogether 2" )  
  checkTrue( identical( dAll[, c("SUBJ", "X1")], dExt ), 
    msg = "check altogether 3" )  
    
    
}

# tests added to comply with issue 3 of sourceforge
# Tue Jul 24 09:08:08 BST 2007 @380 /Internet Time/
test.data.covariates.sf3 <- function() {
  # test the rounding
  checkException( createContinuousCovariates( 10, mean = "100,100,100", names = c("X", "Y", "Z"), digits = "2,3"  ), 
    msg = "digits should have the right length" )
  checkException( createContinuousCovariates( 10, mean = "100,100,100", names = c("X", "Y", "Z"), digits = "2,3,-2"  ), 
    msg = "no negative digits" )
  out <- createContinuousCovariates( 10, mean = "100,100,100", names = c("X", "Y", "Z"), digits = "2,3,2"  )
  checkEquals( out[,2] , round(out[,2], 2), 
    msg = "check the use of a digits vector")
  checkEquals( out[,3] , round(out[,3], 3), 
    msg = "check the use of a digits vector (2)")
  checkEquals( out[,4] , round(out[,4], 2), 
    msg = "check the use of a digits vector (3)")
  out <- createContinuousCovariates( 10, mean = "100,100,100", names = c("X", "Y", "Z"), digits = "3"  )
  checkEquals( out[,2:4] , round(out[,2:4], 3), 
    msg = "check the use of a digits not vector")
}
