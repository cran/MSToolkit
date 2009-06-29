if( !exists("unitTestPath")) unitTestPath <- "."
testDir <- file.path( unitTestPath, "testdata.datastorage" )


# Date Jun 30 2007
# Author: Francisco
test.createDirectories <- function() {
  dir.create( tempDir <- tempfile() )
  
  x <- createDirectories(workingPath = tempDir)
  checkTrue(all(x), msg = "all of the directories should be created")
  checkTrue(all(file.exists(file.path(tempDir, c("ReplicateData", "MacroEvaluation", "MicroEvaluation")))))
  
  x <- createDirectories(dirNames = c("Rep", "MacroEvaluation", "Micro"), workingPath = tempDir)
  checkTrue(!any(x))
  checkException(createDirectories(dirName = c(), workingPath = tempDir))

  checkException( createDirectories(dirNames = c("1","2","3"), workingPath = tempDir) )
  try(unlink(tempDir, recursive = TRUE))
}

# Date: Jul 1 2007
# Author: Francisco

test.removeDirectories <- function()
{
  tempDir <- file.path(tempdir(), "DataStorageTemp")
  checkTrue(dir.create(tempDir))
  createDirectories(workingPath = tempDir )
  
  # createDirectories()
  removeDirectories(workingPath = tempDir )
  checkTrue(length(dir(path = tempDir)) == 0)
  
  createDirectories(dirNames = c("Rep", "Micro", "Macro"), workingPath = tempDir)
  removeDirectories(dirNames = c("Rep", "Micro", "Macro"), workingPath = tempDir)
  
  checkTrue(length(dir(path = tempDir)) ==0 )
  createDirectories(dirNames = c("ReplicateData", "MacroEvaluation"), workingPath = tempDir)
  removeDirectories(dirNames = "ReplicateData", workingPath = tempDir)
  checkTrue(length(dir(path=tempDir))==1)
  unlink(tempDir, recursive = TRUE)

}

# Author: Francisco
# Date: Jul 3 2007

test.readData <- function()
{   
  x <- read.csv(paste(testDir,"/microSummary.csv", sep=""))
  y <- readData(dataType = "Micro", dataNumber = 1, workingPath = testDir)
  checkTrue(identical(x[1:10,-c(1,2,13)], y[,-11]))
  checkException(readData(dataType = "Micro", dataNumber = 2, workingPath = testDir))
  checkTrue(!identical(x[2:11,],readData(dataType = "Micro", dataNumber = 1, workingPath = testDir)))
  checkException(readData(dataType = "microeval", dataNumber =1))

  x <- read.csv(paste(testDir,"/ReplicateSample.csv", sep=""))
  x1 <- x[20:80, ]
  rownames(x1) <- 1:61 
  y <- readData(dataType = "Replicate", dataNumber = 10, workingPath = testDir)
  checkException(readData(dataType = "Replicate", dataNumber = 11, workingPath = testDir))
                                 
}

test.writeData <- function()
{
  tempDir <- file.path(tempdir(), "DataStorageTemp")
  checkTrue(dir.create(tempDir))

	x <- read.csv(paste(testDir,"/ReplicateSample.csv",sep=""))[20:30,]
	createDirectories("ReplicateData", workingPath = tempDir)
	writeData(x, dataNumber = 1001, dataType = "Replicate", workingPath = tempDir)
	checkTrue(file.exists(writtenFile <- paste(tempDir, "/ReplicateData/replicate1001.csv", sep="")))
  unlink(tempDir, recursive = TRUE)
}


test.readAllData <- function(){
	
	tempDir <- file.path(tempdir(), "DataStorageTemp")
  checkTrue(dir.create(tempDir))
	microData <- read.csv(file.path(testDir, "microSummary.csv")) [1:12,-c(2,13)]
  createDirectories("MicroEvaluation", workingPath = tempDir)
 
	writeData(microData[1:3,], dataNumber = 1, dataType = "Micro", workingPath=tempDir)
	writeData(microData[4:6,], dataNumber = 2, dataType = "Micro", workingPath=tempDir)
	writeData(microData[7:9,], dataNumber = 3, dataType = "Micro", workingPath=tempDir)
	writeData(microData[10:12,], dataNumber = 4, dataType = "Micro", workingPath=tempDir)
	x <- readAllData(dataType = "Micro", workingPath = tempDir)

  rownames(microData)  <- rownames(x)
  checkEquals(x[,-1], microData, msg = "checking the readAllData function with MicroEvaluation data") 	
  checkTrue(all(x$Replicate == rep(1:4, each=3)), msg = "Check subset replicate variable created correctly")
	
  # Now check it with a subset of data
	x <- readAllData(dataType = "Micro", workingPath = tempDir, replicates = 2:3)
  
  y <- microData[4:9, ]
  rownames(y) <- rownames(x)
  checkEquals(x[,-1], y, msg = "checking the readAllData function with a reading of partial MicroEvaluation data")
  checkTrue(all(x$Replicate == rep(2:3, each=3)), msg = "Check subset replicate variable created correctly")
  unlink(tempDir, recursive = TRUE)
 
}

# SF issue 6
# Tue Jul 24 12:35:40 BST 2007 @524 /Internet Time/
test.readnonmemdata.sf6 <- function( ){
  if("MSToolkit" %in% search() ){ # need that because .readAndCheckInputFile is internal
    readFun <- MSToolkit:::.readAndCheckInputFile
    
    nohData <- readFun( file.path( unitTestPath , "testdata.datastorage","NONMEMDataNoHeader.fit"  ) )
    heaData <- readFun( file.path( unitTestPath , "testdata.datastorage","NONMEMDataWithHeader.fit") )
    csvData <- readFun( file.path( unitTestPath , "testdata.datastorage","NONMEMcsv.csv"           ) )
   
    checkEquals( nohData, heaData,  msg = "check import of NONMEM data (1)" )
    checkEquals( nohData, csvData,  msg = "check import of NONMEM data (2)" )
    
    checkEquals( nrow(nohData), 10, msg = "check import of NONMEM data (3)" )
    checkEquals( nrow(heaData), 10, msg = "check import of NONMEM data (4)" )
    checkEquals( nrow(csvData), 10, msg = "check import of NONMEM data (5)" )
    
    
  }
  
}


