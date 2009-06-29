"analyzeData" <- function(
  replicates = "*",                  #@ Replicates to perform analysis on 
  analysisCode,                      #@ Function taking a data
  macroCode,                         #@ Macro evaluation code
  interimCode = NULL,                #@ Interim analysis code
  software = "R",                    #@ Software for analysis: R or SAS
  grid = TRUE,                       #@ Split analysis across the grid?
  waitAndCombine = TRUE,             #@ Wait for all analyses to finish, then combine into single file?
  cleanUp = FALSE,                   #@ Delete micro/macro directories on completion?
  removeMissing = TRUE,              #@ Remove Missing rows?
  removeParOmit = TRUE,              #@ Remove Parameter Omit rows?
  removeRespOmit = TRUE,             #@ Remove Response Omit rows?
  seed = .deriveFromMasterSeed(),    #@ Random number seed
  parOmitFlag = "PAROMIT",           #@ Parameter omit flag name
  respOmitFlag = "RESPOMIT",         #@ Response omit flag name
  missingFlag = "MISSING",           #@ Missing flag name
  interimCol = "INTERIM",            #@ Interim variable name
  doseCol = "DOSE",                  #@ Dose variable name
  sleepTime = 15,                    #@ Number of seconds to sleep between checking for grid jobs
  deleteCurrData = TRUE,             #@ Delete current analysis results before executing
  workingPath = getwd()              #@ Working path containing data
)
{
  ###############################################################################
	# � Mango Solutions, Chippenham SN14 0SQ 2006
	# analyzeData.R Tue Jul 03 16:24:00 BST 2007 @447 /Internet Time/
	#
	# Author: Richard, Romain
	###############################################################################
	# DESCRIPTION: High level function to analyze simulated trial datasets
  # KEYWORDS: high, analyze                                                
	###############################################################################
  # TESTME
  funCall <- match.call()

  ## Check network connectivity  
  macroCode <- .checkFun(macroCode, "data")
  replicates <- .checkReplicates( replicates, workingPath = workingPath)

  if (grid && !.checkGridAvailable()) grid <- FALSE
  if (length(replicates) == 1) grid <- waitAndCombine <- FALSE

  ## Check directories
  if (deleteCurrData) removeDirectories(c("Micro", "Macro"), workingPath = workingPath)
  createDirectories(c("MicroEvaluation", "MacroEvaluation"), workingPath = workingPath)
  
  ## Split jobs and call grid
  if (grid) {
    funCall[[1]] <- as.name(".ectdSubmit")              # Call the .ectdSubmit function for LSF split
    funCall$grid <- funCall$waitAndCombine <- funCall$deleteCurrData <- FALSE     # Don't split grid job over grid or compile
    funCall$func <- "analyzeData"                       # Grid function to call is analyzeData
    funCall$debug <- TRUE                               # Set debug flag on the grid system
    funCall$packages <- c("MSToolkit", "MASS")          # Required packages
    if (software == "SAS") funCall$reqSas <- TRUE       # Need to queue on a SAS machine
    repSplit <- .splitGridVector(replicates)            # Split replicates for Grid execution
    gridJobs <- lapply(repSplit, function(i, call) {
      call$replicates <- i
      eval(call)
    }, call=funCall)
    evalTime <- Sys.time()                              # Store time at grid evaluation
  }
  else {                       
    # <SLOW>
    # find something better
    dataDoses <- sort(unique(readData(replicates[1], dataType = "Replicate", workingPath = workingPath)[[doseCol]]))
    # </SLOW>
    for (i in replicates) {
      microData <- analyzeRep(replicate = i, analysisCode = analysisCode, 
        interimCode = interimCode, software = software, removeMissing = removeMissing, 
        removeParOmit = removeParOmit, removeRespOmit = removeRespOmit, 
        seed = seed + i, parOmitFlag = parOmitFlag, respOmitFlag = respOmitFlag, 
        missingFlag = missingFlag, interimCol = interimCol, doseCol = doseCol, workingPath = workingPath)   
            
      writeData(microData, i, "Micro", workingPath = workingPath)
      
      macroData <- macroEvaluation(microData, macroCode = macroCode, 
        interimCol = interimCol, doseCol = doseCol)
      
      writeData(macroData, i, "Macro", workingPath = workingPath)
    }
  }                  
  if (waitAndCombine) {   
    if (grid) {
      suppressWarnings(require(Rlsf, quietly=TRUE))
      gridStatus <- sapply(gridJobs, lsf.job.status)
      checkJobs <- gridStatus %in% c("EXIT", "DONE")
      iter <- 1
      while (any(!checkJobs)) {
        if (any(gridStatus == "DONE")) {
          compileSummary("Micro", workingPath = workingPath)
          compileSummary("Macro", workingPath = workingPath)
        }
        ## Write log file
        writeLogFile(gridStatus, evalTime, workingPath = workingPath)
        iter <- iter + 1
        if (iter > 1000) ectdStop("Job timed out")
        Sys.sleep(sleepTime)
        gridStatus <- sapply(gridJobs, lsf.job.status)
        checkJobs <- gridStatus %in% c("EXIT", "DONE")
      }
      writeLogFile(gridStatus, evalTime, workingPath = workingPath)
    }
    
    compileSummary("Micro", workingPath = workingPath)
    compileSummary("Macro", workingPath = workingPath)      
    
  }
  .cleanup( cleanUp = cleanUp, grid = grid, workingPath = workingPath )
  
  invisible()
}

