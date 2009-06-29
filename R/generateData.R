generateData <- function(
		 replicateN ,	                              #@  Number of replicates
		 subjects ,	                                #@ Number of subjects in simulation
		 treatSubj = subjects,	                    #@ Number of subjects to which to allocate treatments, or a vector of allocations
		 treatDoses ,	                              #@ Treatment doses
		 treatSeq ,	                                #@ Treatment matrix for crossover designs
		 treatType = "Parallel",	                  #@ Treatment type: Parallel or Crossover
		 treatPeriod ,	                            #@ Treatment time points
		 genParNames ,	                            #@ Names of fixed effects to generate
		 genParMean ,	                              #@ Means for generating fixed parameters
		 genParVCov  = 0,	                          #@ Covariance matrix for generating fixed parameters
		 respEqn ,	                                #@ Formula for creating response
		 respName = "RESP",	                        #@ Response variable name
		 treatProp ,                                #@ Proportions for sampling
		 treatOrder = FALSE,	                      #@ Logical flag: should allocations be assigned in order
		 conCovNames ,	                            #@ Continuous covariate names
		 conCovMean ,	                              #@ Continuous covariate means
		 conCovVCov ,	                              #@ Continuous covariate covariance matrix
		 conCovCrit = NULL,	                        #@ Continuous covariate acceptable range
		 conCovDigits = 3,	                        #@ Continuous covariate rounding digits
		 conCovMaxDraws = 100,	                    #@ Continuous covariate maximum draws
		 disCovNames ,	                            #@ Discrete covariate names
		 disCovVals ,	                              #@ Discrete covariate values
		 disCovProb ,	                              #@ Discrete covariate probabilities
		 disCovProbArray ,	                        #@ Array of probabilities for multivariate sampling
		 extCovNames ,	                            #@ Names for the continuous covariates
		 extCovFile ,	                              #@ File from which to import (including full or relative path)
		 extCovSubset ,	                            #@ Subset to apply to data
		 extCovRefCol ,	                            #@ Reference variable
		 extCovSameRow = TRUE,	                    #@ Logical flag: should covariates sampled be from the same row
		 extCovDataId = idCol,	                    #@ Subject variable name from file
		 genParCrit,	                              #@ Range of acceptable values for generated fixed effects
		 genParBtwNames ,	                          #@ Between subject effects to generate
		 genParBtwMean ,	                          #@ Means for generated between subject effects
		 genParBtwVCov ,	                          #@ Covariance matrix for generated between subject effects
		 genParBtwCrit ,	                          #@ Range of acceptable values for generated between subject effects
		 genParErrStruc = "None",	                  #@ Function to map generated effects: Additive, Proportional or None
		 genParMaxDraws = 100,	                    #@ Maximum number of iterations to generate valid parameters
		 extParFile ,	                              #@ File name for external parameter data to import
		 extParNames ,	                            #@ Names of parameters to import from external file
		 extParBtwNames ,	                          #@ Between subject effects variables to import from external file
		 extParBtwNums , 	                          #@ Integer mapping between random and fixed effects in imported parameter data
		 extParSubset = NULL,	                      #@ Subsets to be applied to imported parameter before sampling
		 extParErrStruc = "None",	                  #@ Function to map effects from imported parameter data: Additive, Proportional or None
		 extParRefColData ,	                        #@ Reference column in imported parameter data
		 extParRefColName ,	                        #@ Reference column name from imported parameter data
		 extParDataId = idCol,                      #@ Subject variable in external parameter file
		 respInvLink,	                              #@ Inverse link function for the linear predictor
		 respDist = "Normal",	                      #@ Outcome response variable distribution
		 respVCov ,	                                #@ Residual error (co)variance to apply to generated response
		 respErrStruc = "Additive",	                #@ Function describing how to apply residual error to the generated response: Additive or Proportional
		 respCrit,	                                #@ Range of acceptable values for created response
		 respDigits = 3,	                          #@ Number of digits to which to round the created response
		 mcarProp = 0,	                            #@ Proportion of observations to set to missing at random
		 mcarRule,	                                #@ Rule to specify which observations of the data should be included for MCAR allocation
		 dropFun ,	                                #@ User defined function to define criteria for subject dropout
		 dropFunExtraArgs = list(),	                #@ Additional arguments to the dropout function
		 interimSubj ,	                            #@ Proportion of total subjects to be assigned to each interim analysis
		 interimMethod = "Sample",	                #@ Method for creating interim variable: Sample or Proportion
     seed = .deriveFromMasterSeed(),            #@ random seed
		 idCol = "SUBJ",	                            #@ Subject variable name
		 doseCol = "DOSE",	                        #@ Dose variable name
		 timeCol = "TIME",	                        #@ Time variable name
		 trtCol = "TRT",	                          #@ Treatment variable name
		 parOmitFlag = "PAROMIT",	                  #@ Parameter omit flag name
		 respOmitFlag = "RESPOMIT",                 #@ Response omit flag name
		 missingFlag = "MISSING",                   #@ Missingness flag name
		 interimCol = "INTERIM",                    #@ Interim variable name
		 parBtwSuffix = ".Between",                 #@ Suffix for retained between subject effects variables
		 deleteDirs = TRUE,                         #@ Should existing data be deleted before starting generation phase
		 covDiff = TRUE,                            #@ Should covariates differ between replicates
		 treatDiff = TRUE,                          #@ Should treatment allocation differ between replicates
     workingPath = getwd()                      #@ Working directory from which to create data
){
 	###############################################################################
	# � Mango Solutions, Chippenham SN14 0SQ 2006
	# generateData.R Mon Jul 02 15:14:30 BST 2007 @447 /Internet Time/
	#
	# Author: Richard
	###############################################################################
	# DESCRIPTION: High level function to generate simulated trial data
  # KEYWORDS: high, generate
	###############################################################################
  # TESTME
  
   
  # TODO: Better way of extracting default arguments to this function
  defNames <- c("treatSubj", "treatType", "respName", "treatOrder", "conCovCrit", "conCovDigits", "conCovMaxDraws", "extCovSameRow", "extCovDataId", 
    "genParErrStruc", "respDist", "respErrStruc", "respDigits", "genParVCov", "mcarProp", "dropFunExtraArgs", "interimMethod", "seed", "idCol", "doseCol", "timeCol",
    "trtCol", "parOmitFlag", "respOmitFlag", "missingFlag", "interimCol", "parBtwSuffix", "deleteDirs", "covDiff", "treatDiff", "extParDataId")
  callNames <- union(names(match.call())[-1], defNames)

  ## Inner Function - Creates call lists from a vector of argument maps
  innerCallList <- function(Vec) {
    Vec <- Vec[Vec %in% callNames]
    if (length(Vec)) lapply(Vec, get, envir=parent.frame()) else list()
  }

  ## Set Argument calling lists: matching of arguments
  treatList <- innerCallList(c(doses = "treatDoses", times = "treatPeriod", type = "treatType", sequence = "treatSeq", doseCol = "doseCol", timeCol = "timeCol", trtCol = "trtCol"))
  allocateList <- innerCallList(c(subjects = "treatSubj", prop = "treatProp", ordered = "treatOrder", idCol = "idCol", trtCol = "trtCol"))
  covList <- innerCallList(c(subjects = "subjects", conNames = "conCovNames", conMean = "conCovMean", conCov = "conCovVCov", conRange = "conCovCrit", conDigits = "conCovDigits", 
    conMaxDraws = "conCovMaxDraws", disNames = "disCovNames", disValues = "disCovVals", disProbs = "disCovProb", disProbArray = "disCovProbArray", extNames = "extCovNames", 
    extFile = "extCovFile", extSubset = "extCovSubset", extRefCol = "extCovRefCol", extSameRow = "extCovSameRow", extDataId = "extCovDataId", idCol = "idCol", workingPath = "workingPath"))
  parList <- innerCallList(c(subjects = "subjects", genNames = "genParNames", genFixedMean = "genParMean", genFixedCov = "genParVCov", genFixedRange = "genParCrit", 
    genBetweenNames = "genParBtwNames", genBetweenMean = "genParBtwMean", genBetweenCov = "genParBtwVCov", genBetweenRange = "genParBtwCrit", genErrStruc = "genParErrStruc", 
    genMaxDraws = "genParMaxDraws", extFile = "extParFile", extNames = "extParNames", extBetween = "extParBtwNames", extBetweenNums = "extParBtwNums", extSubset = "extParSubset", 
    extErrStruc = "extParErrStruc", extRefCol = "extParRefColData", extRefColName = "extParRefColName", extDataId = "extParDataId", suffix = "parBtwSuffix", idCol = "idCol", 
    flagName = "parOmitFlag", workingPath = "workingPath"))
  respList <- innerCallList(c(equation = "respEqn", name = "respName", invLink = "respInvLink", distribution = "respDist", covariance = "respVCov", errStruc = "respErrStruc", 
    range = "respCrit", digits = "respDigits", flagName = "respOmitFlag"))
  mcarList <- innerCallList(c(prop = "mcarProp", rule = "mcarRule", flagName = "missingFlag" ))

  dropList <- innerCallList(c(dropFunc = "dropFun", idCol = "idCol", timeCol = "timeCol", flagName = "missingFlag"))
  interimList <- innerCallList(c(subjects = "subjects", proportion = "interimSubj", idCol = "idCol", interimCol = "interimCol", method = "interimMethod"))

  ## Set directory structures
  if (deleteDirs) removeDirectories("ReplicateData", workingPath = workingPath)
  createDirectories("ReplicateData", workingPath = workingPath)
  
  ## Derive Treatment Data
  treatData <- do.call(createTreatments, treatList)
  allocateList$trts <- max(treatData[[trtCol]])                          
  ## Allocate treatments if required
  if (!treatDiff) allocData <- do.call(allocateTreatments, allocateList)
  if (!covDiff) covData <- do.call(createCovariates, covList)
             
  
  ## Loop around replicates
  if (length(replicateN) == 1) replicateN <- 1:replicateN
  for (i in replicateN) {

    ## Set low level component seeds
    allocateList$seed <- seed + 1 * i
    covList$seed      <- seed + 2 * i
    parList$seed      <- seed + 3 * i
    respList$seed     <- seed + 4 * i
    mcarList$seed     <- seed + 5 * i
    dropList$seed     <- seed + 6 * i
    interimList$seed  <- seed + 7 * i

    ## Replicate Looping: Core Data Structure
    if (covDiff) covData <- do.call(createCovariates, covList)
    if (treatDiff) allocData <- do.call(allocateTreatments, allocateList)
    coreData <- merge(merge(treatData, allocData, by=trtCol), covData, by=idCol)
		sortBy <- c(idCol, trtCol, timeCol, doseCol)
		sortBy <- sortBy [ sortBy %in% names(coreData) ]
		if (length(sortBy)) coreData <- coreData [ do.call("order", coreData[sortBy]),,drop=FALSE]

    ## Replicate Looping: Parameters and Reponse
    if (!missing(extParRefColData) && length(extParRefColData) == 1 && is.character(extParRefColData) && !length(grep(",", extParRefColData))) {
      if (!length(grep(".refCol", extParRefColData))) extParRefColData <- paste(extParRefColData, ".refCol", sep="")
      if (extParRefColData %in% names(coreData)) parList$extRefCol <- coreData[[extParRefColData]]
      else parList <- parList[names(parList) != "extRefCol"]      
    }
    coreData <- merge(coreData, do.call(createParameters, parList), by=idCol)
    respList$data <- coreData
    respData <- do.call(createResponse, respList)
    if (is.data.frame(respData) && nrow(respData) == nrow(coreData)) coreData <- cbind(coreData, respData) else ectdStop("Could not create response variable")

    ## Replicate Looping: Flags and Interims
    if (mcarProp > 0) {
      mcarList$data <- coreData
      coreData <- do.call(createMCAR, mcarList)
    }
    if (!missing(dropFun)) {
      dropList$data <- coreData
      coreData <- do.call(createDropout, c(dropList, dropFunExtraArgs))
    }

    if (!missing(interimSubj)) coreData <- merge(coreData, do.call(createInterims, interimList), by=idCol)
    
    ## Replicate Looping: Exporting Data
    writeData(coreData, i, dataType = "Replicate", workingPath = workingPath)    
    
    .log( sprintf("gendata replicate %5d / %5d", i, length(replicateN)) )
  }
  
  invisible()  
}



