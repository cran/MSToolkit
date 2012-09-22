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
  parOmitFlag = getEctdColName("ParOmit"),           #@ Parameter omit flag name
  respOmitFlag = getEctdColName("RespOmit"),         #@ Response omit flag name
  missingFlag = getEctdColName("Missing"),           #@ Missing flag name
  interimCol = getEctdColName("Interim"),            #@ Interim variable name
  doseCol = getEctdColName("Dose"),                  #@ Dose variable name
  sleepTime = 15,                    #@ Number of seconds to sleep between checking for grid jobs
  deleteCurrData = TRUE,             #@ Delete current analysis results before executing
  initialDoses = NULL,					#@ Initial doses to use for "Interim 1"
  stayDropped = TRUE,				#@ Dose dropping flag: if a dose is dropped, should it stay dropped?
  fullAnalysis = TRUE,			#@ Perform a full analysis
  workingPath = getwd(),              #@ Working path containing data
  method = getEctdDataMethod()
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
	replicates <- .checkReplicates( replicates, workingPath = workingPath, method = method)

	if (grid && !.checkGridAvailable()) grid <- FALSE
	if (length(replicates) == 1) grid <- waitAndCombine <- FALSE

	## Check directories
	if (deleteCurrData) removeDirectories(c("Micro", "Macro"), workingPath = workingPath)
	createDirectories(c("MicroEvaluation", "MacroEvaluation"), workingPath = workingPath)

	## Split jobs and call grid
	if (grid) {

		getPaths <- get("externalPaths", envir = .ectdEnv)
		snowopt <- getPaths[grepl("^SNOW_OPT\\.", names(getPaths))]
		names(snowopt) <- gsub("^SNOW_OPT\\.", "", names(snowopt))
		splitf <- as.numeric(as.factor(names(snowopt))) * ceiling(seq_along(snowopt) / length(unique(snowopt))) / seq_along(unique(snowopt))
		snowopt <- lapply(split(snowopt, as.factor(splitf)), as.list)
		islocal <- all(sapply(snowopt, function(X) X[["host"]]) %in% c(Sys.info()[["nodename"]], "localhost"))

                nclusters <- getOption('mc.cores', 2L)
                cl <- parallel:::makeCluster(nclusters)
                doParallel:::registerDoParallel(cl)
                stopCluster <- parallel:::stopCluster

		repSplit <- .splitGridVector(replicates, ceiling(length(replicates) / nclusters ))
		`%dopar%` <- foreach:::"%dopar%"
		k <- 0

		tmp <- foreach:::foreach(k = 1:length(repSplit), .packages = c("MSToolkit", "MASS")) %dopar% {
			for (i in repSplit[[k]]) {

				microData <- analyzeRep(replicate = i, analysisCode = analysisCode,
						interimCode = interimCode, software = software, removeMissing = removeMissing,
						removeParOmit = removeParOmit, removeRespOmit = removeRespOmit,
						seed = seed + i, parOmitFlag = parOmitFlag, respOmitFlag = respOmitFlag,
						missingFlag = missingFlag, interimCol = interimCol, doseCol = doseCol,
						initialDoses = initialDoses, stayDropped = stayDropped, fullAnalysis = fullAnalysis,
						workingPath = workingPath, method = method)

				# Write out data
				if (is.data.frame(microData) && nrow(microData)) {

					writeData(microData, i, "Micro", workingPath = workingPath)

					macroData <- macroEvaluation(microData, macroCode = macroCode,
							interimCol = interimCol, doseCol = doseCol)

					writeData(macroData, i, "Macro", workingPath = workingPath)
				}
				else ectdWarning(paste("No return output from replicate", i))
			}
		}

		stopCluster(cl)
		evalTime <- Sys.time()                              # Store time at grid evaluation
	} else {

		# Loop through and analyze replicates
		for (i in replicates) {

			## TODO: Update analyzeRep and performAnalysis with data storage method ..
			microData <- analyzeRep(replicate = i, analysisCode = analysisCode,
				interimCode = interimCode, software = software, removeMissing = removeMissing,
				removeParOmit = removeParOmit, removeRespOmit = removeRespOmit,
				seed = seed + i, parOmitFlag = parOmitFlag, respOmitFlag = respOmitFlag,
	        	missingFlag = missingFlag, interimCol = interimCol, doseCol = doseCol,
				initialDoses = initialDoses, stayDropped = stayDropped, fullAnalysis = fullAnalysis,
				workingPath = workingPath, method = method)

			# Write out data
			if (is.data.frame(microData) && nrow(microData)) {

				writeData(microData, i, "Micro", workingPath = workingPath)

				macroData <- macroEvaluation(microData, macroCode = macroCode,
					interimCol = interimCol, doseCol = doseCol)

				writeData(macroData, i, "Macro", workingPath = workingPath)
			}
			else ectdWarning(paste("No return output from replicate", i))
		}
	}

	if (waitAndCombine) {

    	compileSummary("Micro", workingPath = workingPath)
    	compileSummary("Macro", workingPath = workingPath)

	}
	.cleanup( cleanUp = cleanUp, grid = grid, workingPath = workingPath )

	invisible()
}

