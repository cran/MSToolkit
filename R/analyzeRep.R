analyzeRep <- function(
  analysisCode		,           #@	File containing the actual analysis code to run on the data
  replicate		,               #@	Replicate number of data to analyze
  removeMissing	= TRUE	,     #@	Logical flag: remove rows where the “Missing” Flag is set to 1?
  removeParOmit	= TRUE	,     #@	Logical flag: remove rows where the “Parameter Omit” Flag is set to 1?
  removeRespOmit= TRUE	,     #@	Logical flag: remove rows where the “Response Omit” Flag is set to 1?
  interimCode		,             #@	Interim analysis Code to run on the data between interims (eg. can be used to drop doses)
  software = c("R","SAS")	,   #@	Software system in which the analysis should take place: R or SAS
  seed =	.deriveFromMasterSeed()	,#@	Random number generation seed
  parOmitFlag	 = "PAROMIT",   #@	Parameter omit flag name
  respOmitFlag = "RESPOMIT",  #@	Response omit flag name
  missingFlag = "MISSING",   #@	Missing flag name
  interimCol	 = "INTERIM",   #@	Interim variable name
  doseCol	     = "DOSE", 	    #@	Dose variable name
  infile =  .dataGetFullPath( replicate, dataType="Replicate", workingPath = workingPath),                    #@  data file to use
  workingPath = getwd()
){
  ###############################################################################
  # � Mango Solutions, Chippenham SN14 0SQ 2006
  # analyzeRep.R Wed Jul 04 12:20:41 BST 2007 @514 /Internet Time/
  #
  # Author: Romain    
  ###############################################################################
  # DESCRIPTION: wrapper for the analysis step
  # KEYWORDS: component:analysis
  ###############################################################################
  
  if( !is.numeric(replicate) || length(replicate) != 1 || replicate <= 0 )  
    ectdStop("replicate must be a single positive integer")
    
  ## check that the software is SAS or R 
  software <- try( match.arg(software), silent = TRUE )
  software %of% "try-error"  && ectdStop("The software should be `R` or `SAS`")
  
  .log( "Analysing replicate $replicate ($infile) with $software" )
  
  # checking the macro code
  if (software == "R") {
    if (class(analysisCode) == "function") analysisCode <- .checkFun( analysisCode, "data" )
    else {
     if (!file.exists(analysisCode)) ectdStop(paste("Cannot file R analysis code file \"", analysisCode, "\"", sep=""))
    }
  }
  else {
    if (!file.exists(analysisCode)) ectdStop(paste("Cannot file SAS analysis code file \"", analysisCode, "\"", sep=""))
  }
  
  # check on the replicate
  .log( "Analysing file $infile")
  
  ## checks on inputs
  doseCol      <- parseCharInput( doseCol     , expected = 1, valid = TRUE, convertToNumeric = FALSE )
  
  ## import the data
  idata <- readData( dataNumber = replicate, dataType = "Replicate", variables = doseCol, workingPath = workingPath )
  columns <- names( idata )
  doses <- sort( unique( idata[[ doseCol ]]  ) )

  ## check the flags  
  parOmitFlag	 <- parseCharInput( parOmitFlag	, expected = 1, convertToNumeric = FALSE )
  valid <- try( validNames( parOmitFlag ), silent = TRUE )
  if( valid %of% "try-error" || parOmitFlag %!in% columns ) removeParOmit <- FALSE
  
  respOmitFlag <- parseCharInput( respOmitFlag, expected = 1, convertToNumeric = FALSE )
  valid <- try( validNames( respOmitFlag ), silent = TRUE )                    
  if( valid %of% "try-error" || respOmitFlag %!in% columns ) removeRespOmit <- FALSE
    
  missingFlag <- parseCharInput( missingFlag, expected = 1, convertToNumeric = FALSE )
  valid <- try( validNames( missingFlag ), silent = TRUE )
  if( valid %of% "try-error" || missingFlag %!in% columns ) removeMissing <- FALSE
  
  interimCol   <- parseCharInput( interimCol  , expected = 1, convertToNumeric = FALSE, valid = TRUE )
  valid <- try( validNames( interimCol ), silent = TRUE )
  if( valid %of% "try-error" || interimCol %!in% columns || !is.numeric(idata[[interimCol]]) || any(idata[[interimCol]] < 0) ) interimCode <- NULL
    
  ## check the software
  software <- try( match.arg( software ), silent= TRUE)
  software %of% "try-error" && ectdStop("software should be `R` or `SAS`")
  
  
  ## subset data according to the remove Flags
  removeSub <- NULL                
  if( removeParOmit ) removeSub <- c( removeSub , paste( "( ", parOmitFlag  , " != 1 ) ", sep = "") )
  if( removeMissing ) removeSub <- c( removeSub , paste( "( ", missingFlag , " != 1 ) ", sep = "") ) 
  if( removeRespOmit) removeSub <- c( removeSub , paste( "( ", respOmitFlag , " != 1 ) ", sep = "") )
  if (length(removeSub)) {
    removeSub <- paste( removeSub , collapse = " & " )
    idata <- idata[ eval( parse( text = removeSub ), idata ), ,drop = FALSE ]
  }
  
  .log(" ... full analysis")
  fullAnalysis <-  performAnalysis( analysisCode = analysisCode, 
    seed = seed, data = idata, 
    software = software, doses = doses, infile = infile  )                                                                    
  
  ## add more variables to the dataset  
  alldata <- data.frame( INTERIM = 0, INTERIMC = "FULL", fullAnalysis , 
    DROPPED = 0, STOPPED = 0)  
    
  ## cycle through the interims
  if( !is.null(interimCode) ){
    # check if there is code
    
    uniqueInterim <- unique( idata [[interimCol]] )
    missing(interimCode) && ectdStop("No interim code function found")    
                                                   
    # check if the function exists
    interimCode <- try( match.fun(interimCode), silent =TRUE )
    interimCode %of% "try-error" && ectdStop("The interimCode function is not a valid function")
    
    # number of interim
    nInterim <- max( idata[[ interimCol ]])
  
    dataChanges <- NULL
    dropped<- NULL

    for( int in 1:nInterim ){
      .log( " ... interim $int / $nInterim" )
      
      # make the new subset
      intSubset <- c( interimCol %.% ">0", interimCol %.% " <= " %.% int )
                          
      # perform the analysis on the interim data
      newAnalysis <- try( 
        performAnalysis( analysisCode, seed = seed, data = idata, 
          software = software, subset = intSubset, dataChanges = dataChanges, 
          doses = doses, infile=infile ), 
        silent = TRUE  )
       newAnalysis %of% "try-error" && ectdStop("Error when executing `performAnalysis`\n\t$newAnalysis")
      
      newAnalysis <- data.frame( 
        INTERIM = int, 
        INTERIMC = if(int == nInterim) "FINAL" else int, 
        newAnalysis ) 
      
      # call the interimAnalysis function to get data changes
      iList <- interimAnalysis( newAnalysis, interimCode )
      if( "DROP" %in% names(iList) ){
        dataChanges <- rbind( dataChanges, cbind( 
          "( DOSE == " %.% iList$DROP %.% ") & (INTERIM > " %.% int  %.% ")" , 
          "INTERIM", 
          "-" %.% int ) )
      }
            
      # update the analysis data frame
      if( "DROP" %in% names(iList) ) {
        dropped <- unique( c(dropped, iList$DROP) )
      }
      newAnalysis <- data.frame(
        newAnalysis, 
        DROPPED = if( is.null(dropped) ) 0 else 1 * ( newAnalysis[[doseCol]] %in% dropped  ) , 
        STOPPED = 1 * ( "STOP" %in% names(iList) && iList$STOP )
        )
      
      # update the output  
      iC <- c(as.character(alldata$INTERIMC), as.character(newAnalysis$INTERIMC)) 
      # a warning is expected here because of the INTWERIMC columnn, but is dealt with
      # outside of the rbind call
      alldata <- suppressWarnings( rbind( alldata, newAnalysis) )
      alldata$INTERIMC <- iC 
       
      if( "STOP" %in% names(iList) && iList$STOP ) break
    }
  }
  alldata 
  
  }
           
