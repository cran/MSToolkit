createExternalParameters <- function(
  subjects,            #@ Subjets for which to create parameters
  file,                #@ File name for data to import
  names,               #@ Names of fixed effects in data
  betNames,            #@ Between subject effects variables in the data
  betNums,             #@ Integer vector mapping between subject effects onto fixed effects
  errStruc="None",     #@ Function to map effects: Additive, Proportional or None
  suffix=".Between",   #@ Suffix to use for retained between subject effects variables
  subset=NULL,         #@ Subset to apply to the data
  refCol,              #@ Reference column for sampling
  refColName,          #@ Column name in data for referenced sampling
  idCol = "SUBJ",        #@ “ID” variable name for return data
  seed = .deriveFromMasterSeed(), #@ Random seed
  flagName="PAROMIT",  #@ Parameter omit flag name
  refColSuffix="refCol",
  dataId = idCol,
  workingPath = getwd() #@ Working directory
){
  ###############################################################################
  # � Mango Solutions, Chippenham SN14 0SQ 2006
  # createExternalParameters.R Thu Jun 07 14:20:55 BST 2007 @597 /Internet Time/
  #
  # Author: Romain    
  ###############################################################################
  # DESCRIPTION: generate a set of model parameter values for each subject in a 
  #              simulated dataset
  # KEYWORDS: component:data:parameter io
  ###############################################################################
  set.seed(seed) 
  
  subjects <- .expandSubjects( subjects )                      
  names    <- parseCharInput( names , convertToNumeric = FALSE, checkdup = TRUE, valid = TRUE)  
  idCol    <- parseCharInput( idCol , convertToNumeric = FALSE, expected = 1, valid = TRUE)
  dataId   <- parseCharInput( dataId, convertToNumeric = FALSE, expected = 1, valid = TRUE)
  flagName <- parseCharInput( flagName, convertToNumeric = FALSE, expected = 1, valid = TRUE)
  allnames <- c( dataId, names )
  if( length(suffix) != 1 )
    ectdStop("The `suffix` argument should be of length 1")
       
  errStruc <- initialChar( errStruc, "nap", 
    "The error structure must be one of: `none`, `additive` or `proportional`" )
                                          
  ## handle the between names
  if(!missing(betNames)){
    betNames <- parseCharInput( betNames, convertToNumeric = FALSE, valid = TRUE)
    
    if( missing(betNums) && betNames %!l% names ) 
      ectdStop("No `betNums` and length of names different than length of betNames") 
    
    betNums <- if(missing(betNums)) 1:length(betNames) else parseCharInput(betNums, checkdup = TRUE, convertToNumeric = TRUE)
    if( any(betNums<1 | betNums>length(names)))
      ectdStop("Incorrect value of `betNums`")
    if(betNums %!l% betNames) 
      ectdStop("`betNums` does not have the same length as `betNames`")
    allnames <- c( allnames, betNames )
  }
  any( duplicated(allnames) ) && ectdStop("Duplicated names" %.nt% paste(allnames, collapse = ","))

  idata <- .readAndCheckInputFile( file.path(workingPath, file), allnames  )
  if(!missing(refColName)){
    validNames(refColName)
    # find the real refColName ( remove the suffix )
    datanames <- names(idata)
    
    # verify that the refCol variable is in the file
    rx <- "\\." %.% refColSuffix %.% "$"
    if( refColName %~% rx )
      refColName <- refColName %-~% rx
    suffixedRefColName <- refColName %.% '.' %.% refColSuffix
    if( suffixedRefColName %in% datanames ) {
      refColName <- refColName
    } else if(refColName %!in% datanames) {
      ectdStop( "The reference column is not in the imported parameter file, checked:\n `$refColName`, `$suffixedRefColName`" )    
    }
    
    
    if(!missing(refCol) ){
      refCol <- parseCharInput( refCol )
      refcolvalues <- idata[[refColName]]
      if( refCol %!allin% refcolvalues ) 
        ectdStop( "Not all the values of `refCol` are found in the `$refColName` column of the imported data"  )
      if( refCol %!l% subjects )
        ectdStop( "The number of elements in `refCol` is not the same as the number of subjects" )
    } else refCol <- unique(idata[[refColName]])
  }


  ## subset the input data 
  if( !is.null(subset)){
     subset <- parseRangeCode( subset )
     idata <- idata[ eval(subset, idata), , drop = FALSE ]
     if( nrow(idata) == 0 )
       ectdStop( "No data left after applying the subset" )
  }  

  ## replace dataId by idCol
  names( idata )[ names(idata) == dataId ] <- idCol
  if (!missing(refColName) && refColName == dataId) refColName <- idCol
  
  ## figure out how to subset the data to possibly take care of the refCol settings
  ## see design doc for parameter version 0.3 section 3.2.3.2
  if(missing(refColName)){
    idx <- sample( nrow(idata) , size = nSubjects, replace = TRUE )
    idata <- idata[ idx, ]
  } else {
    # <SLOW>
    # potential slow code, callin sample within sapply
    idx <- sapply( refCol, function(x) sample(which(idata[[refColName]] == x),size =1 ))
    # </SLOW>
    idata <- idata[ idx, ] 
    if( refColName %in% names(idata) ) refColName <- refColName %.% '.' %.% refColSuffix
    idata <- .eval( "data.frame( idata[,-which(names(idata) == '$idCol')], $refColName = idata[[idCol]] )" )
  }
 
  out <- switch( errStruc, 
  "a" = {
    fixed   <- idata[,c( names),drop = FALSE]
    between <- idata[,betNames,drop=FALSE]
    fixed[, names[betNums]] <- fixed[, names[betNums]] + between
    fixed
  }, 
  "p" = {
    fixed   <- idata[,c(names),drop = FALSE]
    between <- idata[,betNames,drop=FALSE]
    fixed[, names[betNums]] <- exp ( fixed[, names[betNums]] + between ) 
    fixed
  }, 
  "n" = {
    idata[, c( names, if(missing(betNames)) NULL else betNames ) ]
  })
  out <- .eval("data.frame( $idCol = subjects, out, $flagName = 0, row.names = 1:nSubjects )")
  out
}
