"compileSummary" <- function (
  dataType =  c("MicroEvaluation", "MacroEvaluation"),   #@ The type of data to compile.  Should be "MicroEvaluation" or "MacroEvaluation".
  replicates = NULL, 
  tryPerl = FALSE,
  workingPath = getwd()
  ){
  ###############################################################################
  # Mango Solutions, Chippenham SN14 0SQ 2006
  # compileSummary.R Fri Jun 25 12:45:35 BST 2007 @445 /Internet Time/
  # Author: Francisco, Romain
  ###############################################################################
  # DESCRIPTION: Loads all of the micro/macro data into a single 
  # data frame augmented by a replicate number colunm and
  # writes the data frame to the file "microSummary.csv"
  # KEYWORDS: io
  ###############################################################################
          
  dataType <- try( match.arg( dataType ), silent = TRUE )
  if( dataType %of% "try-error") ectdStop("Invalid data type: $dataType")
  prefix <- casefold( substring( dataType, 1, 5) )     
  
  ## test that at least some data is there
  if( !file.exists( file.path(workingPath, dataType) ) )
    ectdStop("directory $dataType unavailable under $workingPath") 
  if( !length( repfiles <- dir( file.path(workingPath, dataType), pattern =  "m[ia]cro[[:digit:]]{4}\\.csv" ))) 
    ectdStop("no data files in $workingPath/$dataType") 
  if( !is.null(replicates) && !all( out <- file.exists( targetfiles <- file.path(workingPath, dataType, sprintf("%s%04d.csv", prefix, replicates)) )  ))
    ectdStop( paste( "Impossible to compile the data, the following files are missing: ", paste(targetfiles[!out] , collapse = "\n\t"), sep = "\n\t" ) )
  
  nRep <- if(is.null(replicates)) length(repfiles) else length(replicates)
  .log( "Compiling $nRep $dataType files" )  

  ### Add a try(...) around the system call to check that Perl is set up correctly to do this
  ###  If problems with Perl call then throw a sensible error using ectdStop
  ###  AND / OR switch to the backup else{ ... } option

  if( tryPerl && .canUsePerl()){
    dataType <- substring(dataType, 1, 5)
    perlCall <- sprintf("%s/exec/compile --path=%s --type=%s > %s/%sSummary.csv", .path.package("MSToolkit"), 
      workingPath, dataType, workingPath, casefold(dataType) )
    if( !is.null(replicates)) perlCall <- paste( perlCall, paste(replicates, collapse=",") )  
    system( perlCall )
  } 
  else {                                        
    dataToWrite <- readAllData(dataType = dataType, replicates = replicates, workingPath = workingPath)
    summaryFile <- file.path(workingPath, sprintf("%sSummary.csv", prefix ))
    write.csv(dataToWrite, file = summaryFile, row.names = FALSE)
  }
}                                                                                         
