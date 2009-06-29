###############################################################################
# Mango Solutions, Chippenham SN14 0SQ 2006
# utils.R Fri Jun 01 10:57:12 BST 2007 @456 /Internet Time/
#
# Author: Romain
###############################################################################
# DESCRIPTION: set of utility functions, not exported
# KEYWORDS: component:helper 
###############################################################################

.nonCumulativeFromCumulative <- function( proportion ){
  
  ## format it correctly
  proportion <- parseCharInput( proportion ) 
  
  ## check that the numbers are probabilities \in (0,1]
  if(max(proportion) >  1) ectdStop("Proportion greater than 1")
  if(min(proportion) <= 0) ectdStop("Proportion lower than 0")
  
  ## append 1 if needed
  if(max(proportion) != 1) proportion <- c(proportion, 1)
  
  ## generate the non-cumulative proportions
  proportion <- diff( c(0, proportion)  )
  if(any(proportion <= 0)) ectdStop("Proportion not increasing")
  proportion
}

.deriveFromMasterSeed <- function(){
  sample(1:999, 1) 
}
 
### regular expression toys
                                
.quotes <- "(\\\"|\')"
        

.strinterp <- function(txt){
  myrx <- "\\$(\\{(.*?)\\}|[\\.]?[a-zA-Z][\\.0-9a-zA-Z]*)"
  
  gx  <- gregexpr( myrx , txt, perl = TRUE)[[1]]
  va  <- substring( txt, gx + 1, gx + attr(gx, "match.length")-1) %-~% "[\\{\\}]"
  spl <- strsplit( txt, myrx , perl = TRUE )[[1]]
  if(length(va) == 1 && va == "") return(txt)
  if(length(va) == length(spl)) spl <- c(spl, "")

  out <- spl[1]
  for( i in seq( along = va )) {
    varName <- try( get(va[i], pos = parent.frame(2) )[1] ,silent = TRUE )
    if( varName %of% "try-error") varName <- va[i]
    out <- out %.% varName  %.% spl[i+1]
  }
  out
}

.allSameLength <- function( ... ){
  all( diff( sapply(list(...), length)) == 0 ) 
}

.requiredArgs <- function(arg, msg){
  if( missing(arg) ) {
    sc <- sys.call()
    nameArg <- as.character(sys.call()[2])
    if( missing(msg) ) msg <- "The argument `" %.% nameArg %.% "` is missing" 
    ectdStop(msg)
  }
}

# changes 10 into 1:10, check that 10 is positive
# write the number of subjects in the parent environment as `nSubjects`
.expandSubjects <- function(subjects){
  if(length(subjects) == 1) {
    if(subjects < 1) ectdStop("subjects must be positive")
    subjects <- 1:subjects 
  } else {
    subjects
  }
  assign("nSubjects", length(subjects), parent.frame())
  subjects
}

.handleProbArray <- function( probArray, values, probs){
  
  if( !missing(probArray) && is.matrix(probArray) ){
    sum(probArray) == 1 || ectdStop("`probArray` does not sum up to one")
    if( missing(values) ) { 
      values <- dimnames(probArray)
      if( is.null(values) ){
        values <- sapply( dim(probArray), seq)
      }
    }
    if( !all(  dim(probArray)  == sapply(values, length)  )) 
      ectdStop("Dimension problem between `probArray` and `values`")
    out <- cbind( do.call( expand.grid, values ), probs = as.vector( probArray) )
    out <- subset( out, probs > 0)
    
  } else { 
  
  
    ## make tests on the probArray or build it
    if( missing(probArray) ){ # try to build it
      if(missing(values) || missing(probs) )
        ectdStop("`values` and `probs` must be supplied if probArray is missing")
      probArray <- probs[[1]]
      lp <- length(probs)
      if(lp>1){
        for(idx in 2:lp){
          probArray <- probArray %o% probs[[idx]]
        }
      }
    } else {   # check the probArray
      if(sum(probArray[,ncol(probArray)]) != 1)
        ectdStop("the probArray does not sum up to one")
      if(length(dim(probArray)) != length(values))
        ectdStop("The `probArray` has wrong dimensions")
    }
    
    ## make the grid                               
    out <- cbind( do.call( expand.grid, values ), PROB = as.vector(probArray) )
  }
  ## make sure they are all factors
  for( va in 1:length(values)){
    out[,va] <- factor(out[,va])
  }
  out
}

.eval <- function( txt ){
  eval( parse( text = .strinterp(txt) ), parent.frame() )
}

.dummy <- function(...){
  cat( "\n\nFunction `"%.% as.character(match.call()[1]) %.% "` not yet implemented\n\n" ) 
}

.log <- function( ... , file = ectdLogFile(), verbose = ectdVerbose()){
  if( verbose ){ 
    msg <- paste( ..., "\n", sep = "" )
    msg <- .strinterp( msg )
    msg <- sprintf( "[%s] %s", format( Sys.time(), ectdDateFormat() ) , msg )
    cat( msg, file = file, append = TRUE)
  }
  invisible( NULL )
}

.checkFun <- function(
  fun, 
  expectedArgs
){

  fun <- try( eval( match.fun(fun), parent.frame() ), silent = TRUE )
  if(fun %of% "try-error") 
    ectdStop("not a valid function")   
  if( !missing(expectedArgs) ){  
    expectedArgs <- parseCharInput( expectedArgs, convertToNumeric = FALSE )           
    if( expectedArgs %!allin% names(formals(fun)) ) 
      ectdStop("Problem with the arguments of the function")
  }
  fun


}

.checkReplicates <- function( replicates, workingPath = getwd() ){
 
  ## Ensure the replicate files exist
  if( is.character(replicates) ){
    length( replicates) == 1 || ectdStop("replicates must be of length one if it is a character vector")  
    repFiles <-  dir("ReplicateData", pattern = "[0-9]{4}\\.csv$", full = TRUE)
    length(repFiles) > 0 || ectdStop( "The replicate director was not found or is empty" )
    replicates <- as.numeric( gsub( ".*([0-9]{4})\\.csv", "\\1", repFiles ) )
  } 
  else { ## them replicates is a numeric vector   
    repFiles <- .dataGetFullPath( replicates, dataType="Replicate", workingPath = workingPath)
    ectdStop("Some replicate data files do not exist") %unless%  all(file.exists( repFiles ))
  }                              
  replicates 
}

.checkGridAvailable <- function(){
  .Platform$OS.type != "windows" && suppressWarnings(require(Rlsf, quietly=TRUE))
}

.splitGridVector <- function(vec, nReps = 100) {
  startVec <- rep(1:ceiling(length(vec)/nReps), each=nReps)[1:length(vec)]
  if (any(startVec > 1) & sum(startVec == max(startVec)) < .1 * nReps) {
    startVec[startVec == max(startVec)] <- max(startVec)-1
  }
  split(vec, startVec)
}

".ectdSubmit" <- function(func, ..., savelist=c(), packages=NULL, ncpus=1, debug=FALSE, reqSas = FALSE) {

    # Set the RLSF_R environmental variable
    if (length(grep("solaris", version$platform, ignore.case = TRUE))) Sys.putenv("RLSF_R" = Sys.getenv("RLSF_UNIX"))
    else Sys.putenv("RLSF_R" = Sys.getenv("RLSF_LINUX"))
    .log("Setting the RLSF_R environmental variable to", Sys.getenv("RLSF_R"))

    # Build the call to the grid
    fname <- tempfile(pattern = "Rlsf_data", tmpdir = getwd())
    lsf.call <- as.call(list(as.name(func), ...) )
    savelist <- c(savelist, "lsf.call", "packages")
    save(list=savelist, file=fname)
    script <- paste(file.path(.path.package("Rlsf"), "RunLsfJob"), fname)
    jobid <- .Call("lsf_job_submit", as.integer(debug), script, as.integer(ncpus), PACKAGE="Rlsf")
    
    # If SAS is required, modify the LSF call
    if (reqSas) {
       try(system(paste("bmod -R sas", jobid), intern=TRUE), silent = TRUE)
       .log("Setting the job resource tag to SAS ...")
    }

    # Return the job list information
    if (jobid) list(jobid=jobid,fname=fname,debug=debug)
    else return(NULL)
  }

.ectdSasCall <- function(params, 
  sasLoc = if (.Platform$OS.type == "windows") Sys.getenv("SASPATH_WIN") else Sys.getenv("SASPATH_UNIX"), 
  macroLoc = file.path(.path.package("MSToolkit"), "sasAnalysis.sas"), 
  logFile = file.path(workingPath, "sasLogfile.log"),
  printFile = file.path(workingPath, "sasOutput.lst"),
  workingPath = getwd()) 
{
  sasDir <- gsub( "\\\\[^\\]*$", "", sasLoc)
  if(!file.exists(sasDir)) ectdStop("SAS is not available on the system: \n\t$sasLoc")
  callOptions <- if (.Platform$OS.type == "windows") "-nosplash -icon -xmin -noxwait" else "-NOTERMINAL"
  sasCall <- paste("\"", sasLoc, "\" -SYSIN \"", macroLoc, "\" -SYSPARM \"", params, "\" -LOG \"", logFile, "\" -PRINT \"", printFile, "\" ", callOptions, sep="")
  .log("Calling SAS with call string: ", sasCall)
  invisible(try(system(sasCall)))
}


.roundIt <- function( data, digits ){
  if(!missing(digits)){   
    digits <- parseCharInput( digits, convertToNumeric = TRUE )
    nCov <- ncol( data )
    if(any(digits < 0 )) ectdStop("`digits` should be a positive vector")
    len.di <- length(digits)
    if( len.di == 1 ) {
      data <- round( data, digits = digits)
    } else if( len.di == nCov ) {
      for( i in 1:nCov){
        data[,i] <- round( data[,i], digits = digits[i] )
      }
    } else {
      ectdStop("`digits should be of length one or $nCov, not ${len.di}`")
    }
  }
  data
}


.cleanup <- function( 
  cleanUp = TRUE,       # do I do any cleanup
  grid = FALSE,         # was the grid used
  workingPath = getwd() # where to work 
  ){
  
if( cleanUp ){
   .log("removing micro and macro directories")
   removeDirectories(c("MicroEvaluation", "MacroEvaluation"), workingPath = workingPath)
   if(grid){ 
     .log("Removing Rlsf generated files")
     try(file.remove(list.files(pattern="Rlsf_.*")), silent = TRUE)
   }
}
  
}

