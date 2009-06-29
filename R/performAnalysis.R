performAnalysis <- function(
  analysisCode,  #@ File containing the actual analysis code to run on the data
  doses,         #@ Doses for which estimates are expected
  data,          #@ Input dataset
  software = c("R","SAS"),      #@ Software for analysis: R or SAS
  infile,        #@ Input file name for external call
  subset. = NULL,        #@ Subsets to be applied to the data
  dataChanges = NULL,   #@ Changes to be made to the data before analysis
  seed=.deriveFromMasterSeed(), #@ Random number generation seed
  workingPath = getwd()
)
{
  ###############################################################################
  # � Mango Solutions, Chippenham SN14 0SQ 2006
  # performAnalysis.R Wed Jun 27 11:08:20 BST 2007 @464 /Internet Time/
  #
  # Author: Romain    
  ###############################################################################
  # DESCRIPTION: analyze a single replicate of data
  # KEYWORDS: component:analysis
  ###############################################################################
  
  ## check that the software 
  software <- try( match.arg(software), silent = TRUE )
  software %of% "try-error"  && ectdStop("The software should be `R` or `SAS`")
    
  switch( software, 
    "SAS" = {
       # Ensure that the "infile" file exists
       if (!file.exists(infile)) ectdStop("Cannot find replicate data file to analyze")
       
       # Set up parameters for the SAS call
       if (length(subset.) == 1 && is.character(subset.) && gsub(" ", "", subset.) == "") subset. <- NULL
       if (length(dataChanges) == 1 && is.character(dataChanges) && gsub(" ", "", dataChanges) == "") dataChanges <- NULL
       if (length(subset.)) sasSubsets <- convertToSASCode(subset.) else sasSubsets <- "**** No subsets to apply ****;"
       if (length(dataChanges)) sasChanges <- convertToSASCode(dataChanges) else sasChanges <- "**** No data changes to apply ****;" 
  
       # Set location for temporary CSV file
       outfile <- paste(infile, ".temporary", sep="")
       sasParameters <- paste(infile, outfile, file.path(workingPath, analysisCode), sasSubsets, sasChanges, seed, sep="#")
       .log(paste("Calling SAS with execution string \"", sasParameters, "\"", sep=""))
  
       trySas <- .ectdSasCall(sasParameters)
       trySas %of% "try-error"  && ectdStop("Problems occurred when calling SAS in batch mode (see SAS log file for more details)")
       
       # Call SAS
       if (file.exists(outfile)) {
         sasData <- read.csv(outfile)
         sasData <- checkMicroFormat( doses = doses, dat = sasData )
         try(file.remove(outfile))
       }
       else sasData <- createEmptyMicro( doses = doses )
       return(sasData)
    }, 
    "R" = {
      set.seed( seed )
      
      ## apply the dataChanges if needed
      if( !missing(dataChanges) ){
        data <- .applyDataChanges( dataChanges, data )
      }
      
      ## apply any subset. to the data
      if( !missing(subset.) ){
        subset. <- try( parseRangeCode( subset. ) ,silent =  TRUE )
        subset. %of% "try-error" && ectdStop("Error when parsing the subset. code \n\t$subset.")
        data <- try( data[ eval(subset., data) , ,drop = FALSE], silent = TRUE )
        data %of% "try-error" && ectdStop( "Errors when applying the subset. :\n\t$data" )    
      }
      
      if( missing(doses)) doses <- unique( data$DOSE )
      
      ## run the analysis code
      if (class(analysisCode) == "function") analysisOutput <- try( analysisCode(data)  , silent = TRUE )
      else analysisOutput <- try(eval(parse(analysisCode)), silent = TRUE)
      
      out <- if( analysisOutput %of% "try-error" ){
        ectdWarning(
          "Error when executing analysis code: " %.nt% 
          ( analysisOutput %-~% "^[^:]*:") %.nt%    # extract the message from the `try`
          "... creating an empty summary file" )
        
        createEmptyMicro( doses = doses )
      } else {
        checkMicroFormat( doses = doses, dat = analysisOutput ) 
      }
      
      return( out )
    })
     
  
  
  
  
}

.checkCorrectDataChanges <- function(
  dataChanges,          #@ matrix of 3 columns describing the changes to make to the data
  data = NULL,          #@ dataset
  checkData = TRUE      #@ should we check the data?
){
  ## check the structure of the matrix  
  if( !is.matrix(dataChanges) || !is.character(dataChanges) || ncol(dataChanges) != 3 ){
    ectdStop("`dataChanges` must be a matrix with 3 columns")
  }    

  ## check that the target variables are in the data
  if(checkData && dataChanges[,2] %!allin% names(data) ){
    ectdStop("The target variables :" %.% paste( dataChanges[,2] %wo% names(data), collapse = ", " ) %.% "are not in the dataset"  )
  }
  
}


.applyDataChanges <- function(
  dataChanges,  #@ matrix of 3 columns describing the changes to make to the data
  data          #@ dataset
){
  if( missing(dataChanges) || is.null(dataChanges) ) return(data)
  
  .checkCorrectDataChanges( dataChanges, data )
  
  for( i in 1:nrow(dataChanges) ){
    test <- try( data[ eval( parse(text=dataChanges[i,1]), data) , dataChanges[i,2] ] <- as.numeric( dataChanges[i,3] ) )
    test %of% "try-error" && ectdStop("Error when applying the datachanges to the dataset\n\t$test")   
  }
  
  data

}

