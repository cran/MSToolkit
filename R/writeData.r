writeData <- function( 
  dat,                        #@ Data to write, should be a data frame
  dataNumber,                 #@ The number of the data entry
  dataType = c("ReplicateData", "MacroEvaluation", "MicroEvaluation"),                   #@ Type of data ("Replicate", "Micro" or "Macro"
  workingPath = getwd(),      #@ Working directory 
  append = FALSE              #@ If TRUE, this routine will append to the end of a pre-existing file
)
{
  ###############################################################################
  # Mango Solutions, Chippenham SN14 0SQ 2006
  # writeData.R Wed Jun 20 20:23:26 BST 2007 @418
  #
  # Author: Francisco
  ###############################################################################
  # DESCRIPTION: Writes a single entry of data to a file.  Returns TRUE if the operation was successful and FALSE otherwise.
  # KEYWORDS: IO
  ###############################################################################  
  
  dataType <- match.arg(dataType)                                    # Check data type
  if(!is.data.frame(dat)) ectdStop("dat is not a data frame")        # Check that dat is a data frame
  
  # Get full path to data
  fullPath <- .dataGetFullPath(dataNumber = dataNumber, dataType = dataType, workingPath = workingPath)
  
  # Write the file
  tryWrite <- try(write.csv(dat, fullPath, append = append, row.names = FALSE))
  if(tryWrite %of% "try-error") 
    ectdStop("Error when writing the data to file $fullPath:\n\t$tryWrite" )
  .log( "writing $dataType data number $dataNumber" )
  
  invisible(TRUE)
}                        