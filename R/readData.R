readData <- function(
  dataNumber,               #@ The number of the data entry, should be between 1 and 9999
  dataType = c("ReplicateData", "MicroEvaluation", "MacroEvaluation"),        #@ String containing the type of data to be read ("Replicate", "Micro" or "Macro")
  variables = NULL,         #@ The variables we are expecting in the data
  workingPath = getwd()     #@ Working directory
  )
  ###############################################################################
  # Mango Solutions, Chippenham SN14 0SQ 2006
  # readData.R Fri Jun 22 15:47:41 BST 2007 @418 /Internet Time/
  #
  # Author: Francisco
  ###############################################################################
  # DESCRIPTION: Reads an individual data entry of the specified type (Replicate, Micro or Macro) and returns it as a data frame
  # KEYWORDS:IO
  ###############################################################################
{
  # Check data type
  dataType <- match.arg(dataType)
  # Get the location of the file containing the specified data element
  fullPath <- .dataGetFullPath(dataNumber = dataNumber, dataType = dataType, workingPath = workingPath) 
  # Read the data
  dat <- .readAndCheckInputFile(fullPath, variables)  
  return(dat)
}