removeDirectories <- function(  
  dirNames = c("ReplicateData", "MicroEvaluation", "MacroEvaluation"), 
                         #@ A vector containing the full names of the directories to be removed
  workingPath = getwd()  #@ Directory in which to remove directories
) {
  ###############################################################################
  # Mango Solutions, Chippenham SN14 0SQ 2006
  # removeDirectories.R 20/06/2007 15:28:29 BST 2007 @445 /Internet Time/
  # Author: Francisco
  ###############################################################################
  # DESCRIPTION: Tries to remove named subdirectories.  Returns a logical vector 
  # representing the success or failure of directory removal
  # KEYWORDS: IO
  ###############################################################################

  # Quit if the list is too short
  if(!length(dirNames)) ectdStop("No directories to remove")
  
  # Match directory name against expected inputs
  dirNames <- match.arg(dirNames, several.ok = TRUE)
  
  # Create full directory paths
  fullPaths <- file.path(workingPath, dirNames)

  # Create directories using the "dir.create" function
  result <- sapply(fullPaths, unlink, recursive = TRUE)
  result <- as.logical(as.vector(result)) # Convert result to logicals with no names
  
  if (any(result)) .log(paste("Removed directory", fullPaths[result]))

  return(result)	
}
