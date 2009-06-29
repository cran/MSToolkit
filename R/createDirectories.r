createDirectories <- function(  
  dirNames = c("ReplicateData", "MicroEvaluation", "MacroEvaluation"), #@ A vector containing the full names of the directories to be created.
  workingPath = getwd(), #@ Directory in which to create directories
  warn = FALSE           #@ Should the dir.create function show warnings?
) 
{
  ###############################################################################
  # Mango Solutions, Chippenham SN14 0SQ 2006
  # createDirectories.R 20/06/2007 15:28:29 BST 2007 @445 /Internet Time/
  # Author: Francisco
  ###############################################################################
  # DESCRIPTION: Tries to create named subdirectories for storing replicate, micro 
  # evaluation and macro evaluation data.  Returns a logical vector representing
  # the success or failure of directory creation
  # KEYWORDS: IO
  ###############################################################################
  
  # Quit if the list is too short
  if(!length(dirNames)) ectdStop("No directories to create")
  
  # Match directory name against expected inputs
  dirNames <- match.arg(dirNames, several.ok = TRUE)
  
  # Create full directory paths
  fullPaths <- file.path(workingPath, dirNames)

  # Create directories using the "dir.create" function
  result <- sapply(fullPaths, dir.create, showWarnings = warn)
  
  # Log the creation of the directories
  if (any(result)) .log(paste("Created directory", fullPaths[result]))
  return(result)	
}
