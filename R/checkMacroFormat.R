                    
checkMacroFormat <- function(
   data    #@ Data to check
)
{
  ###############################################################################
  # Mango Solutions, Chippenham SN14 0SQ 2006
  # Fri Jun 21 16:51 BST 2007 @445 /Internet Time/
  # Author: Francisco
  ###############################################################################
  # DESCRIPTION: Checks whether a data frame conforms with the expected 
  # "Macro Evaluation" data format
  # KEYWORDS:misc, IO 
  ###############################################################################  

  if(data %!of% "data.frame" || nrow(data) != 1) ectdStop("Macro evaluation data must be a data frame with a single row")
  invisible(TRUE)
}
  