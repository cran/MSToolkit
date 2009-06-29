checkDropOutFun <- function(
  fun,          #@ dropout function to check
  data,         #@ Data on which to execute function
  sizeSubset = 5, #@ size of the subset to use to perform the check
  useSubset = TRUE, #@ Logical flag, do we subset before performing the check
  ...           #@ Extra arguments to pass to the function
){
###############################################################################
# � Mango Solutions, Chippenham SN14 0SQ 2006
# checkDropOutFun.R Tue Jun 19 11:12:38 BST 2007 @467 /Internet Time/
#     
# Author: Romain    
###############################################################################
# DESCRIPTION: check the validity of the drop out function
# KEYWORDS: component:support
###############################################################################

  ## make sure it is a function
  fun <- try( match.fun(fun), silent = TRUE )
  fun %of% "try-error" && ectdStop("Dropout function not found")
  fun %of% "function"  || ectdStop("Dropout function specified is not a valid function")
  
  nf <- names( formals( fun ) )  
  if (!any(nf == "data")) ectdStop("The dropout function must have a `data` argument")
  
  # Run function on section of data
  hd <- if( useSubset ) head( data, n = sizeSubset  ) else data
  out <- try( fun( hd, ... ) , silent = TRUE) 
  out %of% "try-error" && ectdStop("Error when calling the dropout function on a subset of data")
  
  length(out) == nrow(hd ) || ectdStop("The dropout function outputs a vector of wrong length")
  unique( as.integer( out )) %allin% c(0,1) || ectdStop("The dropout function outputs a vector with values different from 0 and 1")
  
  invisible( TRUE )
}
