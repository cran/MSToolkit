parseCharInput <- function(
  input,             #@ comma separated character vector to split into numeric values
  convertToNumeric = TRUE,  #@ convert to numeric at the end ?
  sort = FALSE,             #@ sort the values ?
  expected,           #@ expected length of the output
  msg = "wrong length",      #@ error message when wrong length
  checkdup = FALSE, 
  missingMsg = 'Unknown',               #@ error message if missing input
  checkProb = FALSE,         #@ check that this sums up to one
  valid = FALSE
  ){
  ###############################################################################
  # Mango Solutions, Chippenham SN14 0SQ 2006
  # parseCharInput.R Fri Wed Jun 06 12:55:37 BST 2007 @538 /Internet Time/
  #
  # Author: Romain
  ###############################################################################
  # DESCRIPTION: parses a csv character string
  # KEYWORDS: check, component:support
  ###############################################################################
  
  missing(input) && ectdStop(missingMsg)
  
  if(is.null(input)) return(NULL)
  
  out <- if(is.numeric(input) ) input else {
    ## handles the commas (TESTFIX)
    if( input %~% ",$")
      ectdStop("Traling comma not accepted")
    if( input %~% "^," )
      ectdStop("Leading comma not accepted")
    if( input %~% ",{2,}")
      ectdStop("The separator should only be **one** comma")
      
    ## a numeric value is made numbers, dots, plus, minus and possibly e for the scientific
    ## notation (is there a need to be more clever with the scientific notation) 
    if( convertToNumeric && input %~% "[^0-9eE\\.,[:space:]\\+\\-]" ) 
      ectdStop("Impossible to convert to numbers")
    out <- unlist( strsplit(input, "[[:space:]]*,[[:space:]]*" ) )
    if(convertToNumeric) out <- as.numeric(out)  
    out
  }
    
  valid && validNames( out )
  
  if( !missing(expected) && length(out) != expected )
    ectdStop( msg )
  if( checkdup && any(duplicated(out)))
    ectdStop( "Duplicated values in " %.% deparse(substitute(input)))
  
  ## convert to numeric and sort if required
  if(checkProb  ){
    if(!is.numeric(out) || sum(out) != 1 )
      ectdStop("Parsed values don't sum up to one")
  }
  if(sort) {
    sort(out) 
  } else {
    out
  }
}

