interimAnalysis <- function(
  data,          #@ Input dataset
  interimCode    #@ interim code
){
  ###############################################################################
  # � Mango Solutions, Chippenham SN14 0SQ 2006
  # interimAnalysis.R Wed Jun 27 12:36:28 BST 2007 @525 /Internet Time/
  #
  # Author: Romain    
  ###############################################################################
  # DESCRIPTION: interim analysis support function
  # KEYWORDS: component:analysis
  ###############################################################################

  data %of% "data.frame" || ectdStop("data must be a data frame") 
  if( missing(interimCode) || is.null(interimCode) ) return(list()) 
  interimCode <- try( match.fun (interimCode), silent = TRUE)
  interimCode %of% "try-error" && ectdStop("The interimCode function generated errors")
  
  # remove some na's
  data <- na.omit(data)
  
  out <- try( interimCode( data )  , silent = TRUE)
  out %of% "try-error" && ectdStop("Error when applying the interim code on the data")
  out %of% "list"      || ectdStop("The output of the interim code is not a list")
  if(length(out) == 0 ) return(out)
  length(out) > 2      && ectdStop("The length of the output list can not be greater than 2")
  nm <- names(out)
  nm %allin% c("STOP", "DROP") || ectdStop("the names of the output list must be `STOP` or `DROP`")
  if("STOP" %in% nm){
    ( is.logical(out$STOP) && length(out$STOP) == 1 ) || ectdStop("STOP must a logical of length 1") 
  }
  if("DROP" %in% nm){
    is.numeric(out$DROP) || ectdStop("DROP must be a numeric vector") 
  }
  out
}
