macroEvaluation <- function(
  data,                   #@ dataset
  macroCode,              #@ macro code
  interimCol = "INTERIM", #@ name of the INTERIM column 
  doseCol = "DOSE"        #@ name of the DOSE column
  ){
  ###############################################################################
  # � Mango Solutions, Chippenham SN14 0SQ 2006
  # macroEvaluation.R Wed Jun 27 14:32:46 BST 2007 @606 /Internet Time/
  #
  # Author: Romain    
  ###############################################################################
  # DESCRIPTION: summarise a single set of 
  # KEYWORDS: component:analysis
  ###############################################################################

  .log( "calling macro evaluation function" )
  interimCol <- parseCharInput( interimCol, expected = 1, convertToNumeric = FALSE, valid = TRUE )
  doseCol    <- parseCharInput( doseCol   , expected = 1, convertToNumeric = FALSE, valid = TRUE )
  
  if( data %!of% "data.frame"){
    ectdStop("data must be a data frame")
  }
 
  if( c(interimCol, doseCol) %!allin% names(data) ){
    ectdStop("Columns $interimCol and $doseCol must be in the dataset") 
  }
  
  out <- try( macroCode(data), silent = TRUE)
  if(out %of% "try-error"){
    ectdStop("Error when calling the macroCode \n\t$out")
  }
  .log("Checking macro evaluation data")
  checkMacroFormat(out)
  
  out  
  
}
