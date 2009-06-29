parseRCode <- function( 
  code     #@ code to parse
){
  ###############################################################################
  # � Mango Solutions, Chippenham SN14 0SQ 2006
  # parseRCode.R Thu Jun 07 14:39:44 BST 2007 @610 /Internet Time/
  #
  # Author: Romain    
  ###############################################################################
  # DESCRIPTION: parse R code
  # KEYWORDS: component:support 
  ###############################################################################
  
  result <- try( parse( text = code ),  silent = TRUE )
  if( result %of% "try-error" ) {
    ectdStop('parsing problem: ' %.% ( result %-~% "^[^:]*:" )     ) 
  } else {  
    result 
  }
  
}              
