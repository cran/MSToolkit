validNames <- function( ... ) { 
  ################################################################################
  # Mango Solutions, Chippenham SN14 0SQ 2006
  # validNames.R Fri Jun 01 14:06:41 BST 2007 @587 /Internet Time/
  #
  # Author: Romain
  ################################################################################
  # DESCRIPTION: check if the names are valid R names
  # KEYWORDS: component:support check
  ################################################################################
  
  sapply( list(...), function(x){
    if( !is.null(x) && x %!~% "^[\\.]?[a-zA-Z][\\.0-9a-zA-Z]*$"  ){
       wrongs <- paste( x[..nm], collapse= ", ")
       ectdStop( "$wrongs : invalid R name(s)" )
    }
  })
  TRUE
          
}

