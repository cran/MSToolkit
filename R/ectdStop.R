ectdStop <- function( msg, call. = TRUE, domain = NULL, 
  verbose = ectdVerbose() ) {
###############################################################################
# � Mango Solutions, Chippenham SN14 0SQ 2006
# ectdStop.R Tue Jun 19 14:20:35 BST 2007 @597 /Internet Time/
#
# Author: Romain           
###############################################################################
# DESCRIPTION: generate error message
# KEYWORDS: error, component:support
###############################################################################

try(
  msg <- if( verbose )  .strinterp(msg) %.n% 
     "----------------------------------------------------------------------" %.n% 
     paste( sapply( sys.calls(), function(x)  as.character(x[[1]]) ), collapse = " > " ) %.n%
     "----------------------------------------------------------------------"
     else .strinterp(msg)
  , silent = TRUE)
  .Internal(stop(TRUE, msg ))

}
