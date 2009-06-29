addResidualError <- function( 
  response,                       #@ numeric vector of response data
  covariance,                     #@ lower triangle or matrix
  errStruc = "additive",          #@ function describing how to apply residual error
  seed = .deriveFromMasterSeed( ) #@ Random Seed to use
  ) { 
  ################################################################################
  # Mango Solutions, Chippenham SN14 0SQ 2006
  # addResidualError.R Tue Jun 19 16:17:20 BST 2007 @678 /Internet Time/
  #
  # Author: Romain
  ################################################################################
  # DESCRIPTION: add residual error to a response
  # KEYWORDS: component:response
  ################################################################################

  .requiredArgs(response, "The `response` variable is required")
  .requiredArgs(covariance, "The `covariance` argument is required")
  set.seed(seed)

  # <TODO>
  # currently handle only one variable
  covariance <- parseCovMatrix( covariance, 1)                  
  # </TODO>

  errFun <- if(is.function(errStruc)) errStruc else {
    errStruc <- initialChar( errStruc, "ap", 
      "`errStruc` should be `additive`, `proportional` or a function")
    switch( errStruc, 
       "a" = function(x,y) x+y,      # additive
       "p" = function(x,y) exp(x+y)  # proportional
       )
  }
  
  error <- rnorm( length(response), mean = 0, 
    sd = sqrt(covariance[1,1]) )
  
  if( length(formals(errFun)) <2  ){
    ectdStop("The error function should take at least two arguments")
  }
  
  out <- errFun( response, error )
  if( out %!of% "numeric"){
    ectdStop("The error function should return a numeric vector") 
  }
  if( out %!l% response ){     
    ectdStop(
      "The error function supplied generates a vector that does not have" %.nt%
      "the same length as the response vector supplied" 
      ) 
  }
  out
  
  
}

