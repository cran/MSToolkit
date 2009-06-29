createResponse <- function( 
  data,          #@ data structure to which 
  equation,      #@ function for creating response     >> createResponseVariable
  name = "RESP", #@ Response variable name
  invLink,       #@ inverse link function for predictor
  distribution = "normal",  #@ Outcome variable distribution
  covariance,    #@ Residual error (co)variance
  errStruc = "additive",      #@ Function describing how to apply residual error
  range,         #@ Range of Acceptable values for created response
  digits = 3,    #@ Number of digits to which round the response
  seed = .deriveFromMasterSeed(), 
  flagName = "RESPOMIT"
) {
  ################################################################################
  # � Mango Solutions, Chippenham SN14 0SQ 2006
  # createResponse.R Wed Jun 20 09:10:48 BST 2007 @382 /Internet Time/
  #
  # Author: Romain
  ###############################################################################
  # DESCRIPTION: create response, wrapper function
  # KEYWORDS: datagen, component:data:response
  ##############################################################################
  
  set.seed(seed)
 
  ## initial tests
  validNames( flagName, name)
  if( flagName == name ){
    ectdStop("Arguments `flagName` and `name` should be different")
  }
  
  digits <- parseCharInput( digits, expected = 1, msg = "digits should be only one number" )
  if( digits < 0  ) 
    ectdStop( "`digits` must be a positive integer value, is now : $digits" )
               
  distribution <- initialChar( distribution, "nlbp", 
    "distribution must be either `Normal`, `LogNormal`, `Binomial` or `Poisson`")
 
  # Handle the invLink  
  if( missing(invLink)) { # come up with the defaults
    invLink <- switch( distribution,  "n" = NULL,  "l" = exp,  "b" = plogis, "p" = exp)
  } else { # make some testing to make sure it works alright
  # is the function available ? 
    if( is.character( invLink ) ) {
      invLink <- try( match.fun(invLink), silent = TRUE )
      if( invLink %of% "try-error") ectdStop( "The `invLink` function specified is not a valid function" )
    }  
    # does it work correctly ?
    testInvLink <- try( invLink( rep(1, 5) ), silent = TRUE )
    if( testInvLink %of% "try-error" )
      ectdStop("Errors when calling the invLink function")
    if( length(testInvLink) != 5 )
      ectdstop(
        "The `invLink` function does not output a vector of same length as its inputs")  
  } 
  
  ## create the response
  name %<-% createResponseVariable( data, equation )
  
  ## add error
  # i need to use get because the user might use the `name` in the 
  # `range` code and s?he can give the `name` s?he wants
  if( !missing(covariance) ) 
    name %<-% addResidualError( get(name), covariance, errStruc, seed )
                                              
  ## apply the inverselink function 
  if( !is.null(invLink) ){
    sumNa <- sum(is.na(get(name)))
    name %<-% suppressWarnings(invLink(get(name)))
    naNow <- sum(is.na(get(name)))
    if (naNow > sumNa) ectdStop(paste("Applying inverse link function generated", naNow - sumNa, "missing values in the data"))
  } 
  
  ## for lognormal and normal: do nothing, otherwise :
  if( distribution == 'b' ) {           # binomial
    # check if the probabilities are \in [0,1]
    probs <- get(name)
    if( any(probs < 0 || probs > 1)) ectdStop( "the probabililties are not between 0 and 1")  
    name %<-% rbinom( nrow(data), prob = probs, size = 1 )
  } else if( distribution == 'p' ) {    # poisson
    lambda <- get(name)
    negTest <- lambda < 0
    if( any(negTest)) {
      pNeg <- round(100 * sum(negTest)/length(lambda))
      ectdWarning(paste(pNeg, "% of lambda values are less than 0 or missing - setting these values to 0 for the poission distribution draw", sep=""))
      lambda[negTest] <- 0
    }
    name %<-% rpois( nrow(data), lambda = lambda )    
  }
     
  ## find out which to flag as omit  
  if( missing(range)){
    omit <- rep( 0, nrow(data) )  
  } else {
    rangeExp <- parseRangeCode ( range )
    omit <- 1 * !eval( rangeExp , data )
    if( omit %!allin% c(0,1) ) 
      ectdStop( "The range code does not produce only 0 and 1 or TRUE and FALSE" )
  }      
  
  ## round the response
  name %<-% round( get(name), digits = digits )
    
  ## make the output data frame
  .eval( "data.frame( $name = get(name), $flagName = omit )" )  
}
                                     
