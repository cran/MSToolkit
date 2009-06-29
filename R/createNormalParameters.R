createNormalParameters <- function( 
  subjects,             #@ subjects for which to create parameters
  names,                #@ names of parameters to generate
  mean,                 #@ means for fixed parameters
  covariance = 0,       #@ covariance matrix for fixed parameters
  range,                #@ range of acceptable values
  betNames,             #@ between subject effects to create
  betMean,              #@ means for between subject effect
  betCov = 0,           #@ covariance for the between subject effect
  betRange,             #@ range of acceptable values for the between subject effet
  errStruc = "None",    #@ function to map effect (Additive, Proportional, or None)
  suffix = ".Between",  #@ Suffix to use for retain between subject effects
  idCol = "SUBJ",         #@ ID variable name for return data
  maxDraws = 10,        #@ Maximum number of iterations for valid parameters
  seed = .deriveFromMasterSeed( ),  #@ Random seed
  flagName = "PAROMIT", 
  digits = 3  #@ number of digits to round to
) {
  ################################################################################
  # � Mango Solutions, Chippenham SN14 0SQ 2006
  # createNormalParameters.R Wed Jun 20 11:39:52 BST 2007 @486 /Internet Time/
  #
  # Author: Romain
  ###############################################################################
  # DESCRIPTION: create parameters from a normal distribution
  # KEYWORDS: datagen, component:data:allocate
  ##############################################################################
  
  set.seed(seed)
  
  ## initial tests
  subjects <- .expandSubjects( subjects ) 
  mean <- parseCharInput( mean )
  nFixed <- length(mean)
  idCol <- parseCharInput( idCol, expected = 1, convertToNumeric = FALSE, valid = TRUE)
  flagName <- parseCharInput( flagName, expected = 1, convertToNumeric = FALSE, valid = TRUE)
  maxDraws <- parseCharInput( maxDraws, expected = 1, convertToNumeric = TRUE)

  names <- if(missing(names)) {
    "X" %.% 1:nFixed 
  } else { 
    parseCharInput( names, convertToNumeric = FALSE, 
      checkdup = TRUE, expected = nFixed, valid = TRUE )
  }
  covariance <- parseCovMatrix( covariance, nFixed )
  
  errStruc <- initialChar( errStruc, "nap", 
    "`errStruc` must be `Additive`, `Proportional` or `None`" )
      
  maxDraws < 1 && ectdStop("The maximum number of draws should be a positive integer")
  digits < 0   && ectdStop("The `digits` argument must be positive")
  
  ## generate the fixed effect parameters
  if( missing(range)){
    # just generate one set
    fixed <- as.data.frame( matrix( 
      mvrnorm( n = 1, mu = mean,  Sigma = covariance ), nrow = 1 ) )
    names( fixed ) <- names
  } else { 
    # generate maxDraws sets and keep the one that is conforms to the `range` code
    # I think that is a good compromise, it generates all the data instead of looping
    # looping would be slower
    fixed <- if(maxDraws == 1) as.data.frame( matrix( mvrnorm( n = 1, mu = mean,  Sigma = covariance ), nrow = 1 ) )
    else data.frame(mvrnorm( n = maxDraws, mu = mean,  Sigma = covariance ) ) 
    names( fixed ) <- names
    range <- parseRangeCode( range )                 
    alright <- try( eval( range, fixed ) )
    
    if( alright %of% "try-error" ) ectdStop( "Error when calling the range code on the fixed effect data" )
    if( !any( alright) ) ectdStop( "Of the $maxDraws draws taken, not 1 set of fixed effects was within the range specified" )
    
    fixed <- fixed[ sample(which(alright), size = 1), , drop = F ]
  }     
  ## repeat the fixed effect 
  fixed <- fixed[ rep(1,length(subjects) ) ,,drop = FALSE]
  names( fixed ) <- names

  ## now deal with the between subject effects
  if( !missing(betNames) ){
    betNames <- parseCharInput( betNames, convertToNumeric = FALSE, checkdup = TRUE, valid = TRUE )
    if( betNames %!allin% names ){
      ectdStop( 
        "Some between subject effects do not have a fixed parameter: " %.nt%
        paste( betNames %wo% names, collapse = ", " ))
    }
    
    
    nBetween <- length( betNames )
    
    betCov   <- parseCovMatrix( betCov, nBetween )
    betMean <- if( missing(betMean) ) {
      rep(0, nBetween)
    } else { 
      parseCharInput( betMean, expected = nBetween, 
        msg = "Wrong length for the between subject effect vector")
    }
    
    if( missing(betRange) ){
      # just generate the data once
      between <- as.data.frame( mvrnorm( nSubjects, mu = betMean, Sigma = betCov ) )
      omitFlag <- rep(0, nSubjects)
    } else {
      betRange <- parseRangeCode( betRange )
      
      # prepare an empty dataset to fill with the "alright" values
      between <- do.call( data.frame, 
        structure( rep(list(rep(0, nSubjects)), nBetween), names = betNames) )    
      
        
        
      nBet <- 0  
      for( i in 1:maxDraws){
        newsets <- as.data.frame( mvrnorm( nSubjects, mu = betMean, Sigma = betCov ) )
        names( newsets ) <- betNames
        alright <- try( eval( betRange, newsets) )
        if( alright %of% "try-error") next
        
        indxs <- which( alright ) # sets that we can keep from newsets
        howManyToAdd <- min( nSubjects - nBet, length( indxs ) ) 
        if( howManyToAdd == 0) next
        between[ nBet + 1:howManyToAdd, ] <- newsets[ indxs[1:howManyToAdd] , ,drop = FALSE] 
        nBet <- nBet + howManyToAdd
        .log( "iteration ", sprintf("%5d", i), ",", sprintf("%6d",nBet), " between subjects (", 
          sprintf( "%6.2f", round(nBet / nSubjects * 100,2) ), "%)\n" )
        if( nBet == nSubjects ) break
      }
      if( nBet == 0 ) ectdStop( paste("After", maxDraws, "attempts, no between subject effects were drawn that are within the range specified" ) )
      if( nBet != nSubjects ) ectdStop( paste("After", maxDraws, "attempts, only", nBet, "of the", nSubjects, "between subject effects required were within the range specified" ))
      
      or <- sample(1:nSubjects)
      omitFlag <- rep( c(0, 1), c(nBet, nSubjects - nBet) )[or]
      between <- between[ or , ]       
    }
    
      
    ## apply the suffix to the between data
    names( between ) <- betNames %.% suffix
    
    switch( errStruc, 
      "a" = {                    # additive ( fixed + between )
        out <- fixed
        for( bn in betNames ){
          out[[ bn ]] <- out[[ bn ]] + between[[ bn %.% suffix ]] 
        }
        out <- .roundIt( out, digits )
      }, 
      "p" = {                    # proportional exp ( fixed + between)
        out <- fixed               
        for( bn in betNames ){
          out[[ bn ]] <- exp(  out[[ bn ]] + between[[ bn %.% suffix ]] ) 
        }      
        out <- .roundIt( out, digits )
      }, 
      "n" = {                    # keep fixed and between
        digits <- parseCharInput( digits, convertToNumeric = TRUE)
        fixed   <- .roundIt( fixed, digits )
        if( any(digits<0) ) ectdStop("`digits` should be positive")
        if( length(digits) == 1 || ncol(fixed) == ncol(between) ) 
          between <- .roundIt( between, digits)
        else {
          between <- .roundIt( between, digits = digits[ names %in% betNames ] )
        }
        out <- data.frame( fixed, between) 
      })
      
      out <- .eval( "data.frame( $idCol = subjects, out, $flagName = omitFlag )" ) 
     
  } else {   # no between
    out <- .roundIt( fixed, digits )
    out <- .eval( "data.frame( $idCol = subjects, out, $flagName = rep(0, nSubjects) )" ) 
    
  }
  rownames(out) <- 1:nSubjects
  out   
  
}
