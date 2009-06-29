allocateTreatments <- function( 
 trts,                 #@    Number of treatments to which to allocate subjects
 subjects,             #@    Number of subjects to which to allocate treatments, or a vector of allocations
 prop = NULL,          #@    Proportions for sampling
 ordered = FALSE,      #@    Logical Flag, should allocations be assigned in order
 seed = .deriveFromMasterSeed( ), #@ Random seed to allocate treatments
 idCol = "SUBJ",         #@     Subject variable name
 trtCol = "TRT"        #@     Treatment variable name
) {
  ################################################################################
  # � Mango Solutions, Chippenham SN14 0SQ 2006
  # allocateTreatments.R Fri Jun 01 12:18:52 BST 2007 @513 /Internet Time/
  #
  # Author: Romain
  ###############################################################################
  # DESCRIPTION: allocate treatments to subjects
  # KEYWORDS: datagen, component:data:allocate
  ##############################################################################
  
  set.seed(seed)
  validNames( idCol, trtCol )  
  if(idCol == trtCol){
    ectdStop("`idCol` and `trtCol` should be different") 
  }
  subjects <- parseCharInput( subjects )
  trts     <- parseCharInput( trts ) 
  prop     <- parseCharInput( prop )
  if( is.null(prop) ) {
    prop <- rep( 1/trts, trts)
  }        
  if( sum(prop) != 1 ){
    ectdStop( "`prop` does not sum up to one")
  } 
  if( length(prop) != trts){
    ectdStop( "`prop` should have the same length as the number of treatments: $trts" )    
  }
                                        
  if( length(trts) > 1 ){
    trts <- trts[1]
    ectdWarning( "Only the first number in the treatment vector ($trts) will be used" )
  }
  if( any(subjects < 0)){
    ectdStop( "Negative value in `subjects`")
  }      
  if( length(subjects) != 1 && length( subjects ) != trts ){
    ectdStop( "`subjects` must have the same length as the number of treatments: $trts" )
  }
  
  
  nSubjects <- sum( subjects )
  alloc <- if( length(subjects) == 1 ){
    sample( 1:trts, replace=TRUE, size = subjects, prob = prop) 
  } else {
    rep( 1:trts, subjects )
  }
  if( 1:trts %!allin% unique(alloc)  ) {
    ectdWarning( "Not all the treatments have been allocated")
  }
  if( ordered && is.unsorted(alloc) ) alloc <- sort( alloc )
  if( !ordered ) alloc <- sample( alloc )
     
  .eval(" data.frame( $idCol=1:sum(subjects), $trtCol=alloc ) ")
  
}

