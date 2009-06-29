checkMicroFormat <- function (
  doses,                # A vector of doses
  dat,                  # A data frame which will be checked for the correct micro evaluation data format
  doseCol = "DOSE",
  microColumnNames =  c( doseCol, "MEAN", "SE", "LOWER", "UPPER", "N")   # The column names that every micro evaluation data frame should contain.   
) {
  ###############################################################################
  # Mango Solutions, Chippenham SN14 0SQ 2006
  # checkMicroFormat.R Wed Jun 27 14:54:17 BST 2007
  # Author: Francisco
  ###############################################################################
  # DESCRIPTION: Checks whether or not a given data frame (dat) is of the correct format for a micro evaluation
  # data entry.  If it is not, it either augments the input in certain ways or it creates an empty data frame whose 
  # DOSE column is given by doses.  The input data frame will also be augmented and/or reduced so that its dose column ultimately
  # matches doses.  Any rows which must be added will have NA entries for all of its columns other than DOSE
  # KEYWORDS:  
  # Documented in Support Functions Design Specification 
  ###############################################################################

  doseCol <- parseCharInput( doseCol, convertToNumeric = FALSE, expected = 1, valid = TRUE )
  
  if(!is.data.frame(dat))
    ectdStop("Input is not a data frame")
  doses <- parseCharInput(doses, convertToNumeric = FALSE, checkdup = TRUE, sort = TRUE)
  if(length(doses) == 0)
    ectdStop("Doses vector is empty")
    
  # If the data frame doesn't have enough rows or columns create a new one with 
  # createEmptyMicro
  if(nrow(dat) == 0 || ncol(dat) < length(microColumnNames))
    return(createEmptyMicro(doses, doseCol, microColumnNames))
  
  # convert everything to upper case to avoid problems with SAS
  colTest <- casefold(microColumnNames) %!allin% casefold(names(dat))  
  
  # check that all of the necessary column names appear in dat.  
  # If not use createEmptyMicro to create a new micro evaluation df
  if( colTest ) 
      # Try a case-insensitive check if the above doesn't hold, since case-sensitive checks seem to cause problems for SAS
    return(createEmptyMicro(doses, doseCol, microColumnNames))
  names(dat)[casefold(names(dat)) == casefold(doseCol)] <- doseCol    
  if(any(duplicated(dat[doseCol])))
    return(createEmptyMicro(doses, doseCol, microColumnNames))
  
  # We've passed all of the preliminary input checks.  Now create a data frame derived from those rows of dat that match
  # the elements of doses
  # First find which column is the DOSE column
  doseIndex <- match(doseCol,names(dat))
  doseIndex <- doseIndex[!is.na(doseIndex)] 
  #Now find which elements of doses occur in the DOSE column of dat (and their indices)
  x <- match(doses, dat[,doseIndex]) 
  matchVec <- x[!is.na(x)]     
  
  if(length(matchVec) == 0)
    return(createEmptyMicro(doses, doseCol, microColumnNames))

  
  # Initialize the return value to the matching rows
  res <- dat[matchVec,]                                   
  # print(res)

  # Now pad res so that all of the entries from the doses vector appear.  First find
  # which ones don't appear        
  x <- match(doses, res[,doseIndex])
  toAdd <- doses[is.na(x)]

  # If there aren't any to add, do nothing
  if(length(toAdd) == 0)
    return(res)
  
  #else create one row for each missing value, with the non-dose columns initialized to NA
  #extraRows <- as.data.frame(cbind(toAdd, matrix(nrow = length(toAdd), ncol = length(res)-1)))
  extraRows <- as.data.frame(matrix(nrow = length(toAdd), ncol = length(res)))
 
  colnames(extraRows) <- names(res)
  extraRows[doseCol] <- toAdd  
  # print(extraRows)
  
  # Add in the missing dose rows and then sort the rows so that the DOSE values appear in descending order
  res <- rbind(res, extraRows)
  or <- order(res[,doseIndex])
  res <- res[or,]
  # hack to repair the row labels since the extraction/merging process breaks them
  rownames(res) <- 1:nrow(res)
  res

}