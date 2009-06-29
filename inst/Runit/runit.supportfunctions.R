
if( !exists("unitTestPath")) unitTestPath <- "."
testdata.supportfunctions.dir <- file.path( unitTestPath, "testdata.supportfunctions" )

.data.frame.compare <- function(
  frame1, 
  frame2
)
{
  ###############################################################################
  # Mango Solutions, Chippenham SN14 0SQ 2006
  # Sunday July 1 2007
  # Author: Francisco
  ###############################################################################
  # DESCRIPTION: Compares two data frames that may have NA or NULL entries
  # KEYWORDS: 
  ###############################################################################
  
  if(any(!is.data.frame(frame1) || !is.data.frame(frame2)))
    stop("Non-data frame input")
  if(dim(frame1) != dim(frame2))
    return(FALSE)
  if(names(frame1) != names(frame2))
    return(FALSE)
  #if(length((!is.na(frame1) & !is.null(frame1))) != length(!is.na(frame2) & !is.null(frame2)))
  #  return(FALSE)                          

  # Condition 1: The non-NULL and non-NA entries of both data frames must be equal
  cond1 <- all(frame1[(!is.na(frame1) & !is.null(frame1))] == frame2[(!is.na(frame2) & !is.null(frame2))])
  # condition 2: The NA entries of both data frames must occur in the same places
  cond2 <- all(is.na(frame1) == is.na(frame2))
  # condition 3: The NULL entries of both data frames must occur in the same places
  cond3 <- all(is.null(frame1) == is.null(frame2))
  
  cond1 && cond2 && cond3
  
} 


# Author: Francisco
 # Date: Jun 26 2007


 test.checkSymmetricPDMatrix <- function()
 {
  
  # check matrix filled with NA values
  testMatrix <- matrix(ncol=2, nrow=2)
  checkException(checkSymmetricPDMatrix(testMatrix))
  # testMatrix <- diag(100)
  #checkException(checkSymmetricPDMatrix(testMatrix))
  
  # Check negative-definite matrix
  checkException(checkSymmetricPDMatrix(-1 * testMatrix))
  testMatrix[1,2] <- 0.1
  checkException(checkSymmetricPDMatrix(testMatrix))
  
  #Check for handling of non-square matrices
  checkException(checkSymmetricPDMatrix(matrix(1, ncol =2, nrow = 3)))
  # Check 0 matrix
  #< DOES NOT FAIL ANYMORE SINCE VERSION 0.5: testMatrix <- matrix(0, ncol = 2, nrow = 2)
  #< DOES NOT FAIL ANYMORE SINCE VERSION 0.5: checkException(checkSymmetricPDMatrix(testMatrix))
  #testMatrix <- matrix(1, ncol = 1, nrow = 1)
  #checkException(checkSymmetricPDMatrix(testMatrix))
  
 }

 # Author: Francisco
 # Date: Jun 26 2007

 test.initialChar <- function()
 {
  #cat("Testing initialChar... \n")
  checkEquals(initialChar("23524A"), "a")
  #<T> No letters present, should generate exception
  checkException(initialChar("123"))
  #<T> First letter is a lower case "b"
  checkEquals(initialChar("123b456d"), "b")
  checkException(initialChar("123a456b", adm = "BCD"))
  checkEquals(initialChar("34Zy", adm="werZ"), "z")
  checkException(initialChar(""))
 }
#  Author: Francisco
#  Date: Jun 27 2007
 
 test.createEmptyMicro <- function()
 {
  checkException(createEmptyMicro()) 
  checkException(createEmptyMicro(doses=NULL))
  checkException(createEmptyMicro(doses = c()))
  checkException(createEmptyMicro(doses = c(1, "1")), msg = "Duplicate doses are not allowed")
  #<T> Duplicate doses are not allowed
  checkException(createEmptyMicro(doses = c(1, 1, 2)), msg = "Duplicate doses are not allowed in the doses vector")
  checkException(createEmptyMicro(doses = c(1, 2, 3), doseCol = "DOSE", microColumnNames = c("DOSES", "MEAN", "SE", "UPPER", "LOWER"))
    , msg = "Dose column is not found in the column names")
  testd <- read.csv(file.path(testdata.supportfunctions.dir,"micro0004.csv"))
  checkTrue(.data.frame.compare(testd, createEmptyMicro(doses = c(10, 20, 30, 40))), msg = "These two frames should be equal")
  testd <- read.csv(file.path(testdata.supportfunctions.dir,"micro0005.csv"))
  checkTrue(.data.frame.compare(testd, createEmptyMicro(doses = "10, 20, 30, 40", doseCol = "D", microColumnNames = c("D", "MEAN", 
  "LOWER", "UPPER"))), msg  = "These two frames should be equal")
 
 }                                                        
 
 test.ectdStop <- function()
 {
  checkException(ectdStop("test"))
  # checkException(etcdStop(NULL))
 }
 
 test.ectdWarning <- function()
 {
  # checkException(ectdWarning(NULL))
 }
 
 test.parseCharInput <- function()
 {
  checkEquals(parseCharInput("100,200,300"), c(100, 200, 300))
  checkException(parseCharInput("1", expectedLength = 2))
  checkEquals(parseCharInput("1, 2, 3", convertToNumeric = FALSE), c("1", "2", "3"))
  
  checkException(parseCharInput("1,1", checkdup = TRUE))
  checkException(parseCharInput("1,1", expectedLength = 3))
  checkException(parseCharInput("1,4", checkProb = TRUE))
  
 }

# Author: Romain
test.parseCovMatrix <- function(){
  
  # wrong dimensions
  checkException( parseCovMatrix("1,1,1,2", 2) )
  
  # not PD
  checkException( parseCovMatrix("0,1,0", 2) )
  
  mat <- parseCovMatrix("1,0,1,.5,0,1", 3)
  mat2 <- rbind( c(1 , 0, .5), 
                 c(0 , 1, 0 ), 
                 c(.5, 0, 1 ) )
  
  checkTrue( all(mat == t(mat)) )
  checkEquals( mat, mat2 , 
    msg = "Testing that the matrix is generated a la NONMEM")
}

test.parseRangeCode <- function()
{
  checkEquals(deparse(parseRangeCode("x<4")), "expression((x < 4))")
  expr <- deparse(parseRangeCode(c("2 <= 10 >= x", "x >= x", "z < 4 < 1")))

  # Concatenate any output seperated by deparse and remove all whitespace
  expr <- gsub( "[[:space:]]", "", paste(expr, collapse="") )
  print(expr)
  checkEquals(expr, "expression((2<=10)&(10>=x)&(x>=x)&(z<4)&(4<1))")
  checkException(parseRangeCode(c("x < 4", "(2 > z")))
  checkException(parseRangeCode(c("1 > z >= 4", "y")))

  expr <- deparse(parseRangeCode(c("1 >=               x         <             z", "1         <                2")))
  expr <- gsub(  "[[:space:]]", "", paste(expr, collapse="") )
  checkEquals(expr, "expression((1>=x)&(x<z)&(1<2))")
}

test.parseHashString <- function()
{
  checkException(parseHashString("2#A"))
  checkException(parseHashString("1,2,3#2,;,4"))
  checkException(parseHashString(parseHashString("1,2,3,4#5,,6"), msg = "Two consecutive commas are not allowed"))
  checkEquals(list(c(1,2,3)), parseHashString("1,2,3"))
  checkEquals(list(c(0), c(5,5,5,5,5,5,5,5), c(4)), parseHashString("0#5,5,5,5,5,5,5,5#4"))
  checkException(parseHashString())
  #checkEquals(parseHashString("1,2#3,4#5,6"), c(1,2,3,4,5,6))                      
}

test.validNames <- function()
{
  checkException(validNames(".2"), msg = "Can't follow initial period with a number")
  checkException(validNames("foo", "bar", "5ar"), "Variable names cannot start with a number")
  checkException(validNames("barof$oap"), "$ is not allowed in a variable name")
}

# Author: Francisco
# Date: July 13

test.checkMicroFormat <- function()
{
  # load some test data
  x1 <- read.csv(paste(testdata.supportfunctions.dir, "micro0001.csv", sep="/"))
  # x2 <- read.csv(paste(testdata.supportfunctions.dir, "testdata.createEmptyMicro1.csv", sep="/"))
  
  checkTrue(identical(createEmptyMicro(doses = c(11, 20, 30, 40)), checkMicroFormat(dat = x1, doses = c(11, 20, 30, 40))),
  msg = "doses is not a subset of the dose vector in x1")
  checkTrue(.data.frame.compare(x1, checkMicroFormat(dat = x1, doses = c(0, 10, 25, 50, 100))), 
  msg = "All of the doses are contained in x1, and so are the default column names")
  checkTrue(.data.frame.compare(createEmptyMicro(doses = c(0, 10, 25, 50, 100), microColumnNames = c("DOSE", "FOO")), 
  checkMicroFormat(dat = x1, doses = c(0, 10, 25, 50, 100) , doseCol = "DOSE", microColumnNames = c("DOSE", "FOO"))), 
  msg = "FOO is not a column name in x1")
  x1 <- read.csv(paste(testdata.supportfunctions.dir, "micro0002.csv", sep="/"))
  checkTrue(.data.frame.compare(x1,checkMicroFormat(dat = x1, doses = c(25, 50, 100) , doseCol = "DOSE",
   microColumnNames = c("INTERIM","INTERIMC","DOSE","MEAN","SE","LOWER","UPPER","N","DROPPED","STOPPED","K"))), msg = "")
  x1 <- read.csv(paste(testdata.supportfunctions.dir, "micro0001.csv", sep="/"))                                             
  x2 <- read.csv(paste(testdata.supportfunctions.dir, "micro0003.csv", sep="/"))
  checkMicroFormat(x1, doses = c(5, 15, 25,50,100))
  checkTrue(.data.frame.compare(x2, checkMicroFormat(x1, doses = c(5, 15, 25,50,100))))
}                      