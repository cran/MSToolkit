\name{checkMicroFormat}
\alias{checkMicroFormat}

\title{Check that an input data structure is a valid "Micro Evaluation" structure} 
\description{\code{checkMicroFormat} checks that a given data frame meets certain criteria and returns either a modified version
of that data frame or an "empty" micro evaluation frame with given dose data and column names.  See the details section below
for more information. 
}

\usage{
checkMicroFormat(doses, dat, doseCol = "DOSE", microColumnNames = c(doseCol, "MEAN", "SE", "LOWER", "UPPER", "N"))
}

\arguments{
  \item{doses}{ (Required) A vector of doses for which the input data should contain results }
  \item{dat}{ (Required) A data frame to check for validity }
  \item{doseCol}{ (Optional) The "Dose" variable name within the supplied data.  "DOSE" by default }
  \item{microColumnNames}{ (Optional) The names of the columns that should exist in the supplied data frame.  By default, the function will look for variables "MEAN", "SE", "LOWER", "UPPER" and "N" in addition to the "dose" variable specified by argument "doseCol" }
}
\details{

\code{checkMicroFormat} will first perform a series of basic checks on \code{dat}.  If any of these fail it will call \code{\link{createEmptyMicro}(doses, doseCol, microColumnNames)} to generate an "empty" micro evaluation data frame for the given dose vector and column names.  

The checks for a valid structure are as follows: 
(1) \code{dat} has at least 1 row
(2) \code{dat} contains all of the columns specified by \code{microColumnNames} 
(3) \code{dat}'s "Dose" column should not have any duplicate entries.

If all of these checks pass, then \code{checkMicroFormat} will construct a new data frame with the following characteristics:
(1) It will have the same column names as \code{dat}, 
(2) It will have all of those rows, and only those rows of dat that have a dose column entry that matches some element of \code{doses}
(3) Its dose column will contain any elements of \code{doses} that are missing from \code{dat}'s \code{DOSE} column by inserting additional rows padded out by NA as necessary

}

\value{A data frame as described above}

\author{ Mango Solutions & Pfizer \email{mstoolkit@googlemail.com} }

\note{ Future versions of MSToolkit will relax the format of the MicroEvaluation dataset allowing a more free structure for this dataset. }

\seealso{\code{\link{createEmptyMicro}}}
\examples{
  
  # Check structure of a return object from createEmptyMicro
  checkMicroFormat(dat = createEmptyMicro(doses = c(10,20,30)),doses=c(10,20))
  
  # Construct example dataset
  x <- data.frame(INTERIM=rep(0, 5), INTERIMC=rep(4, 5), D=c(0, 10, 25, 50, 100), MEAN=1:5, SE=1:5, LOWER=1:5, UPPER=1:5, N=rep(50, 5), DROPPED=rep(0, 5), STOPPED=rep(0, 5)) 
  #  > x
  #  INTERIM INTERIMC   D MEAN SE LOWER UPPER  N DROPPED STOPPED
  #1       0        4   0    1  1     1     1 50       0       0
  #2       0        4  10    2  2     2     2 50       0       0
  #3       0        4  25    3  3     3     3 50       0       0
  #4       0        4  50    4  4     4     4 50       0       0
  #5       0        4 100    5  5     5     5 50       0       0

  # Check structure
  checkMicroFormat(dat = x, doses = c(10, 15, 50, 80), doseCol = "D", microColumnNames = c("INTERIM","INTERIMC","D","MEAN","SE","LOWER" ,"UPPER","N","DROPPED","STOPPED"))

}
\keyword{datagen}

