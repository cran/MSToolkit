\name{createTreatments}
\alias{createTreatments}

\title{ Create Treatment Design Data } 
\description{
  Create treatment group for Parallel or Crossover design
}
\usage{
createTreatments(doses, times = NULL, type = "Parallel", sequence, 
  doseCol = "DOSE", timeCol = "TIME", trtCol = "TRT")
}

\arguments{
  \item{doses}{ (Required) Vector of doses to use.  Alternatively can be a comma separated string of numbers }
  \item{times}{ (Optional) Vector of time points for dosing.  No time element by default }
  \item{type}{ (Optional) Type of dosing regime to create: "Parallel" or "Crossover".  "Parallel" by default.  See "details" section.  }
  \item{sequence}{ (Optional) Crossover sequence matrix.  By default, no crossover is performed.  See "details" section. }
  \item{doseCol}{ (Optional) Dose variable name to create ("DOSE" by default) }
  \item{timeCol}{ (Optional) Time variable name to create ("TIME" by default) }
  \item{trtCol}{ (Optional) Treatment variable name to create ("TRT" by default) }
}
\details{
  \code{createTreatments} is used to specify the possible treatments available for the design. By default it assumes a "Parallel" group design but crossover designs with multiple sequences are possible by specifying \code{type} as "Crossover" and giving a matrix of possible sequences with each column specifying a treatment sequence.  It is also possible to define repeated measures designs by specifying values for \code{times}. If negative values are given in \code{times} then DOSE will automatically be set to zero (placebo) in order to simulate placebo run-in designs. \emph{TIME=0} is viewed as immediately post-dose and so is included with the appropriate DOSE value for each treatment arm.

  MSToolkit was design with early phase trials in mind where we wish to simulate and compare several doses. However it is possible to view "DOSE" as a label and work with TRT or DOSE as an arbitrary variable - allowing for simulation and evaluation of designs with active comparators, no placebo arm (e.g. bioequivalence), or non-clinical designs, changing the \code{doseCol}, \code{timeCol} and \code{trtCol} to appropriate useful labels e.g. FACTOR, LITTER, TEMPERATURE, PRESSURE, etc.

  The function will first check for the required inputs.  If \code{type} is "Parallel", then doses must be provided.  If \code{type} is "Crossover", the \code{sequence} matrix must be provided.
  
  If the \code{type} is "Parallel" and there is are no specified \code{times}, the function will create a data frame with Treatment set to "1 to number of doses" and a Dose variable with the doses specified.
  If the \code{type} is "Parallel" and there is a specified \code{times} vector, the function will create a data frame with a parallel treatment regime for each dose specified.

  If the \code{type} is "Crossover", and there is are no specified \code{times} input, the \code{times} input will be set to "1 to number of rows of the sequence matrix"
  If \code{times} has been supplied, and has leading non-positive elements, the \code{sequence} matrix is appended to a set of run-in measurements (where dose is set to 0).
  Based on the \code{times} and \code{sequence} matrix, a data frame is created by aligning each column of the matrix with the times specified.
}
\value{
  A data frame containing a treatment, dose and (optionally) a time variable
}

\author{ Mango Solutions & Pfizer \email{mstoolkit@googlemail.com} }
\seealso{ \code{\link{allocateTreatments}} }
\examples{

createTreatments(doses=c(0, 15, 30))
#  TRT DOSE
#1   1    0
#2   2   15
#3   3   30
createTreatments(doses=c(0, 15, 30), times=0:2)
#  TRT TIME DOSE
#1   1    0    0
#2   1    1    0
#3   1    2    0
#4   2    0   15
#5   2    1   15
#6   2    2   15
#7   3    0   30
#8   3    1   30
#9   3    2   30

createTreatments(sequence=cbind(c(0, 15, 30), c(15, 30, 0), c(30, 0, 15)))
#  TRT TIME DOSE
#1   1    1    0
#2   1    2   15
#3   1    3   30
#4   2    1   15
#5   2    2   30
#6   2    3    0
#7   3    1   30
#8   3    2    0
#9   3    3   15

createTreatments(sequence=cbind(c(0,0, 15, 30), c(0,15, 30, 0), c(0,30, 0, 15)), times=c(-1,1,2,3))
#   TRT TIME DOSE
#1    1   -1    0
#2    1    1    0
#3    1    2   15
#4    1    3   30
#5    2   -1    0
#6    2    1   15
#7    2    2   30
#8    2    3    0
#9    3   -1    0
#10   3    1   30
#11   3    2    0
#12   3    3   15

}


\keyword{ datagen }

