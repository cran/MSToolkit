\name{createCovariates}
\alias{createCovariates}
\title{ Create covariates }
\description{
  Function to create covariates. Discrete, continuous and/or from an external
  file. This function acts as a wrapper for the functions : 
  \code{\link{createContinuousCovariates}}, \code{\link{createDiscreteCovariates}} and
  \code{\link{createExternalCovariates}}
}
\usage{
createCovariates(subjects, conNames = NULL, conMean, conCov, conRange = NULL, conDigits, 
  conMaxDraws = 100, disNames = NULL, disValues, disProbs, disProbArray, extNames = NULL,
  extFile, extSubset, extRefCol, extSameRow = TRUE, extDataId = idCol, workingPath = getwd(), idCol = "SUBJ", 
  seed = .deriveFromMasterSeed())
}
\arguments{
  \item{subjects}{ (Required) Subjects for which to create covariates }
  \item{conNames,conMean,conCov,conRange,conDigits,conMaxDraws}{ (Optional) Arguments for the \code{\link{createContinuousCovariates}} function.  The \code{\link{createContinuousCovariates}} function is not called if these arguments are not provided }
  \item{disNames,disValues,disProbs,disProbArray}{ (Optional) Arguments for the \code{\link{createDiscreteCovariates}} function.  The \code{\link{createDiscreteCovariates}} function is not called if these arguments are not provided }
  \item{extNames,extFile,extSubset,extRefCol,extSameRow,extDataId}{ (Optional) Arguments for the \code{\link{createExternalCovariates}} function.  The \code{\link{createExternalCovariates}} function is not called if these arguments are not provided }
  \item{workingPath}{ (Optional) Working directory from which to import covariate data.  By default, the current working directory is used }
  \item{idCol}{ (Optional) Name of the subject column. Must be a valid R name (See \code{\link{validNames}}) and not be found in anyone of \code{conNames}, \code{extNames} or \code{disNames}.  "SUBJ" is used by default }
  \item{seed}{ (Optional) Random seed to use by all the lower-level functions.  By default, this is derived from the current random seed }
}

\value{
  Data frame merging the results from the lower-level functions :\code{\link{createContinuousCovariates}}, \code{\link{createDiscreteCovariates}}
   and \code{\link{createExternalCovariates}}
}
\details{
  According to the presence of each \code{names} argument, the function calls lower-level functions to generate covariates.
  Foe example, if \code{conNames} is given, the function will attempt to generate covariates from
  a continuous distribution using the \code{\link{createContinuousCovariates}} function.
  
  If no names is given, a data frame containing only the subject column will be created.
  
  Arguments are systematically passed to lower-level function according to a name convention. 
  For example, the \code{conRange} argument is passed to \code{\link{createContinuousCovariates}} as the \code{range} argument, 
  the \code{extFile} is passed to the \code{\link{createExternalCovariates}} as the \code{file} argument, ...
}
\section{Warning}{
  The function will generate an error if there is any duplicated names between 
  \code{conNames}, \code{extNames}, \code{disNames} and \code{idCol}.
}

\author{ Mango Solutions & Pfizer \email{mstoolkit@googlemail.com} }
\seealso{ 
  \code{\link{createContinuousCovariates}}, 
  \code{\link{createDiscreteCovariates}} and
  \code{\link{createExternalCovariates}}  for details about each source of covariate and examples.}
\examples{

  ## unit tests for the covariates component of the MSToolkit package 
  \dontrun{
    file.show( system.file( "Runit", "runit.data.covariates.R", package = "MSToolkit") )
  
    wPath <- system.file( "Runit", "data", "createCovariates", package = "MSToolkit") 
    dAll <- createCovariates( 30, 
      conNames = "X,Y", conMean = "0,0" , conCov = "1,0,1", conRange = "-1<X<1",    # continuous
      disNames = "P1,P2", disValues = "1,2#3,5,6" , disProbs = ".5,.5#.3,.3,.4",    # discrete
      extNames = "X1", extFile = "testCovariates.csv", workingPath = wPath )        # external  
 }
}
\keyword{ datagen }
\keyword{ IO }


