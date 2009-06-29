\name{removeDirectories}
\alias{removeDirectories}

\title{Remove directories} 
\description{
This function removes directories containing data for the current scenario. Optionally called at the start of analyzeData in order to overwrite ReplicateData, MicroData and MacroData datasets with the new results.
}

\usage{
removeDirectories(dirNames = c("ReplicateData", "MicroEvaluation", "MacroEvaluation"), workingPath = getwd())
}

\arguments{
  \item{dirNames}{ (Optional) A vector of directories to remove from the "workingPath" directory.  Default is to remove "ReplicateData", "MicroEvaluation" and "MacroEvaluation" directories }
  \item{workingPath}{ (Optional) The working directory from which to remove the directories.  By default, directories are removed from the current working directory }
}

\details{
  \code{dirNames} must be a character vector.  The function will attempt to remove each of the directories identified in the \code{dirNames} vector from within the \code{workingPath} directory
}  
\value{
  A logical vector the same length as the \code{dirNames}, indicating whether or not the corresponding directories were successfully removed
}

\author{ Mango Solutions & Pfizer \email{mstoolkit@googlemail.com} }
\examples{
  \dontrun{
    createDirectories(dirNames = c("ReplicateData", "MicroEvaluation"))
    removeDirectories(dirNames = c("ReplicateData", "MicroEvaluation"))
  }
}
\keyword{data}
