\name{performAnalysis}
\alias{performAnalysis}

\title{ Runs the analysis code against one dataset } 
\description{
  That function is iteratively called from \code{\link{analyzeRep}} to analyze 
  a replicate dataset at each step defined by the interims.
}
\usage{
performAnalysis(analysisCode, doses, data, software = c("R", "SAS"), infile, 
  subset., dataChanges, seed = .deriveFromMasterSeed(), workingPath = getwd())
}

\arguments{
  \item{analysisCode}{ (Required) Analysis code: An R function, a reference to an external R script, or a reference to an external SAS script }
  \item{doses}{ (Required) Vector of doses for which estimates are expected }
  \item{data}{ (Required) Input dataset on which to perform analysis }
  \item{software}{ (Optional) Software for analysis: "R" or "SAS" (default is "R") }
  \item{infile}{ (Optional) Input file name for external call. Only meaningful in the SAS context.  Required for calls to SAS }
  \item{subset.}{ (Optional) Subsets to be applied to the data before doing any analysis.  By default, no subsets are applied }
  \item{dataChanges}{ (Optional) Changes to be made to the data before analysis. Typically these are changes due to the interim code. See \code{\link{interimAnalysis}}}
  \item{seed}{ (Optional) Random number generation seed.  By default, this is derived from the current random seed }
  \item{workingPath}{ (Optional) Working directory for the analysis.  The default is the current working directory }
}
\details{
  The function tries to perform the analysis contained in the \code{analysisCode} against the \code{data}.  
  The \code{analysisCode} specified is either an external file containing code (SAS and R), or an R function (R only).
  
  If software is set to "SAS" the analysis code is assumed to be a reference to an external SAS script.  The SAS code must accept a singe dataset called work.infile, and create an output dataset called work.outfile.  The work.outfile dataset must be a valid "Micro Evaluation" structure as specified in the help file for \code{\link{checkMicroFormat}}.
  If the software is "R", the analysis code input must be either an R function or an R script.  The R analysis code must also return a valid "Micro Evaluatoin" structure as specified in function \code{\link{checkMicroFormat}} 
}
\value{
The function should output a valid "Micro Evaluation" data structure as specified in the \code{\link{checkMicroFormat}} help file.
}
\author{ Mango Solutions & Pfizer \email{mstoolkit@googlemail.com} }
\seealso{ \code{\link{checkMicroFormat}}}
\examples{\dontrun{
  
  # Emax example
  anaCode <- function(data){
    with( data, {
      uniDoses <- sort( unique(DOSE))
      outDf <- data.frame( DOSE = uniDoses, 
        MEAN = tapply(RESP, DOSE, mean) , 
        SE   = tapply(RESP, DOSE, sd )  )
      outDf$LOWER <- outDf$MEAN - 2 * outDf$SE
      outDf$UPPER <- outDf$MEAN + 2 * outDf$SE
      outDf$N     <- table(DOSE)[ as.character(uniDoses) ]
      outDf 
    }) 
  }
  
  # example data
  exData <- system.file( "Runit", "data", "analyseRep", "ReplicateData", "replicate0001.csv", package = "MSToolkit")
  out <- performAnalysis(anaCode, data = exData, doses = c(0, 5, 25, 50, 100) )
  checkMicroFormat( out )       # Check the format of the return structure

}}


\keyword{ models }

