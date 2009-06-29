\name{analyzeRep}
\alias{analyzeRep}

\title{ Analyse a single replicate of data } 
\description{
  The \code{analyzeRep} function analyzes a single replicate of data, possibly at different interim levels.
}
\usage{
  analyzeRep(analysisCode, replicate, removeMissing = TRUE, removeParOmit = TRUE, 
  removeRespOmit = TRUE, interimCode, software = c("R", "SAS"), seed = .deriveFromMasterSeed(), 
  parOmitFlag = "PAROMIT", respOmitFlag = "RESPOMIT", missingFlag = "MISSING", interimCol = "INTERIM", doseCol = "DOSE", 
  infile = .dataGetFullPath( replicate, dataType="Replicate", workingPath = workingPath), workingPath = getwd())
}

\arguments{
  \item{analysisCode}{ (Required) File containing analysis code (for R or SAS) or an R function for analysis (R only) }
  \item{replicate}{ (Required) Replicate number of data to be analyzed }
  \item{removeMissing}{ (Optional) Should rows marked as 'Missing' during the data generation step be removed from the data before analysis is performed?  TRUE by default }
  \item{removeParOmit}{ (Optional) Should any rows marked as 'Omitted' during the parameter data generation step (ie. parameters out of range) be removed from the data before analysis is performed?  TRUE by default }
  \item{removeRespOmit}{ (Optional) Should any rows marked as 'Omitted' during the response generation step (ie. responses out of range) be removed from the data before analysis is performed?  TRUE by default }
  \item{interimCode}{ (Optional) An R function to be applied to interim datasets in order to make interim decisions.  See the help file for the \code{\link{interimAnalysis}} function for more information. }
  \item{software}{ (Optional) The software to be used for analysis: either "R" or "SAS".  "R" is the default software used }
  \item{seed}{ (Optional) Random number seed to use for the analysis.  Based on the current random seed by default }
  \item{parOmitFlag}{ (Optional) Parameter omit flag name.  "PAROMIT" by default }
  \item{respOmitFlag}{ (Optional) Response omit flag name.  "RESPOMIT" by default }
  \item{missingFlag}{ (Optional) Missing flag name.  "MISSING" by default }
  \item{interimCol}{ (Optional) Interim variable name.  "INTERIM" by default }
  \item{doseCol}{ (Optional) Dose variable name.  "DOSE" by default }
  \item{infile}{ (Optional) Data file to use in the analysis, automatically populated using the replicate number.  Derived from the "replicate" number and working path by default }
  \item{workingPath}{ (Optional) Root directory in which replicate data is stored, and in which we should perform the analysis.  Current working directory is used by default }
}
\details{
  The \code{\link{analyzeRep}} function calls the \code{\link{performAnalysis}} function in order to analyze and summarize a single simulated replicate dataset (held in the "ReplicateData" subdirectory of the specified working path).

  The first step of the analysis is to use the \code{removeMissing}, \code{removeParOmit} and \code{removeRespOmit} flags (together with the \code{missingFlag}, \code{parOmitFlag} and \code{respOmitFlag} inputs) in order to subset the data if required.  For example, we may wish to remove all observations flagged as "missing" in an earlier simulation of subject dropout.  The subset is applied to the data before the analysis.

  The analysis code must be either an R function, a reference to an external R script, or a reference to an external SAS script.  If the \code{software} is set as "SAS", it is assumed that the \code{analysisCode} file is an external SAS script.
  If the analysis code is a SAS script, it must accept a singe dataset called work.infile, and create an output dataset called work.outfile.  The work.outfile dataset must be a valid "Micro Evaluation" structure as specified in the help file for \code{\link{checkMicroFormat}}.    
  If the \code{software} is "R", the analysis code input must be either an R function or an R script.  The R analysis code must also return a valid "Micro Evaluatoin" structure as specified in function \code{\link{checkMicroFormat}} 

  The first step in \code{\link{analyzeRep}} is to perform a full analysis on the data (which has possibly been subset by the \code{remove...} inputs).  Following the analysis, the \code{\link{checkMicroFormat}} function is used to ensure the return data is a valid "Micro Evaluation" data structure.
  The return structure is appended with drop and stop flags (set to 0) and with interim variables (where interim is "FULL").
  
  If interim analysis is to be performed using \code{interimCode} then values for \code{interimSubj} must be specified in \code{\link{generateData}}. If interim proportions have been specified in \code{interimSubj} then analysis is performed across the dataset as a whole, then across each interim dataset in turn. At each interim \code{interimCode} is called to apply decision-making e.g. dropping doses, stopping the study. If any doses are dropped then existing data from these doses are carried forward into subsequent interim analysis, however no new subjects on these doses are carried forward into future datasets.
  
  Finally, all micro evaluation outputs (with appended interim variables and drop/stop flags) are combined and returned.  
}
\value{
  A "Micro Evaluation" structure with additional variables (interim column, drop flag and stop flag)
}

\author{ Mango Solutions & Pfizer \email{mstoolkit@googlemail.com} }

\note{  
  There are some restrictions on the code inputs to the \code{\link{analyzeData}} function.  These restrictions are discussed here:
  
  Analysis Code: The \code{analysisCode} input must be either an R function or a reference to an external file.  If it is a reference to external file, it must contain either SAS code (if \code{software} is "SAS") or R code (if \code{software} is "R").
    If the code is an R function, or an external R script, it must accept a data frame as its only argument and return an acceptable "Micro Evaluation" data frame as set out in \code{\link{checkMicroFormat}}.
    If the code is an external SAS script, it must accept use a SAS dataset called "work.infile" and create a SAS dataset called "work.outfile" that conforms to the "Micro Evalutation" format as set out in \code{\link{checkMicroFormat}}.
    More information on "Micro Evaluation" structures can be found in the help file for function \code{\link{checkMicroFormat}}.
  
  Interim Code: The \code{interimCode} input must be an R function that accepts a single "Micro Evaluation" data input, and returns an R "list" structure that is either empty or contains one or more of the following elements:
    An element called "STOP" which is a logical vector of length 1.  This tells the \code{\link{analyzeData}} function whether the analysis should be halted at this interim
    An element called "DROP" which is a vector of numeric values relating to doses in the data to drop before the next interim is analyzed.
    More information on "Micro Evaluation" structures can be found in the help file for function \code{\link{interimAnalysis}}.

}

\seealso{ 
  \code{\link{performAnalysis}} is called by \code{analyzeRep} to perform
  each analysis on the subset of data. \code{\link{interimAnalysis}} executes the 
  \code{interimCode} and updates the data changes accordibgly.
  
  \code{\link{analyzeData}} calls \code{analyzeRep} sequentially.
}
\examples{
\dontrun{  
  # Analysis Code
  emaxFun <- function(data){
    library(DoseResponse)
    with( data, {
      uniDoses <- sort( unique(D))                                                                    
      eFit <- emaxalt( RESP, D )
      outDf <- data.frame( D = uniDoses, 
        MEAN = eFit$dm[as.character(uniDoses)], 
        SE = eFit$dsd[as.character(uniDoses)] )
      outDf$LOWER <- outDf$MEAN - 2 * outDf$SE
      outDf$UPPER <- outDf$MEAN + 2 * outDf$SE
      outDf$N     <- table(DOSE)[ as.character(uniDoses) ]
      outDf 
     }) 
   }                                                                                                                   
  
  analyzeRep(replicate = 1, analysisCode = emaxFun)
}
}

\keyword{ datagen }
\keyword{ IO }
\keyword{ models }


