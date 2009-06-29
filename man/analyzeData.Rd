\name{analyzeData}
\alias{analyzeData}

\title{ Analyze simulated data replicates } 
\description{
  Analyzes a set of simulated trial data, possibly including interim analyses
}
\usage{
analyzeData(replicates = "*", analysisCode, macroCode, interimCode = NULL, 
  software = "R", grid = TRUE, waitAndCombine = TRUE, cleanUp = FALSE, 
  removeMissing = TRUE, removeParOmit = TRUE, removeRespOmit = TRUE, 
  seed = .deriveFromMasterSeed(), parOmitFlag = "PAROMIT", 
  respOmitFlag = "RESPOMIT", missingFlag = "MISSING", interimCol = "INTERIM",
  doseCol = "DOSE", sleepTime = 15, deleteCurrData = TRUE, workingPath = getwd())
}

\arguments{
  \item{replicates}{ (Optional) Vector of replicates on which to perform analysis: all replicates are analyzed by default}
  \item{analysisCode}{ (Required) File containing analysis code (for R or SAS) or an R function for analysis (R only)}
  \item{macroCode}{ (Required) An R function to be used for macro evaluation of the result datasets.  See the help file for the \code{\link{macroEvaluation}} function for more information }
  \item{interimCode}{ (Optional) An R function to be applied to interim datasets in order to make interim decisions.  See the help file for the \code{\link{interimAnalysis}} function for more information. }
  \item{software}{ (Optional) The software to be used for analysis: either "R" or "SAS".  "R" is the default software used }
  \item{grid}{ (Optional) If available, should the analysis be split across an LSF GRID.  TRUE by default }
  \item{waitAndCombine}{ (Optional) (For GRID running) Should the process wait for all analyses to finish, then combine into micro and macro summary files?  TRUE by default}
  \item{cleanUp}{ (Optional) Should micro/macro directories and "Rlsf*" files be removed on completion?  TRUE by default}
  \item{removeMissing}{ (Optional) Should rows marked as 'Missing' during the data generation step be removed from the data before analysis is performed?  TRUE by default}
  \item{removeParOmit}{ (Optional) Should any rows marked as 'Omitted' during the parameter data generation step (ie. parameters out of range) be removed from the data before analysis is performed?  TRUE by default}
  \item{removeRespOmit}{ (Optional) Should any rows marked as 'Omitted' during the response generation step (ie. responses out of range) be removed from the data before analysis is performed?  TRUE by default}
  \item{seed}{ (Optional) Random number seed to use for the analysis.  Based on the current random seed by default }
  \item{parOmitFlag}{ (Optional) Parameter omit flag name.  "PAROMIT" by default }
  \item{respOmitFlag}{ (Optional) Response omit flag name.  "RESPOMIT" by default }
  \item{missingFlag}{ (Optional) Missing flag name.  "MISSING" by default }
  \item{interimCol}{ (Optional) Interim variable name.  "INTERIM" by default }
  \item{doseCol}{ (Optional) Dose variable name.  "DOSE" by default }
  \item{sleepTime}{ (Optional) Number of seconds to sleep between iterative checks for GRID job completion.  15 seconds are used by default}
  \item{deleteCurrData}{ (Optional) Should any existing micro evaluation and macro evaluation data be removed before new analysis is performed?  TRUE by default }
  \item{workingPath}{ (Optional) Root directory in which replicate data is stored, and in which we should perform the analysis.  Current working directory is used by default }
}
\details{
          
The first task of the function will be to check the options specifed:
* If the LSF GRID network is unavailable or if the length of the \code{replicates} input is 1, the \code{grid} flag will be set to FALSE
* If the \code{grid} flag is TRUE, the call to \code{\link{analyzeData}} will be split across multiple processors using the "Rlsf" library 
* If the length of the \code{replicates} vector is 1, the \code{waitAndCombine} flag will be set to FALSE
* If the \code{waitAndCombine} flag is set to FALSE, the "cleanUp" flag will also be set to FALSE

The \code{\link{analyzeData}} function will perform analysis across each replicate specified in the \code{replicates} vector.  For each replicate, the function will first call the \code{\link{analyzeRep}} with the required inputs.
The output from the call to \code{\link{analyzeRep}} will be a data frame containing micro evaluation data.  This data frame will be checked to ensure it is of the correct format.
If the return from \code{\link{analyzeRep}} is a valid "Micro Evaluation" dataset, it will be saved to the "MicroEvaluation" folder, and also passed to the \code{\link{macroEvaluation}} function for further analysis.
If the return from \code{\link{macroEvaluation}} is a valid "Macro Evaluation" dataset, it will be saved to the "MicroEvaluation" folder.

If interim analysis is to be performed using \code{interimCode} then values for \code{interimSubj} must be specified in \code{\link{generateData}}. If interim proportions have been specified in \code{interimSubj} then analysis is performed across the dataset as a whole, then across each interim dataset in turn. At each interim \code{interimCode} is called to apply decision-making e.g. dropping doses, stopping the study. If any doses are dropped then existing data from these doses are carried forward into subsequent interim analysis, however no new subjects on these doses are carried forward into future datasets.

If the \code{waitAndCombine} flag is set to TRUE, the function will wait until all GRID jobs are finished (if GRID has been used), then compile the "Micro" and "Macro" evaluation results into single summary files (using the \code{\link{compileSummary}} function).

}
\value{
  This function will produce no direct output.  Analysis, summary and log files will be produced.
}

\author{ Mango Solutions & Pfizer \email{mstoolkit@googlemail.com} }
\note{  
  There are some restrictions on the code inputs to the \code{\link{analyzeData}} function.  These restrictions are discussed here:
  
  Analysis Code: The \code{analysisCode} input must be either an R function or a reference to an external file.  If it is a reference to external file, it must contain either SAS code (if software is "SAS") or R code (if software is "R").
    If the code is an R function, or an external R script, it must accept a data frame as its only argument and return an acceptable "Micro Evaluation" data frame as set out in \code{\link{checkMicroFormat}}.
    If the code is an external SAS script, it must accept use a SAS dataset called "work.infile" and create a SAS dataset called "work.outfile" that conforms to the "Micro Evalutation" format as set out in \code{\link{checkMicroFormat}}. Since the dataset infile is created using the "work" library, no \code{libname} statement is necessary in the SAS analysis code.
    More information on "Micro Evaluation" structures can be found in the help file for function \code{\link{checkMicroFormat}}.
  
  Interim Code: The "interimCode" input must be an R function that accepts a single "Micro Evaluation" data input, and returns an R "list" structure that is either empty or contains one or more of the following elements:
    An element called "STOP" which is a logical vector of length 1.  This tells the \code{\link{analyzeData}} function whether the analysis should be halted at this interim
    An element called "DROP" which is a vector of numeric values relating to doses in the data to drop before the next interim is analyzed.
    More information on "Micro Evaluation" structures can be found in the help file for function \code{\link{interimAnalysis}}.

  Macro Code: The \code{macroCode} input must be an R function that accepts an enhanced "Micro Evaluation" data input, and returns a valid "Macro Evaluation" data structure (as specified in the help file for the \code{\link{checkMacroFormat}} function.

}
\seealso{ 
  \code{\link{analyzeRep}}, \code{\link{macroEvaluation}}, \code{\link{compileSummary}} and \code{\link{generateData}}
}
\examples{\dontrun{

# Standard analysis code
emaxCode <- function(data){
  library(DoseResponse)
  with( data, {
    uniDoses <- sort( unique(DOSE))                                                                    
    eFit <- emaxalt( RESP, DOSE )
    outDf <- data.frame( DOSE = uniDoses, 
      MEAN = eFit$dm[as.character(uniDoses)], 
      SE = eFit$dsd[as.character(uniDoses)] )
    outDf$LOWER <- outDf$MEAN - 2 * outDf$SE
    outDf$UPPER <- outDf$MEAN + 2 * outDf$SE
    outDf$N     <- table(DOSE)[ as.character(uniDoses) ]
    outDf 
  }) 
}
             
# Macro evaluation code
macrocode <- function(data) {
  # making up a t-test
  mu0   <- data$MEAN[ data$DOSE == 0 & data$INTERIM == 0]
  mu100 <- data$MEAN[ data$DOSE == 100 & data$INTERIM == 0]
  n0    <- data$N[ data$DOSE == 0 & data$INTERIM == 0]
  n100  <- data$N[ data$DOSE == 100 & data$INTERIM == 0]
  sd0   <- data$SE[ data$DOSE == 0 & data$INTERIM == 0]
  sd100 <- data$SE[ data$DOSE == 100 & data$INTERIM == 0]
  
  sddiff <- if( n0 == n100 ){
    sqrt( (sd0^2 + sd100^2)  / (n0 + n100) )
  } else {
    sqrt( (1/n0 + 1/n100) * ( (n0-1)*sd0^2 + (n100-1)*sd100^2  ) / (n0+n100-2)  )
  }
  tstat  <- ( mu100 - mu0 ) / sddiff 
  success <- abs(tstat) > qt( .975, n0+n100-2)
  
  data.frame( SUCCESS = success, TSTAT = tstat )
}
  
# Interim analysis code
interimCode <- function( data ){
  dropdose  <- with( data, DOSE [ sign(UPPER) != sign(LOWER) & DOSE != 0] )
  outList <- list()
  if( length(dropdose) > 0 ) outList$DROP <- dropdose
  outList$STOP <- length(dropdose) == nrow(data)-1
  outList
}
   
# Run analysis
analyzeData( 1:5, analysisCode = emaxCode, macroCode = macrocode, 
  interimCode = interimCode )

}
}


\keyword{ datagen }

