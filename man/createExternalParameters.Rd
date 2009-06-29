\name{createExternalParameters}
\alias{createExternalParameters}
\title{Sample parameters from an external file}
\description{
  Create parameters for a simulated population by sampling them from an external file. 
}
\usage{
  createExternalParameters(subjects, file, names, betNames, betNums, errStruc = "None", 
  suffix = ".Between", subset = NULL, refCol, refColName, idCol = "SUBJ", 
  seed = .deriveFromMasterSeed(), flagName = "PAROMIT", refColSuffix = "refCol", 
  dataId = idCol, workingPath = getwd())
}
\arguments{
  \item{subjects}{ (Required) Number of subjects for which to create parameters }
  \item{file}{ (Required) External file from which to extract parameters }
  \item{names}{ (Required) Names of parameters to import (refers to variables in the input data) }
  \item{betNames}{ (Required) Between subject effects variables in the data }
  \item{betNums}{ (Required) Integer vector mapping between subject effects onto fixed effects }
  \item{errStruc}{ (Optional) (None) Function to map between subject effects: Additive, Proportional or None }
  \item{suffix}{ (Optional) Suffix to use for retained between subject effects variables.  By default, ".Between" is used as the default }
  \item{subset}{ (Optional) Subset to apply to the data.  By default, no subset is applied }
  \item{refCol}{ (Optional) Vector of data to be matched to the reference column for sampling (no reference data by default) }
  \item{refColName}{ (Optional) Column name in data for referenced sampling (no reference column by default) }
  \item{idCol}{ (Optional) Subject variable name in the data ("SUBJ" by default) }
  \item{seed}{ (Optional) Random number seed.  Derived from the current seed by default }
  \item{flagName}{ (Optional) Name for "parameter omit" flag ("PAROMIT" by default) }
  \item{refColSuffix}{ (Optional) Suffix to add to reference variable in the data ("refCol" by default) }
  \item{dataId}{ (Optional) The subject variable name in the data.  By default, this is taken to be the same as the "idCol" input }
  \item{workingPath}{ (Optional) Working path from which to import covariate file.  By default, the working path will be used }
}
\details{
  The data is first imported from the specified file, and any subset applied to it.  The function next checks that all required variables specifed appear in the imported data.
  Fixed effects (specified using names) are selected from the file along with between subject effects (if they are also identified).
  If both fixed and between subject parameters have been selected, the function will use the \code{errStruc} input as follows:
  * None = Keep between subject effect seperate (and use the \code{suffix} suppied to name the variables)
  * Additive = Add each specified between subject effects to the corresponding fixed effect (based on the mapping specified in betNums)
  * Proportional = Add each between subject effect to the corresponding fixed effect (based on the mapping specified in betNums) then exponentiate the result  
  
  If the \code{refCol} input is specified, this variable should also be retained in the result.
  The "parameter omit" flag will contain repeated "0" elements in the return result.

  \code{createExternalParameters} assumes that sampling is performed for parameters at the subject level. That is, fixed and random effects can be sampled and combined using \code{errStruc} or retained as separate variables in order to create subject-specific parameter values. Future versions of MSToolkit will allow sampling at the replicate level e.g. sampling from MCMC posterior distribution output from WinBUGS.
}
\value{
  A data frame containing a subject variable, a "parameter omit flag" and a set of extracted parameters
}
\author{ Mango Solutions & Pfizer \email{mstoolkit@googlemail.com} }\seealso{ 
  \code{\link{createParameters}} and \code{\link{createNormalParameters}}
}
\examples{
\dontrun{
  wPath <- system.file( "Runit", "data", "createParameters", package = "MSToolkit" ) 
 
  createExternalParameters( subjects = 100, names = "E0,ED50", 
    file = "testParam.csv", errStruc = "None", betNames = "B1,B2", workingPath = wPath )
}}

\keyword{ datagen }

