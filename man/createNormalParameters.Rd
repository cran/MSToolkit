\name{createNormalParameters}
\alias{createNormalParameters}

\title{ Create parameters based on a multivariate normal distribution } 
\description{
  Creates a set of fixed (and optionally between subject) parameter variables by sampling from a multivariate normal distribution
}
\usage{
createNormalParameters(subjects, names, mean, covariance = 0, range, betNames, betMean, betCov, betRange, 
errStruc = "None", suffix = ".Between", idCol = "SUBJ", maxDraws = 10, seed = .deriveFromMasterSeed(), 
flagName = "PAROMIT", digits = 3)
}

\arguments{
  \item{subjects}{ (Required) Subjects for which to create parameters }
  \item{names}{ (Required) Names of parameters to generate }
  \item{mean}{ (Required) Means for fixed parameters }
  \item{covariance}{ (Optional) Covariance matrix for fixed parameters.  By default, this is a matrix of zeros, representing no error to be included }
  \item{range}{ (Optional) Range of acceptable values.  Missing by default - by default all generated values are acceptable. }
  \item{betNames}{ (Optional) Names of fixed effects for which to create random between subject variance.  Default is to not create between subject data }
  \item{betMean}{ (Optional) Means for the between subject effects.  Default is to not create between subject data  }
  \item{betCov}{ (Optional) Covariance matrix for the between subject effects.  Default is to not create between subject data  }
  \item{betRange}{ (Optional) Range of acceptable values for the between subject effects.  Default is to not create between subject data  }
  \item{errStruc}{ (Optional) Function to map effect: Additive, Proportional, or None (default is "None"). If \code{errStruc} is set to "None" then fixed and between subject random effects are returned separately. }
  \item{suffix}{ (Optional) Suffix to use for retain between subject effects (default is ".Between") }
  \item{idCol}{ (Optional) Subject variable name for return data ("SUBJ" by default) }
  \item{maxDraws}{ (Optional) Maximum number of iterations for valid parameters (10 by default) }
  \item{seed}{ (Optional) Random number generation seed.  By default, this is dervied from the current random seed }
  \item{flagName}{ (Optional) Flag name for parameters out of bounds ("PAROMIT" by default) }
  \item{digits}{ (Optional) Number of digits to which to round generated continuous parameters.  Can be an atomic integer, in which case all variables are rounded according to it. Can be a numeric vector of the same length of the number of fixed parameters, in which case each parameter is rounded according to its value.  The default is 3 }
}
\details{
  \code{createNormalParameters} creates fixed effect parameters which can vary between trial replicates. For each replicate a single draw is taken from a multivariate normal distribution centred on \code{mean} and with (co)variance \code{covariance}. Within each replicate individual subjects can vary around the sampled fixed effect value by specifying \code{betCov}. Usually this is only used if there are repeated measures for a single subject. The between subject variance is added to the fixed effect to give an individualised value to the parameter for each subject.

  The function will first create a single set of fixed effects, to be repeated for each subject specified.  This single set of fixed effects will be taken from a multivariate normal distribution with mean and covariance matrix supplied.
  If the \code{range} argument is provided, the function will take \code{maxDraws} samples of fixed effects and select one set of effect where the values are all "in range".  If none of the derived sets satisfy the \code{range} condition, the function will stop and return an error.

  If the \code{betNames} input has been suppied, and the \code{betNames} vector is a subset of the fixed effect names, between subject effects will be derived.
  If the \code{betMean} input has not been supplied, the mean of the between effects distribution will be 0.  Samples for each subject will be taken from a multivariate normal distribution with mean \code{betMean} and covariate matrix  \code{betCov}.
  If the \code{betRange} input has been specified, the function will repetitively attempt to create between subject effects within the given range.  If, after \code{maxDraws} attempts there are still values outside of the range, the "parameter omit" flag will be set to "1" to signify and "out of range" element.

  If fixed and between subject effects have been generated, the \code{errStruc} input will specify how the between subject effects are to be applied:
  * None = Keep between subject effect seperate (and use the \code{suffix} suppied to name the variables)
  * Additive = Add each between subject effect to the corresponding fixed effect
  * Proportional = Add each between subject effect to the corresponding fixed effect, then exponentiate the result
}
\value{
  A data frame containing a subject variable, a "parameter omit flag" and a set of extracted parameters
}
\author{ Mango Solutions & Pfizer \email{mstoolkit@googlemail.com} }
\note{
  MSToolkit v2.0 applies \code{range} arguments to the fixed and random effects separately. This checks whether these individual components are within acceptable ranges, however it does not apply a range to the combination of these. If the combination of these values is required to be positive, we suggest working with the "Proportional" value for \code{errStruc} or work with log-transformed variables.  Future versions of MSToolkit will apply a range check to the combined parameter.
}

\seealso{ \code{\link{createParameters}} and \code{\link{createExternalParameters}}  }
\examples{
\dontrun{
createNormalParameters(5, "E0,ED50,EMAX", mean = c(0, 50, 100), covariance = diag(c(1, 5, 10)))
#  SUBJ       E0     ED50     EMAX   PAROMIT
#1    1 -0.8356286 50.41064 98.01898       0
#2    2 -0.8356286 50.41064 98.01898       0
#3    3 -0.8356286 50.41064 98.01898       0
#4    4 -0.8356286 50.41064 98.01898       0
#5    5 -0.8356286 50.41064 98.01898       0
createNormalParameters(5, "E0,ED50,EMAX", mean = c(0, 50, 100), covariance = diag(c(1, 5, 10)), betNames = c("E0", "EMAX"), betCov = diag(2), errStruc = "Additive")
#  SUBJ       E0     ED50     EMAX   PAROMIT
#1    1  0.7596522 50.41064 98.59476       0
#2    2 -0.5061208 50.41064 97.71359       0
#3    3 -1.6560970 50.41064 99.53076       0
#4    4 -0.3481996 50.41064 98.40882       0
#5    5 -0.0973039 50.41064 97.39774       0

% added for SF issue 8
% Tue Jul 24 10:35:42 BST 2007 @441 /Internet Time/
# no covariance by default 
createNormalParameters(5, "E0,ED50,EMAX", mean = c(0, 50, 100) )  
#  SUBJ E0 ED50 EMAX PAROMIT
#1    1  0   50  100       0
#2    2  0   50  100       0
#3    3  0   50  100       0
#4    4  0   50  100       0
#5    5  0   50  100       0

}
}

\keyword{ datagen }

