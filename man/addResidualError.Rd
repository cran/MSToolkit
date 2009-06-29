\name{addResidualError}
\alias{addResidualError}
\title{ Add residual error to the response. }
\description{
  Adds residual error to a generated response, based on a supplied variance-covariance matrix
}
\usage{
addResidualError(response, covariance, errStruc = "additive", seed = .deriveFromMasterSeed())
}

\arguments{
  \item{response}{ (Required) Numeric vector of response data }
  \item{covariance}{ (Required) Residual error (co)variance.  This should be the lower-triangle of the matrix, or the matrix itself. The function \code{\link{parseCovMatrix}} is used to ensure that the matrix is in the right format. Release 2.0 of MSToolkit only allows 1 residual error term. If a matrix is supplied only the first variance will be used in calculation of the residual error. Subsequent releases will allow full residual error variance-covariance matrices.}
  \item{errStruc}{ (Optional) Function describing how to apply residual error: "Additive" or "Proportional".   "Additive" is the default}
  \item{seed}{ (Optional) Random seed to use.  Derived from the current random seed by default }
}
\details{
  The first step in the algorithm will be to set the random number seed to 
  the \code{seed} argument.  
  Then, a number of samples will be drawn from a multivariate normal 
  distribution with mean 0 and covariance set by the (parsed) \code{covariance} input. 
  The number of samples to take will be set by the number of elements in the 
  \code{response} vector.

 Based on the \code{errStruc} input, the algorithm should continue as follows. 
 If it is Additive, the residual error will be added to the \code{response} vector. If 
 it is Proportional, the residual error will be added to the \code{response} vector (usually log-transformed outcome),
 then the resulting vector will be exponentiated. This allows log-normal error structures.
}
\value{
  A numeric vector
}
\author{ Mango Solutions & Pfizer \email{mstoolkit@googlemail.com} }
\seealso{ 
  This function is typically not directly called by the user, but rather called 
  by the function \code{\link{createResponse}} that is the high-level function 
  for the response component. 
  
  The function \code{\link{createResponseVariable}} is also in the response component. 
  It creates the the response onto which \code{addResidualError} adds error.
}
\examples{
  myVec <- 1:10
  addResidualError(response = myVec, covariance = "1" )
}
\keyword{ datagen }

