\name{K.fun}
\alias{K.fun}
\alias{e16.1.1}
\title{quarter period K}
\description{
  Calculates the K.fun in terms of either \eqn{m}{m} (\code{K.fun()})
  or \eqn{k}{k} (\code{K.fun.k()}).
}
\usage{
K.fun(m, strict=TRUE, maxiter=7)
}
\arguments{
  \item{m}{Real or complex parameter}
  \item{strict}{Boolean, with default \code{TRUE} meaning to return an
    error if the sequence has not converged exactly, and \code{FALSE}
    meaning to return the partial sum, and a warning}
  \item{maxiter}{Maximum number of iterations}
}
\references{
   R. Coquereaux, A. Grossman, and B. E. Lautrup. \dQuote{Iterative
method for calculation of the Weierstrass elliptic function}. IMA
Journal of Numerical Analysis, vol 10, pp119-128, 1990
}
\author{Robin K. S. Hankin}
\examples{
K.fun(m=0.4)

\dontrun{
K.fun(0.4,strict=F, maxiter=5) - ellint_Kcomp(sqrt(0.4))
}
}
\keyword{math}
