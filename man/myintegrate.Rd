\name{myintegrate}
\alias{myintegrate}
\alias{integrate.contour}
\alias{integrate.segments}
\title{Complex integration}
\description{
  Integration of complex valued functions along the real axis
  (\code{myintegrate()}), along arbitrary paths
  (\code{integrate.contour()}), and following arbitrary straight line
  segments (\code{integrate.segments()}).
}
\usage{
myintegrate(f, lower,upper, ...)
integrate.contour(f,u,udash, ...)
integrate.segments(f,points, close=TRUE, ...)
}
\arguments{
  \item{f}{function, possibly complex valued}
  \item{lower}{Lower limit of integration in \code{myintegrate()}; a
    real number}
  \item{upper}{Upper limit of integration in \code{myintegrate()}; a
    real number (for complex values, use \code{integrate.contour()} or \code{integrate.segments()})}
  \item{...}{Extra arguments passed to \code{integrate()}}
  \item{u}{Function mapping \eqn{[0,1]}{[0,1]} to the contour.  For a
    closed contour, require that \eqn{u(0)=u(1)}{u(0)=u(1)}.}
  \item{udash}{Derivative of \code{u}.}
  \item{points}{In function \code{integrate.segments()}, a vector of complex
numbers.  Integration will be taken over straight segments joining
consecutive elements of \code{points}.}
  \item{close}{In  function \code{integrate.segments()}, a Boolean
variable with default \code{TRUE} meaning to integrate along the segment
from \code{points[n]} to \code{points[1]} in addition to the internal
segments.} 
}
\author{Robin K. S. Hankin}
\examples{
# To integrate round the unit circle, we need the contour and its
# derivative:

 u <- function(x){exp(pi*2i*x)}
 udash <- function(x){pi*2i*exp(pi*2i*x)}

# First, some elementary functions, for practice:
integrate.contour(function(z){1/z},u,udash)            # should be 2*pi*i
integrate.contour(function(z){log(z)},u,udash)         # should be -2*pi*i
integrate.contour(function(z){sin(z)+1/z^2},u,udash)   # should be zero

# Now, some elliptic functions:
g <- c(3,2+4i)
Zeta <- function(z){zeta(z,g)}
Sigma <- function(z){sigma(z,g)}
WeierstrassP <- function(z){P(z,g)}

jj <- integrate.contour(Zeta,u,udash) 
abs(jj-2*pi*1i)                              # should be zero
abs(integrate.contour(Sigma,u,udash))        # should be zero
abs(integrate.contour(WeierstrassP,u,udash)) # should be zero



# Now integrate over a semicircle.  Observe how it's better to split the
# path into two separate pieces.

R <- 400
u1     <- function(x){R*exp(pi*1i*x)}
u1dash <- function(x){R*pi*1i*exp(pi*1i*x)}
u2     <- function(x){R*(2*x-1)}
u2dash <- function(x){R*2}

f <- function(z){exp(1i*z)/(1+z^2)}

answer.approximate <-
    integrate.contour(f,u1,u1dash) +
    integrate.contour(f,u2,u2dash) 

answer.exact <- pi/exp(1)
abs(answer.approximate - answer.exact)


# Now try integrating over a triangle, with base on the real axis:
abs(integrate.segments(f,c(-R,R,1i*R))- answer.exact)
}
\keyword{math}
