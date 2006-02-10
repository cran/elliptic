\name{myintegrate}
\alias{myintegrate}
\alias{integrate.contour}
\alias{integrate.segments}
\concept{Complex integration}
\concept{Path integration}
\concept{Contour integration}
\concept{Cauchy's theorem}
\concept{Cauchy's integral theorem}
\concept{Cauchy's formula}
\concept{Residue theorem}
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
  \item{lower,upper}{Lower and upper limits of integration in \code{myintegrate()}; 
    real numbers (for complex values, use \code{integrate.contour()} or
    \code{integrate.segments()})}
  \item{u}{Function mapping \eqn{[0,1]} to the contour.  For a
    closed contour, require that \eqn{u(0)=u(1)}}
  \item{udash}{Derivative of \code{u}}
  \item{points}{In function \code{integrate.segments()}, a vector of complex
    numbers.  Integration will be taken over straight segments joining
    consecutive elements of \code{points}}
  \item{close}{In function \code{integrate.segments()}, a Boolean
    variable with default \code{TRUE} meaning to integrate along the segment
    from \code{points[n]} to \code{points[1]} in addition to the internal
    segments}
  \item{...}{Extra arguments passed to \code{integrate()}}
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



# Now integrate f(x) = exp(1i*x)/(1+x^2) from -Inf to +Inf along the
# real axis, using the Residue Theorem.  This tells us that integral of
# f(z) along any closed path is equal to pi*2i times the sum of the
# residues inside it.  Take a semicircular path P from -R to +R along
# the real axis, then following a semicircle in the upper half plane, of
# radius R to close the loop.  Now consider large R.  Then P encloses a
# pole at +1i [there is one at -1i also, but this is outside P, so
# irrelevent here] at which the residue is -1i/2e.  Thus the integral of
# f(z) = 2i*pi*(-1i/2e) = pi/e along P; the contribution from the
# semicircle tends to zero as R tends to infinity; thus the integral
# along the real axis is the whole path integral, or pi/e.

# We can now reproduce this result analytically.  First, choose an R:
R <- 400

# now define P.  First, the semicircle, u1:
u1     <- function(x){R*exp(pi*1i*x)}
u1dash <- function(x){R*pi*1i*exp(pi*1i*x)}

# and now the straight part along the real axis, u2:
u2     <- function(x){R*(2*x-1)}
u2dash <- function(x){R*2}

# Better define the function:
f <- function(z){exp(1i*z)/(1+z^2)}

# OK, now carry out the path integral.  I'll do it explicitly, but note
# that the contribution from the first integral should be small:

answer.approximate <-
    integrate.contour(f,u1,u1dash) +
    integrate.contour(f,u2,u2dash) 

# And compare with the analytical value:
answer.exact <- pi/exp(1)
abs(answer.approximate - answer.exact)


# Now try the same thing but integrating over a triangle, using
# integrate.segments().  Use a path P' with base from -R to +R along the
# real axis, closed by two straight segments, one from +R to 1i*R, the
# other from 1i*R to -R:

abs(integrate.segments(f,c(-R,R,1i*R))- answer.exact)



# Now in the interests of search engine findability, here is an
# application of Cauchy's integral formula, or Cauchy's formula.  I will
# use it to find sin(0.8):

u     <- function(x){exp(pi*2i*x)}
udash <- function(x){pi*2i*exp(pi*2i*x)}

g <- function(z){sin(z)/(z-0.8)}

a <- 1/(2i*pi)*integrate.contour(g,u,udash)


abs(a-sin(0.8))



}
\keyword{math}
