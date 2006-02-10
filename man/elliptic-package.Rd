\name{elliptic-package}
\alias{elliptic-package}
\alias{elliptic}
\docType{package}
\title{
Elliptic and modular functions
}
\description{
 A suite of elliptic and related functions including Weierstrass and
 Jacobi forms.  Also includes various tools for manipulating and
 visualizing complex functions
}
\details{
\tabular{ll}{
Package: \tab elliptic\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2007-05-01\cr
License: \tab GPL
}

The primary function in package \pkg{elliptic} is \code{P()}: this
calculates the Weierstrass~\eqn{\wp}{P} function, and may take named
arguments that specify either the invariants~\code{g} or half
periods \code{Omega}.  The derivative is given by function \code{Pdash}
and the Weierstrass sigma and zeta functions are given by functions
\code{sigma()} and \code{zeta()} respectively; these are documented in
\code{?P}.  Jacobi forms are documented under \code{?sn} and modular
forms under \code{?J}.

Notation follows Abramowitz and Stegun (1965) where possible, although
there only real invariants are considered; \code{?e1e2e3} and
\code{?parameters} give a more detailed discussion.  Various equations
from AMS-55 are implemented (for fun); the functions are named after
their equation numbers in AMS-55; all references are to this work unless
otherwise indicated.

The package uses Jacobi's theta functions (\code{?theta} and
\code{?theta.neville}) where possible: they converge very quickly.

Various number-theoretic functions that are required for (eg) converting
a period pair to primitive form (\code{?as.primitive}) are implemented;
see \code{?divisor} for a list.

The package also provides some tools for numerical verification of
complex analysis such as contour integration (\code{?myintegrate}) and
Newton-Rapheson iteration for complex functions
(\code{?newton.rapheson}).

Complex functions may be visualized using \code{view()}; this is
customizable but has an extensive set of built-in colourmaps.
}

\author{
  Robin K. S. Hankin; r.hankin@noc.soton.ac.uk
}
\references{

  R. K. S. Hankin.  \emph{Introducing Elliptic, an R package for
    Elliptic and Modular Functions}.  Journal of Statistical Software,
  Volume 15, Issue 7.  February 2006.

  M. Abramowitz and I. A. Stegun 1965. \emph{Handbook of Mathematical
  Functions.}  New York, Dover.


  K. Chandrasekharan 1985. \emph{Elliptic functions},  Springer-Verlag.


  E. T. Whittaker and G. N. Watson 1952.  \emph{A Course of Modern
    Analysis}, Cambridge University Press (fourth edition).
}
\keyword{ package }
\examples{
     ## Example 8, p666, RHS:
 P(z=0.07 + 0.1i, g=c(10,2)) 

     ## Now a nice little plot of the zeta function:
 x <- seq(from=-4,to=4,len=100)
 z <- outer(x,1i*x,"+")
 par(pty="s")
 view(x,x,limit(zeta(z,c(1+1i,2-3i))),nlevels=3,scheme=1)
 view(x,x,P(z*3,params=equianharmonic()),real=FALSE)

     ## Some number theory:
 mobius(1:10)
 plot(divisor(1:300,k=1),type="s",xlab="n",ylab="divisor(n,1)")

    ## Primitive periods:
 as.primitive(c(3+4.01i , 7+10i))
 as.primitive(c(3+4.01i , 7+10i),n=10)   # Note difference

    ## Now some contour integration:
 f <- function(z){1/z}
 u <- function(x){exp(2i*pi*x)}
 udash <- function(x){2i*pi*exp(2i*pi*x)}
 integrate.contour(f,u,udash) - 2*pi*1i



    ## PUT THIS IN EQUIANHARMONIC.Rd
 x <- seq(from=-10,to=10,len=200)
 z <- outer(x,1i*x,"+")
 view(x,x,P(z,params=lemniscatic()),real=FALSE)
 view(x,x,P(z,params=pseudolemniscatic()),real=FALSE)
 view(x,x,P(z,params=equianharmonic()),real=FALSE)
 


}
