\name{equianharmonic}
\alias{equianharmonic}
\alias{lemniscatic}
\alias{pseudolemniscatic}
\title{Special cases of the Weierstrass elliptic function}
\description{
Gives parameters for the equianharmonic case, the lemniscatic case, and
the pseudolemniscatic case.
}
\usage{
equianharmonic(...)
lemniscatic(...)
pseudolemniscatic(...)
}
\arguments{
  \item{\dots}{Ignored}
}
\details{
  These functions return values from section 18.13, p652; 18.14, p658;
  and 18.15, p662.  They use elementary functions (and the gamma
  function) only, so ought to be more accurate and faster than calling
  \code{parameters(g=c(1,0))} directly.

  Note that the values for the half periods correspond to the general
  case for complex \code{g2} and \code{g3} so are simple linear
  combinations of those given in AnS.
    
  One can use \code{parameters("equianharmonic")} \emph{et seq} instead.
 
}
\value{
Returns a list with the same elements as \code{parameters()}.
}
\references{ams55}
\author{Robin K. S. Hankin}
\seealso{\code{\link{parameters}}}
\examples{
P(z=0.1+0.1212i,params=equianharmonic())
}
\keyword{math}
