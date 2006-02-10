\name{farey}
\alias{farey}
\title{Farey sequences}
\description{
  Returns the Farey sequence of order \eqn{n}{n}
}
\usage{
farey(n, print=FALSE, give.series = FALSE)
}
\arguments{
  \item{n}{Order of Farey sequence}
  \item{print}{Boolean, with \code{TRUE} meaning to print out the text
    version of the Farey sequence in human-readable form.  Default value
    of \code{FALSE} means not to print anything.}
  \item{give.series}{Boolean, with \code{TRUE} meaning to return the
    series explicitly, and default \code{FALSE} meaning to return a 3
    dimensional array as detailed below.}
}
\details{
  If \code{give.series} takes its default value of \code{FALSE}, return
  a matrix \code{a} of dimension \code{c(u,2)} where \code{u} is a
  (complicated) function of \code{n}.  The rows of \code{a} are the 

}

\references{An introduction to the theory of numbers (fifth edition),
  G. H. Hardy and  E. M. Wright, Oxford University Press,1985.}
\author{Robin K. S. Hankin}
\seealso{\code{\link{unimodular}}}
\examples{
farey(3)
}
\keyword{array}
