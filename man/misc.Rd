\name{misc}
\alias{Im<-}
\alias{Re<-}
\title{Manipulate real or imaginary components of an object}
\description{
Manipulate real or imaginary components of an object
}
\usage{
Im(x) <- value
Re(x) <- value
}
\arguments{
  \item{x}{Complex-valued object}
  \item{value}{Real-valued object}
}
\references{
  J. Kotus and M. Urb\'{a}nski 2003.  \dQuote{Hausdorff dimension and Hausdorff
  measures of Julia sets of elliptic functions}.  Bulletin of the London
  Mathematical Society, volume 35, pp269-275

  K. Briggs 1995.  \dQuote{A torus map based on Jacobi's sn()}.  Computers and
  Graphics, volume 19, number 3, pp451-453

  S. D. Panteliou and A. D. Dimarogonas and I. N .Katz 1996.  \dQuote{Direct
  and inverse interpolation for Jacobian elliptic functions, zeta
  function of Jacobi and complete elliptic integrals of the second
  kind}.  Computers and Mathematics with Applications, volume 32,
  number 8, pp51-57.

  E. L. Wachspress 2000.  \dQuote{Evaluating Elliptic functions and their
  inverses}.  Computers and Mathematics with Applications, volume 29,
  pp131-136.

  D. G. Vyridis and S. D. Panteliou and I. N. Katz 1999.  \dQuote{An inverse
  convergence approach for arguments of Jacobian elliptic functions}.
  Computers and Mathematics with Applications, volume 37, pp21-26.

  S. Paszkowski 1997.  \dQuote{Fast convergent quasipower series for some
  elementary and special functions}.   Computers and Mathematics with
  Applications, volume 33, number 1/2, pp181-191.

  M. Abramowitz and I. A. Stegun 1965.  \dQuote{Handbook of mathematical
  functions, \emph{ams-55}}, New York: Dover

  K. Chandrasekharan 1985.  \dQuote{Elliptic functions}, Springer-Verlag

  E. T. Whittaker and G. N. Watson 1952.  \dQuote{A course of modern
  analysis},  fourth edition, Cambridge University Press.
  }
\author{Robin K. S. Hankin}
\examples{
x <- 1:10
Im(x) <- 1

x <- 1:5
Im(x) <- 1/x


}
\keyword{math}
