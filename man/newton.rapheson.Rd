\name{newton.rapheson}
\alias{newton.rapheson}
\title{Newton Rapheson iteration to find roots of equations}
\description{
Newton Rapheson iteration to find roots of equations with the emphasis
on complex functions
}
\usage{
newton.rapheson(initial, f, fdash, maxiter, tol = .Machine$double.eps)
}
\arguments{
  \item{initial}{Starting guess}
  \item{f}{Function for which \eqn{f(z)=0}{f(z)=0} is to be solved for
    \eqn{z}{z}}
  \item{fdash}{Derivative of function (note: Cauchy-Riemann conditions
    assumed)} 
  \item{maxiter}{Maximum number of iterations attempted}
  \item{tol}{Tolerance: iteration stops if \eqn{|f(z)|<tol}{|f(z)|<tol}}
}
\details{
  Bog-standard
}
\value{
  Returns \eqn{z}{z} with \eqn{|f(z)|<tol}{|f(z)|<tol}
}
\author{Robin K. S. Hankin}
\examples{

#Find the two square roots of 2+i:
f <- function(z){z^2-(2+1i)}
fdash <- function(z){2*z}
newton.rapheson( 1.4+0.3i,f,fdash,maxiter=10)
newton.rapheson(-1.4-0.3i,f,fdash,maxiter=10)

#Now find the three cube roots of unity:
g <- function(z){z^3-1}
gdash <- function(z){3*z^2}
newton.rapheson(-0.5+1i,g,gdash,maxiter=10)
newton.rapheson(-0.5-1i,g,gdash,maxiter=10)
newton.rapheson(+0.5+0i,g,gdash,maxiter=10)


}
\keyword{math}
