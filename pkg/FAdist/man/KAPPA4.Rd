\name{KAPPA4}
\Rdversion{1.1}
\alias{dkappa4}
\alias{pkappa4}
\alias{qkappa4}
\alias{rkappa4}

\title{Four-Parameter Kappa Distribution}

\description{
Density, distribution function, quantile function and random generation for the four-parameter kappa distribution with shape1, shape2, scale, and location parameters equal to \code{shape1}, \code{shape2}, \code{scale}, and \code{location}, respectively.}


\usage{
dkappa4(x,shape1,shape2,scale=1,location=0,log=FALSE)
pkappa4(q,shape1,shape2,scale=1,location=0,lower.tail=TRUE,log.p=FALSE)
qkappa4(p,shape1,shape2,scale=1,location=0,lower.tail=TRUE,log.p=FALSE)
rkappa4(n,shape1,shape2,scale=1,location=0)
}


\arguments{
	\item{x,q}{vector of quantiles.}
	\item{p}{vector of probabilities.}
	\item{n}{number of observations.}
	\item{shape1}{shape parameter.}
	\item{shape2}{shape parameter.}
	\item{scale}{scale parameter.}
	\item{location}{location parameter.}
	\item{log,log.p}{logical; if TRUE, probabilities p are given as log(p).}
	\item{lower.tail}{logical; if TRUE (default), probabilities are \emph{P[X <= x]},otherwise, \emph{P[X > x]}.}
}

\details{See References}

\value{
\code{dkappa4} gives the density, \code{pkappa4} gives the distribution function, \code{qkappa4} gives the quantile function, and \code{rkappa4} generates random deviates.


}
\references{
Hosking, J.R.M. (1994). The four-parameter kappa distribution. IBM Journal of Research and Development, 38(3), 251-258.
}






\examples{
x <- rkappa4(1000,.1,.2,12,110)
hist(x,freq=FALSE,col='gray',border='white')
curve(dkappa4(x,.1,.2,12,110),add=TRUE,col='red4',lwd=2)
}

\keyword{distribution}

