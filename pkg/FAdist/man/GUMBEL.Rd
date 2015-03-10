\name{GUMBEL}
\Rdversion{1.1}
\alias{dgumbel}
\alias{pgumbel}
\alias{qgumbel}
\alias{rgumbel}

\title{Gumbel Distribution (for maxima)}

\description{
Density, distribution function, quantile function and random generation for the Gumbel distribution (for maxima) with scale and location parameters equal to \code{scale} and \code{location}, respectively.}


\usage{
dgumbel(x,scale=1,location=0,log=FALSE)
pgumbel(q,scale=1,location=0,lower.tail=TRUE,log.p=FALSE)
qgumbel(p,scale=1,location=0,lower.tail=TRUE,log.p=FALSE)
rgumbel(n,scale=1,location=0)
}


\arguments{
	\item{x,q}{vector of quantiles.}
	\item{p}{vector of probabilities.}
	\item{n}{number of observations.}
	\item{scale}{scale parameter.}
	\item{location}{location parameter.}
	\item{log,log.p}{logical; if TRUE, probabilities p are given as log(p).}
	\item{lower.tail}{logical; if TRUE (default), probabilities are \emph{P[X <= x]},otherwise, \emph{P[X > x]}.}
}

\details{
If \emph{X} is a random variable distributed according to a Gumbel distribution, it has density
\cr
			f(x) = 1/scale*exp(-(x-location)/scale-exp(-(x-location)/scale))
}

\value{
\code{dgumbel} gives the density, \code{pgumbel} gives the distribution function, \code{qgumbel} gives the quantile function, and \code{rgumbel} generates random deviates.


}
\references{Coles, S. (2001) An introduction to statistical modeling of extreme values. Springer}

\examples{
x <- rgumbel(1000,3,100)
hist(x,freq=FALSE,col='gray',border='white')
curve(dgumbel(x,3,100),add=TRUE,col='red4',lwd=2)
}

\keyword{distribution}

