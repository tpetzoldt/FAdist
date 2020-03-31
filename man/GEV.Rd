\name{GEV}
\Rdversion{1.1}
\alias{dgev}
\alias{pgev}
\alias{qgev}
\alias{rgev}

\title{Generalized Extreme Value Distribution (for maxima)}

\description{
Density, distribution function, quantile function and random generation for the generalized extreme value distribution (for maxima) with shape, scale, and location parameters equal to \code{shape}, \code{scale}, and \code{location}, respectively.}


\usage{
dgev(x,shape=1,scale=1,location=0,log=FALSE)
pgev(q,shape=1,scale=1,location=0,lower.tail=TRUE,log.p=FALSE)
qgev(p,shape=1,scale=1,location=0,lower.tail=TRUE,log.p=FALSE)
rgev(n,shape=1,scale=1,location=0)
}


\arguments{
	\item{x,q}{vector of quantiles.}
	\item{p}{vector of probabilities.}
	\item{n}{number of observations.}
	\item{shape}{shape parameter.}
	\item{scale}{scale parameter.}
	\item{location}{location parameter.}
	\item{log,log.p}{logical; if TRUE, probabilities p are given as log(p).}
	\item{lower.tail}{logical; if TRUE (default), probabilities are \emph{P[X <= x]},otherwise, \emph{P[X > x]}.}
}

\details{
If \emph{X} is a random variable distributed according to a generalized extreme value distribution, it has density
\cr
			f(x) = 1/scale*(1+shape*((x-location)/scale))^(-1/shape-1)*exp(-(1+shape*((x-location)/scale))^(-1/shape))
}

\value{
\code{dgev} gives the density, \code{pgev} gives the distribution function, \code{qgev} gives the quantile function, and \code{rgev} generates random deviates.
}

\references{Coles, S. (2001) An introduction to statistical modeling of extreme values. Springer}

\examples{
x <- rgev(1000,-.1,3,100)
hist(x,freq=FALSE,col='gray',border='white')
curve(dgev(x,-.1,3,100),add=TRUE,col='red4',lwd=2)
}

\keyword{distribution}

