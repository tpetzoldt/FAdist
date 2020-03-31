\name{KAPPA}
\Rdversion{1.1}
\alias{dkappa}
\alias{pkappa}
\alias{qkappa}
\alias{rkappa}

\title{Kappa Distribution}

\description{
Density, distribution function, quantile function and random generation for the kappa distribution with shape and scale parameters equal to \code{shape} and \code{scale}, respectively.}


\usage{
dkappa(x,shape=1,scale=1,log=FALSE)
pkappa(q,shape=1,scale=1,lower.tail=TRUE,log.p=FALSE)
qkappa(p,shape=1,scale=1,lower.tail=TRUE,log.p=FALSE)
rkappa(n,shape=1,scale=1)
}


\arguments{
	\item{x,q}{vector of quantiles.}
	\item{p}{vector of probabilities.}
	\item{n}{number of observations.}
	\item{shape}{shape parameter.}
	\item{scale}{scale parameter.}
	\item{log,log.p}{logical; if TRUE, probabilities p are given as log(p).}
	\item{lower.tail}{logical; if TRUE (default), probabilities are \emph{P[X <= x]},otherwise, \emph{P[X > x]}.}
}

\details{
If \emph{X} is a random variable distributed according to a kappa distribution, it has density
\cr
			f(x) = shape/scale*(shape+(x/scale)^shape)^(-(shape+1)/shape)
}

\value{
\code{dkappa} gives the density, \code{pkappa} gives the distribution function, \code{qkappa} gives the quantile function, and \code{rkappa} generates random deviates.
}

\examples{
x <- rkappa(1000,12,10)
hist(x,freq=FALSE,col='gray',border='white')
curve(dkappa(x,12,10),add=TRUE,col='red4',lwd=2)
}

\keyword{distribution}

