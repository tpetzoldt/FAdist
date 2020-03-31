\name{GenPARETO}
\Rdversion{1.1}
\alias{dgp}
\alias{pgp}
\alias{qgp}
\alias{rgp}

\title{Generalized Pareto Distribution}

\description{
Density, distribution function, quantile function and random generation for the generalized Pareto distribution with shape and scale parameters equal to \code{shape} and \code{scale}, respectively.}


\usage{
dgp(x,shape=1,scale=1,log=FALSE)
pgp(q,shape=1,scale=1,lower.tail=TRUE,log.p=FALSE)
qgp(p,shape=1,scale=1,lower.tail=TRUE,log.p=FALSE)
rgp(n,shape=1,scale=1)
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
If \emph{X} is a random variable distributed according to a generalized Pareto distribution, it has density
\cr
			f(x) = 1/scale*(1-shape*x/scale)^((1-shape)/shape)
}

\value{
\code{dgp} gives the density, \code{pgp} gives the distribution function, \code{qgp} gives the quantile function, and \code{rgp} generates random deviates.
}

\references{Coles, S. (2001) An introduction to statistical modeling of extreme values. Springer}

\examples{
x <- rgp(1000,-.2,10)
hist(x,freq=FALSE,col='gray',border='white')
curve(dgp(x,-.2,10),add=TRUE,col='red4',lwd=2)
}

\keyword{distribution}

