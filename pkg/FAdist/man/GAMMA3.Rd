\name{GAMMA3}
\Rdversion{1.1}
\alias{dgamma3}
\alias{pgamma3}
\alias{qgamma3}
\alias{rgamma3}

\title{Three-Parameter Gamma Distribution (also known as Pearson type III distribution)}

\description{
Density, distribution function, quantile function and random generation for the 3-parameter gamma distribution with shape, scale, and threshold (or shift) parameters equal to \code{shape}, \code{scale}, and \code{thres}, respectively.}


\usage{
dgamma3(x,shape=1,scale=1,thres=0,log=FALSE)
pgamma3(q,shape=1,scale=1,thres=0,lower.tail=TRUE,log.p=FALSE)
qgamma3(p,shape=1,scale=1,thres=0,lower.tail=TRUE,log.p=FALSE)
rgamma3(n,shape=1,scale=1,thres=0)
}


\arguments{
	\item{x,q}{vector of quantiles.}
	\item{p}{vector of probabilities.}
	\item{n}{number of observations.}
	\item{shape}{shape parameter.}
	\item{scale}{scale parameter.}
	\item{thres}{threshold or shift parameter.}
	\item{log,log.p}{logical; if TRUE, probabilities p are given as log(p).}
	\item{lower.tail}{logical; if TRUE (default), probabilities are \emph{P[X <= x]},otherwise, \emph{P[X > x]}.}
}

\details{
If \emph{Y} is a random variable distributed according to a gamma distribution (with shape and scale parameters), then \emph{X = Y+m} has a
3-parameter gamma distribution with the same shape and scale parameters, and with threshold (or shift) parameter \emph{m}.
}

\value{
\code{dgamma3} gives the density, \code{pgamma3} gives the distribution function, \code{qgamma3} gives the quantile function, and \code{rgamma3} generates random deviates.
}

\references{BOBEE, B. and F. ASHKAR (1991). The Gamma Family and Derived Distributions Applied in Hydrology. Water Resources Publications, Littleton, Colo., 217 p.}

\seealso{
	\code{\link{dgamma}}, \code{\link{pgamma}}, \code{\link{qgamma}}, \code{\link{rgamma}}
}
\examples{
thres <- 10
x <- rgamma3(n=10,shape=2,scale=11,thres=thres)
dgamma3(x,2,11,thres)
dgamma(x-thres,2,1/11)
}

\keyword{distribution}

