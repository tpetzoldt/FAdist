\name{LLOGIS}
\Rdversion{1.1}
\alias{dllog}
\alias{pllog}
\alias{qllog}
\alias{rllog}

\title{Log-Logistic Distribution}

\description{
Density, distribution function, quantile function and random generation for the log-logistic distribution with shape and scale parameters equal to \code{shape} and \code{scale}, respectively.}


\usage{
dllog(x,shape=1,scale=1,log=FALSE)
pllog(q,shape=1,scale=1,lower.tail=TRUE,log.p=FALSE)
qllog(p,shape=1,scale=1,lower.tail=TRUE,log.p=FALSE)
rllog(n,shape=1,scale=1)
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
If \emph{Y} is a random variable distributed according to a logistic distribution (with location and scale parameters), then \emph{X = exp(Y)}  has a
log-logistic distribution with shape and scale parameters corresponding to the scale and location parameteres of \emph{Y}, respectively.
}

\value{
\code{dllog} gives the density, \code{pllog} gives the distribution function, \code{qllog} gives the quantile function, and \code{rllog} generates random deviates.


}
\references{}



\seealso{
	\code{\link{dlogis}}, \code{\link{plogis}}, \code{\link{qlogis}}, \code{\link{rlogis}}
}
\examples{
x <- rllog(10,1,0)
dllog(x,1,0)
dlogis(log(x),0,1)/x
}

\keyword{distribution}

