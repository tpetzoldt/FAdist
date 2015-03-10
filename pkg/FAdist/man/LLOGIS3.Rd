\name{LLOGIS3}
\Rdversion{1.1}
\alias{dllog3}
\alias{pllog3}
\alias{qllog3}
\alias{rllog3}

\title{Three-Parameter Log-Logistic Distribution}

\description{
Density, distribution function, quantile function and random generation for the 3-parameter log-logistic distribution with shape, scale, and threshold (or shift) parameters equal to \code{shape}, \code{scale}, and \code{thres}, respectively.}


\usage{
dllog3(x,shape=1,scale=1,thres=0,log=FALSE)
pllog3(q,shape=1,scale=1,thres=0,lower.tail=TRUE,log.p=FALSE)
qllog3(p,shape=1,scale=1,thres=0,lower.tail=TRUE,log.p=FALSE)
rllog3(n,shape=1,scale=1,thres=0)
}


\arguments{
	\item{x,q}{vector of quantiles.}
	\item{p}{vector of probabilities.}
	\item{n}{number of observations.}
	\item{shape}{shape parameter.}
	\item{scale}{scale parameter.}
	\item{thres}{threshold (or shift) parameter.}
	\item{log,log.p}{logical; if TRUE, probabilities p are given as log(p).}
	\item{lower.tail}{logical; if TRUE (default), probabilities are \emph{P[X <= x]},otherwise, \emph{P[X > x]}.}
}

\details{
If \emph{Y} is a random variable distributed according to a logistic distribution (with location and scale parameters), then \emph{X = exp(Y)+m}  has a 3-parameter 
log-logistic distribution with shape and scale parameters corresponding to the scale and location parameteres of \emph{Y}, respectively; and threshold parameter \eqn{m}.
}

\value{
\code{dllog3} gives the density, \code{pllog3} gives the distribution function, \code{qllog3} gives the quantile function, and \code{rllog3} generates random deviates.


}


\seealso{
	\code{\link{dlogis}}, \code{\link{plogis}}, \code{\link{qlogis}}, \code{\link{rlogis}}, \code{\link{dllog}}, \code{\link{pllog}}, \code{\link{qllog}}, \code{\link{rllog}}
}
\examples{
m <- 100
x <- rllog3(10,1,0,m)
dllog3(x,1,0,m)
dllog(x-m,1,0)
dlogis(log(x-m),0,1)/(x-m)
}

\keyword{distribution}

