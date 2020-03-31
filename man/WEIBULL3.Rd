\name{WEIBULL3}
\Rdversion{1.1}
\alias{dweibull3}
\alias{pweibull3}
\alias{qweibull3}
\alias{rweibull3}

\title{Three-Parameter Weibull Distribution}

\description{
Density, distribution function, quantile function and random generation for the 3-parameter Weibull distribution with shape, scale, and threshold (or shift) parameters equal to \code{shape}, \code{scale}, and \code{thres}, respectively.}


\usage{
dweibull3(x,shape,scale=1,thres=0,log=FALSE)
pweibull3(q,shape,scale=1,thres=0,lower.tail=TRUE,log.p=FALSE)
qweibull3(p,shape,scale=1,thres=0,lower.tail=TRUE,log.p=FALSE)
rweibull3(n,shape,scale=1,thres=0)
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
If \emph{Y} is a random variable distributed according to a Weibull distribution (with shape and scale parameters), then \emph{X = Y+m}  has a
3-parameter Weibull distribution with shape and scale parameters corresponding to the shape and scale parameteres of \emph{Y}, respectively; and threshold parameter \emph{m}.
}

\value{
\code{dweibull3} gives the density, \code{pweibull3} gives the distribution function, \code{qweibull3} gives the quantile function, and \code{rweibull3} generates random deviates.
}

\seealso{
	\code{\link{dweibull}}, \code{\link{pweibull}}, \code{\link{qweibull}}, \code{\link{rweibull}}
}
\examples{
m <- 100
x <- rweibull3(10,3,1,m)
dweibull3(x,3,1,m)
dweibull(x-m,3,1)
}

\keyword{distribution}

