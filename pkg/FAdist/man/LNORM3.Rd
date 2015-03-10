\name{LNORM3}
\Rdversion{1.1}
\alias{dlnorm3}
\alias{plnorm3}
\alias{qlnorm3}
\alias{rlnorm3}

\title{Three-Parameter Lognormal Distribution}

\description{
Density, distribution function, quantile function and random generation for the 3-parameter lognormal distribution with shape, scale, and threshold (or shift) parameters equal to \code{shape}, \code{scale}, and \code{thres}, respectively.}


\usage{
dlnorm3(x,shape=1,scale=1,thres=0,log=FALSE)
plnorm3(q,shape=1,scale=1,thres=0,lower.tail=TRUE,log.p=FALSE)
qlnorm3(p,shape=1,scale=1,thres=0,lower.tail=TRUE,log.p=FALSE)
rlnorm3(n,shape=1,scale=1,thres=0)
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
If \emph{Y} is a random variable distributed according to a normal distribution (with location(mean) and scale(standard deviation) parameters), then \emph{X = exp(Y)+m}  has a
3-parameter lognormal distribution with shape and scale parameters corresponding to the scale and location parameteres of \emph{Y}, respectively; and threshold parameter \emph{m}.
}

\value{
\code{dlnorm3} gives the density, \code{plnorm3} gives the distribution function, \code{qlnorm3} gives the quantile function, and \code{rlnorm3} generates random deviates.


}
\references{}




\seealso{
	\code{\link{dnorm}}, \code{\link{pnorm}}, \code{\link{qnorm}}, \code{\link{rnorm}}, \code{\link{dlnorm}}, \code{\link{plnorm}}, \code{\link{qlnorm}}, \code{\link{rlnorm}}
}
\examples{
m <- 100
x <- rlnorm3(10,1,0,m)
dlnorm3(x,1,0,m)
dlnorm(x-m,0,1)
dnorm(log(x-m),0,1)/(x-m)
}

\keyword{distribution}

