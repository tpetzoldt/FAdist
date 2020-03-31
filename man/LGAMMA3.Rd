\name{LGAMMA3}
\Rdversion{1.1}
\alias{dlgamma3}
\alias{plgamma3}
\alias{qlgamma3}
\alias{rlgamma3}

\title{Log-Pearson Type III Distribution}

\description{
Density, distribution function, quantile function and random generation for the log-Pearson type III distribution with shape1, shape2, and scale parameters equal to \code{shape}, \code{scale}, and \code{thres}, respectively.}


\usage{
dlgamma3(x,shape=1,scale=1,thres=1,log=FALSE)
plgamma3(q,shape=1,scale=1,thres=1,lower.tail=TRUE,log.p=FALSE)
qlgamma3(p,shape=1,scale=1,thres=1,lower.tail=TRUE,log.p=FALSE)
rlgamma3(n,shape=1,scale=1,thres=1)
}


\arguments{
	\item{x,q}{vector of quantiles.}
	\item{p}{vector of probabilities.}
	\item{n}{number of observations.}
	\item{shape}{shape1 parameter.}
	\item{scale}{shape2 parameter.}
	\item{thres}{scale parameter.}
	\item{log,log.p}{logical; if TRUE, probabilities p are given as log(p).}
	\item{lower.tail}{logical; if TRUE (default), probabilities are \emph{P[X <= x]},otherwise, \emph{P[X > x]}.}
}

\details{
If \emph{Y} is a random variable distributed according to a gamma distribution (with shape and scale parameters), then \emph{X = exp(Y+m)} has a
log-Pearson type III distribution with shape1 and shape2 parameters corresponding to the shape and 1/scale parameteres of \emph{Y}, and with scale parameter \emph{m}.
}

\value{
\code{dlgamma3} gives the density, \code{plgamma3} gives the distribution function, \code{qlgamma3} gives the quantile function, and \code{rlgamma3} generates random deviates.


}
\references{BOBEE, B. and F. ASHKAR (1991). The Gamma Family and Derived Distributions Applied in Hydrology. Water Resources Publications, Littleton, Colo., 217 p.}


\seealso{
	\code{\link{dgamma}}, \code{\link{pgamma}}, \code{\link{qgamma}}, \code{\link{rgamma}}, \code{\link{dgamma3}}, \code{\link{pgamma3}}, \code{\link{qgamma3}}, \code{\link{rgamma3}}
}
\examples{
thres <- 10
x <- rlgamma3(n=10,shape=2,scale=11,thres=thres)
dlgamma3(x,2,11,thres)
dgamma3(log(x),2,1/11,thres)/x
dgamma(log(x)-thres,2,11)/x
}

\keyword{distribution}

