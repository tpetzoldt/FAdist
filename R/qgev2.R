# extended version supporting shape == 0 and vectorized arguments

qgev2 <-
function(p,shape=1,scale=1,location=0,lower.tail=TRUE,log.p=FALSE)
{
	if(log.p) p <- exp(p)
	if(!lower.tail) p <- 1 - p

	xF <- with(expand.args(p, shape, scale, location),
	  ifelse(shape==0,
	         location - scale * log(-log(p)),
	      	 location + scale/shape * ((-log(p))^(-shape) - 1)
	  )
	)

	return(xF)
}
