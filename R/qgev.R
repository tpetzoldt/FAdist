qgev <-
function(p,shape=1,scale=1,location=0,lower.tail=TRUE,log.p=FALSE)
{
	if(log.p) p <- exp(p)
	if(!lower.tail) p <- 1 - p

	if (shape == 0) {
	  xF <- location - scale * log(-log(p))
	} else {
	  xF <- location+scale/shape*((-log(p))^(-shape)-1)
	}

	return(xF)
}

