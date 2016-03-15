weibull <- function(u, b, t) {
	return ( exp( -log(log(1-u))/b - log(t) ))
}