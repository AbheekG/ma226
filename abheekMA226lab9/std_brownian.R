m <- 10	# No. of paths
n <- 5000	# No of time points
t <- 5
dt <- t/n

X <- seq(0, t, dt)
pal <- palette()

for (i in 1:m) {
	Z <- rnorm(n)
	W <- cumsum(c(0, dt*Z))
	if(i == 1) {
		plot(X, W, ylim=c(-0.15, 0.15), col=pal[i %% 8 + 1], cex=0.0001)
	} else {
		lines(X, W, col=pal[i %% 8 + 1], cex=0.0001)
	}
}

rm(list = ls())