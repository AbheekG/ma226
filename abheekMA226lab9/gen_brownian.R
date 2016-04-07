m <- 10	# No. of paths
n <- 5000	# No of time points
t <- 5
dt <- t/n
mu <- 0.06
sigma <- 0.3
w0 <- 5

w2 <- vector(,m)
w5 <- vector(,m)
T <- seq(0, t, dt)
pal <- palette()

for (i in 1:m) {
	Z <- rnorm(n)
	X <- cumsum(c(w0, mu*dt + sigma*dt^(1/2)*Z))
	w2[i] <- X[n*(2/t) + 1]
	w5[i] <- X[n + 1]
	if(i == 1) {
		plot(T, X, ylim=c(3.5, 7), col=pal[i %% 8 + 1], cex=0.0001, main="General Brownian Motion, mu = 0.06, sigma = 0.3", xlab="Time", ylab="X", type="l")
	} else {
		lines(T, X, col=pal[i %% 8 + 1], cex=0.0001)
	}
}

dev.copy(png,"plot2.png");
dev.off ();

cat(" E[X(2) = ",mean(w2),"\n")
cat(" E[X(5) = ",mean(w5),"\n")

rm(list = ls())