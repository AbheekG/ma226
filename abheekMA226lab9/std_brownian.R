m <- 10	# No. of paths
n <- 5000	# No of time points
t <- 5
dt <- t/n

w2 <- vector(,m)
w5 <- vector(,m)
T <- seq(0, t, dt)
pal <- palette()

for (i in 1:m) {
	Z <- rnorm(n)
	W <- cumsum(c(0, dt^(1/2)*Z))
	w2[i] <- W[n*(2/t) + 1]
	w5[i] <- W[n + 1]
	if(i == 1) {
		plot(T, W, ylim=c(-5, 5), col=pal[i %% 8 + 1], cex=0.0001, main="Standard Brownian Motion", xlab="Time", ylab="W", type="l")
	} else {
		lines(T, W, col=pal[i %% 8 + 1], cex=0.0001)
	}
}

dev.copy(png,"plot1.png");
dev.off ();

cat(" E[W(2)] = ",mean(w2),"\n")
cat(" E[W(5)] = ",mean(w5),"\n")

rm(list = ls())