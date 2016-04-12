m <- 10	# No. of paths
n <- 5000	# No of time points
t <- 5
dt <- t/n

w2 <- vector(,m)
w5 <- vector(,m)
T <- seq(0, t, dt)
pal <- palette()
y0 <- 5

Mu <- 0.0325 - 0.05*T[2:(n+1)]
Sigma <- 0.012 + 0.0138*T[2:(n+1)] + 0.00125*(T[2:(n+1)]^2)

for (i in 1:m) {
	Z <- rnorm(n)
	Y <- cumsum(c(y0, Mu*dt + Sigma*dt^(1/2)*Z))
	w2[i] <- Y[n*(2/t) + 1]
	w5[i] <- Y[n + 1]
	if(i == 1) {
		plot(T, Y, ylim=c(4.3, 5.1), col=pal[i %% 8 + 1], cex=0.0001, main="Euler Approximation", xlab="Time", ylab="Y", type="l")
	} else {
		lines(T, Y, col=pal[i %% 8 + 1], cex=0.0001)
	}
}

dev.copy(png,"plot3.png");
dev.off ();

cat(" E[Y(2)] = ",mean(w2),"\n")
cat(" E[Y(5)] = ",mean(w5),"\n")

rm(list = ls())