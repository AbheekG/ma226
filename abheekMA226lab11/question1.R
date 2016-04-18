van_der_corput <- function(n, base = 2) {
	X <- seq(1, n, 1)
	k <- 1
	m <- 0
	while(k < n) {
		m <- m + 1
		k <- k*base
	}
	# print(m)
	Y <- vector(,n)
	k <- base
	for (i in 1:m) {
		Y <- Y + (X %% base)/k
		k <- k*base
		X <- as.integer(X/base)
	}
	return(Y)
}

lcg <- function(n, a = 48271, p = 2147483647, seed = 1073741824, b = 0) {
	X <- vector(,n)
	X[1] <- (a*seed) %% p
	for (i in 2:n) {
		X[i] = (a*X[i-1]) %% p
	}
	return(X/p)
}

X <- van_der_corput(100000)
Y <- lcg(100000)

cat("first 25 values of Van der Corput sequence\\\\\n")
print(X[1:25])

cat("\n\nOverlapping pairs plot for 1000 values of Van der Corput sequence\n")
plot(X[1:999], X[2:1000], main="Overlapping pairs plot for 1000 values of Van der Corput sequence", xlab = "x(i)", ylab="x(i+1)")
dev.copy(png,"plot1a.png");
dev.off ();

#cat("\n\nDensity Plot for 100 values")
attach(mtcars)
par(mfrow=c(1,2)) 
hist(X[1:100], main="100 values Van der Corput")
hist(Y[1:100], main="100 values LCG")
dev.copy(png,"plot1b.png");
dev.off ();

#cat("\n\nDensity Plot for 100000 values")
attach(mtcars)
par(mfrow=c(1,2)) 
hist(X, main="100000 values Van der Corput")
hist(Y, main="100000 values LCG")
dev.copy(png,"plot1c.png");
dev.off ();