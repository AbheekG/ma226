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

X <- van_der_corput(100000, 2)
Y <- van_der_corput(100000, 3)

plot(X[1:100], Y[1:100], main="Harlton sequence 100 values, p1 = 2, and p2 = 3")
dev.copy(png,"plot2a.png");
dev.off();

plot(X, Y, cex=0.0001, main="Harlton sequence,100000 values, p1 = 2, and p2 = 3")
dev.copy(png,"plot2b.png")
dev.off();