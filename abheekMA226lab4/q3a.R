#Discrete Inverse transformation method
n <- 100000	 #No of values generated, taking 1000 as 10 is too small for any measurement
p <- c(0.05, 0.25, 0.45, 0.15, 0.10)
x <- c(1:5)

u <- runif(n)
rd <- vector(,n)

for (i in 1:n) {
	for (j in 1:5) {
		if (((sum(p[1:j]) - p[j]) < u[i]) & (u[i] < sum(p[1:j]))) {
			rd[i] = j
		}
	}
}

#print(rd)

cat("Mean, Theoretical = ", sum(p*x), ", Stimulated = ", mean(rd), "\n")
cat("Variance, Theoretical = ", (sum(p*x^2) - 9), ", Stimulated = ", var(rd), "\n")
hist(rd, main="Discrete Distribution (for 100000 values)", xlab="Range of random numbers", ylab="Density", breaks=50)
dev.copy(png,"plot3a.png");
dev.off ();
rm(list = ls())