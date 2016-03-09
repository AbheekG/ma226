n <- 100000	 #No of values generated
c <- ((2 * exp(1)) / pi )^(1/2) 
x <- runif(n)
g <- vector(,n)
u <- runif(n)

for (i in 1:n) {
	if (x[i] < 1/2) {
		g[i] <- x[i]
		x[i] <- log(2*x[i])
	} else {
		g[i] <- 1 - x[i]
		x[i] <- -log(2*(1-x[i]))
	}
}

f <- ((1 / (2 * pi)) ^ (1/2)) * (exp(-(x^2)/2))

rd <- x[(g*u*c) < f]

cat("Acceptance probability, Theoretical = ", 1/c, ", Stimulated = ", length(rd)/n, "\n")
cat("Mean, Theoretical = 0", ", Stimulated = ", mean(rd), "\n")
cat("Median, Theoretical = 0", "Stimulated = ", median(rd), "\n")
cat("Variance, Theoretical = 1", ", Stimulated = ", var(rd), "\n")
cat("Standard Deviation, Theoretical = 1", ", Stimulated = ", sd(rd), "\n")

hist(rd, main="Standard Normal Distribution", xlab="Range of random numbers", ylab="Density", breaks=500)
dev.copy(png,"plot1.png");
dev.off ();
rm(list = ls())
