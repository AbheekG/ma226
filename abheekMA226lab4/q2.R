n <- 100000	 #No of values generated
c <- ((2 * exp(1)) / pi )^(1/2) 
x <- runif(n)
x <- -log(x)
u <- runif(n)

g <- exp(-x)
f <- ((2/pi) ^ (1/2)) * (exp(-(x^2)/2))

rd <- x[(g*c*u) < f]

cat("Acceptance probability, Theoretical = ", 1/c, ", Stimulated = ", length(rd)/n, "\n")
cat("Mean, Theoretical = ", (2/pi)^(1/2), ", Stimulated = ", mean(rd), "\n")
cat("Variance, Theoretical = ", 1 - (2/pi), ", Stimulated = ", var(rd), "\n")
cat("Standard Deviation, Theoretical = ", (1 - 2/pi)^(1/2), ", Stimulated = ", sd(rd), "\n")

hist(rd, main="Standard Half Normal Distribution", xlab="Range of random numbers", ylab="Density", breaks=500)
dev.copy(png,"plot2.png");
dev.off ();
rm(list = ls())