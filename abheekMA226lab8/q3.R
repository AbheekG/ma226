m <- 100
z <- 1.96	#for 95% confidence interval

while (m<=100000) {
	U <- runif(m)
	Y1 <- exp(U^(1/2))

	X <- U
	X <- X^(1/2)
	c <- -cov(X,Y1)/var(X)
	mean_x <- mean(X)
	Y <- Y1 + c*(X - mean_x)
	I <- mean(Y)
	std_dev <- sd(Y)
	cat("m = ", m,"\n")
	cat("Expected value, I = ", I, "\n")
	cat("Variance = ", std_dev^2, "\n")
	cat("95% confidence interval = (", I - z*std_dev/(m^(1/2)), ", ",I + z*std_dev/(m^(1/2)), ")\n")
	cat("Variance reduction = ", 100*(1 - var(Y)/var(Y1)) ,"%\n\n")
	m <- m*10
}

rm(list = ls())