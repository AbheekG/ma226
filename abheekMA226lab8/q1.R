m <- 100
z <- 1.96	#for 95% confidence interval

while (m<=100000) {
	U <- runif(m)
	Y <- exp(U^(1/2))
	I <- mean(Y)
	SD <- sd(Y)
	cat("m = ", m,"\n")
	cat("Expected value = ", I, "\n")
	cat("95% confidence interval = (", I - z*SD/(m^(1/2)), ", ",I + z*SD/(m^(1/2)), ")\n\n")
	m <- m*10
}

rm(list = ls())