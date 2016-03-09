rd <- runif(5000)
rd <- -log(rd)*5
cat("Mean = ", mean(rd), "\n")
cat("Minimum = ", min(rd), "\n")
cat("Maximum = ", max(rd), "\n")
hist(rd, main="Exponential distribution with mean = 5", xlab="Range of random numbers", ylab="Density")
dev.copy(png,"plot1.png");
dev.off ();
rm(list = ls())