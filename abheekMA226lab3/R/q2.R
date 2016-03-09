rd1 <- runif(5000)
rd2 <- runif(5000)
rd3 <- runif(5000)
rd4 <- runif(5000)
rd5 <- runif(5000)

rd <- -(log(rd1) + log(rd2) + log(rd3) + log(rd4) + log(rd5))/5
cat("Mean = ", mean(rd), "\n")
cat("Minimum = ", min(rd), "\n")
cat("Maximum = ", max(rd), "\n")
hist(rd, main="Gamma distribution with N = 5 and Lamda = 5", xlab="Range of random numbers", ylab="Density")
dev.copy(png,"plot2.png");
dev.off ();
rm(list = ls())