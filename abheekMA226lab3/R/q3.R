#c <- 2.109376
rd1 <- runif(50000) #Taking g(x) as uniform dist rd1 is c*g(x)
rd2 <- runif(50000, 0, 2.109375)
fx <- 20 * rd1 * (1 - rd1)^3

rd <- rd1[rd2 < fx]

cat("Mean = ", mean(rd), "\n")
cat("Minimum = ", min(rd), "\n")
cat("Maximum = ", max(rd), "\n")
hist(rd, main="Distribution with f(x) = 20x(1-x)^3", xlab="Range of random numbers", ylab="Density")
dev.copy(png,"plot3.png");
dev.off ();
rm(list = ls())