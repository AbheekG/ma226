start.time <- Sys.time()
#Box Muller method
n <- 10000 #no of values
u1 <- runif(n)
u2 <- runif(n)

temp <- ((-2) * log(u1))^(1/2)

z1 <- temp * cos(2 * pi * u2)
z2 <- temp * sin(2 * pi * u2)

end.time <- Sys.time()
time.taken <- end.time - start.time


cat("Box-Muller Method\n")
cat("Time taken for 10000 numbers generated = ", time.taken,"\n")
cat("For 100 values\n")
cat("Sample's Mean, X = ", mean(z1[1:100]), ", Y = ", mean(z2[1:100]), "\n")
cat("Sample's Variance, X = ", var(z1[1:100]), ", Y = ", var(z2[1:100]), "\n")
cat("Sample's Covariance = ", cov(z1[1:100], z2[1:100]), "\n\n")

cat("For 500 values\n")
cat("Sample's Mean, X = ", mean(z1[1:500]), ", Y = ", mean(z2[1:500]), "\n")
cat("Sample's Variance, X = ", var(z1[1:500]), ", Y = ", var(z2[1:500]), "\n")
cat("Sample's Covariance = ", cov(z1[1:500], z2[1:500]), "\n\n")

cat("For 10000 values\n")
cat("Sample's Mean, X = ", mean(z1), ", Y = ", mean(z2), "\n")
cat("Sample's Variance, X = ", var(z1), ", Y = ", var(z2), "\n")
cat("Sample's Covariance = ", cov(z1, z2), "\n\n")

plot(z1[1:100], z2[1:100], col='black',  main="2-D Standard Normal Distribution, 100 values,", xlab="X", ylab="Y")
dev.copy(png,"plot1_1.png");
dev.off ();

hist(z1[1:100], main="Standard Normal Distribution, X, 100 values", xlab="Range of random numbers", ylab="Density", breaks=15)
dev.copy(png,"plot1_1_X.png");
dev.off ();

hist(z2[1:100], main="Standard Normal Distribution, Y, 100 values", xlab="Range of random numbers", ylab="Density", breaks=15)
dev.copy(png,"plot1_1_Y.png");
dev.off ();



plot(z1[1:500], z2[1:500], col='black', cex=0.5, main="2-D Standard Normal Distribution, 500 values", xlab="X", ylab="Y")
dev.copy(png,"plot1_2.png");
dev.off ();

hist(z1[1:500], main="Standard Normal Distribution, X, 500 values", xlab="Range of random numbers", ylab="Density", breaks=20)
dev.copy(png,"plot1_2_X.png");
dev.off ();

hist(z2[1:500], main="Standard Normal Distribution, Y, 500 values", xlab="Range of random numbers", ylab="Density", breaks=20)
dev.copy(png,"plot1_2_Y.png");
dev.off ();



plot(z1, z2, pch=16, col='black', cex=0.5, main="2-D Standard Normal Distribution, 10000 values", xlab="X", ylab="Y")
dev.copy(png,"plot1_3.png");
dev.off ();

hist(z1, main="Standard Normal Distribution, X, 10000 values", xlab="Range of random numbers", ylab="Density", breaks=25)
dev.copy(png,"plot1_3_X.png");
dev.off ();

hist(z2, main="Standard Normal Distribution, Y, 10000 values", xlab="Range of random numbers", ylab="Density", breaks=25)
dev.copy(png,"plot1_3_Y.png");
dev.off ();


norm <- function(x, mu, sig) {
	return ( (1/ ((2*pi)^(1/2) * sig )) * exp ( -((x-mu)/sig)^2 / 2) )
}


##For N(μ= 0, σ=5)
x1 <- z1[1:500] * 5^(1/2)
y1 <- z2[1:500] * 5^(1/2)

cat("N(μ= 0, σ=5) for 500 values\n")
cat("Sample's Mean, X = ", mean(x1[1:500]), ", Y = ", mean(y1[1:500]), "\n")
cat("Sample's Variance, X = ", var(x1[1:500]), ", Y = ", var(y1[1:500]), "\n")
cat("Sample's Covariance = ", cov(x1[1:500], y1[1:500]), "\n\n")

plot(x1[1:500], y1[1:500], col='black', cex=0.5, main="2-D N(μ= 0, σ=5), 500 values", xlab="X", ylab="Y")
dev.copy(png,"plot3_1.png");
dev.off ();

hist(x1[1:500], main="N(μ= 0, σ=5), X, 500 values", xlab="Range of random numbers", ylab="Density", breaks=20)
points(x1 , 500 * norm(x1, 0, 5) , col='black', cex=0.5)
dev.copy(png,"plot3_1_X.png");
dev.off ();

hist(y1[1:500], main="N(μ= 0, σ=5), Y, 500 values", xlab="Range of random numbers", ylab="Density", breaks=20)
points(y1 , 500 * norm(y1, 0, 5) , col='black', cex=0.5)
dev.copy(png,"plot3_1_Y.png");
dev.off ();


##For N(μ= 5, σ=5)



x2 <- x1 + 5
y2 <- y1 + 5

cat("N(μ= 5, σ=5) for 500 values\n")
cat("Sample's Mean, X = ", mean(x2[1:500]), ", Y = ", mean(y2[1:500]), "\n")
cat("Sample's Variance, X = ", var(x2[1:500]), ", Y = ", var(y2[1:500]), "\n")
cat("Sample's Covariance = ", cov(x2[1:500], y2[1:500]), "\n\n")

plot(x2[1:500], y2[1:500], col='black', cex=0.5, main="2-D N(μ= 5, σ=5), 500 values", xlab="X", ylab="Y")
dev.copy(png,"plot3_2.png");
dev.off ();

hist(x2[1:500], main="N(μ= 5, σ=5), X, 500 values", xlab="Range of random numbers", ylab="Density", breaks=20)
points(x2 , 500 * norm(x2, 5, 5) , col='black', cex=0.5)
dev.copy(png,"plot3_2_X.png");
dev.off ();

hist(y2[1:500], main="N(μ= 5, σ=5), Y, 500 values", xlab="Range of random numbers", ylab="Density", breaks=20)
points(y2 , 500 * norm(y2, 5, 5) , col='black', cex=0.5)
dev.copy(png,"plot3_2_Y.png");
dev.off ();
rm(list = ls())