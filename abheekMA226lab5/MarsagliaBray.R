#Marsaglia Bray method
start.time <- Sys.time()
n <- 12500 	#no of values
p <- 0
q <- 0
u1 <- vector(,0)
u2 <- vector(,0)

while (q < 10000) {
	p <- p + n
	u <- runif(n)
	v <- runif(n)
	u <- 2*u - 1
	v <- 2*v - 1
	temp <- u^2 + v^2
	u1 <- c( u1, u[temp < 1] )
	u2 <- c( u2, v[temp < 1] )
	q <- length(u1)
	n <- (10000 - q + 1) * 1.25
}

temp1 <- u1[1:1000]^2 + u2[1:1000]^2
temp2 <- ((-2) * log(temp1))^(1/2)
temp1 <- temp1^(1/2)

z1 <- temp2 * (u1[1:1000] / temp1)
z2 <- temp2 * (u2[1:1000] / temp1)

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken


cat("Marsaglia-Bray Method\n")
cat("Time taken for 10000 numbers generated = ", time.taken,"\n")
cat("Rejection ratio, Theoretical = ", 1 - pi/4, ", Generated = ", 1 - q/p, "\n\n")
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
dev.copy(png,"plot2_1.png");
dev.off ();

hist(z1[1:100], main="Standard Normal Distribution, X, 100 values", xlab="Range of random numbers", ylab="Density", breaks=15)
dev.copy(png,"plot2_1_X.png");
dev.off ();

hist(z2[1:100], main="Standard Normal Distribution, Y, 100 values", xlab="Range of random numbers", ylab="Density", breaks=15)
dev.copy(png,"plot2_1_Y.png");
dev.off ();



plot(z1[1:500], z2[1:500], col='black', cex=0.5, main="2-D Standard Normal Distribution, 500 values", xlab="X", ylab="Y")
dev.copy(png,"plot2_2_1.png");
dev.off ();

hist(z1[1:500], main="Standard Normal Distribution, X, 500 values", xlab="Range of random numbers", ylab="Density", breaks=20)
dev.copy(png,"plot2_2_X.png");
dev.off ();

hist(z2[1:500], main="Standard Normal Distribution, Y, 500 values", xlab="Range of random numbers", ylab="Density", breaks=20)
dev.copy(png,"plot2_2_Y.png");
dev.off ();



plot(z1, z2, pch=16, col='black', cex=0.5, main="2-D Standard Normal Distribution, 10000 values", xlab="X", ylab="Y")
dev.copy(png,"plot2_3.png");
dev.off ();

hist(z1, main="Standard Normal Distribution, X, 10000 values", xlab="Range of random numbers", ylab="Density", breaks=25)
dev.copy(png,"plot2_3_X.png");
dev.off ();

hist(z2, main="Standard Normal Distribution, Y, 10000 values", xlab="Range of random numbers", ylab="Density", breaks=25)
dev.copy(png,"plot2_3_Y.png");
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
dev.copy(png,"plot4_1.png");
dev.off ();

hist(x1[1:500], main="N(μ= 0, σ=5), X, 500 values", xlab="Range of random numbers", ylab="Density", breaks=20)
points(x1 , 500 * norm(x1, 0, 5) , col='black', cex=0.5)
dev.copy(png,"plot4_1_X.png");
dev.off ();

hist(y1[1:500], main="N(μ= 0, σ=5), Y, 500 values", xlab="Range of random numbers", ylab="Density", breaks=20)
points(y1 , 500 * norm(y1, 0, 5) , col='black', cex=0.5)
dev.copy(png,"plot4_1_Y.png");
dev.off ();


##For N(μ= 5, σ=5)



x2 <- x1 + 5
y2 <- y1 + 5

cat("N(μ= 5, σ=5) for 500 values\n")
cat("Sample's Mean, X = ", mean(x2[1:500]), ", Y = ", mean(y2[1:500]), "\n")
cat("Sample's Variance, X = ", var(x2[1:500]), ", Y = ", var(y2[1:500]), "\n")
cat("Sample's Covariance = ", cov(x2[1:500], y2[1:500]), "\n\n")

plot(x2[1:500], y2[1:500], col='black', cex=0.5, main="2-D N(μ= 5, σ=5), 500 values", xlab="X", ylab="Y")
dev.copy(png,"plot4_2.png");
dev.off ();

hist(x2[1:500], main="N(μ= 5, σ=5), X, 500 values", xlab="Range of random numbers", ylab="Density", breaks=20)
points(x2 , 500 * norm(x2, 5, 5) , col='black', cex=0.5)
dev.copy(png,"plot4_2_X.png");
dev.off ();

hist(y2[1:500], main="N(μ= 5, σ=5), Y, 500 values", xlab="Range of random numbers", ylab="Density", breaks=20)
points(y2 , 500 * norm(y2, 5, 5) , col='black', cex=0.5)
dev.copy(png,"plot4_2_Y.png");
dev.off ();
rm(list = ls())
