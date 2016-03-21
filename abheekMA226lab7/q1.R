library(MASS)

n <- 1000
a <- c(-0.25, 0, 0.25)

Z <- matrix( rnorm(2 * n), nrow=n, ncol=2)

for (i in 1:3) {

	Sigma <- matrix( c(1, 2*a[i], 2*a[i], 4), nrow=2, ncol=2)
	Mu <- c(5,8)
	A <- chol(Sigma)
	X <- Z %*% A
	X[,1] = X[,1] + Mu[1]
	X[,2] = X[,2] + Mu[2]

cat("For, a = ", a[i], "\\\\\n")
cat("Mean, X1 = ", mean(X[,1]), ", X2 = ", mean(X[,2]), "\\\\\n")
cat("Variance, X1 = ", var(X[,1]), ", X2 = ", var(X[,2]), "\\\\\n")
cat("Covariance = ", cov(X[,1], X[,2]), "\\\\\\\\\n")

plot(X[,1],X[,2], main="Bivariate Normal Dist.(1000 values)", xlab="X1", ylab="X2")
z.kde=kde2d(X[,1],X[,2])
contour(z.kde,add=TRUE) 
image(z.kde); 
contour(z.kde, add = T)

	if(i == 1)
		dev.copy(png, "plot1_1.png")
	if(i == 2)
		dev.copy(png, "plot1_2.png")
	if(i == 3)
		dev.copy(png, "plot1_3.png")
	dev.off ()

plot(ecdf(X[,1]), main="Cumulative Distribution Function of X1")
par(new=TRUE)
plot(ecdf(rnorm(n, mean = 5, sd = 1)), col='red', axes=FALSE, main="")
	
	if(i == 1)
		dev.copy(png, "plot2_1.png")
	if(i == 2)
		dev.copy(png, "plot2_2.png")
	if(i == 3)
		dev.copy(png, "plot2_3.png")
	dev.off ()

plot(ecdf(X[,2]), main="Cumulative Distribution Function of X2")
par(new=TRUE)
plot(ecdf(rnorm(n, mean = 8, sd = 2)), col='red', axes=FALSE, main="")
	
	if(i == 1)
		dev.copy(png, "plot2_4.png")
	if(i == 2)
		dev.copy(png, "plot2_5.png")
	if(i == 3)
		dev.copy(png, "plot2_6.png")
	dev.off ()
}