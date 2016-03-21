library(MASS)

n <- 1000
sigma1 <- 1
sigma2 <- 2
rho <- c(-0.25, 0, 0.25)
mu1 <- 5
mu2 <- 8

z1 <- rnorm(n)
z2 <- rnorm(n)

for (i in 1:3) {
	
	x2 <- mu2 + sigma2 * z1
	x1 <- (mu1 + rho[i]*(sigma1/sigma2)*(x2 - mu2) ) + ( sigma1	* (1 - rho[i]^2 )^(1/2) ) * z2

cat("For, a = ", a[i], "\\\\\n")
cat("Mean, X1 = ", mean(x1), ", X2 = ", mean(x2), "\\\\\n")
cat("Variance, X1 = ", var(x1), ", X2 = ", var(x2), "\\\\\n")
cat("Covariance = ", cov(x1, x2), "\\\\\\\\\n")

plot(x1, x2, main="Bivariate Normal Dist.(1000 values)", xlab="X1", ylab="X2")
z.kde=kde2d(x1, x2)
contour(z.kde,add=TRUE) 
image(z.kde); 
contour(z.kde, add = T)

	if(i == 1)
		dev.copy(png, "plot3_1.png")
	if(i == 2)
		dev.copy(png, "plot3_2.png")
	if(i == 3)
		dev.copy(png, "plot3_3.png")
	dev.off ()
}