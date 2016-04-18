library(MASS)
#F* = (1+lamda)F - (lamda)^2

# 1-D Weibull
weibull_pdf_1d <- function(x, alpha, theta)
{
	return (alpha * theta * x^(alpha - 1) * exp(-theta * x^alpha))
}

weibull_se_1d <- function(x, alpha, theta)
{
	return (exp(-theta * x^alpha))
}

weibull_cdf_1d <- function(x, alpha, theta)
{
	return (1 - weibull_se_1d(x, alpha, theta))
}

#Generates n 1-D weibull numbers with given parameters
weibull_generator_1d <- function(alpha, theta, n)
{
	u <- runif(n)
	return (((-1/theta) * log(u))^(1/alpha))
}

# 2-D Marshall–Olkin bivariate Weibull
mobw_pdf_2d <- function(x1, x2, alpha, lamda0, lamda1, lamda2)
{
	if(x1 < x2)
	{
		return (weibull_pdf_1d(x1, alpha, lamda1) * weibull_pdf_1d(x2, alpha, lamda0 + lamda2))
	}
	else if (x1 > x2)
	{
		return (weibull_pdf_1d(x1, alpha, lamda0 + lamda1) * weibull_pdf_1d(x2, alpha, lamda2))
	}
	else
	{
		return ((lamda0/(lamda0 + lamda1 + lamda2)) * weibull_pdf_1d(x1, alpha, lamda0 + lamda1 + lamda2))
	}
}

mobw_se_2d <- function(x1, x2, alpha, lamda0, lamda1, lamda2)
{
	z <- pmax(x1, x2)
	return (weibull_se_1d(x1, alpha, lamda1) * weibull_se_1d(x2, alpha, lamda2) * weibull_se_1d(z, alpha, lamda0))
}

mobw_cdf_2d <- function(x1, x2, alpha, lamda0, lamda1, lamda2)
{
	if(x1 < x2)
	{
		return (weibull_cdf_1d(x1, alpha, lamda1) * weibull_cdf_1d(x2, alpha, lamda0 + lamda2))
	}
	else if (x1 > x2)
	{
		return (weibull_cdf_1d(x1, alpha, lamda0 + lamda1) * weibull_cdf_1d(x2, alpha, lamda2))
	}
	else
	{
		return ((lamda0/(lamda0 + lamda1 + lamda2)) * weibull_cdf_1d(x1, alpha, lamda0 + lamda1 + lamda2))
	}
}

mobw_cdf_partial_x1 <- function(x1, x2, alpha, lamda0, lamda1, lamda2)
{
	if(x1 < x2)
	{
		return (weibull_pdf_1d(x1, alpha, lamda1) * weibull_cdf_1d(x2, alpha, lamda0 + lamda2))
	}
	else if (x1 > x2)
	{
		return (weibull_pdf_1d(x1, alpha, lamda0 + lamda1) * weibull_cdf_1d(x2, alpha, lamda2))
	}
	else
	{
		return ((lamda0/(lamda0 + lamda1 + lamda2)) * weibull_pdf_1d(x1, alpha, lamda0 + lamda1 + lamda2))
	}
}

mobw_cdf_partial_x2 <- function(x1, x2, alpha, lamda0, lamda1, lamda2)
{
	if(x1 < x2)
	{
		return (weibull_cdf_1d(x1, alpha, lamda1) * weibull_pdf_1d(x2, alpha, lamda0 + lamda2))
	}
	else if (x1 > x2)
	{
		return (weibull_cdf_1d(x1, alpha, lamda0 + lamda1) * weibull_pdf_1d(x2, alpha, lamda2))
	}
	else
	{
		return ((lamda0/(lamda0 + lamda1 + lamda2)) * weibull_cdf_1d(x1, alpha, lamda0 + lamda1 + lamda2))
	}
}

# PDF of our distribution function
f <- function(x1, x2, lamda, alpha, lamda0, lamda1, lamda2)
{
	return ( (1 + lamda) * mobw_pdf_2d(x1, x2, alpha, lamda0, lamda1, lamda2)
			- lamda*2*( (mobw_cdf_partial_x2(x1, x2, alpha, lamda0, lamda1, lamda2)
			   * mobw_cdf_partial_x1(x1, x2, alpha, lamda0, lamda1, lamda2))
			 + (mobw_cdf_2d(x1, x2, alpha, lamda0, lamda1, lamda2)
			   * mobw_pdf_2d(x1, x2, alpha, lamda0, lamda1, lamda2)) ) )
}

#Parameters for bivariate weibull
n <- 10000
alpha <- vector(,2)	#TODO select about 2 values >0
lamda <- vector(,2)	#TODO select some value in 0 to -1
lamda0 <- vector(,2) #TODO select about 2 values >0
lamda1 <- vector(,2)	#TODO select about 2 values >0
lamda2 <- vector(,2)	#TODO select about 2 values >0
alpha[1] <- 3;
alpha[2] <- 7;
lamda[1] <- -0.3;
lamda[2] <- -0.75;
lamda0[1] <- 1;
lamda0[2] <- 5;
lamda1[1] <- 4;
lamda1[2] <- 8;
lamda2[1] <- 2;
lamda2[2] <- 9;
d <-1;

#TODO start loop
for(p in 1:2){
	Alpha <- alpha[p];

for(j in 1:2){
	Lamda <- lamda[j];

for(k in 1:2){
	Lamda0 <- lamda0[k];

for(l in 1:2){
	Lamda1 <- lamda1[l];

for(m in 1:2){
Lamda2 <- lamda2[m];
U0 <- weibull_generator_1d(Alpha, Lamda0, n)
U1 <- weibull_generator_1d(Alpha, Lamda1, n)
U2 <- weibull_generator_1d(Alpha, Lamda2, n)

# Here (X1, X2) follows Marshall–Olkin bivariate Weibull
# distribution with parameters (alpha, lamda0, lamda1, lamda2)
X1 <- pmin(U0, U1)
X2 <- pmin(U0, U2)

# Now using Acceptance-Rejection to find required distribution
# we take the sample distribution MOBW
c <- 1 - 3*Lamda
U <- runif(n)
Y1 <- vector(,0)
Y2 <- vector(,0)

for (i in 1:n) {
	x1 <- X1[i]
	x2 <- X2[i]
	u <- U[i]
	if (f(x1, x2, Lamda, Alpha, Lamda0, Lamda1, Lamda2) > c * u * mobw_pdf_2d(x1, x2, Alpha, Lamda0, Lamda1, Lamda2))
	{
		Y1 <- c(Y1, x1)
		Y2 <- c(Y2, x2)
	}
}

#TODO print images
png(paste0("Question2_",toString(d),"a.png"));
plot(Y1,Y2,main=paste0("Alpha=",toString(Alpha), ", Lamda=", toString(Lamda),", Lamda0=", toString(Lamda0),", Lamda1=", toString(Lamda1),", Lamda2=", toString(Lamda2) ))
z.kde=kde2d(Y1,Y2)
contour(z.kde, add = TRUE)
dev.off();

png(paste0("Question2_",toString(d),"b.png"));
plot(Y1,Y2,main=paste0("Alpha=",toString(Alpha), ", Lamda=", toString(Lamda),", Lamda0=", toString(Lamda0),", Lamda1=", toString(Lamda1),", Lamda2=", toString(Lamda2) ))
z.kde=kde2d(Y1,Y2)
contour(z.kde,add=TRUE) 
image(z.kde); 
contour(z.kde, add = TRUE)
dev.off();

d <- d+1;
#TODO end for loop

}
}
}
}
}

rm(list = ls())

