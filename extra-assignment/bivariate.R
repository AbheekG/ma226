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
	if (x1 < x2)
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
	if (x1 < x2)
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
	if (x1 < x2)
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
	if (x1 < x2)
	{
		return (weibull_cdf_1d(x1, alpha, lamda1) * weibull_pdf_1d(x2, alpha, lamda0 + lamda2))
	}
	else if (x1 >= x2)
	{
		return (weibull_cdf_1d(x1, alpha, lamda0 + lamda1) * weibull_pdf_1d(x2, alpha, lamda2))
	}
	else
	{
		return (1)
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
alpha <- 1	#TODO select about 2 values >0
lamda <- -0.2	#TODO select some value in 0 to -1
lamda0 <- 2 #TODO select about 2 values >0
lamda1 <- 3	#TODO select about 2 values >0
lamda2 <- 4	#TODO select about 2 values >0

#TODO start loop

U0 <- weibull_generator_1d(alpha, lamda0, n)
U1 <- weibull_generator_1d(alpha, lamda1, n)
U2 <- weibull_generator_1d(alpha, lamda2, n)

# Here (X1, X2) follows Marshall–Olkin bivariate Weibull
# distribution with parameters (alpha, lamda0, lamda1, lamda2)
X1 <- pmin(U0, U1)
X2 <- pmin(U0, U2)

# Now using Acceptance-Rejection to find required distribution
# we take the sample distribution MOBW
c <- 1 - 3*lamda
U <- runif(n)
Y1 <- vector(,0)
Y2 <- vector(,0)

for (i in 1:n) {
	x1 <- X1[i]
	x2 <- X2[i]
	u <- U[i]
	if (f(x1, x2, lamda, alpha, lamda0, lamda1, lamda2) > c * u * mobw_pdf_2d(x1, x2, alpha, lamda0, lamda1, lamda2))
	{
		Y1 <- c(Y1, x1)
		Y2 <- c(Y2, x2)
	}
}

#TODO print images
#TODO end for loop

rm(list = ls())

