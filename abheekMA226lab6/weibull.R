weibull <- function(u, b, t) {
	return ( exp( (log(-log(1-u))/b) - log(t) ))
}

b1 <- 2
t1 <- 1
b2 <- 1.5
t2 <- 1
p <- 0.4

n <- 50

u1 <- runif(n)
u2 <- runif(n)
x <- vector(,n)

for (i in 1:n)
{
	if(u1[i] < p) {
		x[i] <- weibull(u2[i], b1, t1)
	} else {
		x[i] <- weibull(u2[i], b2, t2)
	}
}

print(x)

hist(x, main="Mixed Weibull Distribution, parameters (2, 1, 1.5, 1, 0.4), 50 values", xlab="Range of random numbers", ylab="Density")
dev.copy(png,"plot3.png");
dev.off ();