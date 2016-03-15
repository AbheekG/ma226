n <- 50
lamda <- 2
u <- runif(n)
p0 <- exp(-lamda)
x <- vector(,n)
pms <- vector(,10)

for(j in 1:n)
{
	p <- p0
	f <- p
	i <- 0
	repeat {
		if(u[j] < f) {
			x[j] <- i
			if(i <= 10) {
				pms[i] <- pms[i] + 1
			}			
			break
		}
		p <- (lamda * p) / (i+1)
		f <- f + p
		i <- i + 1
	}
}

print(x[1:50])

pms <- pms/sum(pms)
cdf <- cumsum(pms)

hist(x, main="Poisson Distribution, mean = 2, 50 values", xlab="Range of random numbers", ylab="Density")
dev.copy(png,"plot2a.png");
dev.off ();

plot(1:10, pms, col='black', cex=1, main="Poisson Distribution, mean = 2, Probability Mass Function", xlab="Range of random numbers", ylab="Probability Mass Function")
dev.copy(png,"plot2b.png");
dev.off ();

plot(1:10, cdf, col='black', cex=1, main="Poisson Distribution, mean = 2, Cumulative Distribution Function", xlab="Range of random numbers", ylab="Cumulative distribution Function")
dev.copy(png,"plot2c.png");
dev.off ();
