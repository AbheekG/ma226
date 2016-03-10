#taking 3 p randomly.
q <- c(0.5, 0.8, 0.25)

#no of random numbers
n <- 50

u <- runif(n)

for (i in 1:3)
{
	r <- as.integer(log(u)/log(q[i])) + 1
	print(r)
	hist(r, main=paste("Geometric Distribution for about 50 values with p = ", 1-q[i]) , xlab="Range of random numbers", ylab="Density", breaks=50)
	dev.copy(png, paste("plot1_",i,".png") )
	dev.off ()
}