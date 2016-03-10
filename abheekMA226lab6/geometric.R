#taking 3 p randomly.
q <- c(0.5, 0.8, 0.25)

#no of random numbers
n <- 50

u <- runif(n)

for (i in 1:3)
{
	r <- as.integer(log(u)/log(q[i])) + 1
	print(1-q[i])
	print(r)
	hist(r, main=paste("Geometric Distribution for about 50 values with p = ", 1-q[i]) , xlab="Range of random numbers", ylab="Density", breaks=50)
	if(i == 1)
		dev.copy(png, "plot1_1.png")
	if(i == 2)
		dev.copy(png, "plot1_2.png")
	if(i == 3)
		dev.copy(png, "plot1_3.png")
	dev.off ()
}