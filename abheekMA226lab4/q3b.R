#Discrete acceptance rejection method
n <- 100000	 #No of values generated, taking 1000 as 10 is too small for any measurement
p <- c(0.05, 0.25, 0.45, 0.15, 0.10)
x <- c(1:5)
c <- max(p)/0.2

u <- runif(n)
v <- runif(n)
y <- as.integer(5*u) + 1

rd <- y[ v*0.45 < p[y] ]

#print(rd)

cat("Mean, Theoretical = ", sum(p*x), ", Stimulated = ", mean(rd), "\n")
cat("Variance, Theoretical = ", (sum(p*x^2) - 9), ", Stimulated = ", var(rd), "\n")
hist(rd, main="Discrete Distribution (for about 100000 values)", xlab="Range of random numbers", ylab="Density", breaks=50)
dev.copy(png,"plot3b.png");
dev.off ();
rm(list = ls())