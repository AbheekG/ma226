#BY WEIBULL DISTRIBUTION
library(cubature)
f<-function(x)
{
	return (a*b)*(x^b)*exp(-1*a*x^b);
}
g<-function(t)
{
	return (-1/a)*log(1/(1-t))^(1/b);
}
a<-1
b<-2
d<-c(-1,-0.5,0,0.5,1)
x<-1
j<-1
exp<-0
for(i in 1:5)
{
	while(j<=10000)
	{
		u<-runif(1)
		if(d[i]!=0)
			x[j]=(1/a)*(-1*log(((d[i]-1)+ sqrt((1+d[i])^2 - 4*u*d[i]))/(2*d[i])))^(1/b)
		else
			x[j]=(1/a)*(-1*log(1-u))^(1/b)
		j=j+1
	}

	j<-1
	exp<-(1+d[i])*integrate(f(x),0,1,1000000)+integrate(g(t),0,1,1000000)

	cat("\nThe Mean of the Distributon calculated is ",mean(x))
	cat("\nThe Varinace of the Distributon calculated is ",var(x))
	cat("\nThe expected value of the new Distributon calculated is(theoretical) ",exp)
	cat("\n")
	h=ecdf(x)
	plot( h,col="red", xlab="", ylab="Cumulative Distribution", main=paste("\nExperimental CDF of X \nlambda = ",d[i],"\nMean = ",mean(x),"\n Variance = ",var(x))) 
	hist(x,probability='TRUE')
	lines(density(x), col='darkorange')
}
