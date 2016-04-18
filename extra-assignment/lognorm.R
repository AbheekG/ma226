f<-function(b)
{
	u<-runif(1)
	return (((1+b)-sqrt((1+b)^2 - 4*b*u))/(2*b))
}

d<-c(-1,-0.5,0,0.5,1)
x<-1
j<-1
for(j in 1:5)
{
	for(i in 1:10000)
	{
		if(d[j]!=0)
			x[i]<-qlnorm(f(d[j]),meanlog=0,sdlog=1,lower.tail=TRUE,log.p=FALSE)
		else
			x[i]<-qlnorm(runif(1),meanlog=0,sdlog=1,lower.tail=TRUE,log.p=FALSE)
	}


cat("\nThe Mean of the Distributon calculated is ",mean(x))
cat("\nThe Varinace of the Distributon calculated is ",var(x))
cat("\n")
h=ecdf(x)
plot( h,col="red", xlab="", ylab="Cumulative Distribution", main=paste("\nExperimental CDF of X \nlambda = ",d[i],"\nMean = ",mean(x),"\n Variance = ",var(x))) 
hist(x,ylim=c(0,1),probability='TRUE')
lines(density(x), col='darkorange')

}
