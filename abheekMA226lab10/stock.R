m <- 500 # No. of paths
n <- 5000  # No of time points
k <- 0

# Time
t <- 5
dt <- t/n

# Properties of Stock selected by us
s0 <- 100   # initial price
Mu <- c(-0.1, 0.05, 0.1)
Sigma <- c(0.01, 0.02, 0.03)

# Some required extras
w5 <- vector(,m)
T <- seq(0, t, dt)
pal <- palette()

for (mu in Mu) {
    for (sigma in Sigma) {

        for (i in 1:m) {
            Z <- rnorm(n)
            S <- cumsum(c(log(s0), (mu - (sigma^2)/2)*dt + sigma*(dt^(1/2))*Z))
            S <- exp(S)
            w5[i] <- S[n + 1]
            if(i == 1) {
                plot(T, S,  col=pal[i %% 8 + 1], cex=0.00001, main=paste0("Stock price, S(0)=",toString(s0),", mu=",toString(mu)," sigma=",toString(sigma)), xlab="Time", ylab="Stock Value", type="l")
            } else if (i <= 10){
                lines(T, S, col=pal[i %% 8 + 1], cex=0.00001)
            }
        }
        k <- k + 1
        dev.copy(png,paste0("plot",toString(k),".png"));
        dev.off ();

        cat("\nStock price, S(0)=",s0,", mu=",mu," sigma=",sigma,"\\\\\\\\\n")
        cat(" Expected value of S(5), Theoretical = ", s0*exp(mu*t), ", Simulated = ", mean(w5),"\\\\\n")
        cat(" Variance of S(5) Theoretical = ", (s0^2)*(exp(2*mu*t)*(exp((sigma^2)*t) - 1)) , ", Simulated = ", var(w5),"\\\\\n\n")
        cat("\\includegraphics{",paste0("plot",toString(k)),"}\n")
        cat("\\pagebreak \n\n")

    }
}

rm(list = ls())
