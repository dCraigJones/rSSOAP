library(vars)


# synthetic data ----------------------------------------------------------

set.seed(123) # Reset random number generator for reasons of reproducability

# Generate sample
t <- 200 # Number of time series observations
k <- 2 # Number of endogenous variables
p <- 2 # Number of lags

# Generate coefficient matrices
A.1 <- matrix(c(-.3, .6, -.4, .5), k) # Coefficient matrix of lag 1
A.2 <- matrix(c(-.1, -.2, .1, .05), k) # Coefficient matrix of lag 2
A <- cbind(A.1, A.2) # Companion form of the coefficient matrices

# Generate series
series <- matrix(0, k, t + 2*p) # Raw series with zeros
for (i in (p + 1):(t + 2*p)){ # Generate series with e ~ N(0,0.5)
  series[, i] <- A.1%*%series[, i-1] + A.2%*%series[, i-2] + rnorm(k, 0, .5)
}

series <- ts(t(series[, -(1:p)])) # Convert to time series format
names <- c("V1", "V2") # Rename variables

plot.ts(series) # Plot the series


# impulse graphs ----------------------------------------------------------
rdii <- Get.RDII(DF$date, DF$Hollybrook, DF$rain)
r <- DF$rain
r[r<0.5]=0


ii <- ts(cbind(DF$rain, rdii))

var.1 <- VAR(ii, 2, type = "none") # Estimate the model

# Calculate the IRF
ir.1 <- irf(var.1, impulse = "X", response = "rdii", n.ahead = 20, ortho = FALSE)


# Plot the IRF
plot(ir.1)


plot(ir.1$irf$X, type="l", lwd=2, ylim=c(0,1e6))
lines(HB$`UH (GPD/Inch)`[1,])
lines(HB$`UH (GPD/Inch)`[2,])
lines(HB$`UH (GPD/Inch)`[3,])
lines(HB$`UH (GPD/Inch)`[4,])
lines(HB$`UH (GPD/Inch)`[5,])

UH <- apply(HB$`UH (GPD/Inch)`, 2, mean)

# imp <- ir.1$irf$X
# imp <- ir.1$Upper$X

PU <- Lag.Rain(DF$rain)
UH <- imp
U <- matrix(c(UH, rep(0,ncol(PU)-length(UH))), ncol=1)
Q.m <- PU%*%U

plot(rdii, type="l")
lines(Q.m, col="red", lwd=2)

imp <- ir.1$irf$X
imp <- ir.1$Upper$X

UH <- imp
U <- matrix(c(UH, rep(0,ncol(PU)-length(UH))), ncol=1)
Q.m <- PU%*%U

lines(Q.m, col="blue", lwd=2)

# try at various initial abstracts to minimize error.







library(rSSOAP)
library(vars)

s <- ts(data.frame(rain=DF$rain, flow=DF$Water))
var.1 <- VAR(s, 2, type="none")
ir.1 <- irf(var.1, impulse="rain", reponse="flow", n.ahead=30, ortho=FALSE)
plot(ir.1$irf$rain[,2])
plot(ir.1)
