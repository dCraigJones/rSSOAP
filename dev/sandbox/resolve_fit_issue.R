# plot(df$date, df$rdi, type="l")
# lines(df$date, df$rain*1000, col="blue", lwd=2)


# lines(df$date
#       , rollapply(df$rain, 30, sum, fill=NA)*100
#       , col="red"
#       , lwd=2
#       )


MODEL_DURATION=3
INITIAL_ABSTRACTION=0

rdi <- df$rdi
rdi <- zero_offset(rdi)
rdi[rdi<0]=0

r <- rain
r <- r - INITIAL_ABSTRACTION
r[r<0]=0
r[is.na(r)]=0

# Translate to time series for VAR
ii <- ts(cbind(r, rdi))
colnames(ii) <- c("r", "rdi")

# Estimate the model
var.1 <- VAR(ii, 2, type = "none")

# Calculate the IRF
ir.1 <- irf(var.1
            , impulse = "r"
            , response = "rdi"
            , n.ahead = MODEL_DURATION
            , ortho = TRUE
            , runs=100
            )

# Return upper limit
#uh <- (ir.1$Upper$r+ir.1$irf$r)/2
uh <- ir.1$Upper$r
uh[uh<0]=0


#PU <- lag_rain(rain)
PU <- lag_rain(r)
UH <- uh
U <- matrix(c(UH, rep(0,ncol(PU)-length(UH))), ncol=1)
Q.m <- PU%*%U

plot(df$date, rdi, type="l")
lines(df$date, Q.m*10, col="red")
lines(df$date, r*1000, col=rgb(0,0,1,0.5), lwd=1)

