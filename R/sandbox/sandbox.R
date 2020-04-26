rollapply(tmp
          , width=7
          , FUN=function(x) {
              fit <- lm(tmp[,2]~ymd(tmp[,1]), na.rm=TRUE)
              return(coef(fit))
          }
)


rollapply(tmp, 7, mean

data("DF")
tmp <- data.frame(date=DF$date, val=DF$Water)

library(zoo)

plot(tmp, ylim=c(0,1e7), type="l")

a <- (tmp$val-mean(tmp$val))/sd(tmp$val)

b <- rollapply(DF$Hollybrook,7, function(x) beta(x), fill=NA)

plot(DF$date, DF$Hollybrook, type="l")
bool <- abs(b) > 1.96*sd(b, na.rm=TRUE)
lines(DF$date, as.numeric(bool)*1e10, col="red")
#is there a way to check a high slope followed by a low slope?

plot(tmp$date
      , rollapply(DF$Hollybrook,7, function(x) beta(x), fill=NA)
      , type="l"
      )

beta <- function(y) {
  #x <- rnorm(7)
  x <- seq_along(y)

  b <- (length(x)*sum(x*y)-sum(x)*sum(y))/(length(x)*sum(x^2)-sum(x)^2)

  return(b)
}

set.seed(42)
tmp <- rnorm(7)
beta(tmp)
lm(seq_along(tmp)~tmp)

