use <- hf %>%
  mutate(dayhour=wday(date)+hour(date)/24) %>%
  group_by(dayhour) %>%
  summarize(bsf=mean(flow))

use2 <- hf %>%
  mutate(dayhour=wday(date)+hour(date)/24) %>%
  left_join(use, by="dayhour") %>%
  mutate(wwf=flow-bsf) %>%
  mutate(wwf2=ifelse(wwf<0,0,wwf))

uh.1 <- shape_daily_hydrograph(use2$date, use2$wwf2, use2$rain, 48, 0.1, 10)

test <- model_hydrograph(use2$rain, uh.1)

plot(use2$wwf2, type="l")
lines(test, col="red")

plot(0,0,type="n", ylim=c(0,10000), xlim=c(0,15))
lines((seq_along(uh.1)-1)/24, uh.1*24)
lines((seq_along(uh.2)-1), uh.2)
lines((seq_along(uh.3)-1), uh.3)

a <- data.frame(x=(seq_along(uh.1)-1), y=uh.1*24)
b <- data.frame(x=(seq_along(uh.2)-1)*24, y=uh.2)
c <- data.frame(x=(seq_along(uh.3)-1)*24, y=uh.3)

a_ <- approx(b$x, b$rdi, xout=1:336)$y
b_ <- approx(b$x, b$rdi, xout=1:336)$y
c_ <- approx(c$x, c$rdi, xout=1:336)$y

a_[is.na(a_)]=0
b_[is.na(b_)]=0
c_[is.na(c_)]=0

uh <- cbind(a_, b_, c_)

plot(apply(uh, 1, sum))
