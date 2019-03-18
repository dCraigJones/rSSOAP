# source("./R/SSOAP.R")

load("./data/DF.RData")

#Q <- DF$Hollybrook
Q <- DF$McMillan1[1:390]+DF$McMillan2[1:390]

Max.Daily.Flow = ceiling(max(Q)/1e6)

#Max.RDII = 1500

Ev <- Get.Events(DF$date, Q, DF$rain)

RD <- Get.RDII(DF$date, Q, DF$rain)

PU <- Get.Rain(DF$rain)

if(!exists("H") | sum(!(Q==Q.prev))>0) {
  H <- matrix(c(rep(0,15*length(Ev))), ncol=15)

  for (i in 1:nrow(H)) {
    H[i,] <- Get.Hydrograph(RD, PU, -21:21+Ev[i])
  }
}



layout(matrix(c(1,2,3,4,4,4), ncol=2, byrow = FALSE), widths=c(1,3))


DWF <- Get.DWF(DF$date, Q, DF$rain)
barplot(c(DWF$weekend, DWF$weekday)/1e3
        , main="DWF"
        , names.arg=c("wkend", "wkday")
        #, names.arg=paste(prettyNum(round(c(DWF$weekday, DWF$weekend)/1e3,0), big.mark = ","), "kGPD")
        , col=c("grey90", "white")
        , horiz=TRUE
        , xlab="Daily Flow (kGPD)"
)
box(bty="l")

GWI <- Get.GWI(DF$date, Q, DF$rain)
GWI <- GWI[GWI>0]
plot(density(GWI/1e3)
     , main="GWI"
     , xlab="kGPD"
     , lwd=2
     , bty="l"
)

R <- DF$rain[Ev]
I <- apply(H,1,max)*R
fit <- lm(I/1e3~R)
x1 <- round(cor(R,I),2)
x2 <- round(coef(fit)[2],2)
plot(R,I/1e3
     , main="RDII"
     , xlim=c(0,6)
     , ylim=c(0,x2*6)
     , cex=1.5
     , pch=4
     , lwd=2
     , ylab="kGPD/inch"
     , xlab="Rain (inch)"
     , bty="l"
)
abline(fit, lty=2)
x1 <- round(cor(R,I),2)
x2 <- round(coef(fit)[2],2)

par(mar=c(6,5,3,5)) #Btm, Left, Top, Right
plot(DF$date, Q/1e6
     , lwd=2
     , type="l"
     , xaxs="i"
     , yaxs="i"
     , xlab=NA
     , ylab="Daily Flow (MGD)"
     , axes=FALSE
     , ylim=c(0,Max.Daily.Flow)
)

lines(DF$date, DF$rain*Max.Daily.Flow/10, type="h", col="blue")
Month <- c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")

axis(1, at=DF$date[which(day(DF$date)==1)], labels=rep(Month,2))
axis(1, at=DF$date[c(which(yday(DF$date)==1),length(DF$date))], labels=c(2017,2018,2019), line=2, lwd=0)
axis(2)
axis(4, at=seq(0,Max.Daily.Flow,length.out=6), labels=seq(0,10,2))

mtext("Rain (inches)", side=4, line=3, cex=0.7)
box(lwd=1)

mDWF <- (DWF$weekday+DWF$weekend)/2
MA <- unname(max(DWF)+quantile(GWI,0.9)+x2*5*1e3)
Jax5YR <- unname(max(DWF)+quantile(GWI,0.9)+x2*6.5*1e3)
Jax25YR <- unname(max(DWF)+quantile(GWI,0.9)+x2*9.5*1e3)
q75th <- as.numeric(quantile(DF$rain[DF$rain>0.5],0.75)*x2+DWF[1]/1e3+quantile(GWI,0.75)/1e3)*1e3
abline(h=c(mDWF, q75th, MA, Jax5YR, Jax25YR)/1e6, lty=2)
axis(2, at=c(mDWF, q75th, MA, Jax5YR, Jax25YR)/1e6, labels=c("DWF", "3Q", "2YR", "5YR", "25YR"), line=1, lwd=0)
axis(4, at=c(mDWF, q75th, MA, Jax5YR, Jax25YR)/1e6, labels=c("DWF", "3Q", "2YR", "5YR", "25YR"), line=1, lwd=0)

legend("top"
       , c("Flow", "Rain")
       , lwd=c(2,2)
       , lty=c(1,1)
       , col=c("black", "blue")
       , inset=c(0.05,0.05)
       , seg.len = 4
       , pch=c(NA,NA)
       , cex=1.0
       , y.intersp=1.0
       , bty = "n"
       , box.col=rgb(1,1,1,0.75)
       , bg=rgb(1,1,1,0.75)
       , horiz=FALSE
       , text.font=2
)

Q.prev <- Q

# text(mdy("11/01/2018"),mDWF/1e6, "DWF", pos=3)
# text(mdy("11/01/2018"),MA/1e6, "MA", pos=3)
# text(mdy("11/01/2018"),Jax5YR/1e6, "5-YR", pos=3)
# text(mdy("11/01/2018"),Jax25YR/1e6, "25-YR", pos=3)
