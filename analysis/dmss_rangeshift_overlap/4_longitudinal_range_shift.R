

#load("./all_info.RData")
load("./all_info_sig_sp.RData")

df <- data
p <- subset(df, df$HA=="pelagic")
d <- subset(df, df$HA=="demersal")
t <- df



foo <- data.frame(label=c("Pelagic","Demersal","Total"),
                  mean=c(mean(p$x_Coefficient)*111,
                         mean(d$x_Coefficient)*111,
                         mean(t$x_Coefficient)*111),
                  sd=c(sd(p$x_Coefficient)*111,
                       sd(d$x_Coefficient)*111,
                       sd(t$x_Coefficient)*111))
par(mar=c(5,5,5,5))
x <- 1:3
plot(x,foo$mean,
     pch=19,
     xlab="",
     ylab="Longitudinal Range Shift (km/year)",
     xaxt="n",
     xlim=c(0.5,3.5),
     ylim=c(-15,15))
arrows(x, foo$mean-foo$sd, x, foo$mean+foo$sd, length=0.05, angle=90, code=3)
axis(side=1,at=1:3,labels=FALSE)
text(x, par("usr")[3]-2, 
     labels = foo$label, pos=3, xpd = TRUE, cex=0.8, srt = 0, offset=0)
abline(h=0, lty=5, col="lightgrey", lwd=2)
