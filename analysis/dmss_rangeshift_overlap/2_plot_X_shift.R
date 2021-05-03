
load("./all_info_sig_sp.RData")
data <- data[order(data[,"x_Coefficient"]),]  #X_centroid排序
data$Index <- 1:38
par(mar=c(5,5,5,10))
plot(data$x_Coefficient, data$Index, 
     type = 'p',
     xlim = c(-0.3, 0.3),
     ylab = " ",
     xlab = "Slope of Longitudinal Range Shift",
     pch = ifelse(data$y_pvalue<0.05,19,1),
     yaxt = "n",
     cex = 1,
     cex.axis = 1
     )
abline(v=0, lty=5, col="lightgrey", lwd=2)
ytick<-seq(1,38, by=1)
axis(side=4, at=ytick, labels = FALSE)
text(par("usr")[2]+0.03, ytick,   
     labels = data$Species, pos=4, xpd = TRUE, cex=0.8, font = 3, srt = 0, offset=0)

legend("topleft",
       legend = c("significant shifting",
                  "non-significant shifting"),
       pch = c(19,1),
       cex = 0.8,
       bty = "n")

