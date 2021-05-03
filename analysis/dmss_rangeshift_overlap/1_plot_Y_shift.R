
load("./all_info_sig_sp.RData")
data <- data[order(data[,"y_Coefficient"]),]  #Y_centroid排序
data$Index <- 1:38
par(mar=c(10,5,5,5))
plot(data$Index, data$y_Coefficient,
     type = 'p',
     ylim = c(-0.2, 0.2),
     xlab = " ",
     ylab = "Slope of Latitudinal Range Shift",
     pch = ifelse(data$y_pvalue<0.05,19,1),
     xaxt = "n",
     cex = 1.2,
     cex.axis = 1
     )
abline(h=0, lty=5, col="lightgrey", lwd=2)
xtick<-seq(1,38, by=1)
axis(side=1, at=xtick, labels = FALSE, padj = 2)
text(xtick,  par("usr")[3]-0.02, 
     labels = data$Species, pos=2, xpd = TRUE, cex=0.7, font = 3, srt = 45, offset = 0)

legend("topright",
       legend = c("significant shifting",
                  "non-significant shifting"),
       pch = c(19,1),
       cex = 0.8,
       bty = "n")

