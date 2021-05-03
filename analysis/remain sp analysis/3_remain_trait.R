library(dplyr)
library(mgcv)

load("./all_info_remain_sp.RData")
remain <- data
load("./all_info_sig_sp_44.RData")
sig <- data
trend <- read.csv("./trend.csv")
remain <- left_join(remain, trend, by=c("FID","Species"))


cd <- subset(remain, remain$Trend=="concave down")
cu <- subset(remain, remain$Trend=="concave up")
ss <- subset(remain, remain$Trend=="s shape")
rss <- subset(remain, remain$Trend=="reverse s shape")
m <- subset(remain, remain$Trend=="m shape")
w <- subset(remain, remain$Trend=="w shape")
ms <- subset(remain, remain$Trend=="m+revS")
ws <- subset(remain, remain$Trend=="w+S")
l <- subset(remain, remain$Trend=="linear")

clist <- list(cd,cu,ss,rss,m,w,ms,ws,l)

col <- c(rgb(237, 15, 7, 100, maxColorValue=255),    #concave down
         rgb(237, 130, 7, 100, maxColorValue=255),    #concave up
         rgb(237, 199, 7, 100, maxColorValue=255),  #s shape
         rgb(7, 237, 145, 100, maxColorValue=255),    #reverse s shape
         rgb(7, 218, 237, 100, maxColorValue=255),    #m shape
         rgb(7, 133, 237, 100, maxColorValue=255),  #w shape
         rgb(7, 49, 237, 100, maxColorValue=255),    #m+revS
         rgb(126, 7, 237, 100, maxColorValue=255),    #w+s
         rgb(128, 128, 128, 100, maxColorValue=255),  #linear
         rgb(211, 211, 211, 100, maxColorValue=255))

str <- "LRS"
X <- na.omit(c(sig[ ,str], remain[,str]))


jpeg(filename = paste0("./2_remain_trait_analysis/",str,".jpeg"), width = 4320, height = 480)
layout(matrix(1:9, 1,9))
par(mar=c(3,7,1,1))
for(i in 1:9){
  print(i)
  hist(X, col=rgb(207, 207, 207,100, maxColorValue=255),
       border=FALSE,
       breaks=seq(floor(min(X)), ceiling(max(X)), (ceiling(max(X))-floor(min(X)))/20),
       xlab="",
       ylab=ifelse(i==1, str, " "),
       main="",
       yaxt=ifelse(i==1, "s", "n"),
       cex.lab=3)
  abline(v=median(X), lty=1, col="darkgrey", lwd=3)
  abline(v=mean(X), lty=2, col="darkgrey", lwd=3)
  ifelse(na.omit(clist[[i]][,str])!=0,
         hist(na.omit(clist[[i]][,str]), col = col[i], add=T, 
              border=FALSE,
              breaks=seq(floor(min(X)), ceiling(max(X)), (ceiling(max(X))-floor(min(X)))/20)),
         next)
  if(i==9){
    legend("topright",
           legend = c("median","mean"),
           col = "darkgrey",
           lty = c(1,2),
           lwd = 3,
           cex = 3,
           bty = "n")
  }
}
dev.off()

# Discrete ----
X <- na.omit(c(sig[ ,"HA"], remain[,"HA"]))
jpeg(filename = "./2_remain_trait_analysis/HA.jpeg", width = 4320, height = 480)
layout(matrix(1:9, 1,9))
par(mar=c(3,7,1,1))
for(i in 1:9){
  x <- barplot(table(X),
          col=rgb(207, 207, 207,100, maxColorValue=255),
          border=FALSE, 
          ylim=c(0,130),
          ylab=ifelse(i==1,"HA"," "), 
          yaxt=ifelse(i==1, "s", "n"),
          cex.lab=3)
  barplot(table(clist[[i]][,"HA"]), add=TRUE, col=col[i], border=FALSE,
          ylab=ifelse(i==1,"HA"," "), 
          yaxt=ifelse(i==1, "s", "n"))
  text(x, table(X), labels=table(X), pos=3, cex=1.5, col="darkgrey")
  text(x, table(clist[[i]][,"HA"]), labels=table(clist[[i]][,"HA"]), pos=3, cex=1.5, col="darkgrey")
}
dev.off()

