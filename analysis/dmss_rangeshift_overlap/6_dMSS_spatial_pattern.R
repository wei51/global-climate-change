library(maps)
library(RColorBrewer)
# load("./all_info.RData")
# df$Index <- 1:89

load("./all_info_sig_sp_44.RData")
df <- data
df$Index <- 1:44



rbPal1 <- colorRampPalette(c("#07889B",'#d9f6fa'))
rbPal2 <- colorRampPalette(c('#f7dcc8','#E37222'))
df <- df[order(df$dMSS), ]
df$Col <- c(rbPal1(50)[as.numeric(cut(subset(df$dMSS, df$dMSS<0),breaks = 50))],
            rbPal2(50)[as.numeric(cut(subset(df$dMSS, df$dMSS>0),breaks = 50))])
                 

fig <- function(df){
  
  lon <- c(-4, 12.75)
  lat <- c(49.5, 61.75)
  par(mar=c(5,5,2,2))
  plot(df$Start_X, df$Start_Y,
       xlim=lon,
       ylim=lat,
       xlab="Longitude",
       ylab="Latitude",
       cex.lab = 1,
       cex.axis = 1.5,
       xaxt = "n",
       yaxt = "n",
       xaxs="i",
       yaxs="i",
       type="n"
  )
  #text(df$Start_X+0.1, df$Start_Y+0.1, df$Index,cex=0.5, col="red")
  map("world", 
      xlim=lon,
      ylim=lat,
      col="gray90", 
      fill=TRUE,
      add=T,
      type="polygon",
      border="grey90")
  points(df$Start_X, df$Start_Y,
         pch=ifelse(df$HA=="pelagic",17,19),
         cex=1.8,
         col=df$Col)
  axis(1, at=c(0,10), labels=c('0','10'))
  axis(2, las=TRUE, at=c(50,60), labels=c('50','60'))
  #text(df$Start_X+0.1, df$Start_Y+0.1, df$Species,cex=0.5, col="red")

}
fig(df)

for(i in 1:nrow(df)){
  
  ifelse( df[i,"dMSS"]>0,
          text(df[i,"Start_X"]+0.1, df[i,"Start_Y"]+0.1, df[i,"Species"],cex=0.5, col="red"),
          print("N") 
  )
  print(i)
text(df[i,"Start_X"]+0.1, df[i,"Start_Y"]+0.1, "        ",cex=0.5, col="red")


#ggplot(data = df, mapping = aes(x = Start_X, y = Start_Y)) + geom_point(aes(colour = dMSS), shape = 19)
color.bar <- function(lut, 
                      min, 
                      max, 
                      nticks, 
                      ticks=seq(min, max, len=nticks), 
                      title='',
                      dMSS) {
  scale = (length(lut)-1)/(max-min)
  print(ticks)
  dev.new(width=1.75, height=5)
  par(mar=c(5,5,5,5))
  plot(c(0,0.01), c(min,max), type='n', bty='n', xaxt='n', xlab='', yaxt='n', ylab='', main=title)
  axis(2, ticks, las=1, labels = FALSE)
  #text(0,max,"High")
  for (i in 1:(length(lut)-1)) {
    y = (i-1)/scale + min
    rect(0,y,0.01,y+1/scale, col=lut[i], border=NA)
  }
  axis(4, 
       seq( min, max, (max-min)/2),
       labels = c(min(df$dMSS), 0, max(df$dMSS)),
       las = 2)
}


jpeg(filename = './Figure/dMSS_spatial_distribution.jpeg', 
     width = 6, height = 6, units = 'in', res = 300)
layout(matrix(1:2, 1,2), widths=c(6,1.5), heights=c(6))
par(mar=c(3,2,1,1), ps=10)
fig(df)
dev.off()
par(mar=c(5,0,1,20))
color.bar(colorRampPalette(c("#07889B",'#d9f6fa','#f7dcc8','#E37222'))(100), 1, 30, 2, 
          title="Status",
          df$dMSS)


