library(maps)
library(fields)
    rotate <- function(x) t(apply(x, 2, rev))
    in_path <- ''
    DATRAS_anomaly <- as.matrix(read.table(paste0(in_path,'sst_anomaly_allyear_mean_till_2011.csv'),F,','))
    y2 <- seq(49.5,61.75,0.25)
    x2 <- seq(-4,12.75,0.25)
    
    jpeg(filename = './output/sst_anomaly_allyear_mean_till_2011.jpg', width=6, height=6, units='in', res=300)
    par(mar=c(5,5,2,2))
    image(x2, y2, 
          z = rotate(DATRAS_anomaly),
          zlim = c(-1,2),
          col = tim.colors(28), 
          #breaks = zbreaks,
          useRaster = TRUE,
          xlab = "Longitude",
          ylab = "Latitude",
          main = "",
          #add=TRUE,
          axes=FALSE,
          xaxs="i",
          yaxs="i",
          cex.lab=1.3)
    map("world", 
        xlim=c(-4, 12.75),
        ylim=c(49.5, 61.75),
        col="gray90", 
        fill=TRUE,
        add=TRUE,
        type="polygon",
        border="grey70")
    axis(1, at=c(0,5,10), labels=c('0','5','10'), cex.axis=1.2, lwd=1.5)
    axis(2, las=TRUE, at=c(50,55,60), labels=c('50','55','60'), cex.axis=1.2, lwd=1.5)
    box(which = "plot",   col = "black", lwd = 1.5)
    dev.off()
    
    
    # bar ----
    
    color.bar <- function(lut, 
                          min, 
                          max, 
                          nticks, 
                          ticks=seq(min, max, len=nticks), 
                          title='',
                          labels) {
      scale = (length(lut)-1)/(max-min)
      print(ticks)
      #dev.new(width=1.75, height=5)
      par(mar=c(2,2,5,5))
      plot(c(0,0.01), 
           c(min,max), 
           type='n', 
           bty='n', 
           xaxt='n', 
           xlab='', 
           yaxt='n',
           ylab='',
           xaxs="i",
           yaxs="i",
           main = title,
           cex.main = 1.3)
      for (i in 1:(length(lut)-1)) {
        y = (i-1)/scale + min
        rect(0,y,0.1,y+1/scale, col=lut[i], border=FALSE)
      }
      axis(4,
           at=seq(round(min,1),
                  round(max,1),
                  (max-min)/nticks),
           las=2,
           labels=labels,
           cex.axis = 1.2,
           tick=FALSE)
      box(which = "plot",   col = "black", lwd = 1.5)
      #box(which = "figure", col = "black", lwd = 1.5)
    }
    
    
    jpeg(filename = './output/2011_bar.jpg', width = 2.5, height = 15, units = 'in', res = 300)
    par(mar=c(5,0,1,20))
    color.bar(tim.colors(28), 
              min=-1,      #min(DATRAS_anomaly, na.rm = T)                      
              max=2,         #max(DATRAS_anomaly, na.rm = T)
              nticks=15, 
              title="SST anomaly",
              #labels=seq(min(DATRAS_anomaly, na.rm = T),max(DATRAS_anomaly, na.rm = T),0.2)
              labels=seq(-1,2,0.2)
    )
    dev.off()