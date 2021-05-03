library(RNetCDF)
library(RColorBrewer)

    bathy_fname <- "gebco_2020_n61.75_s49.5_w-4.0_e12.75.nc"
    nc <- open.nc(bathy_fname)
    tmp <- read.nc(nc)
    z <- array(tmp$elevation, dim=dim(tmp$elevation))
    z <- z[,(seq(ncol(z)))]
    lon<-tmp$lon
    lat<-tmp$lat
    #make palette
    ocean.pal <- colorRampPalette(
                  c("#08519C","#2171B5","#4292C6","#6BAED6",
                    "#9ECAE1","#C6DBEF","#DEEBF7","#F7FBFF"))
    zbreaks <- seq(-1689, 2345, by=10)
    cols <-c(rep("#08306B", sum(zbreaks<(-200))), 
             ocean.pal(sum( zbreaks<=0 & zbreaks>=(-200))-1), 
             rep("#c5c7c9", sum(zbreaks>0)))
    layout(matrix(1:2, 1,2), widths=c(6,1.5), heights=c(6))
    par(mar=c(3,2,1,1), ps=10)
    image(lon, lat, 
          z = z, 
          col = cols, 
          breaks = zbreaks,
          useRaster = TRUE,
          xlab = "Longitude",
          ylab = "Latitude",
          main = "",
          axes=FALSE)
    axis(1, at=c(0,10), labels=c('0','10'))
    axis(2, las=TRUE, at=c(50,60), labels=c('50','60'))
    
    a <- zbreaks[zbreaks<0]
    col_bar <- cols[1:length(zbreaks[zbreaks<0])]
    col <- unique(col_bar)
    par(mar=c(3,0,1,5))
    image(x=0.5, y=a[a>-200], 
          z=matrix(a[a>-200], 1, length(a[a>-200])), 
          col=col[1:length(a[a>-200])-1], 
          breaks=a[a>-200], 
          useRaster=TRUE, 
          xlab="", 
          ylab="", 
          axes=FALSE)
    axis(4, 
         at=seq(-200, 0, 50), 
         las=2,
         labels = c(">200","150","100","50",""),
         cex = 1.2)
