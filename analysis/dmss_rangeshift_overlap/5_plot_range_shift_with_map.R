library(maps)
library(RColorBrewer)
#load("./all_info_remain_sp.RData")
load("./all_info_sig_sp.RData")
df <- data
#df$Index <- 1:83
df$Index <- 1:38

#write.csv(df[ order(df$Species),c("Species","Index")], file="~/Desktop/remain_list.csv", row.names = F)

rbPal <- colorRampPalette(c('red','blue'))
par(mar=c(5,5,5,5))
plot(df$Start_X, df$Start_Y,
     xlim=c( min(df$Start_X), max(df$Start_X) ),
     ylim=c( min(df$Start_Y), max(df$Start_Y) ),
     xlab="Longitude",
     ylab="Latitude",
     pch=ifelse(df$HA=="pelagic",17,19),
     cex=0.8,
     cex.lab = 1.5,
     cex.axis = 1.5,
     col=ifelse(df$Status=="Increase","#E37222","#07889B")
     )
map("world", 
    xlim=c(-6,10),ylim=c(50,62),
    col="gray90", 
    fill=TRUE,
    add=T,
    type="polygon",
    border="grey90")

arrows(df$Start_X, df$Start_Y,
       df$Start_X+ df$x_Coefficient*5, df$Start_Y+df$y_Coefficient*5,
       angle=45,
       length=0.05,
       code=2,
       lwd = 2)

#text(df$Start_X, df$Start_Y+0.1,ifelse(df$Index==20, df$Index," ") ,cex=0.5, col="green")

