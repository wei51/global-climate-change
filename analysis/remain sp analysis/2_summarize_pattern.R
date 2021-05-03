library(dplyr)
library(mgcv)
load("./f_Mean_species_sensitivity_Dataframe.RData")
load("./remain_species.RData")
trend <- read.csv("./trend.csv")


visualize_GAM <- function(df){
  Year <- 1982:2011
  for( i in 1:nrow(df)){
    model <- gam( as.numeric(df[i,-c(1:3)]) ~ s(Year, k=floor((30-sum(is.na(df[i,-c(1:2)])))/5)))
    par(new=ifelse(i==1, FALSE, TRUE))
    plot(model, 
         #se=T, 
         #residuals=T, 
         pch=19, 
         cex=0.25, 
         lwd=2,
         ylab = "",
         yaxt = "n",
         xlim = c(1982, 2011),
         scheme=1, 
         col=ifelse(df[i,"Trend"]=="concave down",rgb(255, 0, 0, 50, maxColorValue=255),
             ifelse(df[i,"Trend"]=="concave up",rgb(255, 165, 0, 50, maxColorValue=255),
             ifelse(df[i,"Trend"]=="double peak",rgb(0, 255, 0, 50, maxColorValue=255),
             ifelse(df[i,"Trend"]=="reverse s shape",rgb(0, 255, 255, 50, maxColorValue=255),
                    rgb(0, 0, 128, 50, maxColorValue=255))))),
         shade=F, 
         shade.col=c(0,0,0,80)
         )
  }
}
FID <- row.names(Mean.Sensitivity.df)
x <- as.data.frame(cbind(FID, as.data.frame(Mean.Sensitivity.df)))
species <- read.csv("./0_Input/Species.csv")
df <- left_join(species, x, by="FID")
df <- subset(df, rowSums(is.na(df[,-c(1,2)]))!=28)
HA <- read.csv("./HabitatAffinity.csv")


a <- left_join(remain_sp, trend, by=c("FID","Species"))
a <- left_join(a, HA, by="FID")
b <- a[ ,c("FID","Species","Trend","HA")]
c <- left_join(b, df, by=c("FID","Species"))



visualize_GAM(c)

visualize_GAM_respectively <- function(df,col){
  Year <- 1982:2011
  for( i in 1:nrow(df)){
    model <- gam( as.numeric(df[i,-c(1:4)]) ~ s(Year, k=floor((30-sum(is.na(df[i,-c(1:4)])))/5)))
    par(new=ifelse(i==1, FALSE, TRUE))
    plot(model, 
         #se=T, 
         #residuals=T, 
         lty=ifelse(df[i,"HA"]=="demersal", 1, 2),
         cex=0.25, 
         lwd=6,
         ylab = "",
         yaxt = "n",
         xlim = c(1982, 2011),
         scheme=1, 
         col= col,
         shade=F, 
         shade.col=c(0,0,0,50),
         main = ifelse(i==1,df$Trend,"")
    )
  }
}

col <- c(rgb(237, 15, 7, 80, maxColorValue=255),    #concave down
         rgb(237, 130, 7, 80, maxColorValue=255),    #concave up
         rgb(237, 199, 7, 80, maxColorValue=255),  #s shape
         rgb(7, 237, 145, 80, maxColorValue=255),    #reverse s shape
         rgb(7, 218, 237, 80, maxColorValue=255),    #m shape
         rgb(7, 133, 237, 80, maxColorValue=255),  #w shape
         rgb(7, 49, 237, 80, maxColorValue=255),    #m+revS
         rgb(126, 7, 237, 80, maxColorValue=255),    #w+s
         rgb(128, 128, 128, 80, maxColorValue=255),  #linear
         rgb(211, 211, 211, 80, maxColorValue=255))

i <- subset(c, c$Trend=="concave down")
j <- subset(c, c$Trend=="concave up")
k <- subset(c, c$Trend=="s shape")
l <- subset(c, c$Trend=="reverse s shape")
m <- subset(c, c$Trend=="m shape")
n <- subset(c, c$Trend=="w shape")
o <- subset(c, c$Trend=="m+revS")
p <- subset(c, c$Trend=="w+S")
q <- subset(c, c$Trend=="linear")

visualize_GAM_respectively(i, col[1])
visualize_GAM_respectively(j, col[2])
visualize_GAM_respectively(k, col[3])
visualize_GAM_respectively(l, col[4])
visualize_GAM_respectively(m, col[5])
visualize_GAM_respectively(n, col[6])
visualize_GAM_respectively(o, col[7])
visualize_GAM_respectively(p, col[8])
visualize_GAM_respectively(q, col[9])


freq <- c(9,16,2,2,11,1,1,1,36,3)
lbls <- c("concave down",
          "concave up",
          "s shape",
          "reverse s shape",
          "m shape",
          "w shape",
          "m+revS",
          "w+s",
          "linear",
          "undefined")

pie(freq,
    labels = lbls,
    col = c(rgb(237, 15, 7, 80, maxColorValue=255),    #concave down
            rgb(237, 130, 7, 80, maxColorValue=255),    #concave up
            rgb(237, 199, 7, 80, maxColorValue=255),  #s shape
            rgb(7, 237, 145, 80, maxColorValue=255),    #reverse s shape
            rgb(7, 218, 237, 80, maxColorValue=255),    #m shape
            rgb(7, 133, 237, 80, maxColorValue=255),  #w shape
            rgb(7, 49, 237, 80, maxColorValue=255),    #m+revS
            rgb(126, 7, 237, 80, maxColorValue=255),    #w+s
            rgb(128, 128, 128, 80, maxColorValue=255),  #linear
            rgb(211, 211, 211, 80, maxColorValue=255)),   #undefined
    border = NA
    )
