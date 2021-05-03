library(dplyr)
library(mgcv)
load("./f_Mean_species_sensitivity_Dataframe.RData")
load("./remain_species.RData")
trend <- read.csv("./trend.csv")
Linear.Regression <- function(folder,df){
  Year <- 1982:2011
  for( i in 1:nrow(df)){
    model <- gam( as.numeric(df[i,-c(1:2)]) ~ s(Year, k=floor((30-sum(is.na(df[i,-c(1:2)])))/5)))
    jpeg(paste0(folder,df[i,"FID"],".jpeg"), res=75, width=500, height=500)
    plot(model, se=T, residuals=T, pch=19, cex=0.25, lwd=1.8,
         xlim = c(1982, 2011),
         scheme=1, col='black' , shade=T, shade.col='pink',
         main = paste0(df[i,"Species"]," / ",df[i,"FID"]))
    text(1996,1.5,
         labels = paste0("Deviance explained: ", round((summary(model)$dev.exp)*100, 2),"%"),
         pos = 4, font = 2)
    text(1996,1.4,
         labels = paste0("p-value : ", round(summary(model)$s.pv, 5)), pos = 4, font = 2)
    text(1996,1.3,
         labels = paste0("k(smooth-term) : ", floor((30-sum(is.na(df[i,-c(1:2)])))/5)), pos = 4, font = 2)
    dev.off()
  }
}

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

a <- left_join(remain_sp, trend, by=c("FID","Species"))
b <- subset(a[ ,c("FID","Species","Trend")], a$Trend!="linear"&a$Trend!="")
c <- left_join(b, df, by=c("FID","Species"))
Linear.Regression("./1_pattern_ananlysis/",c)

visualize_GAM(c)

visualize_GAM_respectively <- function(df,col){
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
         col= col,
         shade=F, 
         shade.col=c(0,0,0,80),
         main = ifelse(i==1,df$Trend,"")
    )
  }
}
cd <- subset(c, c$Trend=="concave down")
cu <- subset(c, c$Trend=="concave up")
dp <- subset(c, c$Trend=="double peak")
rs <- subset(c, c$Trend=="reverse s shape")
sh <- subset(c, c$Trend=="s shape")
visualize_GAM_respectively(cd, rgb(255, 0, 0, 50, maxColorValue=255))
visualize_GAM_respectively(cu, rgb(255, 165, 0, 50, maxColorValue=255))
visualize_GAM_respectively(dp, rgb(0, 255, 0, 50, maxColorValue=255))
visualize_GAM_respectively(rs, rgb(0, 255, 255, 50, maxColorValue=255))
visualize_GAM_respectively(sh, rgb(0, 0, 128, 50, maxColorValue=255))


pie(table(a$Trend),
    labels = c("undefined","concave down","concave up","double peak","linear","reverse s shape","s shape"),
    col = c("grey",
            rgb(255, 0, 0, 50, maxColorValue=255),
            rgb(255, 165, 0, 50, maxColorValue=255),
            rgb(0, 255, 0, 50, maxColorValue=255),
            "lightgrey",
            rgb(0, 255, 255, 50, maxColorValue=255),
            rgb(0, 0, 128, 50, maxColorValue=255)
            ))
