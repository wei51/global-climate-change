
#load("./all_info.RData")
load("./all_info_sig_sp.RData")


data$HA <- gsub("demersal", 0, data$HA)
data$HA <- gsub("pelagic", 1, data$HA)

pca <- prcomp(data[,6:10], scale=T)
summary(pca)
# biplot(pca, xlim=c(-0.4,0.4), ylim=c(-0.4, 0.4),
#        xlabs=rep(".", nrow(data)))
# abline(h=0,v=0,lty=2)
# screeplot(pca,type="l")


library(factoextra)
pca <- prcomp(data[,5:9], scale=T)
par(mar=c(2, 2, 2, 2))
fviz_pca_biplot(pca,
                geom = c("point"),
                xlim=c(-4, 4),
                ylim=c(-4,4),
                title="",
                col.ind="darkgrey",
                pointsize=2,
                labelsize=5)+
theme(  #axis.line=element_blank(),
        #axis.text.x=element_blank(),
        #axis.text.y=element_blank(),
        #axis.ticks=element_blank(),
        #axis.title.x=element_blank(),
        #axis.title.y=element_blank(),
        #axis.title = element_text(size = 7.5),
        #axis.text = element_text(size = 7.5)
        #legend.position="none",
        #panel.background=element_blank(),
        #panel.border=element_blank(),
        #panel.grid.major=element_blank(),
        #panel.grid.minor=element_blank(),
        text = element_text(size=15),
        plot.background=element_blank())

