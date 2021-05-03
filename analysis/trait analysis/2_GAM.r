library(dplyr)
library(mgcv)
#load("./all_info.RData")
load("./all_info_sig_sp.RData")
### MGCV
df <- data

y <- df$dMSS
HA <- df$HA
BL <- df$BL
DR <- df$DR
TL <- df$TL
AAM <- df$AAM
LRS <- df$LRS

# 
m1 <- gam( y~ ti(BL))
summary(m1)
m2 <- gam( y~ s(DR))
m3 <- gam( y~ ti(TL))
m4 <- gam( y~ ti(AAM))
AIC(m1, m2, m3, m4)

m1 <- gam( y~ ti(BL)+ti(TL)+ti(AAM)+ti(DR))
m2 <- gam( y~ te(BL)+te(TL)+te(AAM)+te(DR))
m3 <- gam( y~ s(BL,k=3) +s(TL) +s(AAM) +s(DR,k=3))
m3 <- gam( y~ s(BL,k=3) +s(TL,k=3) +s(AAM,k=3) +s(DR,k=4))
m4 <- gam( y~ t2(BL)+t2(TL)+t2(AAM)+t2(DR))
AIC(m1, m2, m3, m4)
summary(m3)
par(mfrow=c(2,2))
plot(m3, se=T, residuals=T, pch=19, cex=0.25, lwd=1.8,
     scheme=1, col='black' , shade=T, shade.col='gray90')

m1 <- gam( y~HA)
m2 <- gam( y~s(DR))
m3 <- gam( y~s(BL))
m4 <- gam( y~s(AAM))
m5 <- gam( y~s(TL))
m6 <- gam( y~LRS)

summary(m1)
summary(m2)
summary(m3)
summary(m4)
summary(m5)
summary(m6)


#### ******* current used ****** ######
m6 <- gam( y~ HA + s(DR) + s(BL) + s(AAM) + s(TL)  )
m5 <- gam( y~      s(DR) + s(BL) + s(AAM) + s(TL) + LRS )
m4 <- gam( y~      s(DR) + s(BL) + s(AAM) + s(TL)  )
m3 <- gam( y~      s(DR) +         s(AAM) + s(TL)  )
m2 <- gam( y~      s(DR) +         s(AAM)   )
m1 <- gam( y~      s(DR)   )
AIC(m1, m2, m3, m4, m5, m6)


m8.0 <- gam( y~ HA + s(DR,k=4) + s(BL,k=3) + s(AAM,k=3) + s(TL,k=3) + LRS)
m8.1 <- gam( y~ HA + s(DR,k=4) + s(BL,k=3) + s(AAM,k=3) + s(TL,k=3) )
m8.2 <- gam( y~      s(DR,k=4) + s(BL,k=3) + s(AAM,k=3) + s(TL,k=3) )
m8.3 <- gam( y~      s(DR,k=4) +           + s(AAM,k=3) + s(TL,k=3) )
m8.4 <- gam( y~      s(DR,k=4) +                        + s(TL,k=3) )
m8.5 <- gam( y~      s(DR,k=4)                                      )
summary(m8.5)
AIC(m8.0, m8.1, m8.2, m8.3, m8.4, m8.5)
par(mfrow=c(2,2))
plot(m8.3, se=T, residuals=T, pch=19, cex=0.25, lwd=1.8,
     scheme=1, col='black' , shade=T, shade.col='gray90')



#Backward selection
m8.0 <- gam( y~ HA + ti(DR) + ti(BL) + ti(AAM) + ti(TL) + LRS)
m8.1 <- gam( y~ HA + ti(DR) + ti(BL) + ti(AAM) + ti(BL,AAM) + ti(TL))
m8.2 <- gam( y~      ti(DR) + ti(BL) + ti(AAM) + ti(BL,AAM) + ti(TL))
m8.3 <- gam( y~      ti(DR) + ti(BL) + ti(AAM) + ti(BL,AAM))
m8.4 <- gam( y~      ti(DR)          + ti(AAM) + ti(BL,AAM))
m8.5 <- gam( y~      ti(DR)          + ti(AAM))
m8.6 <- gam( y~      ti(DR))
AIC(m8.0, m8.1, m8.2, m8.3, m8.4, m8.5, m8.6)
par(mfrow=c(2,2))
gam.check(m8.3, old.style = TRUE)
par(mar=c(8, 8, 8, 8))
plot(m8.3, se=T, residuals=T, pch=19, cex=0.25, lwd=1.8,
     scheme=1, col='black' , shade=T, shade.col='gray90')

# ----
m0 <- gam( y~ te(DR) + te(BL) + te(AAM) + te(TL))
m1 <- gam( y~ te(DR)          + te(AAM) + te(TL))
m2 <- gam( y~ te(DR)          + te(AAM))
m3 <- gam( y~                   te(AAM))
AIC(m0, m1, m2, m3)
par(mfrow=c(2,2))
gam.check(m8.3, old.style = TRUE)
par(mar=c(8, 8, 8, 8))
plot(m0, se=T, residuals=T, pch=19, cex=0.25, lwd=1.8,
     scheme=1, col='black' , shade=T, shade.col='gray90')


# MuMIn test
library(MuMIn)

# Model selection
out.put <- model.sel(m8.3, m3)
importance(out.put)
model_avg <- model.avg(out.put, revised.var = TRUE, beta=F)
summary(model_avg)
res <- importance(model_avg)
dat <- res[c(4,3,2,1)]
jpeg('relative_importance.jpg',height=1000, width=1000)
par(mar=c(12, 8, 8, 8))
barplot(dat, horiz=T,
        names.arg=c("BL","TL","DR","AAM"),
        las=1,
        xlab=" ",
        border=F,
        col='darkgray',
        width=c(3,3,3,3,3,3,3),
        cex.names=3,
        cex.axis=2,
        cex.lab=3)
title(xlab = "Relative Importance", 
      cex.lab = 3,
      mgp = c(6, 2, 0))
dev.off()


# Reference----


sel.table<-as.data.frame(out.put)[9:13] 
sel.table[,2:3]<- round(sel.table[,2:3],2) 
sel.table[,4:5]<- round(sel.table[,4:5],3)
names(sel.table)[1] = "K" 
sel.table$Model<-rownames(sel.table) 
for(i in 1:nrow(sel.table)) sel.table$Model[i]<- as.character(formula(paste(sel.table$Model[i])))[3] 
sel.table<-sel.table[,c(6,1,2,3,4,5)]
sel.table 
write.csv(sel.table,file='./model_selection_table.csv', row.names = F)



m8.0 <- gam( y~ HA + ti(DR) + BL + AAM + TL + LRS)
m8.1 <- gam( y~ HA + DR + BL + AAM + TL )
out.put <- model.sel(m8.0, m8.1)
model_avg <- model.avg(out.put, revised.var = TRUE, beta=F)
summary(model_avg)


# relative importance
importance(out.put)
model_avg <- model.avg(out.put, revised.var = TRUE, beta=F)
summary(model_avg)


res <- importance(model_avg)
dat <- res[c(5,4,3,2,1)]

jpeg('relative_importance.jpg',height=1000, width=1000)
par(mar=c(8, 8, 8, 8))
barplot(dat, horiz=T,
        names.arg=c("BL","TL","LRS","","BL"),
        las=1,
        xlab="Relative Importance",
        border=F,
        col='darkgray',
        width=c(3,3,3,3,3,3,3),
        cex.names=3,
        cex.axis=2)
dev.off()
model_avg <- model.avg(get.models(out.put, subset = delta < 5))

ma_coefs <- coefTable(model_avg, full=TRUE, adjust.se = TRUE)

top_model <- get.models(output, subset = 1)[[1]]
confint(model_avg, full=T)

options(na.action = "na.fail")
all.parms<-gam( y~ HA + ti(DR) + ti(BL) + ti(AAM) + ti(TL) + LRS
                      + ti(DR,BL)
                      + ti(DR,AAM)
                      + ti(DR,TL))
results<-dredge(all.parms)
candidate <- get.models(results, subset = delta < 5)
top_model <- get.models(results, subset = 1)[[1]]
avg<-model.avg(results, subset= delta < 2, revised.var = TRUE)
