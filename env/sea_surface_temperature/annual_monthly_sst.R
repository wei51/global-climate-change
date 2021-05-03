
# annual mean temperature -------------------------------------------
    sourcedir <- './2_noaa_sst_csv/year/'
    filenames <- list.files(sourcedir, "*.csv", full.names=TRUE)
    temperature2list <- function(i){
      
      df <- read.csv(i)
      temp <- mean(df[,3], na.rm=T)
      return(temp)
    }
    
    templist <- lapply(filenames, temperature2list)
    annual_mean_temperature<- do.call(cbind.data.frame, templist)
    colnames(annual_mean_temperature) <- 1982:2019

# max monthly mean temperature in whole year ----------------------
#rm(list=ls())
    sourcedir <- './2_noaa_sst_csv/month/'
    folders <- list.files(sourcedir, full.names = T)
    
    calculate_monthy_mean <- function(i){
      
      df <- read.csv(i)
      temp <- mean(df[,3], na.rm=T)
      return(temp)
    }
    
    temperature2list <- function(i){
      
      files <- list.files( i, full.names=TRUE)
      templist <- lapply(files, calculate_monthy_mean)
      print(templist)
      return(max(unlist(templist)))
    }
    
    templist <- lapply(folders, temperature2list)
    max_monthly_mean_temperature<- do.call(cbind.data.frame, templist)
    colnames(max_monthly_mean_temperature) <- 1982:2019


# max monthly mean temperature in whole year ----------------------
#rm(list=ls())
    sourcedir <- './2_noaa_sst_csv/month/'
    folders <- list.files(sourcedir, full.names = T)
    
    calculate_monthy_mean <- function(i){
      
      df <- read.csv(i)
      temp <- mean(df[,3], na.rm=T)
      return(temp)
    }
    
    temperature2list <- function(i){
      
      files <- list.files( i, full.names=TRUE)
      templist <- lapply(files, calculate_monthy_mean)
      print(templist)
      return(min(unlist(templist)))
    }
    
    templist <- lapply(folders, temperature2list)
    min_monthly_mean_temperature<- do.call(cbind.data.frame, templist)
    colnames(min_monthly_mean_temperature) <- 1982:2019


# Jan/Feb mean temperature -----------------------------------------
#rm(list=ls())
    sourcedir <- './2_noaa_sst_csv/month/'
    folders <- list.files(sourcedir, full.names = T)
    
    temperature2list <- function(i){
      
      files <- list.files( i, full.names=TRUE)
      print(files)
      jan <- read.csv(files[1])
      feb <- read.csv(files[2])
      df <- rbind(jan, feb)
      temp <- mean(df[,3], na.rm=T)
      return(temp)
    }
    
    templist <- lapply(folders, temperature2list)
    janfeb_mean_temperature<- do.call(cbind.data.frame, templist)
    colnames(janfeb_mean_temperature) <- 1982:2019

# safety check
  # a <- read.csv('./2_noaa_sst_csv/month/1983/198301.csv')
  # b <- read.csv('./2_noaa_sst_csv/month/1983/198302.csv')
  # 
  # a1 <- mean(a[,3], na.rm=T)
  # b1 <- mean(b[,3], na.rm=T)
  # m <- (a1+b1)/2


# Jan/Feb max monthly mean -----------------------------------------
#rm(list=ls())
    sourcedir <- './2_noaa_sst_csv/month/'
    folders <- list.files(sourcedir, full.names = T)
    
    temperature2list <- function(i){
      
      files <- list.files( i, full.names=TRUE)
      print(files)
      jan <- read.csv(files[1])
      mjan <- mean(jan[,3], na.rm=T)
      feb <- read.csv(files[2])
      mfeb <- mean(feb[,3], na.rm=T)
      return(max(mjan, mfeb))
    }
    
    templist <- lapply(folders, temperature2list)
    janfeb_max_monthly_mean_temperature<- do.call(cbind.data.frame, templist)
    colnames(janfeb_max_monthly_mean_temperature) <- 1982:2019


# Jan/Feb min monthly mean -----------------------------------------
#rm(list=ls())
    sourcedir <- './2_noaa_sst_csv/month/'
    folders <- list.files(sourcedir, full.names = T)
    
    temperature2list <- function(i){
      
      files <- list.files( i, full.names=TRUE)
      print(files)
      jan <- read.csv(files[1])
      mjan <- mean(jan[,3], na.rm=T)
      feb <- read.csv(files[2])
      mfeb <- mean(feb[,3], na.rm=T)
      return(min(mjan, mfeb))
    }
    
    templist <- lapply(folders, temperature2list)
    janfeb_min_monthly_mean_temperature<- do.call(cbind.data.frame, templist)
    colnames(janfeb_min_monthly_mean_temperature) <- 1982:2019



# visualize ----
    jpeg(filename = './2_result.jpg', width = 16, height = 12, units = 'in', res = 300)
      par(mar=c(5,5,2,2))
      year <- 1982:2019
      plot( year, annual_mean_temperature, ylim=c(0,20), type='l', lwd=4, ylab="Temperature", col=1, 
            cex.lab=1.5, cex.axis=1.5)
      lines(year, max_monthly_mean_temperature, lwd=4, col=2)
      lines(year, min_monthly_mean_temperature, lwd=4, col=3)
      lines(year, janfeb_max_monthly_mean_temperature, lwd=4, col=4, lty=3)
      lines(year, janfeb_mean_temperature, lwd=4, col=5, lty=3)
      lines(year, janfeb_min_monthly_mean_temperature, lwd=4, col=6, lty=3)
      
      legend("topright", c('annual_mean_temperature',
                           'max_monthly_mean_temperature',
                           'min_monthly_mean_temperature',
                           'janfeb_max_monthly_mean_temperature',
                           'janfeb_mean_temperature',
                           'janfeb_min_monthly_mean_temperature'),
             col=c(1,2,3,4,5,6),
             lty=c(1,1,1,3,3,3),
             lwd=3,
             cex=1.2,
             bty="n")
    dev.off()

# correlation test ----
    
    plot(as.numeric(annual_mean_temperature), as.numeric(janfeb_mean_temperature),
         xlab='Annual_mean_temperature',
         ylab='Jan/Feb_mean_temperature',
         pch=16)
    abline(lm(as.numeric(janfeb_mean_temperature)~as.numeric(annual_mean_temperature)))
    cor.test(as.numeric(annual_mean_temperature), as.numeric(janfeb_mean_temperature))


# visulaize correlation ----
library("ggpubr")
    data <- as.data.frame(cbind(as.numeric(annual_mean_temperature), as.numeric(janfeb_mean_temperature)))
    colnames(data) <- c("annual_mean_temperature", "janfeb_mean_temperature")
    jpeg(filename = './2_correlation.jpg', width = 6, height = 6, units = 'in', res = 300)
      par(mar=c(5,5,2,2))
      ggscatter(data,
                x = "annual_mean_temperature", y = "janfeb_mean_temperature", 
                add = "reg.line", conf.int = TRUE, 
                cor.coef = TRUE, cor.method = "pearson",
                xlab='Annual_mean_temperature',
                ylab='Jan/Feb_mean_temperature',
                cex.lab=1.5,
                cex=1.5)
    dev.off()