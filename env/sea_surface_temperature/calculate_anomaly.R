    
    sourcedir <- './sst_all_1982-2019/'
    filenames <- list.files(sourcedir, "*.csv", full.names=TRUE)
    temperature2list <- function(i){
      
        df <- read.csv(i)
        temp <- df[,3]
        return(temp)
    }
    
    templist <- lapply(filenames, temperature2list)
    
    df <- do.call(cbind.data.frame, templist)
    colnames(df) <- 1982:2019
    df$mean <- rowMeans(df)
    df$anomaly <- df$`2019`-df$mean
    
    anomaly <- matrix(df$anomaly, 50, 68)
    write.table(anomaly, './sst_anomaly_allyear_mean_till_2019.csv', row.names = F, col.names = F, sep=',')
