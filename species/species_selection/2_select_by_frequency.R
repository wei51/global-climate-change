library(dplyr)

    rm(list=ls())
    load("./1_probabilistic_model/2_Grid_after_Probabilistic_Model.RData")
    
    get.grid <- function(i){
    
      a <- as.data.frame(t(i))
      b <- a[-1,]
      b$KLabFID <- rownames(b)
      colnames(b) <- c(1:850,"KLabFID")
      return(b)
    }
    
    get.species.list <- function(i){
      b <- as.data.frame(t(i[,-1]))
      return(rownames(b))
    }
    
    allign <- function(i){
      a <- as.data.frame(i)
      a$Presence <- 1
      colnames(a) <- c("KLabFID", "Presence")
      b <- left_join(sp, a, by="KLabFID")
      b[is.na(b)] <- 0
      return(b$Presence)
    }
    
    # Main -----
      
      grid.list <- lapply(GRID.PM,function(i) get.grid(i))
      species.list <- lapply(GRID.PM,function(i) get.species.list(i))
      names(species.list) <- 1983:2020
      sp <- as.data.frame(unique(unlist(species.list)))
      colnames(sp) <- "KLabFID"
      alligned.species.list <- lapply(species.list,function(i) allign(i))
      
      
      
      species.df<- do.call(cbind.data.frame, alligned.species.list)
      species.df <- cbind(sp, species.df)
      species.df$Freq <- rowSums(species.df[,-1])
      t <- subset(species.df, species.df$Freq>=5) # target
      
      
      sp.list <- t$KLabFID
      GRID.PM.Freq <- list()
      for(i in 1:length(GRID.PM)){
        df <- as.data.frame(t(GRID.PM[[i]][ ,names(GRID.PM[[i]]) %in% sp.list]))
        df$KLabFID <- rownames(df)
        colnames(df) <- c(1:850,"KLabFID")
        GRID.PM.Freq[[i]] <- df
      }
      
      sp.list <- as.data.frame(sp.list)
      colnames(sp.list)<-"KLabFID"
      GRID.PM.Freq.Alligned <- list()
      for(i in 1:length(GRID.PM.Freq)){
        df <- GRID.PM.Freq[[i]]
        GRID.PM.Freq.Alligned[[i]] <- left_join(sp.list, df,by="KLabFID")
      }
      
      save(GRID.PM.Freq, file="./2_frequency/1_Intermediate.RData")  
      save(sp.list, GRID.PM.Freq.Alligned, file="./2_frequency/2_data_for_calculate.RData")  
      