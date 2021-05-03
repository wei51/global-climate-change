library(gmp)


    # Function #########
    
    Build_Species_List.FUN <- function(grid){
      x <- colnames(grid)
      KLabFID <- as.data.frame(x[2:length(x)])
      colnames(KLabFID) <- "KLabFID"
      return(KLabFID)
    }
    PRO.FUN <- function(N, N1, N2){
      J <- c(max(0,N1 + N2 - N):min(N1, N2))
      log.Pj <- NULL
      EXPECTj <- 0
      Outs <- NULL
      for (i in 1:length(J)){
        C1 <- log(chooseZ(N,J[i]))
        C2 <- log(chooseZ(N-J[i],N2-J[i]))
        C3 <- log(chooseZ(N-N2,N1-J[i]))
        C4 <- log(chooseZ(N,N2))
        C5 <- log(chooseZ(N,N1))
        log.Pj[i] <- C1+C2+C3-C4-C5
        EXPECTj[i] <- exp(log.Pj[i])*J[i]
      }
      Outs$J <- J
      Outs$log.Pj <- log.Pj
      Outs$Pj <- exp(log.Pj)
      Outs$EXPECTj <- EXPECTj
      Outs$Qexp <- sum(EXPECTj)
      return(Outs)
    }
    PRO.LIST.FUN <- function(df){
      name <- colnames(df[,-1])
      print(name)
      df <- apply(df[,-1],2,as.numeric)
      N <- nrow(df)
      PROi <- NULL
      PROj <- NULL
      namelist1 <- NULL
      namelist2 <- NULL
      for (i in 1:ncol(df)) {
        print(i)
        print(name[i])
        for (j in 1:ncol(df)) {
          N1 <- colSums(df)[i]
          N2 <- colSums(df)[j]
          Qobs <- sum(df[,i] == 1 & df[,j] == 1)
          PROj[[j]] <- PRO.FUN(N, N1, N2)
          PROj[[j]]$Qobs <- Qobs
          namelist1[j] <- paste(name[i],name[j],sep = '&')
        }
        PROi[[i]] <- setNames(PROj, namelist1)
        namelist2[i] <- name[i]
      }
      PROi <- setNames(PROi, namelist2)
      #colnames(PROi) <- colnames(df[-1])
      #rownames(PROi) <- colnames(df[-1])
      return(PROi)
    }
    
    # Define Positive/Negative/Random 
    PNR.FUN <- function(df) {
      PNR <- matrix(nrow = length(df), ncol = length(df))
    
      rownames(PNR) <- names(df)
      colnames(PNR) <- names(df)
      for (i in 1:length(df)) {
        for (j in 1:length(df)) {
          Qobs <- df[[i]][[j]]$Qobs
          place <- which(df[[i]][[j]]$J == Qobs)
          Plt <- sum(df[[i]][[j]]$Pj[1:place-1])
          Pgt <- sum(df[[i]][[j]]$Pj[(min(place +1, length(df[[i]][[j]]$Pj))):length(df[[i]][[j]]$Pj)])
          Pet <- df[[i]][[j]]$Pj[place]
          PNR[j,i] <- ifelse(Plt+Pet < 0.05, "Negative", ifelse(Pgt+Pet < 0.05, "Positive", "Random"))
        }
      }
      return(PNR)
    }
    
    # Calculate number of random relationship
    # RRC = Random Relationship Count
    Count.Random.Relationship.FUN <- function(pnr){
      rrc <- NULL
      for(i in 1:ncol(pnr)){
        rrc[i] <- sum(pnr[i,]=="Random")
      }
      KLabFID <- colnames(pnr)
      rrc <- cbind(KLabFID,rrc)
      return(rrc)
    }
    RRC <- lapply(PNR, Count.Random.Relationship.FUN)
    # Delete random species 
    Delete.Random.Species.FUN <- function(rrc){
      
      total.relation <- nrow(rrc)-1
      print(total.relation)
      df <- as.data.frame(rrc)
      df <- df[ df$rrc == total.relation, ]
      spe <- as.data.frame(df$KLabFID)
      colnames(spe) <- "KLabFID"
      return(spe)
    }
    #### Read in Grid Data #############
    
    in_path <- "./0_original_data/Grid/"
    files <- list.files(in_path, pattern="*.csv", full.names=T)
    
    #### Main ####
    
    GRID <- lapply(files, function(x) read.csv(x))
    names(GRID) <- 1983:2020
    SpeciesList <- lapply(GRID, Build_Species_List.FUN)
    #setwd( "C:\\Users\\USER\\Desktop\\SpeciesList")
    #for(i in 1:length(files)){
    #  write.csv(SpeciesList[[i]],file=files[i], row.names = F)
    #}
    PROB <- lapply(GRID, PRO.LIST.FUN)
    PNR <- lapply(PROB, PNR.FUN)
    RRC <- lapply(PNR, Count.Random.Relationship.FUN)
    DEL.SPE <- lapply( RRC, Delete.Random.Species.FUN)
    
    save(GRID,PROB,PNR,RRC,DEL.SPE, file = "./1_probabilistic_model/1_IntermediateData.RData")
    
    
    
    
    
    
    # Delete DEL.SPE from SpeciesList
    load("./1_probabilistic_model/1_IntermediateData.RData")
    GRID.PM <- list()
    for(i in 1:length(GRID)){
      GRID.PM[[i]] <- GRID[[i]][ ,!names(GRID[[i]]) %in% as.character(DEL.SPE[[i]]$KLabFID)]
    }
    
    save(GRID.PM, file = "./1_probabilistic_model/2_Grid_after_Probabilistic_Model.RData")
    







