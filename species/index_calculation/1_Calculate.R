library(dplyr)

######## Function : Calculate Overlap ################
    calculate_overlap <- function(data){
      
      PA <- data[,c(-1:-2)]
      PA <- t(PA)
      ovlp <- matrix(nrow=nrow(data),ncol=nrow(data))
      colnames(ovlp) <- data$KLabFID
      rownames(ovlp) <- data$KLabFID
      for(i in 1:nrow(data)){
        print(i)
        for(j in 1:nrow(data)){
          ovlp[i,j] <- sum(PA[ ,i]==1 & PA[ ,j]==1)
        }
      }
      return(ovlp)
    }
    
######## Function : Calculate Sout #################
    
    calculate_Sout <- function(ovlp){
      
      sout <- matrix(nrow=nrow(ovlp),ncol=nrow(ovlp))
      rownames(sout) <- colnames(ovlp)
      colnames(sout) <- c(colnames(ovlp))
      for(i in 1:nrow(ovlp)){
        print(i)
        for(j in 1:nrow(ovlp)){
          if(i==j) sout[i,j] <- 0
          else sout[i,j] <- ovlp[i,j] / ovlp[i,i]
        }
      }
      Sout <- rowSums(sout,na.rm=T)
      sout <- cbind(sout,Sout)
      return(sout)
    }
    
######## Function : Output Data ######
    
    Output.Data.FUN <- function(l,address){
      setwd(address)
      for(i in 1:length(l)) write.csv(l[[i]],file=paste0(names(l[i]),".csv"))
    }
####### Function : 
    
    
    Calculate.Mean.Sensitivity <- function(x){
      x <- as.data.frame(x)
      for(i in 1:nrow(x)){
        print(i)
        y <- x[ ,1:ncol(x)-1]
        if(sum(is.na(y[i,]))!=0){
          noninteraction_sp <- sum(y[i, ]==0, na.rm=T)
          total_sp <- nrow(y)
          interacting_sp <- total_sp - noninteraction_sp
          x$interacting_sp[i] <- interacting_sp
          x$MSS[i] <- (x$Sout[i])/interacting_sp
        }else
        {x$MSS[i] <- x$Sout[i]}
      }
      return(x)
    }
    
    
###### Main #######
    
    
    load("./0_input_data/2_data_for_calculate.RData")
    Overlap.list <- lapply(GRID.PM.Freq.Alligned, calculate_overlap)
    Sout.list <- lapply(Overlap.list,calculate_Sout)
    
    Mean.Sensitivity.List <- lapply(Sout.list, Calculate.Mean.Sensitivity)
    # Output data
    
    Output.Data.FUN(Asymmetry.list, "C:\\Users\\USER\\Desktop\\1_Asymmetry")
    Output.Data.FUN(Sin.list, "C:\\Users\\USER\\Desktop\\2_Species_Robustness")
    Output.Data.FUN(Sout.list, "C:\\Users\\USER\\Desktop\\3_Species_Sensitivity")
    
    
