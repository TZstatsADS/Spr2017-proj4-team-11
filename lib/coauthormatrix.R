library(cluster)
library(plyr)

coauthor_matrix <- function(df){
  n <- nrow(df)
  # a matrix of the number of same coauthors 
  ssim <- matrix(rep(NA,n*n),ncol=n)
  
  for (i in 1:n){
    for (j in 1:n){
      lengthi <- length(unlist(df[[5]][i]))
      lengthj <- length(unlist(df[[5]][j]))
      if (lengthi >0 & lengthj >0 ){
        aaa <- matrix(0,nrow=lengthi,ncol=lengthj)
        for (a in 1:lengthi){
          for (b in 1:lengthj){
            aaa[a,b] <- ifelse((df[[5]][[i]][a] ==   df[[5]][[j]][b]),aaa[a,b]+1,aaa[a,b])
          }
        }
        ssim[i,j] <- sum(aaa)
      }
      
    }
  }
 return(ssim)   
}
  

  