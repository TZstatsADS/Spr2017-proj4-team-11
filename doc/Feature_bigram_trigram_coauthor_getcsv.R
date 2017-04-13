load("../data/data.RData")
library(stringr)

df_bigram <- function(df){
  n <- nrow(df)
  score <- data.frame(matrix(0, ncol = 1, nrow = n^2))
  colnames(score) <- "bigram"
  rownames(score) <- paste("(", rep(0:(n-1), each = n),",", rep(0:(n-1), n), ")", sep = "")

  title_list <- list(NA)
  for (i in 1:n) {
      title_list[[i]] <- strsplit(df[i, 3], split = " ")
      title_list[[i]] <- tolower(unlist(title_list[[i]]))
      title_list[[i]] <- str_replace_all(title_list[[i]],"[[:punct:]]","") 
      temp_vec <- unlist(title_list[[i]])
      title_list[[i]] <- temp_vec[temp_vec != ""
                                  & temp_vec != "a" 
                                  & temp_vec != "an" 
                                  & temp_vec != "and"
                                  & temp_vec != "or"
                                  & temp_vec != "for"
                                  & temp_vec != "to"
                                  & temp_vec != "of"
                                  & temp_vec != "the"
                                  & temp_vec != "in"
                                  & temp_vec != "what"
                                  & temp_vec != "how"
                                  & temp_vec != "why"
                                  & temp_vec != "by"
                                  & temp_vec != "on"
                                  & temp_vec != "its"
                                  & temp_vec != "their"]
  }
  title_list[lapply(title_list,length) < 1] <- "untitled"
  
  for (i in 1:n) {
    for (j in 1:n) {
      for (a in 1:(length(unlist(title_list[[i]]))-1) ) {
        for (b in 1:(length(unlist(title_list[[j]]))-1) ) {
          if (paste(title_list[[i]][a], title_list[[i]][a+1], sep = "") == paste(title_list[[j]][b], title_list[[j]][b+1], sep = "")) {
            score[(i-1)*n+j, 1] <- score[(i-1)*n+j, 1] + 1
          }
        }
      }
    }
  }
  return(score)
}



df_trigram <- function(df){
  n <- nrow(df)
  score <- data.frame(matrix(0, ncol = 1, nrow = n^2))
  colnames(score) <- "trigram"
  rownames(score) <- paste("(", rep(0:(n-1), each = n),",", rep(0:(n-1), n), ")", sep = "")
  
  title_list <- list(NA)
  for (i in 1:n) {
    title_list[[i]] <- strsplit(df[i, 3], split = " ")
    title_list[[i]] <- tolower(unlist(title_list[[i]]))
    title_list[[i]] <- str_replace_all(title_list[[i]],"[[:punct:]]","") 
    temp_vec <- unlist(title_list[[i]])
    title_list[[i]] <- temp_vec[temp_vec != ""
                                & temp_vec != "a" 
                                & temp_vec != "an" 
                                & temp_vec != "and"
                                & temp_vec != "or"
                                & temp_vec != "for"
                                & temp_vec != "to"
                                & temp_vec != "of"
                                & temp_vec != "the"
                                & temp_vec != "in"
                                & temp_vec != "what"
                                & temp_vec != "how"
                                & temp_vec != "why"
                                & temp_vec != "by"
                                & temp_vec != "on"
                                & temp_vec != "its"
                                & temp_vec != "their"]
  }
  title_list[lapply(title_list,length) < 1] <- "untitled"
  
  for (i in 1:n) {
    for (j in 1:n) {
      for (a in 1:(length(unlist(title_list[[i]]))-2) ) {
        for (b in 1:(length(unlist(title_list[[j]]))-2) ) {
          if( paste(title_list[[i]][a], title_list[[i]][a+1], title_list[[i]][a+2], sep = "") == paste(title_list[[j]][b], title_list[[j]][b+1], title_list[[j]][b+2], sep = "")) {
            score[(i-1)*n+j, 1] <- score[(i-1)*n+j, 1] + 1
          }
        }
      }
    }
  }
  return(score)
}



df_coauthor <- function(df){
  n <- nrow(df)
  score <- data.frame(matrix(0, ncol = 1, nrow = n^2))
  colnames(score) <- "coauthor"
  rownames(score) <- paste("(", rep(0:(n-1), each = n),",", rep(0:(n-1), n), ")", sep = "")
  
  for (i in 1:n){
    for (j in 1:n){
      lengthi <- length(unlist(df[[5]][i]))
      lengthj <- length(unlist(df[[5]][j]))
      
      if (lengthi > 0 & lengthj > 0 ){
        aaa <- matrix(0,nrow=lengthi,ncol=lengthj)
        for (a in 1:lengthi){
          for (b in 1:lengthj){
            aaa[a,b] <- ifelse((df[[5]][[i]][a] ==   df[[5]][[j]][b]),aaa[a,b]+1,aaa[a,b])
          }
        }
        score[(i-1)*n+j, 1] <- sum(aaa)
      }
      
    }
  }
  return(score)
}


for (i in 9:12) {
  output1 <- df_bigram(data[[i]])
  output2 <- df_trigram(data[[i]])
  output3 <- df_coauthor(data[[i]])
  output <- cbind(output1,output2,output3)
  write.csv(output, file = paste("../data/feature_bi_tri_coauth_", i, ".csv", sep = ""))
}