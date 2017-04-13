load("../data/data.RData")
library(stringr)

df_bigram <- function(df){
  num <- nrow(df)
  score <- data.frame(matrix(0, ncol = 3, nrow = num^2))
  colnames(score) <- c("bigram","i","j")
  rownames(score) <- paste("(", rep(0:(num-1), each = num),",", rep(0:(num-1), num), ")", sep = "")
  score["i"] <- rep(1:num, each = num)
  score["j"] <- rep(1:num, num)
  
  title_list <- list(NA)
  for (i in 1:num) {
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
  
  for (i in 1:num) {
    for (j in 1:num) {
      for (a in 1:(length(unlist(title_list[[i]]))-1) ) {
        for (b in 1:(length(unlist(title_list[[j]]))-1) ) {
          if (paste(title_list[[i]][a], title_list[[i]][a+1], sep = "") == paste(title_list[[j]][b], title_list[[j]][b+1], sep = "")) {
            score[score$i == i & score$j == j, "bigram"] <- score[score$i == i & score$j == j, "bigram"] + 1
          }
        }
      }
    }
  }
  return(score)
}



df_trigram <- function(df){
  num <- nrow(df)
  score <- data.frame(matrix(0, ncol = 3, nrow = num^2))
  colnames(score) <- c("trigram","i","j")
  rownames(score) <- paste("(", rep(0:(num-1), each = num),",", rep(0:(num-1), num), ")", sep = "")
  score["i"] <- rep(1:num, each = num)
  score["j"] <- rep(1:num, num)
  
  title_list <- list(NA)
  for (i in 1:num) {
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
  
  for (i in 1:num) {
    for (j in 1:num) {
      for (a in 1:(length(unlist(title_list[[i]]))-2) ) {
        for (b in 1:(length(unlist(title_list[[j]]))-2) ) {
          if( paste(title_list[[i]][a], title_list[[i]][a+1], title_list[[i]][a+2], sep = "") == paste(title_list[[j]][b], title_list[[j]][b+1], title_list[[j]][b+2], sep = "")) {
            score[score$i == i & score$j == j, "trigram"] <- score[score$i == i & score$j == j, "trigram"] + 1
          }
        }
      }
    }
  }
  return(score)
}



df_coauthor <- function(df){
  num <- nrow(df)
  score <- data.frame(matrix(0, ncol = 3, nrow = num^2))
  colnames(score) <- c("coauthor","i","j")
  rownames(score) <- paste("(", rep(0:(num-1), each = num),",", rep(0:(num-1), num), ")", sep = "")
  score["i"] <- rep(1:num, each = num)
  score["j"] <- rep(1:num, num)
  
  coauthor_list <- list(NA)
  for (i in 1:num) {
    coauthor_list[[i]] <- unlist(df[i,5])
    coauthor_list[[i]] <- coauthor_list[[i]][coauthor_list[[i]] != "NA"]
  }
  coauthor_list[lapply(coauthor_list,length) < 1] <- "independent work"
  
  for (i in 1:num) {
    for (j in 1:num) {
      if(length(intersect(unlist(coauthor_list[[i]]), unlist(coauthor_list[[j]]))) != 0) {
        score[score$i== i & score$j == j, "coauthor"] <- length(intersect(unlist(coauthor_list[[i]]), unlist(coauthor_list[[j]])))
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
  output <- output[, c(1,4,7)]
  write.csv(output, file = paste("../data/feature_bi_tri_coauth_", i, ".csv", sep = ""))
}