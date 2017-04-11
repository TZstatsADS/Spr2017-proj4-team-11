load("../data/data.RData")
library(stringr)

df_bigram <- function(df){
  num <- nrow(df)
  score <- data.frame(matrix(0, ncol = num, nrow = num))
  colnames(score) <- paste("(0,", 0:(num-1), ")", sep = "")
  rownames(score) <- paste("(", 0:(num-1), ",0)", sep = "")
  
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
            score[i,j] <- score[i,j]+1
          }
        }
      }
    }
  }
  
  return(score)
}



df_trigram <- function(df){
  num <- nrow(df)
  score <- data.frame(matrix(0, ncol = num, nrow = num))
  colnames(score) <- paste("(0,", 0:(num-1), ")", sep = "")
  rownames(score) <- paste("(", 0:(num-1), ",0)", sep = "")
  
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
            score[i,j] <- ifelse( paste(title_list[[i]][a], title_list[[i]][a+1], title_list[[i]][a+2], sep = "") == paste(title_list[[j]][b], title_list[[j]][b+1], title_list[[j]][b+2], sep = ""),
                                  score[i,j]+1, score[i,j])
        }
      }
    }
  }
  
  return(score)
}



df_coauthor <- function(df){
  num <- nrow(df)
  score <- data.frame(matrix(0, ncol = num, nrow = num))
  colnames(score) <- paste("(0,", 0:(num-1), ")", sep = "")
  rownames(score) <- paste("(", 0:(num-1), ",0)", sep = "")
  
  coauthor_list <- list(NA)
  for (i in 1:num) {
    coauthor_list[[i]] <- unlist(df[i,5])
    coauthor_list[[i]] <- coauthor_list[[i]][coauthor_list[[i]] != "NA"]
  }
  coauthor_list[lapply(coauthor_list,length) < 1] <- "independent work"
  
  for (i in 1:num) {
    for (j in 1:num) {
      for (a in 1:length(unlist(coauthor_list[[i]])) ) {
        for (b in 1:length(unlist(coauthor_list[[j]])) ) {
          score[i,j] <- ifelse(coauthor_list[[i]][a] == coauthor_list[[j]][b],
                                      score[i,j]+1, score[i,j])
        }
      }
    }
  }
  
  return(score)
}


for (i in 1:14) {
  output <- df_bigram(data[[i]])
  write.csv(output, file = paste("../data/bigram_", i, ".csv", sep = ""))
}

for (i in 1:14) {
  output <- df_trigram(data[[i]])
  write.csv(output, file = paste("../data/trigram_", i, ".csv", sep = ""))
}

for (i in 1:14) {
  output <- df_coauthor(data[[i]])
  write.csv(output, file = paste("../data/coauthor_", i, ".csv", sep = ""))
}