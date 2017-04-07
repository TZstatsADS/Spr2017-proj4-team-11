# This script contains three feature selection functions given a dataframe with partition

load("../data/data.RData")

# Use AGupta as an example, simulate a partition T
# by randomly putting about 200 of the 577 observations into cluster 1 and the other rest into cluster 2
# Cluster assignment is appended as column "t", taking value 1 or 2
df <- data[[1]]
df$t <- sample(1:2, 577, prob = c(200/577, 377/577), replace = TRUE)

# Fuction f_bigram(dataframe) outputs total number of pairwise overlapping title bigrams in each cluster
f_bigram <- function(df){
  bigram_score <- rep(0, length(unique(df$t)))
  for (j in sort(unique(df$t)) ) {
    df_t <- df[df$t == j, ]
    title_list <- list(NA)
    index <- 1:(nrow(df_t)-1)
    
    for (i in 1:nrow(df_t)) {
      title_list[[i]] <- strsplit(df_t[i, 3], split = " ")
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
                                  & temp_vec != "why"]
    }
    
    for (m in index) {
      for (n in index[-(1:m)]) {
        for (a in 1:(length(unlist(title_list[[m]]))-1) ) {
          for (b in 1:(length(unlist(title_list[[n]]))-1) ) {
            
            if (paste(title_list[[m]][a], title_list[[m]][a+1], sep = "") == paste(title_list[[n]][b], title_list[[n]][b+1], sep = "")) {
              if ( (a-1)==0 | (b-1)==0 ) {
                bigram_score[j] <- bigram_score[j]+1
              } else if ( title_list[[m]][a-1] != title_list[[n]][b-1] ) {
                bigram_score[j] <- bigram_score[j]+1
              } else {
                bigram_score[j] <- bigram_score[j]
              }
            }

          }
        }
      }
    }
    
  }
  return(bigram_score)
}


# Function f_trigram(dataframe) outputs total number of pairwise overlapping title trigrams in each cluster
f_trigram <- function(df){
  trigram_score <- rep(0, length(unique(df$t)))
  for (j in sort(unique(df$t)) ) {
    df_t <- df[df$t == j, ]
    title_list <- list(NA)
    index <- 1:(nrow(df_t)-1)
    
    for (i in 1:nrow(df_t)) {
      title_list[[i]] <- strsplit(df_t[i, 1], split = " ")
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
                                  & temp_vec != "why"]
    }
    
    for (m in index) {
      for (n in index[-(1:m)]) {
        for (a in 1:(length(unlist(title_list[[m]]))-2) ) {
          for (b in 1:(length(unlist(title_list[[n]]))-2) ) {
            trigram_score[j] <- ifelse( paste(title_list[[m]][a], title_list[[m]][a+1], title_list[[m]][a+2], sep = "") == paste(title_list[[n]][b], title_list[[n]][b+1], title_list[[n]][b+2], sep = ""),
                                       trigram_score[j]+1, trigram_score[j])
          }
        }
      }
    }
    
  }
  return(trigram_score)
}


# Function f_coauthor(dataframe) outputs total number of pairwise overlapping coauthorsin each cluster
f_coauthor <- function (df) {
  coauthor_score <- rep(0, length(unique(df$t)))
  
  for (i in 6:(ncol(df)-1)) {
    df[,i] <- as.character(df[,i])
  }
  
  for (j in sort(unique(df$t)) ) {
    df_t <- df[df$t == j, ]
    coauthor_list <- list(NA)
    
    for (i in 1:nrow(df_t)) {
      coauthor_list[[i]] <- paste(df_t[i, 6:(ncol(df_t)-1)])
      coauthor_list[[i]] <- coauthor_list[[i]][coauthor_list[[i]] != "NA"]
    }
    coauthor_list <- coauthor_list[lapply(coauthor_list,length) > 0]
    index <- 1:(length(coauthor_list)-1)
    
    for (m in index) {
      for (n in index[-(1:m)]) {
        for (a in 1:length(unlist(coauthor_list[[m]])) ) {
          for (b in 1:length(unlist(coauthor_list[[n]])) ) {
            coauthor_score[j] <- ifelse(coauthor_list[[m]][a] == coauthor_list[[n]][b],
                                        coauthor_score[j]+1, coauthor_score[j])
          }
        }
      }
    }
    
  }
    
  return(coauthor_score)
}
