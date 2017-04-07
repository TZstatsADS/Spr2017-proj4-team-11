# This script reorganizes profclean.RData into a list of 14 dataframes for easier access and processing

load("../data/profclean.RData")

getcolasvec <- function(data, listNum, colNum) {
  col <- rep(NA)
  for (i in 1:length(data[[listNum]])) {
    col[i] <- unlist(data[[listNum]][[i]][[colNum]])
  }
  return(col)
}

getcolaslist <- function(data, listNum, colNum) {
  col <- list(NA)
  for (i in 1:length(data[[listNum]])) {
    col[[i]] <- data[[listNum]][[i]][[colNum]]
  }
  return(col)
}

getdf <- function(data, listNum){
  col1 <- getcolasvec(data_list, listNum, 1)
  col2 <- getcolasvec(data_list, listNum, 2)
  col4 <- getcolasvec(data_list, listNum, 4)
  col5 <- getcolasvec(data_list, listNum, 5)
  col3 <- getcolaslist(data_list, listNum, 3)
  df <- data.frame(authorNum = col1, paperNum = col2, paperTitle = col4, journalTitle = col5)
  df$coauthor <- col3
  df[,3] <- as.character(df[,3])
  df[,4] <- as.character(df[,4])
  return(df)
}

data <- list(NA)
for (i in 1:14) {
  data[[i]] <- getdf(data_list, i)
}

