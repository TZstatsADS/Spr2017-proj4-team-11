singlecluster <- function(ssim){
  Lm <- as.data.frame(ssim*(-1))
  Lm[is.na(Lm)] <- 0
  N <- nrow(Lm)
  merge <- matrix(0, N-1, 2)
  height <- vector(length = N-1)
  diag(Lm) <- Inf
  
  colnames(Lm) <- -(1:N)
  rownames(Lm) <- -(1:N)
  
  for (m in 1:(N-1)) { 
    cols <- colnames(Lm)
    
    # Find the pair with the most common coauthors
    
    # The which() function returns the row and column position of the pair
    d <- which(Lm == min(Lm), arr.ind = TRUE)[1,,drop=FALSE]
    if (min(Lm) <= (-1)){
      height[m] <- min(Lm) # The height is the value of the pair with the most common coauthors 
      
      # The row and column position of the most common pair is stored as sequence m in the merge object
      merge[m,] <- as.numeric(cols[d])
      
      # The pair with the minimum distance(most coauthors) is merged
      
      # The cluster object is used to find previous clusters that the pair belong to (if they exist)
      # Does this by finding any columns above 0 (since all column names are negative, a positive column value implies it has been clustered)
      cluster <- c(d, which(cols %in% cols[d[1, cols[d] > 0]]))
      
      colnames(Lm)[cluster] <- m # Rename the columns indicated by cluster to the sequence number, m
      
      # Merge the pairs according to single linkage method
      sl <- apply(Lm[d,], 2, min)
      
      # Remove column and row corresponding to old clusters and insert a new column and row for newly formed cluster.
      
      # The insertion of the cluster is done by setting the first sequential row and column of the minimum pair in the distance matrix (top to bottom, left to right) as the cluster resulting from the single linkage step
      Lm[min(d),] <- sl
      Lm[,min(d)] <- sl
      
      # Make sure the minimum distance pair is not used again by setting it to Inf
      Lm[min(d), min(d)] <- Inf
      
      # The removal step is done by setting the second sequential row and column of the minimum pair
      # (farthest right, farthest down) to Inf
      Lm[max(d),] <- Inf
      Lm[,max(d)] <- Inf
    }
  }
  a <- colnames(Lm)
  return(a)
}

# treat each single-element cluster as one cluster 
splitcluster <- function(df){
  abc <- as.data.frame(table(df))
  abc <- abc[order(abc$Freq,decreasing = T),]
  n <- length(df)
  ncluster <- nrow(abc)
  cluster.result <- matrix(0,nrow=n,ncol=1)
  for (i in 1:ncluster){
    a <- which(df==abc$df[i])
    cluster.result[a] <- i
  }
  return(cluster.result)
}

# combine all singl-element cluster into one cluster
comninecluster <- function(df){
  abc <- as.data.frame(table(df))
  abc <- abc[order(abc$Freq,decreasing = T),]
  # n <- nrow(df)
  n <- length(df)
  ncluster <- nrow(abc)
  cluster.result <- matrix(0,nrow=n,ncol=1)
  for (i in 1:ncluster){
    if (abc$Freq[i] > 1){
      a <- which(df==abc$df[i])
      cluster.result[a] <- i
    }
    else{
      cluster.result[a] <- 0
    }
    
  }
  return(cluster.result)
}