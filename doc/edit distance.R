library(stringdist)
library(RecordLinkage)


edit_dis<-function(df){
  paper<-df$Paper
  dis_mat<-stringdistmatrix(paper,method = "dl")#calculate edit distance
  dis_mat<-as.matrix(dis_mat)
  
  dis<-dis_mat[upper.tri(dis_mat)]
  
  #edit distance similarity
  n<-length(paper)
  similarity<-c(NULL)
  
  for(i in 1:n-1){
    for(j in (i+1):n){
      similarity<-c(similarity,levenshteinSim(paper[i],paper[j]))
    }
  }
  
  
  return(list(max=c(max(dis),max(similarity)),
              min=c(min(dis),min(similarity)),
              average=c(mean(dis),mean(similarity))))
}