library(stringdist)
library(RecordLinkage)


edit_dis<-function(df,type){
  paper<-df$paperTitle
  n<-length(paper)
  dis<-c(NULL)
  
  if(n==1){dis<-0}
  else{
  dis_mat<-stringdistmatrix(paper,method = "dl")#calculate edit distance
  dis_mat<-as.matrix(dis_mat)
  
  dis<-dis_mat[upper.tri(dis_mat)]}
  
  #edit distance similarity
  
  similarity<-c(NULL)
  
  if(n==1){similarity<-1}
  else{
  
  for(i in 1:n-1){
    for(j in (i+1):n){
      similarity<-c(similarity,levenshteinSim(paper[i],paper[j]))
    }
  }}
  
  
  if(type=="average"){return(list(edit_distance= mean(dis),similarity=mean(similarity)))}
  if(type=="min"){return(list(edit_distance= max(dis),similarity=min(similarity)))}
  if(type=="max"){return(list(edit_distance= min(dis),similarity=max(similarity)))}
}