

library(text2vec)
library(stringdist)
library(RecordLinkage)

tfidf_corr<-function(df) {
  if(nrow(df)==1){cos<-1}
  else{
    paper<- itoken(df$paperTitle, 
                   preprocessor = tolower, 
                   tokenizer = word_tokenizer,
                   ids = df$PaperID,
                   progressbar = FALSE)
    words <- create_vocabulary(paper, stopwords = c("a", "an", "the", "in", "on",
                                                    "at", "of", "above", "under","for"))
    vectorizer <- vocab_vectorizer(words)
    dtm_paper <- create_dtm(paper, vectorizer)
    tfidf <- TfIdf$new()
    dtm_paper_tfidf <- fit_transform(dtm_paper, tfidf)
    corr_mat<-sim2(x = dtm_paper, method = "cosine", norm = "l2")
    corr_mat<-as.matrix(corr_mat)
    
    
    #cos<-corr_mat[upper.tri(corr_mat)]}
    
    return(corr_mat)
    # if(type=="average"){return(mean(cos))}
    # if(type=="min"){return(min(cos))}
    # if(type=="max"){return(max(cos))}
  }}




edit_dis<-function(df){
  paper<-df$paperTitle
  n<-length(paper)
  #dis<-c(NULL)
  ifelse(paper=="NA","",paper)
  ifelse(is.na(paper),"",paper)
  
  if(n==1){dis<-0}
  
  else{
    
    dis_mat<-stringdistmatrix(paper,method = "dl")#calculate edit distance
    dis_mat<-as.matrix(dis_mat)
    
    #dis<-dis_mat[upper.tri(dis_mat)]
  }
  
  #edit distance similarity
  
  similarity<-matrix(NA,ncol=n,nrow=n)
  
  if(n==1){similarity<-1}
  else{
    
    for(i in 1:n){
      for(j in 1:n){
        if(i==j){similarity[i,j]<-1}
        else if((paper[i]=="")|(paper[j]=="")){similarity[i,j]<-0}
        else{similarity[i,j]<-levenshteinSim(paper[i],paper[j])}
      }
    }}
  
  
  # if(type=="average"){return(list(edit_distance= mean(dis),similarity=mean(similarity)))}
  # if(type=="min"){return(list(edit_distance= max(dis),similarity=min(similarity)))}
  # if(type=="max"){return(list(edit_distance= min(dis),similarity=max(similarity)))}
  
  return(list(sim=similarity,distance=dis_mat))
  
}


feature<-function(a){
  n<-nrow(data[[a]])
  tfidf<-as.vector(tfidf_corr(data[[a]]))
  e_dis<-edit_dis(data[[a]])
  e_distance<-as.vector(e_dis$distance)
  e_sim<-as.vector(e_dis$sim)
  
  feature<-matrix(NA,ncol=3,nrow=n^2)
  
  feature[,1]<-tfidf
  feature[,2]<-e_distance
  feature[,3]<-e_sim
  
  return(feature)
  
}


wcsv<-function(a){
  n<-nrow(data[[a]])
  
  r<-feature(a)
  #time<-system.time(feature(a))
  
  rname<-paste("(",rep(0:(n-1),each=n),",",rep(0:(n-1),n),")")
  
  colnames(r)<-c("tfidf simlarity","edit distance","edit distance similarity")
  rownames(r)<-rname
  
  write.csv(r,paste("../Spr2017-proj4-team-11/data/feature",a,"_shuyi.csv",sep=""))
  #return(time)
}