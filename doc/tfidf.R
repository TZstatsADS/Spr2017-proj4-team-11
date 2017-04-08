library(text2vec)

tfidf_corr<-function(df,type){
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
  
 
  cos<-corr_mat[upper.tri(corr_mat)]}

  
  if(type=="average"){return(mean(cos))}
  if(type=="min"){return(min(cos))}
  if(type=="max"){return(max(cos))}
  
}