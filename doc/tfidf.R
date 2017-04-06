library(text2vec)

tfidf_corr<-function(df){
  paper<- itoken(df$Paper, 
                     preprocessor = tolower, 
                     tokenizer = word_tokenizer,
                     ids = AKumar$PaperID,
                     # turn off progressbar because it won't look nice in rmd
                     progressbar = FALSE)
  words <- create_vocabulary(paper, stopwords = c("a", "an", "the", "in", "on",
                                                     "at", "of", "above", "under","for"))
  vectorizer <- vocab_vectorizer(words)
  dtm_paper <- create_dtm(paper, vectorizer)
  tfidf <- TfIdf$new()
  dtm_paper_tfidf <- fit_transform(dtm_paper, tfidf)
  corr_mat<-sim2(x = dtm_paper, method = "cosine", norm = "l2")
  corr_mat<-as.matrix(corr_mat)
  
 
  cos<-corr_mat[upper.tri(corr_mat)]

  return(list(max=max(cos),min=min(cos),average=mean(cos)))
}