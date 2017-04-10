library(stringr)

data.lib="../data/nameset"
data.files=list.files(path=data.lib,pattern = ".*txt")

data.files

## remove "*.txt"
query.list=substring(data.files, 
                     1, nchar(data.files)-4)

query.list

## add a space
query.list=paste(substring(query.list, 1, 1), 
                 " ", 
                 substring(query.list, 
                           2, nchar(query.list)),
                 sep=""
)

query.list

f.line.proc=function(lin, nam.query="."){
  
  # remove unwanted characters
  char_notallowed <- "\\@#$%^&?" # characters to be removed
  lin.str=str_replace(lin, char_notallowed, "")
  
  # get author id
  lin.str=strsplit(lin.str, "_")[[1]]
  author_id=as.numeric(lin.str[1])
  
  # get paper id
  lin.str=lin.str[2]
  paper_id=strsplit(lin.str, " ")[[1]][1]
  lin.str=substring(lin.str, nchar(paper_id)+1, nchar(lin.str))
  paper_id=as.numeric(paper_id)
  
  # get coauthor list
  lin.str=strsplit(lin.str, "<>")[[1]]
  coauthor_list=strsplit(lin.str[1], ";")[[1]]
  
  #print(lin.str)
  for(j in 1:length(coauthor_list)){
    if(nchar(coauthor_list[j])>0){
      nam = strsplit(coauthor_list[j], " ")[[1]]
      if(nchar(nam[1])>0){
        first.ini=substring(nam[1], 1, 1)
      }else{
        first.ini=substring(nam[2], 1, 1)
      }
    }
    last.name=nam[length(nam)]
    nam.str = paste(first.ini, last.name)
    coauthor_list[j]=nam.str
  }
  
  match_ind = charmatch(nam.query, coauthor_list, nomatch=-1)
  
  #print(nam.query)
  #print(coauthor_list)
  #print(match_ind)
  
  if(match_ind>0){
    
    coauthor_list=coauthor_list[-match_ind]
  }
  
  paper_title=lin.str[2]
  journal_name=lin.str[3]
  
  list(author_id, 
       paper_id, 
       coauthor_list, 
       paper_title, 
       journal_name)
}


data_list=list(1:length(data.files))

for(i in 1:length(data.files)){
  
  ## Step 0 scan in one line at a time.
  filepath <- file.path("~/Spr2017-proj4-team-11/data/nameset",paste(data.files[i],sep=""))
  dat=as.list(readLines(filepath))
  data_list[[i]]=lapply(dat, f.line.proc, nam.query=query.list[i])
}

# This part of script reorganizes data_list into a list of 14 dataframes for easier access and processing

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

# filepaths <- "../data"
# save()
