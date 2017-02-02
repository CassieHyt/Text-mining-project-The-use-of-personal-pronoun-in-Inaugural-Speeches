find_personal_pronoun<-function(words,personal_pro=personal)
{
  score<-NULL
  for (i in 1:4)
  {
    words2<-personal_pro[[i]]
    matches = match(words, words2)
    # get the position of the matched term or NA
    # we just want a TRUE/FALSE
    matches = !is.na(matches)
    score[i] = sum(matches) 
  }
  return(score/sum(score))
}

mat2mat<-function(mat,name = "personal_pron")
{
  new <- matrix(nrow=dim(mat)[1]*dim(mat)[2],ncol=3)
  new[,1]<-round(unname(c(mat[,1],mat[,2],mat[,3],mat[,4])),3)
  new[,2]<-rep(colnames(mat),each=nrow(mat))
  new[,3]<-names(c(mat[,1],mat[,2],mat[,3],mat[,4]))
  colnames(new)<-c("prop","type","name")
  return(new)
}

term_sep<-function(mat=pp_mat)
{
  term1<-NULL
  others<-NULL
  for(i in seq(nrow(inaug.list))) {
    test<-rownames(mat)[i]
    term<-as.numeric(substr(test,nchar(test)-1,nchar(test)))
    if (term == 1)
      term1<-c(term1,i)
    else 
      others<-c(others,i)
  }
  return(list(term1,others))
}

mat2mat<-function(mat,name = "personal_pron")
{
  new <- matrix(nrow=dim(mat)[1]*dim(mat)[2],ncol=3)
  new[,1]<-round(unname(c(mat[,1],mat[,2],mat[,3],mat[,4])),3)
  new[,2]<-rep(colnames(mat),each=nrow(mat))
  new[,3]<-names(c(mat[,1],mat[,2],mat[,3],mat[,4]))
  colnames(new)<-c("prop","type","name")
  return(new)
}

senten_mat<-function(i)#ith president
{
  sentence<-NULL
  filename <- paste0("../data/InauguralSpeeches/inaug", inaug.list$File[i], "-", inaug.list$Term[i], ".txt")
  tx <- readLines(filename,warn=F)
  sentence<-sent_detect(tx,endmarks = c("?", ".", "!", "|",";"))
  sen_mat<-matrix(nrow = length(sentence),ncol = 4)
  colnames(sen_mat)<-c("first1","first2","second","third")
  rownames(sen_mat)<-seq(1:length(sentence))
  for (j in 1:4)
  {
    for (k in 1:length(sentence))
    {
      tx<-paste(sentence[[k]], collapse = " ")
      tx_words <- strsplit(sentence[[k]], split = " ")[[1]]
      date_pattern2 <- "[a-zA-z]+"
      tx_words2 <- grep(tx_words, pattern = date_pattern2, value=T)
      tx_log <- grepl(tx_words2, pattern = date_pattern2)
      matches <- gregexpr(pattern = date_pattern2, text = tx_words2[tx_log])
      tx_words <- unlist(regmatches(tx_words2[tx_log], matches))
      words2<-personal[[j]]
      matches = match(tx_words, words2)
      matches = !is.na(matches)
      sen_mat[k,j] = sum(matches) 
    }}
  return(sen_mat)
}


count_emo<-function(i)
{
  filename <- paste0("../data/InauguralSpeeches/inaug", speech.list$File[i], "-",speech.list$Term[i], ".txt")
  tx <- readLines(filename,warn=F)
  sentences=sent_detect(tx,endmarks = c("?", ".", "!", "|",";"))
  if(length(sentences)>0){
    emotions=get_nrc_sentiment(sentences)
    word.count=word_count(sentences)
    # colnames(emotions)=paste0("emo.", colnames(emotions))
    # in case the word counts are zeros?
    emotions=diag(1/(word.count+0.01))%*%as.matrix(emotions)
    return(emotions)
  }
}



find_sen<-function(i,j)#ith president,jth sentence
{
  filename <- paste0("../data/InauguralSpeeches/inaug", speech.list$File[i], "-",speech.list$Term[i], ".txt")
  tx <- readLines(filename,warn=F)
  sentences=sent_detect(tx,endmarks = c("?", ".", "!", "|",";"))
  print(sentences[j])
}

only_top2<-function(mat)
{
  mat_new<-matrix(0,nrow=nrow(mat),ncol=ncol(mat))
  for(i in 1:nrow(mat))
  {
    valid<-order(mat[i,])[1:2]
    mat_new[i,valid]=mat[i,valid]
  }
  colnames(mat_new)<-colnames(mat)
  rownames(mat_new)<-rownames(mat)
  return(mat_new)
}

topic_wc<-function(i,j)#ith president,jth pronoun
{
  tx<-NULL
  tx_words<-NULL
  mat<-NULL
  mat1<-NULL
  matsort<-NULL
  locate<-unname(which(senten_mat(i)[,j]!=0))
  filename <- paste0("../data/InauguralSpeeches/inaug", speech.list$File[i], "-",speech.list$Term[i], ".txt")
  tx <- readLines(filename,warn=F)
  sentences=sent_detect(tx,endmarks = c("?", ".", "!", "|",";"))
  sentences<-sentences[locate]
  if (i==58) 
  {
    tx<-paste(sentences, collapse = " ")
    tx_words <- strsplit(tx, split = " ")[[1]]
  }
  else
    tx_words <- strsplit(sentences, split = " ")
  date_pattern2 <- "[a-zA-z]+"
  tx_words2 <- grep(tx_words, pattern = date_pattern2, value=T)
  tx_log <- grepl(tx_words2, pattern = date_pattern2)
  matches <- gregexpr(pattern = date_pattern2, text = tx_words2[tx_log])
  tx_words <- unlist(regmatches(tx_words2[tx_log], matches))
  tx_words <- unlist(rm_stopwords(tx_words))
  table<-table(tx_words)
  word<-names(table)
  n<-unname(table)
  matname<-paste(inaug.list$File[i],inaug.list$Term[i])
  mat<-cbind(rep(matname,nrow(n)),word,n)
  mat1<-rbind(mat1,mat)
  colnames(mat1)<-c("name","word","n") 
  matsort<-mat1[order(as.numeric(mat1[,3]),decreasing = T),]
  return(matsort)
}