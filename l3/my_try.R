library(tm)
library(ggplot2)

spam.path <- "data/spam/"
spam2.path <- "data/spam_2/"
easyham.path <- "data/easy_ham/"
easyham2.path <- "data/easy_ham_2/"
hardham.path <- "data/hard_ham/"
hardham2.path <- "data/hard_ham_2/"

get.msg <- function(path) {
  # rt = read as text
  con <- file(path, open="rt", encoding="latin1")
  text <- readLines(con) # array u kojem je svaki item jedna linija teksta

  # seq parametri - od, do, za koliko se povećava
  # which - nađi item u nizu koji je prazan string i vrati njegov indeks uvećan za jedan
  # sve skupa - vrati članove niza od prvog praznog reda pa do kraja
  # prvi prazan red odvaja zaglavlje o tijela poruke
  msg <- text[seq(which(text=="")[1]+1, length(text), 1)]
  close(con)
  # paste konkatenira stringove
  return (paste(msg, collapse="\n"))
}

spam.docs <- dir(spam.path) # lista imena datoteka
spam.docs <- spam.docs[which(spam.docs != "cmds")] # izbaci članove koji su jednaki "cmds"
all.spam <- sapply(spam.docs, function(p) get.msg(paste(spam.path, p, sep="")))

#print(head(all.spam))

get.tdm <- function(doc.vec) {
  doc.corpus <- Corpus(VectorSource(doc.vec))
  control <- list(stopwords=TRUE, removePunctuation=TRUE, removeNumbers=TRUE, minDocFreq=2)
  doc.dtm <- TermDocumentMatrix(doc.corpus, control)
  return (doc.dtm)
}

spam.tdm <- get.tdm(all.spam)

spam.matrix <- as.matrix(spam.tdm)
spam.counts <- rowSums(spam.matrix)
spam.df <- data.frame(cbind(names(spam.counts), as.numeric(spam.counts)), stringsAsFactors=FALSE)
names(spam.df) <- c("term", "frequency")
spam.df$frequency <- as.numeric(spam.df$frequency)

spam.occurrence <- sapply(1:nrow(spam.matrix), function (i) { length(which(spam.matrix[i,] > 0))/ncol(spam.matrix)})
spam.density <- spam.df$frequency/sum(spam.df$frequency)

spam.df <- transform(spam.df, density=spam.density, occurrence=spam.occurrence)

print(head(spam.df))
#n = c(2, 3, 5) 
#s = c("aa", "bb", "cc") 
#b = c(TRUE, FALSE, TRUE) 
#df = data.frame(n, s, b)   
#print(df)

#print(spam.df$frequency/sum(spam.df$frequency))

print(paste("jedan", "dva", sep="+", collapse="-"))
