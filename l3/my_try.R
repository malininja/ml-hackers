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
  msg <- text[seq(which(text=="")[1]+1, length(text), 1)]
  close(con)
  return (paste(msg, collapse="\n"))
}

spam.docs <- dir(spam.path) # lista imena datoteka
spam.docs <- spam.docs[which(spam.docs != "cmds")] # izbaci članove koji su jednaki "cmds"
all.spam <- sapply(spam.docs, function(p) get.msg(paste(spam.path, p, sep="")))

#print(head(all.spam.))

get.tdm <- function(doc.vec) {
  doc.corpus <- Corpus(VectorSource(doc.vec))
  # stranica 82
}