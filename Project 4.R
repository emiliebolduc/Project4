install.packages("RCurl")
install.packages("XML")
install.packages("stringr")
install.packages("tm")

#libraries
library(RCurl)
library(XML)
library(stringr)
library(tm)

# Spam first
length(list.files("spam_2"))
list.files("spam_2")[1:3]

#rename Spame files - DIDN'T WORK
file.rename(list.files(pattern="0*."), paste0("", 1:1396))

# look at the file format of one spam email
file.info("spam_2/00001.317e78fa8ee2f54cd4890fdc09ba8176")
spam1 <- readLines("spam_2/00001.317e78fa8ee2f54cd4890fdc09ba8176")
spam1 <- str_c(spam1, collapse = "")
head(spam1)
spam1

# create corpus for 1
spam1_corpus <- Corpus(VectorSource(spam1))
spam1_corpus[[1]]
meta(spam1_corpus[[1]])

# Combine all files into one big list
file.list <- list.files("spam_2", pattern = "*.*")
head(file.list)
length(file.list)
setwd("/Users/emiliembolduc/Week 10 - Text Mining/Project 4/spam_2")
spam.list <- sapply(file.list, readLines)
class(spam.list)

# Create corpus for all Spam
SpamAll_corpus <- Corpus(VectorSource(spam.list)) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords("english")) %>%
  tm_map(removePunctuation) %>% 
  tm_map(stemDocument) %>% 
  tm_map(stripWhitespace) #%>%
SpamAll_corpus <- tm_map(SpamAll_corpus, removeNumbers)

# Create a Term Document Matrix for all Spam
Spam_tdm <- TermDocumentMatrix(SpamAll_corpus)
Spam_tdm

Spam_matrix <- as.matrix(Spam_tdm)
Spam_matrix <- sort(rowSums(Spam_matrix), decreasing = TRUE)
Spam_df <- data.frame(word = names(Spam_matrix),freq=Spam_matrix)
head(Spam_df, 100)

