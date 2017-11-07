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

# Take a peak at the data
Spam_matrix <- as.matrix(Spam_tdm)
Spam_matrix <- sort(rowSums(Spam_matrix), decreasing = TRUE)
Spam_df <- data.frame(word = names(Spam_matrix),freq=Spam_matrix)
head(Spam_df, 30)

# Add a column with 1 to classify these words with the Spam emails
spam_tdm1 <- Spam_tdm
spam_tdm1$Spam_Ham <- rep(1,nrow(Spam_tdm))

# Make sure it work
Spam_matrix <- as.matrix(spam_tdm1)
Spam_matrix <- sort(rowSums(Spam_matrix), decreasing = TRUE)
Spam_df <- data.frame(Word = names(Spam_matrix), Frequency = Spam_matrix, Spam_Ham = spam_tdm1$Spam_Ham)
head(Spam_df, 30)

# Do the same for the ham files 
# Take a look at the files
length(list.files("/Users/emiliembolduc/Week 10 - Text Mining/Project 4/easy_ham"))
list.files("/Users/emiliembolduc/Week 10 - Text Mining/Project 4/easy_ham")[1:3]

# look at the file format of one Ham email
file.info("/Users/emiliembolduc/Week 10 - Text Mining/Project 4/easy_ham/00001.7c53336b37003a9286aba55d2945844c")
ham1 <- readLines("/Users/emiliembolduc/Week 10 - Text Mining/Project 4/easy_ham/00001.7c53336b37003a9286aba55d2945844c")
ham1 <- str_c(ham1, collapse = "")
head(ham1)

# create corpus for 1 ham email
ham1_corpus <- Corpus(VectorSource(ham1))
ham1_corpus[[1]]
meta(ham1_corpus[[1]])

# Combine all ham files into one big list
hamfile.list <- list.files("/Users/emiliembolduc/Week 10 - Text Mining/Project 4/easy_ham", pattern = "*.*")
head(hamfile.list)
length(hamfile.list)
setwd("/Users/emiliembolduc/Week 10 - Text Mining/Project 4/easy_ham")
ham.list <- sapply(hamfile.list, readLines)
class(ham.list)

# Create corpus for all Ham
HamAll_corpus <- Corpus(VectorSource(ham.list)) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords("english")) %>%
  tm_map(removePunctuation) %>% 
  tm_map(stemDocument) %>% 
  tm_map(stripWhitespace) #%>%
HamAll_corpus <- tm_map(HamAll_corpus, removeNumbers)

# Create a Term Document Matrix for all Ham
Ham_tdm <- TermDocumentMatrix(HamAll_corpus)
Ham_tdm

# Take a peak at the data
Ham_matrix <- as.matrix(Ham_tdm)
Ham_matrix <- sort(rowSums(Ham_matrix), decreasing = TRUE)
Ham_df <- data.frame(word = names(Ham_matrix), freq = Ham_matrix)
head(Ham_df, 30)

# Add a column with 0 to classify these words with the Ham emails
Ham_tdm1 <- Ham_tdm
Ham_tdm1$Spam_Ham <- rep(0,nrow(Ham_tdm))

# Make sure it work
Ham_matrix <- as.matrix(Ham_tdm1)
Ham_matrix <- sort(rowSums(Ham_matrix), decreasing = TRUE)
Ham_df <- data.frame(Word = names(Ham_matrix), Frequency = Ham_matrix, Spam_Ham = Ham_tdm1$Spam_Ham)
head(Ham_df, 30)

# Combine Spam and Ham term document matrices