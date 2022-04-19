messages <- read.table(file = 'smsspamcollection/SMSSpamCollection',
                       quote = "", sep = '\t',
                       header = FALSE, 
                       col.names = c("label","message"))
print(summary(messages))
str(messages)
cat("\n")
print(messages$message[1])
cat("\n")
print(paste("Longest message has: ",max(nchar(messages$message))," Characters"))
cat("\n")
print(paste("shortest message has: ",min(nchar(messages$message))," Characters"))

cat("\n")
print(paste("Index of messages with shortest length"))
print(which(nchar(messages$message) == 2))
print(paste("Shortest message is: ",messages$message[which(nchar(messages$message) == 2)[1]]))
print(paste("Longest message is: ",messages$message[which.max(nchar(messages$message))]))

table(messages$label)

library(tm)

#creating a corpus
corpus <- Corpus(VectorSource(messages$message))
print(corpus)
documents = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("english"))


#install.packages("SnowballC")
library(SnowballC)
corpus <- tm_map(corpus, stemDocument)

#making bag of words

#the rows correspond to documents, and
#the columns correspond to words .

dtm = DocumentTermMatrix(corpus)
print(dtm)

#remove sparse terms
spdtm = removeSparseTerms(dtm, 0.95)
print(spdtm)

#creating a dataframe from document term matrix
temp = as.data.frame(as.matrix(spdtm))
dim(temp)
messagesSparse = as.data.frame(as.matrix(dtm))
dim(messagesSparse)

colnames(messagesSparse) = make.names(colnames(messagesSparse))

#sorting word and finding more frequent
head(sort(colSums(messagesSparse),decreasing = TRUE),n = 20)

messagesSparse$label = messages$label

head(sort(colSums(subset(messagesSparse, spam == 0)),decreasing = TRUE),n = 10)

