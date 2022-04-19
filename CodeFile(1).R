messages <- read.table(file = 'smsspamcollection/SMSSpamCollection',
                       quote = "", sep = '\t',
                       header = FALSE, 
                       col.names = c("label","message"))
summary(messages)

library(dplyr)

messages_groupby_label = messages %>% group_by("label")
summary(messages_groupby_label)

library(tm)
documents <- Corpus(VectorSource(messages$message))
documents = tm_map(documents, content_transformer(tolower))
documents = tm_map(documents, removePunctuation)
documents = tm_map(documents, removeWords, stopwords("english"))
documents[[1]]$content
#documents = tm_map(documents, stemDocument)

#Bag of words

#messages = stop words, normal words, punctuation

bow= DocumentTermMatrix(documents)
bow

spbow = removeSparseTerms(bow, 0.95)
spbow
