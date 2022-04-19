#install.packages("tm")
library(tm)
documents = c("She had toast for breakfast",
              "The coffee this morning was excellent", 
              "For lunch let's all have pancakes", 
              "Later in the day, there will be more talks", 
              "The talks on the first day were great", 
              "The second day should have good presentations too")
documents <- Corpus(VectorSource(documents))
documents = tm_map(documents, content_transformer(tolower))
documents = tm_map(documents, removePunctuation)
documents = tm_map(documents, removeWords, stopwords("english"))
documents[[1]]$content

sdata = c('a', 'b', 'c')
paste(sdata, collapse = " ")