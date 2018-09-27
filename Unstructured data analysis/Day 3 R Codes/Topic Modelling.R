###################################### Topic Modelling ##############################

library(tm)
library(pdftools)
library(topicmodels)

# Importing the pdf file

book_file = paste("I like to eat broccoli and bananas", "I ate a banana and spinach smoothie for breakfast", 
                  "Chinchillas and kittens are cute", "My sister adopted a kitten yesterday", "Look at this cute hamster munching on a piece of broccoli", sep = ".")



# Selecting only words from the file.

file = gsub("[^A-Za-z///' ]", " ", book_file)

# Creating the corpus

doc = Corpus(VectorSource(file))

inspect(doc)

# Performing the basic data cleaning work

doc = tm_map(doc, removeNumbers)

doc = tm_map(doc, content_transformer(tolower))

doc = tm_map(doc, removePunctuation)

inspect(doc)

# Working with stopwords

doc = tm_map(doc, removeWords, stopwords(kind = "english"))

#custom_stop = c("askmanig", "said", "com", "see", "like", "now", "let", "get")

#doc = tm_map(doc, removeWords, custom_stop)

doc = tm_map(doc, stripWhitespace)

inspect(doc)

## Lets create a Document term matrix

dtm = DocumentTermMatrix(doc)

dtm

## Load topicmodels package and run LDA to find n latent topics within the corpus

Topic = LDA(dtm, k = 3, method = "Gibbs", control = list(seed = 1234))

terms(Topic,4)

#probabilities associated with each topic assignment

topicProbabilities = as.data.frame(Topic@gamma)


