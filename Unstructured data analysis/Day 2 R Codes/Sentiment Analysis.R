############################# Way 1 ####################################

# Import the data set Modi tweets

library(stringr)
library(plyr)

tweet = read.csv("E://Unstructure Data - Batch 2B//Data Set//narendramodi_tweets.csv")

# Importing the text file containing positive and neg. words

neg_words = scan("E://Unstructure Data - Batch 2B//Data Set//negative-words.txt", what="character", comment.char=";")
pos_words = scan("E://Unstructure Data - Batch 2B//Data Set//positive-words.txt", what="character", comment.char=";")

# In case I want to update my pos and neg word dict. 

pos_words = c(pos_words, 'new', 'nice', 'good', 'horizon')
neg_words = c(neg_words, 'wtf', 'behind','feels', 'ugly', 'back','worse','shitty', 'bad', 'no','freaking','sucks','horrible')

# Lets Create a sentiment score function

score.sentiment = function(tweets, pos.words, neg.words)
  
{
  
  require(plyr)
  require(stringr)
  
  scores = laply(tweets, function(tweet, pos.words, neg.words) {
    
    # Remove https:// and http://
    tweet = gsub('https://','',tweet) 
    tweet = gsub('http://','',tweet) 
    
    # Remove Graphic characters like emoticons
    tweet = gsub('[^[:graph:]]', ' ',tweet) 
    
    # remove punctuation
    tweet = gsub('[[:punct:]]', '', tweet) 
    
    # remove control characters
    tweet = gsub('[[:cntrl:]]', '', tweet) 
    
    # remove numbers
    tweet = gsub('\\d+', '', tweet) 
    
    # Case to lower
    tweet = tolower(tweet) 
    
    # spliting the tweets by words in a list
    word.list = str_split(tweet, '\\s+') 
    
    # turns the list to vector
    words = unlist(word.list) 
    
    # returns matching values for words from list
    pos.matches = match(words, pos.words)  
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA. we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches) 
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches) 
    
    return(score)
    
  }, pos.words, neg.words )
  
  scores.df = data.frame(score=scores, text=tweets)
  
  return(scores.df)
  
}

analysis = score.sentiment(tweet$text, pos_words, neg_words)

# Ploting the score of tweets

hist(analysis$score)

plot(analysis$score)

####################################### Way 2 #######################################

library(syuzhet)
library(tidyr)

# Subsetting only the text part of our data frame tweet

tweet_text = iconv(tweet$text)

# Obtain sentiment scores from NRC sentiment dictonary

s = get_nrc_sentiment(tweet_text)

head(s)

## You can see the first tweet have the score of joy, suprise and positive

tweet_text[1]

## Lets see how nrc is treating words like wonderfully etc

get_nrc_sentiment('wonderfully')

## lets plot all the sentiments

barplot(colSums(s), las = 2, col = rainbow(10), ylab = 'Count', main = "sentiment analysis using syuzhet")

############################################### Modelling for sentiment analysis #############################################

library(RTextTools)
library(e1071)
library(tm)

# Creating a dummy data set

pos_tweets =  rbind(
  c('I love this car', 'positive'),
  c('This view is amazing', 'positive'),
  c('I feel great this morning', 'positive'),
  c('I am so excited about the concert', 'positive'),
  c('He is my best friend', 'positive')
)

neg_tweets = rbind(
  c('I do not like this car', 'negative'),
  c('This view is horrible', 'negative'),
  c('I feel tired this morning', 'negative'),
  c('I am not looking forward to the concert', 'negative'),
  c('He is my enemy', 'negative')
)

test_tweets = rbind(
  c('feel happy this morning', 'positive'),
  c('larry friend', 'positive'),
  c('not like that man', 'negative'),
  c('house not great', 'negative'),
  c('your song annoying', 'negative')
)

tweets = rbind(pos_tweets, neg_tweets, test_tweets)

tweets1 = as.data.frame(tweets)

colnames(tweets1) <- c("Text","Sentiment")

# Text cleaning

doc = Corpus(VectorSource(tweets1$Text))

doc = tm_map(doc, removeNumbers)

doc = tm_map(doc, content_transformer(tolower))

doc = tm_map(doc, removePunctuation)

doc = tm_map(doc, removeWords, stopwords(kind = "english"))

doc = tm_map(doc, stripWhitespace)

dtm = DocumentTermMatrix(doc)

mat = as.matrix(dtm)

# train the model

classifier = naiveBayes(mat[1:10,], as.factor(tweets1[1:10,2]))

# test the validity

predicted = predict(classifier, mat[11:15,])

predicted

# Confusion matrix

table(tweets1[11:15, 2], predicted)

recall_accuracy(tweets1[11:15, 2], predicted)

# Building various models 

# build the data to specify response variable, training set, testing set.
container = create_container(mat, as.numeric(as.factor(tweets[,2])),
                             trainSize=1:10, testSize=11:15,virgin=FALSE)

# Building various models in single step

models = train_models(container, algorithms=c("SVM", "RF", "TREE"))

# Now, we can classify the testing set using the trained models.

results = classify_models(container, models)

# accuracy table
table(as.numeric(as.factor(tweets[11:15, 2])), results[,"SVM_LABEL"])
table(as.numeric(as.factor(tweets[11:15, 2])), results[,"FORESTS_LABEL"])
table(as.numeric(as.factor(tweets[11:15, 2])), results[,"TREE_LABEL"])


# recall accuracy
recall_accuracy(as.numeric(as.factor(tweets[11:15, 2])), results[,"FORESTS_LABEL"])
recall_accuracy(as.numeric(as.factor(tweets[11:15, 2])), results[,"TREE_LABEL"])
recall_accuracy(as.numeric(as.factor(tweets[11:15, 2])), results[,"SVM_LABEL"])




