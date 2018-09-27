####################################### Bag of words #########################################

# Using Modi's tweets

library(ggplot2)
library(tm)

# Importing the pdf file
file = read.csv("E:/Unstructured Data Analysis/narendramodi_tweets.csv")
head(file,5)

# Selecting only words from the file's text column.

tweet = gsub("[^A-Za-z///' ]", " ", file$text)

# Lets convert this into a data frame

tweet.df = data.frame(text = tweet, stringsAsFactors = FALSE)

# Creating the corpus

doc = VCorpus(VectorSource(tweet))

inspect(doc[[1]])

# Performing the basic data cleaning work

doc = tm_map(doc, removeNumbers)

doc = tm_map(doc, content_transformer(tolower))

doc = tm_map(doc, removePunctuation)

#To find not freq. used words
doc = tm_map(doc,removeWords,stopwords(kind="english"))

inspect(doc[[1]])

# Working with stopwords

doc = tm_map(doc, removeWords, stopwords(kind = "english"))

custom_stop = c("https", "amp", "pmoindia", "will")

doc = tm_map(doc, removeWords, custom_stop)

doc = tm_map(doc, stripWhitespace)

inspect(doc[1:5])

## Lets create a Document term matrix

dtm = DocumentTermMatrix(doc)

dtm

dtm2=DocumentTermMatrix(doc)

#Take words which are used in atleast 3% of document
dtm2=removeSparseTerms(dtm,sparse = .97)

dim(dtm2)
## Removing the sparsity

# The sparsity parameter helps you to removes those terms which have at least a 
# certain percentage of sparse elements. Roughly speaking if you want to keep the terms that 
# appear 3% of the time, set the parameter to 0.97. If you want the terms that occur in 30% 
# of the time, set the parameter to 0.7. The values must be bigger than 0 and smaller than 1.

dtm_without_sparsity = removeSparseTerms(dtm, 0.97)

dtm_without_sparsity

# Converting the DTM into a matrix to view. In this case I will use original DTM

dtm1 = as.matrix(dtm)

# Converting the same to data frame to get the frequency table

df = sort(colSums(dtm1),decreasing=TRUE)
df = data.frame(word = names(df),freq = df)

### Words like "can", "one", "will" etc. give us no information about the subject matter of the documents 
### in which they occur. They can therefore be eliminated without loss. Indeed, they ought to have 
### been eliminated by the stopword removal we did earlier. However, since such words occur very 
### frequently - virtually in all documents - we can remove them by enforcing bounds when creating 
### the DTM, like so: 

dtmr = DocumentTermMatrix(doc, control=list(wordLengths = c(4, Inf)))

dtmr

# Converting the DTMR into a matrix to view

dtm2 = as.matrix(dtmr)

# Converting the same to data frame to get the frequency table

df2 = sort(colSums(dtm2),decreasing=TRUE)
df2 = data.frame(word = names(df2),freq = df2)

## Frequent Terms and Associations

findFreqTerms(dtmr, lowfreq=100)

## I want to find the the words associated with development.

findAssocs(dtmr, 'development', 0.20)

## Lets plot the top 10 words and create a word cloud also lets see if we create a Name cloud

# Ploting bar chart of top 10 words

ggplot(head(df2,10), aes(x = reorder(word, -freq), y = freq)) + 
  geom_bar(stat = "Identity", fill = "blue", width = .5) +
  labs(subtitle="Top 10 Words", 
       y="Frequency", x="Words", title="Bar Plot", 
       caption = "Source: Two States") + theme_bw() + theme(axis.text.x = element_text(angle = 90))

# Lets create a word cloud
library(wordcloud2)

wordcloud2(df2, size=1.6, color='random-dark', backgroundColor = "gray")

# You can also create a correlation matrix. Will not suggest this as it occupy lot of memory.

cor_dtm = cor(dtm2)

library(corrplot)

corrplot(cor_dtm, method="circle")
