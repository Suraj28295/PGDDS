######################################### N Gram phrases ###################################

library(RWeka)
library(tm)
library(pdftools)

# Importing the pdf file
book_file = pdf_text("E://Training data//Unstructure Data//Unstructure Data - Batch 3B//Data Set//2-States-Chetan-Bhagat.pdf")

# Selecting only words from the file.

file = gsub("[^A-Za-z]", " ", book_file)

# Creating the corpus

doc = VCorpus(VectorSource(file))

inspect(doc)

# Performing the basic data cleaning work

doc = tm_map(doc, removeNumbers)

doc = tm_map(doc, content_transformer(tolower))

doc = tm_map(doc, removePunctuation)

inspect(doc)

# Working with stopwords

doc = tm_map(doc, removeWords, stopwords(kind = "english"))

custom_words = c("said", "askmanig", "com")

doc= tm_map(doc, removeWords, custom_words)

# Removing the single letter words

removeSingle <- function(x) gsub(" . ", " ", x) 

doc = tm_map(doc, content_transformer(removeSingle))

# Remove 2, 3 letter words using \b word boundary and posix brackets expression

twoandthree = function(x) gsub(" *\\b[[:alpha:]]{2,3}\\b *", " ", x) 

doc = tm_map(doc, content_transformer(twoandthree))

# Striping White space

doc = tm_map(doc, stripWhitespace)

inspect(doc)


# Creating the Bigram, Trigram and four gram Tokenizer

BigramTokenizer = function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))

TrigramTokenizer = function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))

# Creating the Bigram DTM

DTM.bigram = DocumentTermMatrix(doc, control = list(tokenize = BigramTokenizer))

DTM.bigram

m = as.matrix(DTM.bigram)

df_bigram = sort(colSums(m), decreasing = TRUE)

df = data.frame(words = names(df_bigram), freq = df_bigram)

findAssocs(DTM.bigram, 'ananya mother', corlimit = .4)

# Creating the Trigram DTM

DTM.Trigram = DocumentTermMatrix(doc, control = list(tokenize = TrigramTokenizer))

DTM.Trigram

p = as.matrix(DTM.Trigram)

df_trigram = sort(colSums(p), decreasing = TRUE)

df2 = data.frame(words = names(df_trigram), freq = df_trigram)

