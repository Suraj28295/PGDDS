library(pdftools)
library(lexRankr)

# Importing the pdf file

book_file = pdf_text("E://Unstructure Data - Batch 2B//Data Set//1403.2805.pdf")

head(book_file, 1)
# Taking only the first page

file = book_file[1]

#perform lexrank for top 3 sentences

top_3 = lexRank(file,
                    #only 1 article; repeat same docid for all of input vector
                    docId = rep(1, length(file)),
                    #return 3 sentences to mimick /u/autotldr's output
                    n = 3,
                    continuous = TRUE)

# reorder the top 3 sentences to be in order of appearance in article

order_of_appearance = order(as.integer(gsub("_","",top_3$sentenceId)))

#extract sentences in order of appearance

ordered_top_3 = top_3[order_of_appearance, "sentence"]

ordered_top_3

####################################### Easy Way #####################################
library(LSAfun)

genericSummary(file,k=3)


################################ Summarization of a web page #######################

#load needed packages
library(xml2)
library(rvest)
library(lexRankr)

#url to scrape
monsanto_url = "https://www.theguardian.com/environment/2017/sep/28/monsanto-banned-from-european-parliament"

#read page html
page = read_html(monsanto_url)
#extract text from page html using selector
page_text = html_text(rvest::html_nodes(page, ".js-article__body p"))

#perform lexrank for top 3 sentences
top_3 = lexRank(page_text,
                         #only 1 article; repeat same docid for all of input vector
                         docId = rep(1, length(page_text)),
                         #return 3 sentences to mimick /u/autotldr's output
                         n = 3,
                         continuous = TRUE)

#reorder the top 3 sentences to be in order of appearance in article

order_of_appearance = order(as.integer(gsub("_","",top_3$sentenceId)))

#extract sentences in order of appearance

ordered_top_3 = top_3[order_of_appearance, "sentence"]

ordered_top_3
