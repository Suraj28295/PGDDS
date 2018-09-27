# Scrapping tweets from twitter

# Library required

library(twitteR)
library(ROAuth)
require(RCurl)
library(dplyr)

# Connecting to twitter

consumer_key <- "ylI056vj4011Zv9FJKYINDwyM"
consumer_secret <- "hMRAxxWJFkUikEdFrWvFy5E8V67pmy7f924m57pGtBCgQjza4F"
access_token <- "1856469752-vl7X9RapxqqgclI9mWpAcIWRUStDyeooWWNIdMY"
access_secret <- "S3LGcOZtJ0jrgN2QExyzo2xvK78EB8sGXZnOoBzvIuQsw"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

# Getting tweets with hashtage name

gst = searchTwitter("#GST" , n = 1000, lang = "en", since = '2017-06-1')

# Converting it to a data frame

tweet = twListToDF(gst)

# Getting tweets with handler name. Let's extract tweets of a Modi's twitter handle

modi = userTimeline("@PMOIndia",n=1000)

# Converting it to a data frame

modi_tweet = twListToDF(modi)

# To get tweets from your home timeline.

home = homeTimeline (n=15)

# Converting it into a data frame

home = twListToDF(home)

# Identifying the location of users. Issue is not more that 2% user share they location on twitter.

userInfo = lookupUsers(tweet$screenName) 

userFrame = twListToDF(userInfo)

# Selecting only screen name and location as this will help us to do a marge with tweet data set

user_location = select(userFrame, screenName, location)

# Adding the location in tweet data set

Final_tweet = merge(tweet, user_location, by = "screenName", all.x = TRUE)

