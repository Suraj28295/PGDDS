# Scrapping data from facebook

# Library used

library(Rfacebook)
library(RCurl)

# Connecting to my facebook api

my_data = fbOAuth(app_id="815979781916818",app_secret="bfb03db1b9a748cf64e698b0e8336d12")

# You can save this Auth for future use

save(my_data, file = "my_data")

load("my_data")

# Connecting to my facebook so that we can scrap data 

myself = getUsers("me",token=my_data)

# Now I am connected to facebook api

# List of all the pages you have liked

my_likes = getLikes("me", token = my_data)

# Search Pages that contain a particular keyword

pages = searchPages( string="Modi", token= my_data, n=200)

head(pages$name)

# Extract list of posts from a Facebook page

page = getPage(page="bbcnews", token= my_data, n=200)

# Get all the posts from a particular date

page = getPage("bbcnews", token= my_data, n=100,
                since='2017/06/01', until='2018/02/25')

# Which of these posts got maximum likes?

max_like = page[which.max(page$likes_count),] # You can also use dplyr

# To know what users think about a post, it is important to analyze their comments.
# Extract FB comments on a specific post

post = getPost(page$id[1], token= my_data, n.comments = 100, likes = FALSE) # this will give a list

comments = post$comments 

fix(comments) # This just to give a readable view

# Facebook has more than a like button. Last year, it launched emoji (emoticons). 
# If a post got 1k likes, it does not mean everyone really loves the comment. 
# The reaction can be happy, sad or angry.

# Extract Reactions for most recent post

reaction = getReactions(page$id[1], token= my_data)

# Get Posts of a particular group
# First, searchGroup() function searches id of a group from which you want to pull out posts. 
# Later, the group ID is used as a input value in getGroup() function.

# Extract posts from Machine Learning Facebook group

getID = searchGroup(name="machinelearningforum", token= my_data)

group = getGroup(group_id= getID$id, token= my_data, n=50)








