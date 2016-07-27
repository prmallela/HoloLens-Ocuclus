install.packages("ibmdbR")
library(ibmdbR)
driver.name <- "{IBM DB2 ODBC DRIVER - IBMDBCL1}"
db.name <- "BLUDB"
host.name <- "awh-yp-small03.services.dal.bluemix.net"
port <-"50000"
user.name <-"USERNAME"
pwd <- "PASSWORD"
con.text <- paste("DRIVER=",driver.name,
                  ";Database=",db.name,
                  ";Hostname=",host.name,
                  ";Port=",port,
                  ";PROTOCOL=TCPIP",
                  ";UID=", user.name,
                  ";PWD=",pwd,sep="")
# Connect to using a odbc Driver Connection string to a remote database
con <- odbcDriverConnect(con.text)
idaInit(con)
idaShowTables()

HOLOLENS <- as.data.frame(ida.data.frame('"DASH101971"."HOLOLENS_TWEETS"')[ ,c('MESSAGE_ACTION', 'MESSAGE_BODY', 'MESSAGE_COUNTRY', 'MESSAGE_COUNTRY_CODE', 'MESSAGE_FAVORITES_COUNT', 'MESSAGE_GENERATOR_DISPLAY_NAME', 'MESSAGE_ID', 'MESSAGE_INREPLYTO_URL', 'MESSAGE_LANGUAGE', 'MESSAGE_LOCATION', 'MESSAGE_LOCATION_DISPLAY_NAME', 'MESSAGE_POSTED_TIME', 'MESSAGE_RETWEET_COUNT', 'MESSAGE_URL', 'USER_CITY', 'USER_COUNTRY', 'USER_COUNTRY_CODE', 'USER_DISPLAY_NAME', 'USER_FAVORITES_COUNT', 'USER_FOLLOWERS_COUNT', 'USER_FRIENDS_COUNT', 'USER_GENDER', 'USER_ID', 'USER_IMAGE_URL', 'USER_LISTED_COUNT', 'USER_LOCATION_DISPLAY_NAME', 'USER_REGISTER_TIME', 'USER_SCREEN_NAME', 'USER_STATE', 'USER_STATUSES_COUNT', 'USER_SUB_REGION', 'USER_SUMMARY', 'USER_URL')])
HOLOLENS1 <-subset(HOLOLENS,MESSAGE_LANGUAGE=="en")
OCULUS <- as.data.frame(ida.data.frame('"DASH101971"."OCULUS_TWEETS"')[ ,c('MESSAGE_ACTION', 'MESSAGE_BODY', 'MESSAGE_COUNTRY', 'MESSAGE_COUNTRY_CODE', 'MESSAGE_FAVORITES_COUNT', 'MESSAGE_GENERATOR_DISPLAY_NAME', 'MESSAGE_ID', 'MESSAGE_INREPLYTO_URL', 'MESSAGE_LANGUAGE', 'MESSAGE_LOCATION', 'MESSAGE_LOCATION_DISPLAY_NAME', 'MESSAGE_POSTED_TIME', 'MESSAGE_RETWEET_COUNT', 'MESSAGE_URL', 'USER_CITY', 'USER_COUNTRY', 'USER_COUNTRY_CODE', 'USER_DISPLAY_NAME', 'USER_FAVORITES_COUNT', 'USER_FOLLOWERS_COUNT', 'USER_FRIENDS_COUNT', 'USER_GENDER', 'USER_ID', 'USER_IMAGE_URL', 'USER_LISTED_COUNT', 'USER_LOCATION_DISPLAY_NAME', 'USER_REGISTER_TIME', 'USER_SCREEN_NAME', 'USER_STATE', 'USER_STATUSES_COUNT', 'USER_SUB_REGION', 'USER_SUMMARY', 'USER_URL')])

##########################Word Cloud################################################
#install the necessary packages
install.packages("wordcloud")
install.packages("tm")
install.packages("RColorBrewer")
install.packages("Matrix")

#load the necessary packages
library("wordcloud")
library("tm")
library("RColorBrewer")
library("Matrix")
library(ibmdbR)

#Get tweets only and create Corpus

tweete <- OCULUS$MESSAGE_BODY
tweete2 <-HOLOLENS$MESSAGE_BODY

r_stats_text_corpus<- Corpus(VectorSource(tweete ))
r_stats_text_corpus<- tm_map(r_stats_text_corpus, stripWhitespace)
#r_stats_text_corpus <- tm_map(r_stats_text_corpus, removeWords, c("Samsung", "samsung","galaxys"))   


# create document term matrix applying some transformation
tdm = TermDocumentMatrix(r_stats_text_corpus,
                         control = list(removePunctuation = TRUE,
                                        removeWords = c("the", stopwords("english")),
                                        removeNumbers = TRUE, tolower = TRUE))

# define tdm as matrix
m = as.matrix(tdm)
# get word counts in decreasing order
word_freqs = sort(rowSums(m), decreasing=TRUE) 

#check top 50 most mentioned words
head(word_freqs, 50)

#remove the top words which don't generate insights such as "the", "a", "and", etc.
word_freqs = word_freqs[-(3)]  #Here "3" is 3rd word in the list we want to remove 

# create a data frame with words and their frequencies
dm = data.frame(word=names(word_freqs), freq=word_freqs)

#Plot corpus in a clored graph; need RColorBrewer package
wordcloud(head(dm$word, 50), head(dm$freq, 50), random.order=FALSE, colors=brewer.pal(8, "Dark2"))



#****************Topic Analysis*****************************************************
# import into R a lexicon of Sports words. replace "Sports_Word.txt" with your lexicon
sports.words = scan('Sports_Word.txt', what='character', comment.char=';')

# function to check each tweet whehter contains sports words
score.topic = function(sentences, dict, .progress='none')
{
  require(plyr)
  require(stringr)
  
  # we got a vector of sentences. plyr will handle a list
  # or a vector as an "l" for us
  # we want a simple array of scores back, so we use
  # "l" + "a" + "ply" = "laply":
  scores = laply(sentences, function(sentence, dict) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    # and convert to lower case:
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    topic.matches = match(words, dict)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    topic.matches = !is.na(topic.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(topic.matches)
    
    return(score)
  }, dict, .progress=.progress )
  
  topicscores.df = data.frame(score=scores, text=sentences)
  return(topicscores.df)
}

# check your "tweete" using the function.
topic.scores= score.topic(tweete, sports.words, .progress='text')

# get the subset of your "tweete" which mentioned sports
topic.mentioned = subset(topic.scores, score !=0)

# calculate number of tweets mentioned sports to create your pie chart
N= nrow(topic.scores)
Nmentioned = nrow(topic.mentioned)

dftemp=data.frame(topic=c("Mentioned", "Not Mentioned"), 
                  number=c(Nmentioned,N-Nmentioned))

install.packages("googleVis")
require(googleVis)
Pie <- gvisPieChart(dftemp)
plot(Pie)

#****************Sentiment Analysis*****************************************************

# use the subset tweets to proceed with sentiment analysis

mentionedTweet = topic.mentioned$text

# import positive and negative words list 
pos.words = scan('positive-words.txt', what='character', comment.char=';')
neg.words = scan('negative-words.txt', what='character', comment.char=';')

# add additional words to your list
neg.words = c(neg.words, 'wtf', 'fail')

#Implementing our sentiment scoring algorithm
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  
  # we got a vector of sentences. plyr will handle a list
  # or a vector as an "l" for us
  # we want a simple array of scores back, so we use
  # "l" + "a" + "ply" = "laply":
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    # and convert to lower case:
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

# sentiment analysis of the subset tweets

sentiment.scores= score.sentiment(mentionedTweet, pos.words, neg.words, .progress='text')

score= sentiment.scores$score

# a histogram of sentiment scores and you can see a large portion is neutral
hist(score)

# remove neutral tweets and keep only positve and negative ones
score = subset(score,score!=0)

# create hisogram of sentiment scores
sentiscore = data.frame(score)
Hist<- gvisHistogram(sentiscore, options=list(
  legend="{ position: 'top', maxLines:2 }",
  colors="['#5C3292', '#1A8763', '#871B47']",
  width=400, height=360))
plot(Hist)


############Comparing Word Cloud##################

#Get tweets from two Twitter data sets - df1 & df2
tweete <- df1$MESSAGE_BODY
tweete2 <- df2$MESSAGE_BODY

#use this function to clean the tweets
clean.text = function(x)
{
  # tolower
  x = tolower(x)
  # remove rt
  x = gsub("rt", "", x)
  # remove at
  x = gsub("@\\w+", "", x)
  # remove punctuation
  x = gsub("[[:punct:]]", "", x)
  # remove numbers
  x = gsub("[[:digit:]]", "", x)
  # remove links http
  x = gsub("http\\w+", "", x)
  # remove tabs
  x = gsub("[ |\t]{2,}", "", x)
  # remove blank spaces at the beginning
  x = gsub("^ ", "", x)
  # remove blank spaces at the end
  x = gsub(" $", "", x)
  return(x)
}

# clean tweets
set1 = clean.text(tweete)
set2 = clean.text(tweete2)

set1 = paste(set1, collapse=" ")
set2 = paste(set2, collapse=" ")
all = c(set1, set2)

corpus = Corpus(VectorSource(all))
#corpus = Corpus(VectorSource(set1))


# create term-document matrix
tdm = TermDocumentMatrix(corpus)

tdm = TermDocumentMatrix(
  corpus,
  control = list(
    wordLengths=c(3,20),
    removePunctuation = TRUE,
    stopwords = c("brooklyn", "LIU","The", stopwords("english")),
    removeNumbers = TRUE, tolower = TRUE) )

# convert as matrix
tdm = as.matrix(tdm)

# add column names, use your own names to identify the groups of data
colnames(tdm) = c("Oculus", "HoloLens")

comparison.cloud(tdm, random.order=FALSE,
                 colors = c("#4d94ff", "#ff5050"),
                 title.size=1.5, max.words=100)



# Social Network Analysis ******

install.packages("stringi")
install.packages("jsonlite")
install.packages("magrittr")
install.packages("dplyr")
install.packages("tidyr")
install.packages("Rcpp")
install.packages("igraph")
install.packages("RBGL")
install.packages("sna")
install.packages("RColorBrewer")
install.packages("mclust")
install.packages("ggplot2")

library(stringi)
library(jsonlite)
library(magrittr)
library(dplyr)
library(tidyr)
library(Rcpp)
library(igraph)
library(RBGL)
library(sna)
library(RColorBrewer)
library(mclust)
library(ggplot2)

# Retrieve data from your dashDB table
df <- as.data.frame(ida.data.frame('"DASH5661"."TECHTRIANGLEU_TWEETS"')[ ,c('MESSAGE_ACTION', 'MESSAGE_BODY', 'MESSAGE_COUNTRY', 'MESSAGE_FAVORITES_COUNT', 'MESSAGE_GENERATOR_DISPLAY_NAME', 'MESSAGE_ID', 'MESSAGE_LANGUAGE', 'MESSAGE_LOCATION', 'MESSAGE_LOCATION_DISPLAY_NAME', 'MESSAGE_POSTED_TIME', 'MESSAGE_RETWEET_COUNT', 'USER_CITY', 'USER_COUNTRY', 'USER_DISPLAY_NAME',  'USER_FOLLOWERS_COUNT', 'USER_FRIENDS_COUNT', 'USER_GENDER', 'USER_LOCATION_DISPLAY_NAME', 'USER_SCREEN_NAME')])
tweete2 = df$MESSAGE_BODY
screenname = OCULUS$USER_DISPLAY_NAME
screenname = as.character(screenname)
tweete = as.character(tweete2)

#Define retweete patterns
rt_patterns = grep("(RT|via)((?:\\b\\W*@\\w+)+)", 
                   tweete, ignore.case=TRUE)

# create list to store user names
who_retweet = as.list(1:length(rt_patterns))
who_post = as.list(1:length(rt_patterns))

# for loop to extract poster and retweeter
for (i in 1:length(rt_patterns))
{ 
  # get tweet with retweet entity
  twit = tweete[[rt_patterns[i]]]
  # get retweet source 
  poster = str_extract_all(twit,
                           "(RT|via)((?:\\b\\W*@\\w+)+)")
  #remove ':'
  poster = gsub(":", "", unlist(poster)) 
  # name of retweeted user
  who_post[[i]] = gsub("(RT @|via @|@)", "", poster, ignore.case=TRUE)[1]
  who_post[[i]] = word(who_post[[i]],1)
  # name of retweeting user 
  who_retweet[[i]] = screenname[[rt_patterns[i]]]
}

# unlist
who_post = unlist(who_post)
who_retweet = unlist(who_retweet)

# Remove special characters
who_retweet <- str_replace_all(who_retweet, "[^[:alnum:]]", " ")
who_post <- str_replace_all(who_post, "[^[:alnum:]]", " ")
retweeter_poster = cbind(who_retweet, who_post)

# Remove NA
retweeter_poster <- retweeter_poster[rowSums(is.na(retweeter_poster)) == 0,]
write.table(retweeter_poster, "ps-rt.csv")

# generate graph from edgelist
rt_graph = graph.edgelist(retweeter_poster)

# Create graph
par(bg="white", mar=c(1,1,1,1))
plot(rt_graph, layout=glay,
     vertex.color="gray25",
     vertex.size=4,
     vertex.label=ver_labs,
     vertex.label.family="sans",
     vertex.shape="none",
     # vertex.label.color=hsv(h=0, s=0, v=.95, alpha=0.5),
     vertex.label.cex=0.85,
     edge.arrow.size=0.4,
     edge.arrow.width=0.1,
     edge.width=3,
     edge.color=hsv(h=.95, s=1, v=.7, alpha=0.5))
# add title
title("\nTweets with 'Bklyn Tech':  Who retweets whom",
      cex.main=1, col.main="black") 

# Decompose the graph and return a list of sub graphs whose vertices are greater than 10
dg <- decompose.graph(rt_graph, min.vertices=10) 

# Plot each of the sub graph
plot(dg[[1]], vertex.size=0, edge.arrow.size=0, edge.arrow.width=0) # plot e.g. the 1st one


###################Consumer Profile...................###################
# Get consumer profile related information from your Twitter table. Make changes to access your specific tables
HLCIG<- as.data.frame(ida.data.frame('"DASH101971"."HOLOLENS_TWEETS"')[ ,c('USER_COUNTRY', 'USER_GENDER')])
# Convert to lowercase 
HLCIG$COUNTRY <- tolower(HLCIG$USER_COUNTRY)
# Get subset of data to explore
subdf <- subset(HLCIG, COUNTRY =='united states' | COUNTRY =='canada' | COUNTRY == 'united kindom')
dfrm <-table(subdf[,c(3,2)]) 
dfrm1 <-dfrm[,1:2]
jpeg(type='cairo',"COUNTRYgenderOculus.jpg",width=800,height=600) 
barplot(dfrm1, main="Histogram Of Country by Gender For Oculus",legend = rownames(dfrm1), beside=TRUE,col=c("#4d94ff","#ff4d4d")) 
sink('/dev/null') 
dev.off() 


HLCIG<- as.data.frame(ida.data.frame('"DASH101971"."HOLOLENS_TWEETS"')[ ,c('USER_CITY', 'USER_GENDER')])
# Convert to lowercase 
HLCIG$COUNTRY <- tolower(HLCIG$USER_COUNTRY)
# Get subset of data to explore
subdf <- subset(HLCIG, CITY =='Boston' | City =='new york city' | CITY == 'Seattle')
dfrm <-table(subdf[,c(3,2)]) 
dfrm1 <-dfrm[,1:2]
jpeg(type='cairo',"COUNTRYgenderOculus.jpg",width=800,height=600) 
barplot(dfrm1, main="Histogram Of Country by Gender For Oculus",legend = rownames(dfrm1), beside=TRUE,col=c("#4d94ff","#ff4d4d","#196619")) 
sink('/dev/null') 
dev.off() 



#############Sentiment Analysis on Timed Tweets#############################################
# Create a new attribute based 
HOLOLENS$month = months(as.POSIXct(as.character(HOLOLENS$MESSAGE_POSTED_TIME),format="%Y-%m-%d %H:%M"))


# Get subset according to "month"
m1 = subset(HOLOLENS, month == "January")
m2 = subset(HOLOLENS, month == "February")
m3 = subset(HOLOLENS, month == "March")
m4 = subset(HOLOLENS, month == "April")
m5 = subset(HOLOLENS, month == "May")
m6 = subset(HOLOLENS, month == "June")
m7 = subset(HOLOLENS, month == "July")
m8 = subset(HOLOLENS, month == "August")
m9 = subset(HOLOLENS, month == "September")
m10 = subset(HOLOLENS, month == "October")
m11 = subset(HOLOLENS, month == "November")
m12 = subset(HOLOLENS, month == "December")


# Generate and save sentiment scores for each subset. Here I calculate the ratio of number of positive tweets to number of negative tweets


score1= score.sentiment(m1$MESSAGE_BODY, pos.words, neg.words, .progress='text')
ratio1 = length(subset(score1$score,score1$score>0))/length(subset(score1$score,score1$score<0))

score2= score.sentiment(m2$MESSAGE_BODY, pos.words, neg.words, .progress='text')
ratio2 = length(subset(score2$score,score2$score>0))/length(subset(score2$score,score2$score<0))

score3= score.sentiment(m3$MESSAGE_BODY, pos.words, neg.words, .progress='text')
ratio3 = length(subset(score3$score,score3$score>0))/length(subset(score3$score,score3$score<0))

score4= score.sentiment(m4$MESSAGE_BODY, pos.words, neg.words, .progress='text')
ratio4 = length(subset(score4$score,score4$score>0))/length(subset(score4$score,score4$score<0))

score5= score.sentiment(m5$MESSAGE_BODY, pos.words, neg.words, .progress='text')
ratio5 = length(subset(score5$score,score5$score>0))/length(subset(score5$score,score5$score<0))

score6= score.sentiment(m6$MESSAGE_BODY, pos.words, neg.words, .progress='text')
ratio6 = length(subset(score6$score,score6$score>0))/length(subset(score6$score,score6$score<0))

score7= score.sentiment(m7$MESSAGE_BODY, pos.words, neg.words, .progress='text')
ratio7 = length(subset(score7$score,score7$score>0))/length(subset(score7$score,score7$score<0))

score8= score.sentiment(m8$MESSAGE_BODY, pos.words, neg.words, .progress='text')
ratio8 = length(subset(score8$score,score8$score>0))/length(subset(score8$score,score8$score<0))

score9= score.sentiment(m9$MESSAGE_BODY, pos.words, neg.words, .progress='text')
ratio9 = length(subset(score9$score,score9$score>0))/length(subset(score9$score,score9$score<0))

score10= score.sentiment(m10$MESSAGE_BODY, pos.words, neg.words, .progress='text')
ratio10 = length(subset(score10$score,score10$score>0))/length(subset(score10$score,score10$score<0))

score11= score.sentiment(m11$MESSAGE_BODY, pos.words, neg.words, .progress='text')
ratio11 = length(subset(score11$score,score11$score>0))/length(subset(score11$score,score11$score<0))

score12= score.sentiment(m12$MESSAGE_BODY, pos.words, neg.words, .progress='text')
ratio12 = length(subset(score12$score,score12$score>0))/length(subset(score12$score,score12$score<0))

ratio = c(ratio1, ratio2,ratio3,ratio4,ratio5,ratio6,ratio7,ratio8,ratio9,ratio10,ratio11,ratio12)
month = c("January","February","March","April","May","June","July","August","September","October", "November","December")

plot=data.frame(Month=month, Ratio=ratio)
Line <- gvisLineChart(plot)
plot(Line)
