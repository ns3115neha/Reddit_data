#############################################Reddit###########################################
install.packages("RedditExtractoR")
install.packages("topicmodels")
install.packages("tm")
install.packages("wordcloud")
install.packages("rgexf")
install.packages("SnowballC")
install.packages("lda")
install.packages("syuzhet")


library(RedditExtractoR)
library(topicmodels)
library(tm)
library(wordcloud)
library(rgexf)
library(SnowballC)
library(lda)
library(syuzhet)


#http://www.reddit.com/r/Overwatch/comments/5g19ni/overwatch_wins_game_of_the_year/?ref=search_posts

#get_reddit(search_terms = NA, regex_filter = "", subreddit = NA,cn_threshold = 1, page_threshold = 1, sort_by = "comments",wait_time = 2)
duty <- get_reddit(search_terms = "Call of Duty", regex_filter = "", subreddit = "gaming",cn_threshold = 1, page_threshold = 1, sort_by = "comments",wait_time = 2)
#write.csv(duty, "duty.csv")
dutyInfiniteWarfare <- get_reddit(search_terms = "Call of Duty: Infinite Warfare ", regex_filter = "", subreddit = "gaming",cn_threshold = 1, page_threshold = 1, sort_by = "comments",wait_time = 2)
#write.csv(dutyInfiniteWarfare, "dutyInfiniteWarfare.csv")
Overwatch <- get_reddit(search_terms = "Overwatch", regex_filter = "", subreddit = "gaming",cn_threshold = 1, page_threshold = 1, sort_by = "comments",wait_time = 2)
#write.csv(Overwatch,"Overwatch.csv")


#Find the URL's containing the terms Overwatch and fetching comments based on a URL.
AAA <- reddit_urls(search_terms = "Overwatch", regex_filter = "", subreddit = "gaming",cn_threshold = 1, page_threshold = 1, sort_by = "comments",wait_time = 2)
example_attr = reddit_content(URL="http://www.reddit.com/r/gaming/comments/51tabt/battlefield_one_is_130_usd_for_the_full_gamedont/?ref=search_posts")

##Controversial Reddit 
#http://www.express.co.uk/entertainment/gaming/745259/Overwatch-Reddit-PS4-Xbox-One-PC-Custom-Games-Blizzard

Controversial_Redddit_Overwatch <- reddit_content(URL="https://www.reddit.com/r/Overwatch/comments/5j3p4i/introducing_infiltration_a_fun_new_game_mode_we/")

###### TOPIC MODELLING #######

topicdata <- duty$comment
docs <- Corpus(VectorSource(topicdata))
docs <-tm_map(docs,content_transformer(tolower),lazy=TRUE)
docs <- tm_map(docs, stripWhitespace,lazy=TRUE)
docs <- tm_map(docs,stemDocument,lazy=TRUE)
docs <- tm_map(docs,removePunctuation,lazy=TRUE)
docs <- tm_map(docs,removeWords,stopwords("english"))
dtm <- DocumentTermMatrix(docs)
writeLines(as.character(docs[[40]]))
freq <- colSums(as.matrix(dtm))
length(freq)
ord <- order(freq,decreasing=TRUE)
freq[ord]
K <- 6   # number of topics 
rowTotals <- apply(dtm, 1, sum)
dtm.new <- dtm[rowTotals > 0, ]
lda <- LDA(dtm.new, k=6)
topic <- topics(lda, 1)
term <- terms(lda,4)
term
topics  <- data.frame(topic)
str(topics)

topics_new <- cbind("Topics" = rownames(topics$topics))
qplot(names(topics), ..count.. , data=topics, geom="bar", fill=term[topic],position="dodge", xlab = "Topics", ylab="Count", main="LDA Topic Modelling  -Data  Exploration")


#### WORD CLOUD Call Of Duty  ###

words<- strsplit(as.character(duty$comment), " ")
words<- lapply(words, function(x) x[grep("^[A-Za-z0-9]+$", x)])
words<- unlist(words)
words<- tolower(words)
stopWords<- stopwords("en")
"%!in%" <- function(x,table) match(x,table, nomatch = 0) == 0  ## The function table creates a frequency on the number of times a certain word occurs.
words<- words[words %!in% stopWords]
allwords<- as.data.frame(table(words))
wordcloud(allwords$words, allwords$Freq, random.order = FALSE,min.freq=5, max.words=200,colors = brewer.pal(2, "Dark2"))


#### WORD CLOUD Call Of Duty InfiniteWarfare  ###
words1<- strsplit(as.character(dutyInfiniteWarfare$comment), " ")
words1<- lapply(words1, function(x) x[grep("^[A-Za-z0-9]+$", x)])
words1<- unlist(words1)
words1<- tolower(words1)
stopWords1<- stopwords("en")
"%!in%" <- function(x,table) match(x,table, nomatch = 0) == 0  ## The function table creates a frequency on the number of times a certain word occurs.
words1<- words1[words1 %!in% stopWords1]
allwords1 <- as.data.frame(table(words1))
wordcloud(allwords1$words, allwords1$Freq, random.order = FALSE,min.freq=5, max.words=200,colors = brewer.pal(2, "Dark2"))

#### WORD CLOUD oVERwATCH  ###
words2<- strsplit(as.character(Overwatch$comment), " ")
words2<- lapply(words2, function(x) x[grep("^[A-Za-z0-9]+$", x)])
words2<- unlist(words2)
words2<- tolower(words2)
stopWords2<- stopwords("en")
"%!in%" <- function(x,table) match(x,table, nomatch = 0) == 0  ## The function table creates a frequency on the number of times a certain word occurs.
words2<- words2[words2 %!in% stopWords2]
allwords2 <- as.data.frame(table(words2))
wordcloud(allwords2$words, allwords2$Freq, random.order = FALSE,min.freq=5, max.words=200,colors = brewer.pal(2, "Dark2"))

#Sentiment Analysis

comment_sent<-get_nrc_sentiment(duty$comment)
sum_data_raw_text12 <- data.frame(colSums(comment_sent[1:10]))
names(sum_data_raw_text12)[1] <- 'count'
sum_data_raw_text12 <- cbind('sentiment' = rownames(sum_data_raw_text12), sum_data_raw_text12)
rownames(sum_data_raw_text12) <- NULL
senti_Raw_text12 <- sum_data_raw_text12[1:10,]
offical_remarks <- ggplot(senti_Raw_text12, aes(x=sentiment, y=count, fill=sentiment)) + geom_bar(stat = 'identity') + labs(x="Sentiments", y="Frequency", title="Sentiment Analysis of Call Of duty on Reddit")
print(offical_remarks)
