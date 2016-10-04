# Author: Dennis Walsh
# Last Updated: Sept 30, 2016
# This function takes a dataframe of Tweets, cleans the data - removing 
# punctuation and common words not useful for sentiment analysis, and adds 
# columns to the dataframe corresponding to the number of words in each tweet
# connoting different emotions.
getsentiments <- function(tweets = NULL) {
        # Data Cleaning
        require(tm) #text mining
        require(stringr)
        # Tweet columns we care about
        keepvars <- c('text','id','screenName','created','longitude',
                      'latitude','isRetweet','favoriteCount','term')
        tweets <- tweets[keepvars]
        tweets$geocoded <- ifelse(is.na(tweets$longitude),FALSE,TRUE)
        tweets$created <- as.Date(tweets$created)
        # remove twitter handles
        tweets$cleantext <- str_replace_all(tweets$text, pattern = "@\\w+", 
                                        replacement = "") 
        tweets$cleantext <- str_replace_all(tweets$cleantext, pattern = "�", 
                                        replacement = "") 
        # removing emoticons
        tweets$cleantext <- sapply(tweets$cleantext,function(row) { 
                                iconv(row, "latin1", "ASCII", sub="")})
        # Sentiment Analysis
        require(syuzhet) 
        sentiments <- get_nrc_sentiment(tweets$cleantext)
        tweetswithsentiments <- cbind(tweets, sentiments)
        save(tweetswithsentiments, file = 'tweetswithsentiments.Rda')
}