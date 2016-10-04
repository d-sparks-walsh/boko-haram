library(XML)
library(RDSTK)
library(httr)
# This function takes a dataframe of Tweets, cleans the data - removing 
# punctuation and common words not useful for sentiment analysis, and 
sentimentanalysis <- function(df = NULL) {
        # Data Cleaning
        library(tm) #text mining
        library(stringr)
        # Tweet columns we care about
        keepvars <- c('text','id','screenName','text','created','longitude',
                      'latitude','isRetweet','favoriteCount','term')
        df <- df[keepvars]
        df$geocoded <- ifelse(is.null(df$longitude),FALSE,TRUE)
        df$created <- as.Date(df$created)
        # remove twitter handles
        df$cleantext <- str_replace_all(df$text, pattern = "@\\w+", 
                                        replacement = "") 
        df$cleantext <- str_replace_all(df$cleantext, pattern = "�", 
                                        replacement = "") 
        # removing emoticons
        df$cleantext <- sapply(df$cleantext,function(row) { 
                                iconv(row, "latin1", "ASCII", sub="")})
        # Sentiment Analysis
        require(syuzhet) 
        sentiments <- get_nrc_sentiment(df$cleantext)
        df <- cbind(df, sentiments)
        save(df, file = 'sentimentanalysis.Rda')
}
sentimentanalysis()
load('sentimentanalysis.Rda')

        library(ggplot2)
        sentimentTotals = data.frame(count = colSums(sentiments)) #sum counts and "count =" column name
        sentimentTotals$sentiment = rownames(sentimentTotals) #adding rownames into dataframe
        
        ggplot(sentimentTotals,aes(x=reorder(sentiment,count), y = count))+
          theme_classic()  +
          geom_bar(stat="identity", aes(fill=sentiment))+
          theme(legend.position = "none") + xlab("Sentiment") + ylab("Count") +
          ggtitle("Twitter Sentiment Analysis")
 
plottweets <- function(locations='KPHospLoc.txt', tweets = df) {
        require(ggmap)  
        require(sqldf)
        require(ggrepel)
        locs <- read.table(file = 'KPHospLoc.txt', stringsAsFactors = FALSE, header = TRUE)

        aggtweets <- sqldf(
                'SELECT
                        longitude
                        , latitude
                        , count(id) as number_of_tweets
                        , sum(anger) as anger
                        , sum(anticipation) as anticipation
                        , sum(disgust) as disgust
                        , sum(fear) as fear
                        , sum(joy) as joy
                        , sum(sadness) as sadness
                        , sum(surprise) as surprise
                        , sum(trust) as trust
                        , sum(negative) as negative
                        , sum(positive) as positive
                FROM df
                GROUP BY
                        longitude, latitude'
        )
        aggtweets <- aggtweets[which(aggtweets$number_of_tweets >= 100),]
        
        locmap <- get_map(location = c(min(locs$long), 
                                       min(locs$lat), 
                                       max(locs$long), 
                                       max(locs$lat)), 
                          maptype = 'satellite')
        ggmap(locmap) + 
                geom_point(data = aggtweets, aes(x = as.numeric(longitude), y = as.numeric(latitude), colour = positive/number_of_tweets), 
                           size = aggtweets$number_of_tweets/sum(aggtweets$number_of_tweets) * 400) +
                scale_colour_gradient(low='red',high='green') + 
                geom_point(data = locs, aes(x = long, y = lat), colour = 'white', size = 2)  +
                geom_text_repel(data=locs, aes(x=long, y=lat, label=Hospital), size = 4, colour='white')
}
plottweets()
