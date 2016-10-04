# AUthor: Dennis Walsh
# Last Updated: Sept 30, 2016
# This function takes:
#       1) A vector of search terms to be searched for each location
#       2) An existing dataset of tweets where results can be appended
# and then searches twitter for each term and saves it in a dataset along with
# the search term that returned the tweet. It is intended to help build up a 
# database of Tweets over time because the Twitter search API usually just
# returns Tweets from the last seven days.
savetweetstodb <- function(searchterms = NULL,
                           appendtodataset = NULL) {
        require(twitteR)
        # If no dataset is entered, create a new dataframe
        if (is.null(appendtodataset)) {
                tweets <- data.frame()
                maxid <- NULL
                }
        # Otherwise, load in dataset as dataframe
        else {
                tweets <- load(appendtodataset)
                # Get ID of most recent tweet in dataset, so that search can be 
                # limited to tweets more recent than those already in the 
                # dataset
                maxid <- max(tweets$id)
        }
        for (i in 1:length(searchterms)) {
                        # The first search uses geolocation
                a <- searchTwitter(
                        #Get the current search term
                        term[i],
                        #Limit to 1000 tweets per term
                        n = 1000,
                        lang = 'en',
                        r
                        sinceID = maxid)
                # If the search returned results, convert the results to a 
                # dataframe
                if (length(a) > 0) {
                        tweetsbyterm <- twListToDF(a) 
                        # Create column which specifies which term was searched
                        tweetsbyterm$term <- searchterms[i]
                        # Add tweets to the dataset
                        tweets <- rbind(tweets,tweetsbyterm)
                        }
                        # Wait 10 seconds so the Twitter API doesn't freak out
                        Sys.sleep(10)
        }
        write.csv(tweets, file = 'tweets.csv')
        save(tweets, file = 'tweets.Rda')
}
