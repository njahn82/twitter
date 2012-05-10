#This R (helper) function queries the Twitter Search API
#
#https://dev.twitter.com/docs/api/1/get/search
#
#by hashtags.
#
#It returns basic data such as tweet (id) 
#user, time and tweet text as data.frame for up to 1.500 Tweets.
#
#NB: Provision of query without hashtag "#"

#dependencies
require(RJSONIO)
require(RCurl)

#function

hash.search.result <- function (query) {
  
  url <- paste("http://search.twitter.com/search.json?q=%23",query,
               "&rpp=50&include_entities=true",sep="")
  
  my.data <- data.frame()
  df.tmp <- data.frame()
  
  for (k in 1:30) {
    url.d <- paste(url,"&page=",k,sep="")
    my.json <- fromJSON(getURL(url.d), nullValue = NA)
    
    if(length(my.json$results) == 0)
      break
    else 
      
      for (i in seq_along(my.json$results)){
        id <- my.json$results[[i]]$id
        from_user <- my.json$results[[i]]$from_user
        text <- my.json$results[[i]]$text
        created <- my.json$results[[i]]$created_at
        to_user <-  my.json$results[[i]]$to_user_name
        
        df.tmp <- cbind(id, from_user, text, to_user, created)
        
        my.data <- rbind(my.data, df.tmp)
        
      } 
  }
  return(my.data)
}