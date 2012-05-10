#This R (helper) function queries the Twitter Search API
#
#https://dev.twitter.com/docs/api/1/get/search
#
#by hashtags.
#
#It returns co-occurent hash-tags for each tweet (id) 
#and user as data.frame for up to 1.500 Tweets.
#
#NB: Provision of query without hashtag "#"

#dependencies
require(RJSONIO)
require(RCurl)

#function

hash.search.hash <- function (query) {
  
  url <- paste("http://search.twitter.com/search.json?q=%23",query,
               "&rpp=50&include_entities=true",sep="")
  
  my.data <- data.frame()
  
  for (k in 1:30) {
    url.d <- paste(url,"&page=",k,sep="")
    my.json <- fromJSON(getURL(url.d), nullValue = NA)
    
    if(length(my.json$results) == 0)
      break
    else 
      
      for (i in seq_along(my.json$results)){
        id <- my.json$results[[i]]$id
        from_user <- my.json$results[[i]]$from_user
        hashtag <- my.json$results[[i]]$entities$hashtags
        
        hash <- c()
        
        for (j in seq_along(hashtag)) {
          
          hash.tmp <- hashtag[[j]]$text
          hash <- c(hash, hash.tmp)
          
        }
        id <- rep(id,times=length(hash))
        from_user <- rep(from_user,times=length(hash))
        
        df.tmp <- cbind(id,from_user, hash)
        
        my.data <- rbind(my.data, df.tmp)
        
      } 
  }
  return(my.data)
}
