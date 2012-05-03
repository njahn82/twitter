####Load Data JSON####

library(RJSONIO)
library(RCurl)

url <- c("http://search.twitter.com/search.json?q=%23biconf2012&rpp=50&include_entities=true")

my.data <- data.frame()

for (k in 1:15){
  url.d <- paste(url,"&page=",k,sep="")
  my.json <- fromJSON(getURL(url.d), nullValue = NA)
  
  if(length(my.json$results) == 0)
    break
  else 
    
    for (i in seq_along(my.json$results)){
      
      from_user <- my.json$results[[i]]$from_user
      
      hashtag <- my.json$results[[i]]$entities$hashtags
      
      hash <- c()
      
      for (j in seq_along(hashtag)) {
        
        hash.tmp <- hashtag[[j]]$text
        
        hash <- c(hash,hash.tmp)
        
        user <- rep(from_user, times=length(hash))
        
        df.tmp <- cbind(from_user,hash)
      }
      
      my.data <- rbind(my.data,df.tmp)
      
    }
  
}

###prepare network data

mat <- table(my.data$from_user,my.data$hash)

print(dim(mat))
#[1] 54 43

####igraph

library(igraph)

my.graph <- graph.incidence(mat)

##manual fix encode
V(my.graph)$name[73] <- "tromsö"

V(my.graph)$label = V(my.graph)$name

V(my.graph)$color[1:54] = "#E41A1C"
V(my.graph)$color[55:97] = "#377EB8"

#plot with hashtag labels

V(my.graph)$label[1:54] = NA

plot(my.graph,layout=layout.fruchterman.reingold)

#V(my.graph)$label.color[1:54] = "#E41A1C"
V(my.graph)$label.color[55:97] = "grey30"


V(my.graph)$label.cex = sqrt(degree(my.graph))*0.4
V(my.graph)$size = sqrt(degree(my.graph))*1.5
V(my.graph)$frame.color = NA
