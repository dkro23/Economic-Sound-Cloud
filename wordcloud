######
### Web-scraping and word-cloud practice
### Reference: http://www.sthda.com/english/wiki/text-mining-and-word-cloud-fundamentals-in-r-5-simple-steps-you-should-know
### https://blog.rstudio.com/2014/11/24/rvest-easy-web-scraping-with-r/
######

# Set directory


#### Wordcloud-ing


library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library(png)
library(magick)

#### Webscraping

### Load packages

library(rvest)
library(dplyr)
library(RedditExtractoR)
library(stringr)


### Scraping Reddit

# Test

reddit_data = get_reddit(search_terms = "science",subreddit = "science",cn_threshold=1)
head(reddit_data)
names(reddit_data)

titles<-unique(reddit_data$title)
head(titles)

# Test

reddit_data = get_reddit(subreddit = "Economics",cn_threshold=1,page_threshold = 5,sort_by = "relevance")[,c(14,3)]
x<-unique(reddit_data)
nrow(x)
head(x)

reddit.title<-x[,1]
head(reddit.title)

## Make wordcloud

# Transform

docs<-Corpus(VectorSource(reddit.title))

# Clean

docs <- tm_map(docs, content_transformer(tolower))

docs <- tm_map(docs, removeNumbers)

docs <- tm_map(docs, removeWords, stopwords("english"))

docs <- tm_map(docs, removePunctuation)

#docs <- tm_map(docs, stripWhitespace)

#docs <- tm_map(docs, stemDocument)

# Build doc matrix

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

# Generate wordcloud

set.seed(1234)
png("reddit.title.cloud.png")
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=50, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
dev.off()


### Scraping AER

# Test

url<-html("https://www.aeaweb.org/issues/576")


# Get list of articles

paste(".art_","12885"," a",sep="")

nums<-c(12885:12893)

articles<-c()
for (i in seq_along(1:NROW(nums))){
  articles[i]<-paste(".art_",nums[i]," a",sep="")
}

# Pulling titles -- test

url%>%
  html_node(".journal-article-group , .title a") %>%
  html_text()

# Pulling titles -- 1 page -- Works

titles<-c()
for (i in seq_along(1:NROW(articles))){
  titles[i]<-url%>%
    html_node(articles[i]) %>%
    html_text()
}
titles

#############################
############################


# Pulling title -- 6 pages -- not working

pages<-c(570:576) # to make urls
nums<-c(12776:12893) # to get set of article tags

urls<-list()
for(i in seq_along(1:NROW(pages))){
  urls[[i]]<-paste("https://www.aeaweb.org/issues/",pages[i],sep="")
  urls[[i]]<-read_html(urls[[i]][1])
}
urls

articles<-c()
for (i in seq_along(1:NROW(nums))){
  articles[i]<-paste(".art_",nums[i]," a",sep="")
}
articles


titles<-list() # list to put article titles in by url

for (i in seq_along(1:NROW(urls))){ # by urls
  titles<-c()
  for (j in seq_along(1:NROW(articles))){  # now go through possible articles
    titles[j]<-urls[[i]] %>%
      html_nodes(articles[j]) %>%  # need an ifelse to deal with not articles
      html_text()
  }
}


#############################
#############################
### Try #2 to scrape AER
### Want to scrape abstract as well, so need list of links

### Test

# Get article links

url<-"https://www.aeaweb.org/issues/576"
page<-read_html(url)

links <- page %>% html_nodes("a") %>% html_attr("href")
links <- links[which(regexpr('aer.2', links) >= 1)]

# Scrape title and abstract from 1 article

aer<-"https://www.aeaweb.org"

test.link<-links[1]

test.url<-read_html(paste(aer,test.link,sep=""))

article.title <-test.url%>%
  html_node(".title") %>%
  html_text()

article.abstract <-test.url%>%
  html_node(".abstract") %>%
  html_text()

article.abstract<-sub("Abstract\n\t\t\t\t\t","",article.abstract)
article.abstract<-sub("\t\t\t\t","",article.abstract)

# Scrape title and abstract from entire issue

title.vector<-c()
abstract.vector<-c()
for (i in seq_along(1:NROW(links))){
  aer<-"https://www.aeaweb.org"
  
  test.link<-links[i]
  
  test.url<-read_html(paste(aer,test.link,sep=""))
  
  article.title <-test.url%>%
    html_node(".title") %>%
    html_text()
  
  article.abstract <-test.url%>%
    html_node(".abstract") %>%
    html_text()
  
  article.abstract<-sub("Abstract\n\t\t\t\t\t","",article.abstract)
  article.abstract<-sub("\t\t\t\t","",article.abstract)
  
  title.vector[i]<-article.title
  abstract.vector[i]<-article.abstract
}
head(title.vector)
head(abstract.vector)




### For real

# Get links for a set of issues

issues<-c(560:576)

full.links<-c()
for (i in seq_along(1:NROW(issues))){
  url<-paste("https://www.aeaweb.org/issues/",issues[i],sep="")
  page<-read_html(url)
  
  links <- page %>% html_nodes("a") %>% html_attr("href")
  links <- links[which(regexpr('aer.2', links) >= 1)]

  full.links<-c(full.links,links)
}
full.links

# Scrape titles and abstracts

title.vector<-c()
abstract.vector<-c()
for (i in seq_along(1:NROW(full.links))){
  aer<-"https://www.aeaweb.org"
  
  test.link<-full.links[i]
  
  test.url<-read_html(paste(aer,test.link,sep=""))
  
  article.title <-test.url%>%
    html_node(".title") %>%
    html_text()
  
  article.abstract <-test.url%>%
    html_node(".abstract") %>%
    html_text()
  
  article.abstract<-sub("Abstract\n\t\t\t\t\t","",article.abstract)
  article.abstract<-sub("\t\t\t\t","",article.abstract)
  
  title.vector[i]<-article.title
  abstract.vector[i]<-article.abstract
}
head(title.vector)
head(abstract.vector)


### Create wordcloud for titles

# Transform

docs<-Corpus(VectorSource(title.vector))
inspect(docs)

# Clean

docs <- tm_map(docs, content_transformer(tolower))

docs <- tm_map(docs, removeNumbers)

docs <- tm_map(docs, removeWords, stopwords("english"))

docs <- tm_map(docs, removePunctuation)

#docs <- tm_map(docs, stripWhitespace)

#docs <- tm_map(docs, stemDocument)

# Build doc matrix

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

# Generate wordcloud

set.seed(1234)
png("aer.title.cloud.png")
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=50, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
dev.off()


# Combine PNGs

reddit<-readPNG("reddit.title.cloud.png")
aer<-readPNG("aer.title.cloud.png")

reddit<-image_read("reddit.title.cloud.png")
aer<-image_read("aer.title.cloud.png")

img<-c(reddit,aer)
x<-image_append(img)
image_write(x,"combined.png")

### Create wordcloud for abstracts

# Transform

docs<-Corpus(VectorSource(abstract.vector))

# Clean

docs <- tm_map(docs, content_transformer(tolower))

docs <- tm_map(docs, removeNumbers)

docs <- tm_map(docs, removeWords, stopwords("english"))

docs <- tm_map(docs, removePunctuation)

#docs <- tm_map(docs, stripWhitespace)

#docs <- tm_map(docs, stemDocument)

# Build doc matrix

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

# Generate wordcloud

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=150, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
