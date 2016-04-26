# Twitter term-document-matrix
Lidan Hifi and Hagai Levi  
9 April 2016  



A preface to this work can be found in the [README](README.md).  

We have decided to first show clustering and only then to show the centrality metrics
so that we can include the clusters in the graph when plotting the centrality metrics,
even though it is not the order of the tasks in the assignment.  

We have based on http://www.rdatamining.com/examples/text-mining for retrieving the tweets.


```r
setwd('/Users/hagai_lvi/tmp/data_scientist/assignment_3')
source('./credentials.R')
```


```r
library(twitteR)

# set twitter credentials
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_token_secret)
```

## Collecting data
The data that we will be using is tweets by Barack Obama.  

```r
# Create the data dir if it doesn't exist
if(! file.exists('data')){
  dir.create('data')
}

if(! file.exists('./data/tweets.df')){
  maxID <- NULL
  n_tweets <- 0
  N_TWEETS <- 300
  tweets <- NULL
  while(n_tweets < N_TWEETS) {
    tmp <- userTimeline("BarackObama", n=200, maxID = maxID)
    maxID <- tmp[[length(tmp)]]$id  
    tweets <- append(tweets, tmp)
    n_tweets <- length(tweets)
  }
  
  # Create a dataframe from the data
  df <- do.call("rbind", lapply(tweets, as.data.frame))
  
  # Write the data to file
  tweets_text <- df$text
  write.table(tweets_text, file='./data/tweets.df')

} else{
  tweets_text <- as.vector(unlist(read.table('./data/tweets.df')))
}
```

## Basic exploration of the data
Extract a corpus and a TermDocumentMatrix:

```r
# This function returns the most frequent terms in a TermDocumentMatrix
# as a named vector that includes the frequencies
getMostFrequentTerms <- function(dtm, N){
  m <- as.matrix(dtm)
  v <- sort(rowSums(m), decreasing=TRUE)
  head(v, N)
}

cat(sprintf("Got %i rows", length(tweets_text)))
```

```
## Got 312 rows
```

```r
library(tm)

# build a corpus, which is a collection of text documents
# VectorSource specifies that the source is character vectors.
myCorpus <- Corpus(VectorSource(tweets_text))

# all lowercase
myCorpus <- tm_map(myCorpus, content_transformer(tolower))

# remove punctuation
myCorpus <- tm_map(myCorpus, removePunctuation)

# remove numbers
myCorpus <- tm_map(myCorpus, removeNumbers)

# remove stopwords
myStopwords <- c(stopwords('english'), "available", "via")
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)

# stem words in a text document with the snowball stemmers,
# which requires packages Snowball, RWeka, rJava, RWekajars
myCorpus <- tm_map(myCorpus, stemDocument)

# Create a TermDocumentMatrix
myDtm <- TermDocumentMatrix(myCorpus, control = list(minWordLength = 1))
inspect(myDtm[20:30,20:30])
```

```
## <<TermDocumentMatrix (terms: 11, documents: 11)>>
## Non-/sparse entries: 0/121
## Sparsity           : 100%
## Maximal term length: 11
## Weighting          : term frequency (tf)
## 
##              Docs
## Terms         20 21 22 23 24 25 26 27 28 29 30
##   alreadi      0  0  0  0  0  0  0  0  0  0  0
##   also         0  0  0  0  0  0  0  0  0  0  0
##   alway        0  0  0  0  0  0  0  0  0  0  0
##   america      0  0  0  0  0  0  0  0  0  0  0
##   american     0  0  0  0  0  0  0  0  0  0  0
##   americaus    0  0  0  0  0  0  0  0  0  0  0
##   anniversari  0  0  0  0  0  0  0  0  0  0  0
##   announc      0  0  0  0  0  0  0  0  0  0  0
##   anoth        0  0  0  0  0  0  0  0  0  0  0
##   answer       0  0  0  0  0  0  0  0  0  0  0
##   anybodi      0  0  0  0  0  0  0  0  0  0  0
```

Display words associated with Obama and american

```r
# which words are associated with "obama"?
findAssocs(myDtm, 'obama', 0.30)
```

```
## $obama
## upresid  presid    sotu    live 
##    0.61    0.58    0.41    0.30
```

```r
# which words are associated with "american"?
findAssocs(myDtm, 'american', 0.30)
```

```
## $american
##           cover httpstcoxtyztaa 
##            0.44            0.44
```

Convert to a term matrix

```r
# Transform Data into an Adjacency Matrix
termDocMatrix <- as.matrix(myDtm)

# change it to a Boolean matrix
termDocMatrix[termDocMatrix>=1] <- 1

# transform into a term-term adjacency matrix
termMatrix <- termDocMatrix %*% t(termDocMatrix)

# inspect terms numbered 5 to 10
termMatrix[15:20,15:20]
```

```
##            Terms
## Terms       agre agreement ahead ainut almost alreadi
##   agre        10         0     0     0      0       0
##   agreement    0         2     0     0      0       0
##   ahead        0         0     2     0      0       0
##   ainut        0         0     0     1      0       0
##   almost       0         0     0     0      1       0
##   alreadi      0         0     0     0      0       1
```

The corpus is large, and we can't see clearly a large graph so we are using only a few
frequent items.

```r
library(igraph)

# Get the most frequent words
frequent <- getMostFrequentTerms(myDtm, 20)
frequentTermMatrix <- termMatrix[names(frequent),names(frequent)]
```

## Basic graphs
We construct a graph from the most frequent terms:

```r
# make a binary matrix
frequentTermMatrix[frequentTermMatrix>1] <- 1
g <- graph.adjacency(frequentTermMatrix, mode = "undirected")

# remove self loops
g <- simplify(g)
V(g)$degree <- degree(g)

# Plot the Graph
# set seed to make the layout reproducible
set.seed(100)
lay <- layout.kamada.kawai(g)
plot(g, layout=lay)
```

![](twitter_net_files/figure-html/unnamed-chunk-8-1.png)

## Clustering

### `walktrap` clustering
Adding clustering using the `walktrap` strategy:

```r
# Now add clustering to the graph
community <- walktrap.community(g)
plot(g, layout=lay, vertex.size=5, vertex.color=community$membership, asp=FALSE)
```

![](twitter_net_files/figure-html/unnamed-chunk-9-1.png)

We got 2 communities. The size of each community:


```r
sizes(community)
```

```
## Community sizes
##  1  2 
## 14  6
```

The modularity we got:

```r
modularity(community)
```

```
## [1] 0.1011842
```

### `edge betweenness` clustering

Now clustering using the `edge betweenness` strategy:

```r
# Now add clustering to the graph
community <- edge.betweenness.community(g)
plot(g, layout=lay, vertex.size=5, vertex.color=community$membership, asp=FALSE)
```

![](twitter_net_files/figure-html/unnamed-chunk-12-1.png)

We got 10 communities. The size of each community:


```r
sizes(community)
```

```
## Community sizes
##  1  2  3  4  5  6  7  8  9 10 
## 11  1  1  1  1  1  1  1  1  1
```

The modularity of `edge betweenness`:

```r
modularity(community)
```

```
## [1] 0.01422903
```

## Centrality metrics

### Betweenness
We want to show the betweenness of each term: 

```r
t(betweenness(g))
```

```
##         obama   presid  upresid     sotu doyourjob    fair    senat
## [1,] 6.452417 8.633766 3.536147 1.378211  3.294877 8.79329 1.339286
##      actonclim      job     year     hear     work   garland      judg
## [1,] 0.7944444 10.78615 3.220238 4.641667 1.569877 0.8511905 0.8511905
##        scotus    suprem     court     chang    climat american
## [1,] 2.122222 0.7873377 0.2159091 0.8289683 0.8289683 2.073846
```

And plot it on the graph, together with the clusters:

```r
plot(g, layout=lay, vertex.color=community$membership, vertex.size=betweenness(g), asp=FALSE)
```

![](twitter_net_files/figure-html/unnamed-chunk-16-1.png)

### Closeness

```r
t(closeness(g))
```

```
##           obama     presid upresid       sotu  doyourjob fair      senat
## [1,] 0.04761905 0.04761905    0.04 0.03846154 0.04166667 0.05 0.03846154
##       actonclim        job       year       hear       work    garland
## [1,] 0.03571429 0.05263158 0.03846154 0.04347826 0.03703704 0.03703704
##            judg     scotus     suprem      court      chang     climat
## [1,] 0.03703704 0.03846154 0.03571429 0.03333333 0.03571429 0.03571429
##        american
## [1,] 0.03846154
```

```r
plot(g, layout=lay, vertex.color=community$membership, vertex.size=closeness(g), asp=FALSE)
```

![](twitter_net_files/figure-html/unnamed-chunk-17-1.png)

And as closeness generates small values, we will increases the vertexes size:

```r
g.closeness <- closeness(g)
g.closeness.normalized <- 10 * (g.closeness / min(g.closeness))
t(g.closeness.normalized)
```

```
##         obama   presid upresid     sotu doyourjob fair    senat actonclim
## [1,] 14.28571 14.28571      12 11.53846      12.5   15 11.53846  10.71429
##           job     year     hear     work  garland     judg   scotus
## [1,] 15.78947 11.53846 13.04348 11.11111 11.11111 11.11111 11.53846
##        suprem court    chang   climat american
## [1,] 10.71429    10 10.71429 10.71429 11.53846
```

```r
plot(g, layout=lay, vertex.color=community$membership, vertex.size=g.closeness.normalized, asp=FALSE)
```

![](twitter_net_files/figure-html/unnamed-chunk-18-1.png)

### Eigen values

**For eigenvalues, we will not show a graph, due to negative values.**


```r
eigen_values <- eigen(frequentTermMatrix)$values
names(eigen_values) <- colnames(frequentTermMatrix)
eigen_values
```

```
##         obama        presid       upresid          sotu     doyourjob 
##  1.427923e+01  5.665468e+00  3.114183e+00  1.985561e+00  1.928212e+00 
##          fair         senat     actonclim           job          year 
##  1.240961e+00  9.874387e-01  4.901258e-01  3.329576e-01  1.606521e-16 
##          hear          work       garland          judg        scotus 
##  1.195293e-17 -2.664535e-15 -1.716207e-01 -4.733044e-01 -7.589712e-01 
##        suprem         court         chang        climat      american 
## -1.009830e+00 -1.237362e+00 -1.410289e+00 -2.260556e+00 -2.702208e+00
```
