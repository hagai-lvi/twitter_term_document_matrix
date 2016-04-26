# Twitter networks

This is the second assignment in the data scientist course at BGU university.
The previous assignment is at [yelp-api-exploration](https://github.com/hagai-lvi/yelp-api-exploration)

The main idea of this assignment is to experiment with networks analysis.
We are showing some clustering, and visualization of networks and clusters.

In this assignment we are gathering tweets by Barack Obama. Then we create a
[Document-term matrix](https://en.wikipedia.org/wiki/Document-term_matrix) that
shows us which terms appear in each of the documents (a document is a tweet in
our case).  
After that we are constructing a term-term matrix, that shows to which terms
each term is connected. This is actually a adjacency matrix representation of a
graph that shows the connection between the terms.  
There are many terms, so we only choose the most frequent terms, and show them
in a graph with clustering.


## Check the results
View directly in github [here](twitter_net.md)
