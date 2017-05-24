---
title: "HW3_Solution"
author: "Roy Miron & Bar Golikov"
date: "19 במאי 2017"
output: html_document
---

# Question 1 Part 1

## Setup

Set the working directory and load the igraph and the cluster packages:

```{r setup}
#knitr::opts_knit$set(root.dir = 'C:/Users/Roy/Desktop/GreysAnatomy')
getwd()
#install.packages("igraph")
suppressMessages(library(igraph))
#install.packages("cluster")
suppressMessages(library(cluster))
#install.packages("tm")
suppressMessages(library(tm))
#install.packages("Rfacebook")
#library(Rfacebook) //all the necessary data from facebook is saved offline so don't need that to run

```


Read data from csv file with header:

```{r}
#edgelist = read.csv('C:/Users/Roy/Desktop/GreysAnatomy/ga_edgelist.csv', header = T)
#save(edgelist, file = "edgelist")

load("edgelist")

```

## Graph creation

Create undirected graph to represent our data using igraph:

```{r}
graph = graph.data.frame(edgelist, directed = F)

```

Plot the graph to visuallize the data:

```{r}
plot(graph)

```
![](ex3-Network-Analysis/images/1.png)

## Calculating centerallity by betweenness, closeness, eigencetor:


```{r}
betweenness = betweenness(graph, V(graph), directed = FALSE)
max_betweenness = which(max(betweenness) == betweenness)
max_betweenness
max(betweenness)

```

**1. Highest centerallity by betweenness - "Sloan" (Vertex #3) with the value 115.3667**

```{r}
closeness = closeness(graph, vids = V(graph))
max_closeness = which(max(closeness) == closeness)
max_closeness
max(closeness)

```

**2. Highest centerallity by closeness - "Torres" (Vertex #4) with the value 0.003194888**

```{r}
eigencetor = eigen_centrality(graph, directed = FALSE)
max_eigencetor = which(max(eigencetor$vector) == eigencetor$vector)
max_eigencetor
max(eigencetor$vector)

```

**3. Highest centerallity by eigencetor - "Karev" (Vertex #6) with the value 1**


# Question 1 Part 2


### Cluster 1

Girvan-Newman community detection algorithm:

```{r}
cluster1 = edge.betweenness.community(graph)

```


**1. Plot the graph with unique color for each cluster group**

```{r}
plot(graph, vertex.label=NA, vertex.color=membership(cluster1), vertex.size=8, asp=FALSE)

```
![](ex3-Network-Analysis/images/2.png)

Now let's look at the membership property of our cluster:

```{r}
groups1 = table(cluster1$membership)
plot(groups1, xlab = "Group Number", ylab = "Group Size", type = "p")

```
![](ex3-Network-Analysis/images/3.png)

**2. The above histogram describes our groups - number of groups in the cluster and the size of each group**

Max modularity value of our cluster:

```{r}
max(cluster1$modularity)

```

**3. The maximum modularity value for our cluster is: 0.5774221**

### Cluster 2

Fast-greedy community detection algorithm:

```{r}
graph2 = simplify(graph)
cluster2 = fastgreedy.community(graph2)

```

**1. Plot the graph with unique color for each cluster group**

```{r}
plot(graph2, vertex.label=NA, vertex.color=membership(cluster2), vertex.size=8, asp=FALSE)

```
![](ex3-Network-Analysis/images/4.png)

Now let's look at the membership property of our cluster:

```{r}
groups2 = table(cluster2$membership)
plot(groups2, xlab = "Group Number", ylab = "Group Size", type = "p")

```
![](ex3-Network-Analysis/images/5.png)

**2. The above histogram describes our groups - number of groups in the cluster and the size of each group**

Max modularity value of our cluster:

```{r}
max(cluster2$modularity)

```

**3. The maximum modularity value for our cluster is: 0.5947232**


# Question 2

## Data mining Facebook using the Rfacebook package


Create OAuth for authentication with Facebook:

```{r}
#load("my_oauth") //don't need this to run. all facebook data saved offline

```

Collect the 150 most liked posts out of the last 300 posts on bbc international page:

```{r}
#fb_search = getPage(page = "cnninternational", my_oauth, n = 300)
#save(fb_search, file = "fb_search")

load("fb_search")
fb_posts_with_top_shares = tail(fb_search[order(fb_search$likes_count),], 150)

```

Build corpus from the messages of each post:
- Lower all leters
- Remove numbers, punctuation words, stop words and extra white spaces

```{r}
corpus = Corpus(VectorSource(fb_posts_with_top_shares$message))
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removeNumbers)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, c("the", "and", stopwords("english")))
corpus =  tm_map(corpus, stripWhitespace)

```

Our corpus contains lot of terms including the strings "http" and "cnn" so let's remove them:

```{r}
toSpace = content_transformer( function(x, pattern) gsub(pattern," ",x) )
corpus = tm_map(corpus, toSpace, "http*")
corpus = tm_map(corpus, toSpace, "cnn*")

```

Now we can build the Document-Term matrix from our corpus:

```{r}
docTermMatrix = DocumentTermMatrix(corpus)
matrix = t(as.matrix(docTermMatrix))

```

In this step we keep only the terms that apperas in more then 5 posts (so we will have an interesting graph):

```{r}
colTotals =  rowSums(matrix)
docTermMatrix2 = docTermMatrix[,which(colTotals > 4)]
matrix2 = t(as.matrix(docTermMatrix2))

```

Here we build the adjacency matrix and our graph (Vertex = Term, Edge = between terms that apper together in posts):

```{r}
adjacencyMatrix = matrix2 %*% t(matrix2)
fb_graph = graph.adjacency(adjacencyMatrix, mode = "undirected")

```

Simplify our graph so we no self loops and mulitple edges exist:

```{r}
fb_graph_simple = simplify(fb_graph)
plot(fb_graph_simple, layout=layout_with_kk)

```
![](ex3-Network-Analysis/images/6.png)

# Back to question 1 with our facebook data graph


## Calculating centerallity by betweenness, closeness, eigencetor:

```{r}
betweenness = betweenness(fb_graph_simple, V(fb_graph_simple), directed = FALSE)
max_betweenness = which(max(betweenness) == betweenness)
max_betweenness
max(betweenness)

```

**1. Highest centerallity by betweenness - the term "people" with the value 61.43688**

```{r}
closeness = closeness(fb_graph_simple, vids = V(fb_graph_simple))
max_closeness = which(max(closeness) == closeness)
max_closeness
max(closeness)

```

**2. Highest centerallity by closeness - 3 terms: "donald", "trump", "president" with the value 0.01923077**

```{r}
eigencetor = eigen_centrality(fb_graph_simple, directed = FALSE)
max_eigencetor = which(max(eigencetor$vector) == eigencetor$vector)
max_eigencetor
max(eigencetor$vector)

```

**3. Highest centerallity by eigencetor - 2 terms: "donald", "president" with the value 1**



# Question 1 Part 2 with our graph


### Cluster 1

Girvan-Newman community detection algorithm:

```{r}
fb_cluster1 = edge.betweenness.community(fb_graph_simple)

```


**1. Plot the graph with unique color for each cluster group**

```{r}
plot(fb_graph_simple, vertex.label=NA, vertex.color=membership(fb_cluster1), vertex.size=8, asp=FALSE)

```
![](ex3-Network-Analysis/images/7.png)

Now let's look at the membership property of our cluster:

```{r}
fb_groups1 = table(fb_cluster1$membership)
plot(fb_groups1, xlab = "Group Number", ylab = "Group Size", type = "p")

```
![](ex3-Network-Analysis/images/8.png)

**2. The above histogram describes our groups - number of groups in the cluster and the size of each group**

Max modularity value of our cluster:

```{r}
max(fb_cluster1$modularity)

```

**3. The maximum modularity value for our cluster is: 0.1536495**

### Cluster 2

Fast-greedy community detection algorithm:

```{r}
fb_cluster2 = fastgreedy.community(fb_graph_simple)

```

**1. Plot the graph with unique color for each cluster group**

```{r}
plot(fb_graph_simple, vertex.label=NA, vertex.color=membership(fb_cluster2), vertex.size=8, asp=FALSE)

```
![](ex3-Network-Analysis/images/9.png)

Now let's look at the membership property of our cluster:

```{r}
fb_groups2 = table(fb_cluster2$membership)
plot(fb_groups2, xlab = "Group Number", ylab = "Group Size", type = "p")

```
![](ex3-Network-Analysis/images/10.png)

**2. The above histogram describes our groups - number of groups in the cluster and the size of each group**

Max modularity value of our cluster:

```{r}
max(fb_cluster2$modularity)

```

**3. The maximum modularity value for our cluster is: 0.2558393**







