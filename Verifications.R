# Verification

# Include parameters and Functions
source("Functions.R")
source("Parameters.R")
source("MainFunctions.R")

######## PR and IR inputs
n.test <- 2*pool.r*100
  # going from bottom to up
sensor.test.up.x <- rep(0,n.test)
sensor.test.up.y <- c(-(n.test/2):(n.test/2))/100

PR.test.up <- numeric(n.test)
IR.test.up <- numeric(n.test)
for(i in 1:n.test){
  PR.test.up[i] <- get.PR(c(sensor.test.up.x[i], sensor.test.up.y[i]))
  IR.test.up[i] <- get.IR(c(sensor.test.up.x[i]))
}
# PR: expect to see input values increase from 0 to 255, then decrease back to 0
plot(PR.test.up)
# IR: expect to see only 0
plot(IR.test.up)

  # going from left to right
sensor.test.right.x <- c(-(n.test/2):(n.test/2))/100
sensor.test.right.y <- rep(0,n.test)

PR.test.right <- numeric(n.test)
IR.test.right <- numeric(n.test)
for(i in 1:n.test){
  PR.test.right[i] <- get.PR(c(sensor.test.right.x[i], sensor.test.right.y[i]))
  IR.test.right[i] <- get.IR(c(sensor.test.right.x[i]))
}
# PR: expect to see no inputs, then 127.5 - 255 - 127.5 then no inputs again
plot(PR.test.right)
# IR: expect to see 255 － 0, then drops to 0 for a while, then 0 － 255
plot(IR.test.right)

######## Network output, given inputs
  #check what the weights are
input.to.hidden.weights
hidden.to.output.weights
  # test input, set manually
network.input.test <- c(0, 0, 0, 0)
  # generate output
feedforward(network.input.test)
  # generate activation
feedforward.activation(network.input.test)

######## Network connection pattern
point <- data.frame(x = c(0,1), y = c(1, 1), type = c(1, 1), kind = c(2,2))
ggplot(nodes, aes(x,y, color=factor(layer))) +
  geom_point(size=10, aes(fill=factor(layer))) +
  geom_point(size=10)




input.to.hidden.weights
ncol(input.to.hidden.weights)
hidden.to.output

connections <- map.network(input.to.hidden.weights, hidden.to.output.weights)

for(i in nrow(connections)){
  
}

in.x <- numeric(nrow(connections))
connections$in.y <- numeric

map.network <- function(ihmatrix, homatrix){
  ih <- map.layer(input.to.hidden.weights)
  ho <- map.layer(hidden.to.output.weights) 
  ho$node.in <- ho$node.in + n.input
  ho$node.out <- ho$node.out + n.input
  connections <- rbind(ih, ho)
  return(connections)
}

map.layer <- function(weight.matrix){
    # this is the number of upper and lower layer nodes
  n.in <- nrow(weight.matrix)
  n.out <- ncol(weight.matrix)
    # initialize an array to keep track of the weights between inputting & outputting layers
  node.in <- numeric(n.in*n.out)
  node.out <- numeric(n.in*n.out)
   # mark whether the weight is positive or negative
  weight <- character(n.in*n.out)
  n <- 1
  for (i in 1:n.in){
    for(j in 1:n.out){
      if(weight.matrix[i,j] != 0){
        node.in[n] <- i
        node.out[n] <- j + n.in
        if(weight.matrix[i,j] > 0){
          weight[n] <- "positive"
        } else {weight[n] <- "negative"}
        n <- n + 1
      }
    }
  }
  connections <- data.frame(node.in, node.out, weight)
  connections <- subset(connections, node.in > 0)
  return(connections)
}


library(igraph)
g <- graph(c(1,5,2,3,2,4,2,5,3,5))

plot(g)
membership <- c(1,1,1,1,1)
modularity(g, membership)
modularity_matrix(g, membership)
# First we load the ipgrah package
library(igraph)

# let's generate two networks and merge them into one graph.
g2 <- barabasi.game(50, p=2, directed=F)
g1 <- watts.strogatz.game(1, size=100, nei=5, p=0.05)
g <- graph.union(g1,g2)

# let's remove multi-edges and loops
g <- simplify(g)

# let's see if we have communities here using the 
# Grivan-Newman algorithm
# 1st we calculate the edge betweenness, merges, etc...
ebc <- edge.betweenness.community(g, directed=F)

# Now we have the merges/splits and we need to calculate the modularity
# for each merge for this we'll use a function that for each edge
# removed will create a second graph, check for its membership and use
# that membership to calculate the modularity
mods <- sapply(0:ecount(g), function(i){
  g2 <- delete.edges(g, ebc$removed.edges[seq(length=i)])
  cl <- clusters(g2)$membership
  # March 13, 2014 - compute modularity on the original graph g 
  # (Thank you to Augustin Luna for detecting this typo) and not on the induced one g2. 
  modularity(g,cl)
})

# we can now plot all modularities
plot(mods, pch=20)

# Now, let's color the nodes according to their membership
g2<-delete.edges(g, ebc$removed.edges[seq(length=which.max(mods)-1)])
V(g)$color=clusters(g2)$membership

# Let's choose a layout for the graph
g$layout <- layout.fruchterman.reingold

# plot it
plot(g, vertex.label=NA)

# if we wanted to use the fastgreedy.community agorithm we would do
fc <- fastgreedy.community(g)
com <- community.to.membership(g, fc$merges, steps= which.max(fc$modularity)-1)
V(g)$color <- com$membership+1
g$layout <- layout.fruchterman.reingold
plot(g, vertex.label=NA)
