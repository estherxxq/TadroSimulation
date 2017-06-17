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

map.layer(input.to.hidden.weights)

map.connection(hidden.to.output.weights)

map.network <- function(ihmatrix, homatrix){
  map.
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
  return(connections)
}

