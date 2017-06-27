### Graphing the Network
library(igraph)

plot.network <- function(network){
  
  ID <- network[n.connections + 1] # tadro ID
  
  # The nodes data frame has information about each node
  node.id <- c(1:(n.input+n.hidden+n.output))
  type <- c(rep("input",n.input), rep("hidden",n.hidden), rep("output",n.output))
  # type.n is useful in marking the colors of the nodes
  type.n <- c(rep(1, n.input), rep(2,n.hidden), rep(3, n.output))
  # the "name" and the "type" columns are useful in labling the nodes in the plot
  if (n.input == 4 && n.hidden == 8 && n.output == 2){
    name <- c("lPR","rPR","lIR","rIR", "h1", "h2","h3","h4","h5","h6","h7","h8","offset","speed")
  }
  else {
    name <- type
  }
  # the resulting nodes data frame
  nodes <- data.frame(node.id, name, type, type.n)

  # The connections data frame marks the connections from one node to another
  # first list out all the possible connections
  from <- c(rep(1:n.input,each = n.hidden),
            rep((n.input+1):(n.input+n.hidden), each = n.output))
  to <- c(rep((n.input+1):(n.input+n.hidden), n.input),
          rep((n.input+n.hidden+1):(n.input+n.hidden+n.output), n.hidden))
  weight <- network[1:n.connections]
  connections <- data.frame(from, to, weight)
  # then get rid of the connections with zero weight to get the actual connections
  connections <- connections[connections$weight!=0,]
  
  tadro <- graph_from_data_frame(d=links, vertices=nodes, directed=T)
  node.colrs <- c("red","steel blue", "orange")
  V(tadro)$color <- node.colrs[V(tadro)$type.n]
  # E(tadro)$edge.color <- "gray80"
  
  
  p.network <- plot(tadro, layout = layout_tadro_network(), vertex.color = V(tadro)$color, 
       vertex.label = V(tadro)$name, vertex.size = 30)
  
  return(p.network)
}


# for (i in nrow(links)){
#   if (nodes$from ==)
#   links$
# }




layout_tadro_network <- function(){
  x <- c(2:5, 0:7, 3:4)
  y <- c(2,2,2,2,1,1,1,1,1,1,1,1,0,0)
  layout <- matrix(nrow = 14, ncol = 2)
  layout[,1] <- x
  layout[,2] <- y
  return(layout)
}

