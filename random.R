# Tadro id information
Generation <- 0
Id <- "random"

# Network Parameters
# weights (1, 0 or -1) (random)
input.to.hidden <- sample(c(1, 0, -1), n.input*n.hidden, replace = TRUE)
hidden.to.output <-  sample(c(1, 0, -1), n.hidden*n.output, replace = TRUE)