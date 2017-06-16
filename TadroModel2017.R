library(ggplot2)
library(plotrix)
# Include following files:
# a specific tadro file, which also has experimental parameters. Check if this is the right one.
source("random.R")
# Parameters and Functions
source("Functions.R")
source("Parameters.R")
source("MainFunctions.R")

######## Runs a trial
results <- Run.trial()

######## Make graphs

# add head point
results$head.x <- mapply(find.x, results$tadro.x, results$tadro.ang, tadro.r)
results$head.y <- mapply(find.y, results$tadro.y, results$tadro.ang, tadro.r)

# plotting: point of the first iteration being plotted, and then segments with arrows 
ggplot(results, aes(x = tadro.x, y = tadro.y)) +
  geom_point() +
  geom_segment(aes(xend = head.x, yend = head.y), 
               arrow = arrow(length=unit(0.1, "cm"), ends = "last", type = "closed"), color = "green") +
  geom_path(data = pool, aes(x = xx, y = yy)) +
  geom_segment(data = LShed.mark, aes(x = x1, y = y1, xend = x2, yend = y2)) +
  geom_segment(data = RShed.mark, aes(x = x1, y = y1, xend = x2, yend = y2)) +
  geom_path(data=light, aes(x = xx, y = yy), color = "orange") +
  theme_void()

# view plot
plot.results

# Keep track of all activations
# activations <- array(unlist(matrix(rep(555, times=3*n.hidden), nrow = 3, ncol = 8)), dim = c(3,8,n.iteration))
# inputs <- data.frame(results$lPR, results$rPR, results$lIR, results$rIR)
# for(i in 1:n.iteration){
#   activations[,,i] <- feedforward.activation(unlist(inputs[i,])) 
# }
# activations





