# Tadro id information
Generation <- 0
Id <- "ideal"

# Network Parameters
  # weights (0, 1 or -1) (ideal)
input.to.hidden <- c(
  -1, -1, -1, 1, 1, 1, 0, 0,
  -1, -1, -1, 1, 1, 1, 0, 0,
   0,  0,  0, 0, 0, 0, 1, 1,
   0,  0,  0, 0, 0, 0, 1, 1)

hidden.to.output <- c(
  -1, 0,
  -1, 0,
   1, 0,
   1, 0,
   1, 0,
  -1, 0,
   0, -1,
   0, 1
)
