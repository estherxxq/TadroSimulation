####################################            ###################################
#################################### PARAMETERS ###################################
####################################            ###################################

# Environment
# Tadro Morphology
# Network
# Trial

######################## ENVIRONMENT ########################
pool.r <- 1.52 # radius of pool, in m
pool.x <- 0 # pool center location
pool.y <- 0 

light.x <- pool.x # light location
light.y <- pool.y 
light.r <- 0.1 # Arbitrary radius of lamp. Only used in plotting.

leftShed.x <- - pool.r/2 # x value that marks IR cutoff
rightShed.x <- pool.r/2 # set as half way from the edge to the center of pool
Sheds.y <- sqrt(pool.r^2 - leftShed.x^2) # absolute values of end point y of the sheds. Only used in plotting
max.input <- 255 # the max value for input sensors
# graphing the environment
pool <- circleFun(c(pool.x, pool.y), pool.r*2, npoints = 100)
pool.center <- data.frame(x = pool.x, y = pool.y)
light <- circleFun(c(light.x, light.y), light.r*2, npoints = 20)
LShed.mark <- data.frame(x1 = leftShed.x, x2 = leftShed.x, y1 = -Sheds.y, y2 = Sheds.y)
RShed.mark <- data.frame(x1 = rightShed.x, x2 = rightShed.x, y1 = -Sheds.y, y2 = Sheds.y)

# ggplot(pool,aes(xx,yy)) +
#   geom_path() +
#   geom_segment(data = LShed.mark, aes(x = x1, y = y1, xend = x2, yend = y2)) +
#   geom_segment(data = RShed.mark, aes(x = x1, y = y1, xend = x2, yend = y2)) +
#   geom_point(data = pool.center, aes(x = x, y = y)) +
#   # light and clean background
#   geom_path(data=light, color = "orange") +
#   theme_void()

######################## TADRO MORPHOLOGY ########################
tadro.r <- 0.1 # radius of tadro, in m
mass <- 0.3 # mass of tadro, in kg
min.speed <- 0.05 # minimal speed of tadro, in m/s
max.speed <- 0.1 # maximum speed of tadro, in m/s
min.offset <- 0
max.offset <- 45

PR.ang <- 45 # angle of PR sensors away from center line
IR.ang <- 90 # angle of IR sensors away from center line

lPR.ang <- PR.ang # difference between each sensor and the central line
rPR.ang <- -PR.ang
lIR.ang <- IR.ang
rIR.ang <- -IR.ang
head.ang <- 0 # difference between head ang and central line. They are the same thing so there's no difference
tail.fixation <- 180 # difference between tail fixation and central line
sensor.angles <- c(lPR.ang, rPR.ang, lIR.ang, rIR.ang, tail.fixation, head.ang)

# graphing tadro
# sample.xc <- 0 # can change this
# sample.yc <- 0 # can change this
# sample.ang <- 180 # can change this
# 
# tadro <- circleFun(c(sample.xc, sample.yc), tadro.r*2, npoints = 100) # a circle for tadro's body
# 
# sample.points <- locate.sensors(sensor.angles, sample.xc, sample.yc, sample.ang)
# head.points <- data.frame(xc = sample.xc, yc = sample.yc, xh = sample.points$x[6], yh = sample.points$y[6]) #
# 
# ggplot(tadro, aes(xx,yy)) +
#   geom_path() +
#   geom_point(data = sample.points[1:2,], aes(x, y), shape = 19, size = 8) + # the circles are PR sensors
#   geom_point(data = sample.points[3:4,], aes(x, y), shape = 15, size = 8) + # the squares are IR sensors
#   geom_point(data = sample.points[5,], aes(x, y), shape = 8, size = 11) + # the double triangle is tail fixation point
#   geom_segment(data = head.points, aes(x = xc, y = yc, xend = xh, yend = yh),
#                arrow = arrow(ends = "last", type = "closed")) # the arrow points towards the way tadro is facing

######################## NETWORK ########################
# nodes
n.input <- 4
n.hidden <- 8
n.output <- 2

# this is used in plotting the network
nodes <- data.frame(
  x = c(3, 4, 5, 6,
        1, 2, 3, 4, 5, 6, 7, 8,
        4, 5),
  y = c(4, 4, 4, 4,
        3, 3, 3, 3, 3, 3, 3, 3,
        2, 2),
  node = c(1:14),
  layer = c(rep("input", 4), 
            rep("hidden", 8),
            rep("output", 2))
)



# scalar for sensor inputs
scalar <- 0.01

# weights
# input-hidden and hidden-output both come from the file
input.to.hidden.weights  <- matrix(input.to.hidden, nrow=n.input, ncol=n.hidden, byrow = TRUE)
hidden.to.output.weights <- matrix(hidden.to.output, nrow=n.hidden, ncol=n.output, byrow = TRUE)

######################## TRIAL ########################
n.iteration <- 50
step.size <- 1 # the duration of each iteration, in seconds

# starting at pool edge
# start.x <- 0 # starting position of tadro (at bottom edge of pool)
# start.y <- - (pool.r - tadro.r)
# start.ang <- 135 # facing straight up (90 degrees)

# alternatively, starting at random spot & direction
start.x <- runif(1, (-(pool.r - tadro.r)), ((pool.r - tadro.r)))
start.y <- runif(1, (-(pool.r - tadro.r)), ((pool.r - tadro.r)))
start.ang <- runif(1, 0, 359)

