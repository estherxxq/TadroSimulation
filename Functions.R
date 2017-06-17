
####################################            ###################################
####################################  FUNCTION  ###################################
####################################            ###################################

######################## ENVIRONMENT ########################

# generate PR inputs, takes in a ".pos" list with the x and y coordinate that marks a sensor position
get.PR <- function(PR.pos){
  PR.x <- PR.pos[1]
  PR.y <- PR.pos[2]
  
  # if a sensor is not under a shed, generate light input
  # input should be between 0-100 (****Might want to change how this is calculated)
  if(PR.x < leftShed.x || PR.x > rightShed.x){
    # if the sensor is under a shed, light input = 0
    input <- 0
  }
  else {
    dist <- get.distance(PR.x, PR.y, light.x, light.y)
    input <- max.input * (pool.r-dist)/pool.r  # max.input is defined in the parameters
    # input <- 1/dist^2
  }
  return(input)
} ########################

# generate IR inputs, also takes in .pos list
get.IR <- function(IR.x){
  # if sensor is under a shed, input is between 0-100 based on how far it's from the edge on the x-axis
  if (IR.x < leftShed.x){
    dist <- leftShed.x - IR.x

  } 
  if (IR.x > rightShed.x){
    dist <- IR.x - rightShed.x
  } 
  if (IR.x >= leftShed.x && IR.x <= rightShed.x){
    dist <- 0 # if pool edge = max.input
    #dist <- pool.r/2 # if shed edge = max.input
  } # if sensor is not under a shed, IR input = 0
    # pool edge with max input value
  input <- max.input * dist/(pool.r/2)
    # shed edge with max input value
  # input <- max.input * (1 - dist/(pool.r/2))
  return(input)
} ########################

get.inputs <- function(s){ #take in the "sensors" data
  input <- numeric(nrow(s)) # initialize an input array
  # fill the input array

  for (i in 1:2){
    if(s$name[i] == "lPR" || s$name[i] == "rPR"){ # if the sensor is lPR or rPR, use get.PR function to get PR input
      input[i] <- unlist(get.PR(s[i,2:3]))
    }
    if(s$name[i] == "lIR" || s$name[i] == "rIR"){ # if the sensor is lIR or rIR, use get.IR function to get IR input
      input[i] <- unlist(get.IR(s[i,2:3]))
    }
  }
  return(input)
} ########################

wall.check <- function(t.x, t.y){
  distance <- get.distance(t.x, t.y, pool.x, pool.y)
  max <- pool.r - tadro.r
  if (distance > max){
    t.x <- t.x * max/distance
    t.y <- t.y * max/distance
  } 
  pos <- c(t.x, t.y)
  return(pos)
} ########################

######################## MORPHOLOGY ########################

# take in tadro center point and angle for new point, return location of point
find.point <- function(tadro.pos, angle, radius){
  xc <- tadro.pos[1] # center x coordinate
  yc <- tadro.pos[2] # center y coordinate
  
  if (angle >= 0 && angle < 90){
    angle <- angle
    xp <- xc + radius*cos(rad(angle))
    yp <- yc + radius*sin(rad(angle))
  }
  if (angle >= 90 && angle < 180){
    angle <- angle - 90
    xp <- xc - radius*sin(rad(angle))
    yp <- yc + radius*cos(rad(angle))
  }
  if (angle >= 180 && angle < 270){
    angle <- angle - 180
    xp <- xc - radius*cos(rad(angle))
    yp <- yc - radius*sin(rad(angle))
  }
  if (angle >= 270 && angle < 360){
    angle <- angle - 270
    xp <- xc + radius*sin(rad(angle))
    yp <- yc - radius*cos(rad(angle))
  }
  point <- c(xp, yp)
  return(point)
} ########################

# find the x coordinate of a point, given x coordinate of center, angle and distance
find.x <- function(xc, angle, radius){
  
  if (angle >= 0 && angle < 90){
    angle <- angle
    xp <- xc + radius*cos(rad(angle))
  }
  if (angle >= 90 && angle < 180){
    angle <- angle - 90
    xp <- xc - radius*sin(rad(angle))
  }
  if (angle >= 180 && angle < 270){
    angle <- angle - 180
    xp <- xc - radius*cos(rad(angle))
  }
  if (angle >= 270 && angle < 360){
    angle <- angle - 270
    xp <- xc + radius*sin(rad(angle))
  }
  return(xp)
} ########################

# find the x coordinate of a point, given x coordinate of center, angle and distance
find.y<- function(yc, angle, radius){
  
  if (angle >= 0 && angle < 90){
    angle <- angle
    yp <- yc + radius*sin(rad(angle))
  }
  if (angle >= 90 && angle < 180){
    angle <- angle - 90
    yp <- yc + radius*cos(rad(angle))
  }
  if (angle >= 180 && angle < 270){
    angle <- angle - 180
    yp <- yc - radius*sin(rad(angle))
  }
  if (angle >= 270 && angle < 360){
    angle <- angle - 270
    yp <- yc - radius*cos(rad(angle))
  }
  return(yp)
} #####################

# locate the x and y coordinates of each sensor, servo and the head point
locate.sensors <- function(sensor.ang, xc, yc, ang){ # takes in the array of preset sensor angles, and the current angle
  sensor.angs <- sapply((sensor.ang + ang), adjust.angle) # current angles for the sensors/servo/head
  sensor.x <- mapply(find.x, xc, sensor.angs, tadro.r) # current x for all sensors/servo/head
  sensor.y <- mapply(find.y, yc, sensor.angs, tadro.r) # current y for all sensors/servo/head
  sensor.type <- c("lPR", "rPR", "lIR", "rIR", "tail", "head") # mark the type, in that order
  sensor.points <- data.frame(name = sensor.type, x = sensor.x, y = sensor.y) # summarize them in one data fram
  return(sensor.points)
} #####################

######################### NETWORK ########################

# main network function: feed the inputs throught network to get outputs
# return two outputs, first one is offset, second one is velocity

feedforward <- function(sensor.inputs, activations){
  #to make things easier
  hweight <- input.to.hidden.weights
  oweight <- hidden.to.output.weights
  #initialize hidden and output layers
  input <- sensor.inputs
  hidden.raw <- numeric(n.hidden)
  hidden <- numeric(n.hidden)
  output.raw <- numeric(n.output)
  output <- numeric(n.output) # output nodes are linears, so don't need to check for activation potential
  
  # input to hidden
  for(i in 1:n.hidden){
    # raw activation is the sum of activation * weight
    hidden.raw[i] <- sum(input * hweight[,i])
  }
  # adjust via tanh() function
  hidden <- tanh(hidden.raw)
  # hidden to output
  for(i in 1:n.output){
    # raw activation is the sum of activation * weight
    output.raw[i] <- sum(hidden * oweight[,i])
  }
  # adjust via tanh() function
  output <- tanh(output.raw)

  return(output)
} ########################

# check activations
feedforward.activation <- function(sensor.inputs){
  #to make things easier
  hweight <- input.to.hidden.weights
  oweight <- hidden.to.output.weights
  #initialize hidden and output layers
  input <- sensor.inputs
  hidden.raw <- numeric(n.hidden)
  hidden <- numeric(n.hidden)
  output.raw <- numeric(n.output)
  output <- numeric(n.output) # output nodes are linears, so don't need to check for activation potential
  
  # input to hidden
  for(i in 1:n.hidden){
    # raw activation is the sum of activation * weight
    hidden.raw[i] <- sum(input * hweight[,i])
  }
  # adjust via tanh() function
  hidden <- tanh(hidden.raw)
  
  # hidden to output
  for(i in 1:n.output){
    # raw activation is the sum of activation * weight
    output.raw[i] <- sum(hidden * oweight[,i])
  }
  # adjust via tanh() function
  output <- tanh(output.raw)
  
  # create a matrix to keep track of all activations
  activation <- matrix(rep(555, 3*n.hidden), nrow = 3, ncol = n.hidden)
  activation[1,1:n.input] <- input # row 1 = input layer
  activation[2,1:n.hidden] <- hidden # row 2 = hidden layer
  activation[3,1:n.output] <- output # row 3 = output layer
  return(activation)
} ########################



# scale input values before feeding into network
scale <- function(inputs){
  inputs.scaled <- inputs
  for(i in 1:n.inputs){
    inputs.scaled[i] <- inputs[i] * scalar # scaler is set in the parameters file
  }
  return(inputs.scaled)
}

# get the value for offset based on output (NOT USED FOR NEW NETWORK)
get.offset <- function(a){
  if (a == 0){
    value <- 0
  }
  if (a > 0){
    value <- 1
  }
  if (a < 0){
    value <- -1
  }
  return(value)
} ########################

# get the mode for offset based on output
get.offset.mode <- function(a){
  if (a == 0){
    mode <- "straight"
  }
  if (a > 0){
    mode <- "right"
  }
  if (a < 0){
    mode <- "left"
  }
  return(mode)
} ########################


# get the value for speed based on output (NOT USED FOR NEW NETWORK)
get.speed <- function(b){
  if (b > 0){
    value <- tadro.speed/2
  }
  if (b == 0){
    value <- tadro.speed
  }
  return(value)
} ########################

# get the mode for speed based on output
get.speed.mode <- function(b){
  if (b > 0){
    mode <- "fast"
  }
  if (b == 0){
    mode <- "medium"
  }
  if (b < 0){
    mode <- "slow"
  }
  return(mode)
} ########################

# calculate linear velocity based on offset
get.linear.v <- function(speed, offset){
  l.v <- speed * cos(offset)^2
  return(l.v)
} ########################

# calculate angular velocity based on linear velocity, offset, and tadro radius
get.angular.v <- function(speed, offset){
  a.v <- (speed * sin(offset) * abs(sin(offset))) / tadro.r # in degree/s
  return(a.v)
} ########################

# get the new location of tadro
get.new.location <- function(linear.v, angular.v, ang, xc, yc){
  delta.ang <- angular.v * step.size # to rotate how much, in radian, and in which direction
  ang.new <- adjust.angle(ang + delta.ang) # calculate the new tadro ang
  displacement <- linear.v * step.size # calculate absolute distance tadro traveled
  x.new <- find.x(xc, ang.new, displacement) # new center x
  y.new <- find.y(yc, ang.new, displacement) # new center y
  point.new <- wall.check(x.new, y.new) # check whether bumping into wall, return adjusted values
  pos.new <- c(point.new, ang.new) # combine the three pieces of information
  return(pos.new)
} ########################

######################## BASIC FUNCTIONS ########################

# Turn degree into radian (R operates in radian, not degrees, so must convert)
rad <- function(degree){
  radian <- degree*pi/180
  return(radian)
} ########################

# Turn radian in to degree
degree <- function(rad){
  degree <- rad*180/pi
  return(degree)
} ########################

# If an angle is not between 0 <= ang < 360, adjust it to be within that range
adjust.angle <- function(angle){
  if (angle < 0){
    while(angle < 0){
      angle = 360 + angle
    }
  }
  if (angle >= 360){
    while(angle >= 360){
      angle = angle - 360
    }
  }
  return(angle)
} ########################

# Calculates distance between two points
get.distance <- function(Ax, Ay, Bx, By){
  d <- sqrt((Ax-Bx)^2 + (Ay-By)^2)
  return(d)
} ########################

# This is a function to make a series of circles that I found on the internet
# source: http://stackoverflow.com/questions/6862742/draw-a-circle-with-ggplot2
circleFun <- function(center = c(0,0),diameter, npoints){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(xx = xx, yy = yy))
} ########################







