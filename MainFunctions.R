# This function runs a trial and returns the data log

Run.trial <- function(){ 
  
  ######## initializing the data to keep track of ########
  # keep track of: iteration, tadro x, y, ang, 4 sensor inputs, offset, speed, linear velocity and angular velocity
  # mark each iteration
  iteration <- c(0:n.iteration)
  # for tadro, keep trak of location 
  tadro.x <- numeric(length(iteration))
  tadro.y <- numeric(length(iteration))
  tadro.ang <- numeric(length(iteration))
  # for each sensor, keep track of input
  lPR <- numeric(length(iteration))
  rPR <- numeric(length(iteration))
  lIR <- numeric(length(iteration))
  rIR <- numeric(length(iteration))
  sensor.angles # make sure we have this array of the angle differences between tadro.ang and sensors/servo/head
  
  # keep track of network activation patterns on each layer
  # using a 3 dimensional array: c(iteration, layer, node)
  # activations <- aperm(array(unlist(
  #   as.data.frame(matrix(100:101*1), 32, n.iteration) # matrix (a:b*c): starting from a, to b*c, adding c every time, in b steps in total
  #   ), dim = c(n.iteration, 8, 4)))
  # 

  # for the servo tail, keep track of offset, speed, linear velocity and angular velocity
  offset <- numeric(length(iteration))
  speed <- numeric(length(iteration))
  v.linear <- numeric(length(iteration))
  v.angular <- numeric(length(iteration))
  
  ######## initialize tadro starting position ########
  # at bottom edge of pool, facing straight up (90 degrees)
  tadro.x[1] <- start.x
  tadro.y[1] <- start.y
  tadro.ang[1] <- start.ang
  
  ######## for each iteration ########
  for(i in 1:length(iteration)){
    ### Step 1: Locating Tadro center, sensors, tail fixation point and head
    xc <- tadro.x[i] # current center x
    yc <- tadro.y[i] # current center y
    ang <- tadro.ang[i] # current angle
    # 1 = lPR.ang, 2 = rPR.ang, 3 = lIR.ang, 4 = rIR.ang, 5 = tail.fixation, 6 = ang
    sensor.points <- locate.sensors(sensor.angles, xc, yc, ang)
    lPR.pos <- unlist(sensor.points[1,2:3])
    rPR.pos <- unlist(sensor.points[2,2:3])
    lIR.pos <- unlist(sensor.points[3,2:3])
    rIR.pos <- unlist(sensor.points[4,2:3])
    
    # Step 2: Get sensor readings
    
    # update sensor input arrays here
    lPR[i] <- get.PR(lPR.pos)
    rPR[i] <- get.PR(rPR.pos)
    lIR[i] <- get.IR(lIR.pos[1])
    rIR[i] <- get.IR(rIR.pos[1])
    inputs <- c(lPR[i], rPR[i], lIR[i], rIR[i])
    
    # Step 3: get outputs from network
      # update activation pattern
    outputs <- feedforward(inputs)
    current.offset <- outputs[1] # get current offset and speed data, c(mode, value)
    current.speed <- outputs[2]
    offset[i] <- get.offset.mode(outputs[1]) # update offset and speed data here
    speed[i] <- get.speed.mode(outputs[2])
    
    # Step 4: Calculate new tadro location
    # get linear and angular velocity
    linear.v <- get.linear.v(current.speed, current.offset) # need them to find new tadro location
    angular.v <- get.angular.v(linear.v, current.offset)
    v.linear[i] <- linear.v # update linear.v and angular.v data
    v.angular[i] <- angular.v
    
    pos.new <- get.new.location(linear.v, angular.v, ang, xc, yc) # returns new location, c(x, y, ang)
    
    if (i <= n.iteration){
      tadro.x[i+1] <- pos.new[1]
      tadro.y[i+1] <- pos.new[2]
      tadro.ang[i+1] <- pos.new[3]
      # if i = n.iteration, it's the last run
      # in which case we don't need update tadro location for the nex iteration
    }
  }
  # Step 5: Summarize data
  # keep track of: iteration, tadro x, y, ang, 4 sensor inputs, offset, speed, linear velocity and angular velocity
  trial.data <- data.frame(iteration, tadro.x, tadro.y, tadro.ang, lPR, rPR, lIR, rIR, offset, speed, v.linear, v.angular)
  return(trial.data)
}
