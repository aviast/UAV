library(ggplot2)

# A note on units:
# - all mass units are kilograms
# - all force units are Newtons
# - all distances are in metres
# - all times are in seconds
# - angles are measured in degrees or radians, as necessary

# A blog on performance can be found at: http://klsin.bpmsg.com/how-fast-can-a-quadcopter-fly/

Gravity <- 9.80665

Rho <- 1.293 # kg/m^3

#
# DJI Phantom 3
Mass <- 1.280 # kg
Weight <- Mass * Gravity # 12.6 N

# Coefficient of Drag

Cd <- 1.3

# A guesstimate - 590mm diagonal, roughly half of which contributes to the area.
Area <- (0.590^2)/2

# Static thrust measurements from https://youtu.be/ANxPIi203iQ
# ...actual thrust diminishes as airspeed increases.
# http://www.wired.com/2014/05/modeling-the-thrust-from-a-quadcopter/

Thrust <- function(vel) {
  return(3.7 * Gravity)
}

Drag <- function(vel) {
  return((Cd * Area * Rho * vel^2)/2)
}

# Circle divisions - actually this is the parts of pi radians (half-circle)
CIRCLE_DIVS <- 500

# Number of seconds to calculate position for...
TIME <- 60

# Number of divisions per second
TIME_DIVS <- 10

TIME_VEC <- seq(from = 0, to = TIME, by = 1 / TIME_DIVS)

ACC_DF <- data.frame(time = vector(mode = "numeric", length = length(TIME_VEC) * CIRCLE_DIVS),  # Time, in seconds, since the start
                     angle = vector(mode = "numeric", length = length(TIME_VEC) * CIRCLE_DIVS), # Angle in which thrust is being applied
                     x = vector(mode = "numeric", length = length(TIME_VEC) * CIRCLE_DIVS),     # Horizontal distance from starting point
                     y = vector(mode = "numeric", length = length(TIME_VEC) * CIRCLE_DIVS),     # Horizontal distance from starting point - normal to x in the horizontal plane
                     z = vector(mode = "numeric", length = length(TIME_VEC) * CIRCLE_DIVS),     # Vertical distance from starting point - normal to x and y
                     vertical = vector(mode = "numeric", length = length(TIME_VEC) * CIRCLE_DIVS),
                     horizontal = vector(mode = "numeric", length = length(TIME_VEC) * CIRCLE_DIVS),
                     forcemag = vector(mode = "numeric", length = length(TIME_VEC) * CIRCLE_DIVS),
                     forcedir = vector(mode = "numeric", length = length(TIME_VEC) * CIRCLE_DIVS),
                     accel = vector(mode = "numeric", length = length(TIME_VEC) * CIRCLE_DIVS)
)

ACC_DF$time <- rep(TIME_VEC, each = CIRCLE_DIVS)
ACC_DF$angle <- rep(seq(from = 0, to = pi, length.out = CIRCLE_DIVS), times = length(TIME_VEC))

for (i in TIME_VEC) {
  INDEX <- ACC_DF$time == i
  # The first calculations are based on {x,y,z} == {0,0,0}
  # Subsequent calculations are based off the values for the previous iteration
  if (i == TIME_VEC[1]) {
    ACC_DF[INDEX, "x"] <- 0
    ACC_DF[INDEX, "y"] <- 0
    ACC_DF[INDEX, "z"] <- 0
    
    # Total vertical force - the vertical component of thrust minus weight
    ACC_DF[INDEX, "vertical"] <- (Thrust * cos(ACC_DF[INDEX, "angle"])) - Weight
    
    # Total horizontal force - the horizontal component of thrust
    ACC_DF[INDEX, "horizontal"] <- Thrust * sin(ACC_DF[INDEX, "angle"])
    
    # Total force acting on the UAV - Magnitude
    ACC_DF[INDEX, "forcemag"] <- sqrt(ACC_DF[INDEX, "vertical"]^2 + ACC_DF[INDEX, "horizontal"]^2)
    
    # Total force acting on the UAV - Direction
    ACC_DF[INDEX, "forcedir"] <- atan(ACC_DF[INDEX, "horizontal"] / ACC_DF[INDEX, "vertical"])
    
    # Acceleration is the total force divided by mass in kilograms.
    ACC_DF[INDEX, "accel"] <- ACC_DF[INDEX, "forcemag"] / Mass
  } else {
    # TODO
  }
}

ggplot(ACC_DF[ACC_DF$time == 0,], aes(x = angle, xend = angle, y = 0, yend = accel)) +
  geom_segment() +
  scale_x_continuous(name = "Angle of Bank (degrees)", labels = function(x) {return((x/pi)*180) }, breaks = (0:4 / 4) * pi) +
  scale_y_continuous(name = "Acceleration (ms-2)")





