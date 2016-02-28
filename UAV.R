library(ggplot2)

# A note on units:
# - all mass units are kilograms
# - all force units are Newtons
# - all distances are in metres
# - all times are in seconds
# - angles are measured in degrees or radians, as necessary

Gravity <- 9.80665
#
# Phantom 3
Mass <- 1.280 # kg
Weight <- Mass * Gravity # 12.6 N

# Thrust measurements from https://youtu.be/ANxPIi203iQ
# The total thrust is equal to the force registered on the scale PLUS the weight
# (the scale was calibrated to read zero when the motors were not running)

Thrust <- (3.7 * Gravity) + Weight

# The initial acceleration of a UAV
# 0 degrees is vertical
ACCzero <- data.frame(angle = seq(from = 0, to = 2 * pi, length.out = 1000))

# Total vertical force - the vertical component of thrust minus weight
ACCzero$vertical <- (Thrust * cos(ACCzero$angle)) - Weight

# Total horizontal force - the horizontal component of thrust
ACCzero$horizontal <- Thrust * sin(ACCzero$angle)

# Total force acting on the UAV - Magnitude
ACCzero$forcemag <- sqrt(ACCzero$vertical^2 + ACCzero$horizontal^2)

# Total force acting on the UAV - Direction
ACCzero$forcedir <- atan(ACCzero$horizontal / ACCzero$vertical)

# Acceleration is the total force divided by mass in kilograms.
ACCzero$accel <- ACCzero$forcemag / Mass

ACCzero$forceangle <- (ACCzero$forcedir / (2 * pi)) * 360


ggplot(ACCzero, aes(x = angle, y = accel)) + geom_bar(stat = "identity")
