# load libraries
library(data.table)
library(ggplot2)
library(car)

# load Chirot dataset and convert to data.table
data(Chirot)
Chirot <- data.table(Chirot)

#Preliminary plot 1: Rebellion Intensity by Inequality, Peasantry & Commercialization
ggplot(Chirot, aes(x=inequality, y=intensity)) + geom_point(aes(size=midpeasant, color=commerce)) +
  labs(title="Rebellion Intensity by Inequality, Peasantry & Commercialization",
       x="Inequality by Gini coefficient", y="Intensity of Rebellion", size="Size of Middle Peasantry", 
       color="Commercial Agriculture")

#Preliminary plot 2: Rebellion Intensity by Commercialization of Agriculture
ggplot(Chirot, aes(x=commerce, y=intensity)) + geom_point()  + stat_smooth(method = lm) +
  labs(title="Rebellion Intensity by Commercialization of Agriculture", x="% Commercial Agriculture",
       y="Intensity of Rebellion ", size="Size of middle peasantry")

# Preliminary plot 3 (unused in final paper): Rebellion Intensity by Traditionalism
ggplot(Chirot, aes(x=tradition, y=intensity)) + geom_point()  + stat_smooth(method = lm) +
  labs(title="Rebellion Intensity by Traditionalism", x="Traditionalism",
       y="Intensity of Rebellion ", size="Size of middle peasantry")

# Prelminary plot 4 (unused in final paper): Midpeasantry as mediator variable
ggplot(Chirot, aes(x=inequality, y=midpeasant)) + geom_point()

# Initial regression measuring commerce + inequality + tradition
regression <- lm(intensity~commerce + inequality + tradition, data=Chirot)
summary(regression)

# Revised regression measuringing just commerce and tradition against intensity
regression2 <- lm(intensity~commerce + tradition, data=Chirot)
summary(regression2)

# Add regression column to Chirot dataset
Chirot[,predicted_intensity := predict(regression2, data=Chirot)]

# Final plot with regression line       
ggplot(Chirot, aes(x=commerce, y=intensity)) +
  geom_point(aes(color=tradition)) + geom_point(aes(y=predicted_intensity), color="red") + 
  stat_smooth(method=lm, aes(y=predicted_intensity), color="red") + 
  labs(title="Predicted Intensity (Red) vs. Actual Intensity (Blue)", x="% Commercial Agriculture",
       y="Intensity of Rebellion", color="Traditionalism")
