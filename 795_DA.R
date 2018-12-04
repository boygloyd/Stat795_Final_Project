# Load in libraries
library(MASS)
library(corrplot)
library(car)

rm(list=ls(all=TRUE)) # remove all previous objects from memory
options(warn=-1)  # forces R to ignore all warning messages

# Read in data, classify factors
golf=read.csv("~/GitTemp/Stat795_Final_Project/Presentation/data/prov1_f.csv",header=FALSE)
colnames(golf) = c("hitnum", "block", "ballnum", "ballmodel", "carry", "c_disp", "total_distance", "t_disp", "traj")
attach(golf)
block = as.factor(block)
dim(golf) # 252 9
names(golf)

############################
# Exploratory Data Analysis
############################

plot(ballmodel,total_distance, xlab="Ball model", ylab= "Total distance") #boxplot of total distance according to ball model type
#From the box plots, we can see that the total distance is considerably high for new balls compared to new and refurbished balls. Washed balls travel higher total distance compared to the refurbished balls.

plot(ballmodel,carry, xlab="Ball model", ylab= "Carry") #boxplot of carry according to ball model type
#From the box plots, we can see that the carry dispersion is almost same for new and refurbished balls whereas it is considerably less for washed balls as compared to new and refurbished balls.

plot(ballmodel,c_disp, xlab="Ball model", ylab= "Carry Dispersion") #boxplot of carry dispersions according to ball model type
#From the box plots, we can see that the carry dispersion is almost same for new and refurbished balls whereas it is considerably less for washed balls as compared to new and refurbished balls.

#correlation between the continuous variables
pcorr = cor(as.data.frame(cbind(carry,c_disp,total_distance,t_disp,traj)))
corrplot(pcorr)
# Total distance has a high positive correlation with carry but a large negative correlation with trajectory.
# Total dispersion has a high positive correlation with carry dispersion.
# Trajectory has a high negative correlation with carry and total distance which seems logical.



#################Check block and treatment effect

#For total distance
fit.final.1 <- aov(total_distance ~  factor(block) + ballmodel , data=golf)
summary(fit.final.1)
#Significant impact of block and treatment on total distance

#Total dispersion
fit.final.2 <- aov(t_disp ~  factor(block) + ballmodel , data=golf)
summary(fit.final.2)
#Significant impact of block on total dispersion but no significant impact of treatment on total dispersion

#Carry
fit.final.3 <- aov(carry ~  factor(block) + ballmodel , data=golf)
summary(fit.final.3)
#Significant impact of block and treatment on carry

#Carry dispersion
fit.final.4 <- aov(c_disp ~  factor(block) + ballmodel , data=golf)
summary(fit.final.4)

#Hence we can say the distance travelled by the ball depends on the model of the ball.