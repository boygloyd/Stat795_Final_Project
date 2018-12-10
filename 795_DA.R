# Load in libraries
library(MASS)
library(corrplot)
library(car)
library(RCurl)

rm(list=ls(all=TRUE)) # remove all previous objects from memory
options(warn=-1)  # forces R to ignore all warning messages

# Read in data, classify factors
golf=read.csv(text=getURL("https://raw.githubusercontent.com/boygloyd/Stat795_Final_Project/master/Presentation/data/prov1_f.csv"), header=F)
#golf=read.csv("C:/Users/adity/Desktop/Study/Semester 3/STAT 795/Golf/golf.csv",header=TRUE)
colnames(golf) = c("hitnum", "block", "ballnum", "ballmodel", "carry", "c_disp", "total_distance", "t_disp", "traj")
attach(golf)
block = as.factor(block)
dim(golf) # 252 9
names(golf)


#test comment

############################
# Exploratory Data Analysis
############################

#ggplot(golf, aes(y=total_distance,x=ballmodel))+geom_boxplot()
plot(ballmodel,total_distance, xlab="Ball model", ylab= "Total distance") #boxplot of total distance according to ball model type
#From the box plots, we can see that the total distance is considerably high for new balls compared to new and refurbished balls. Washed balls travel higher total distance compared to the refurbished balls.

#ggplot(golf, aes(y=carry,x=ballmodel))+geom_boxplot()
plot(ballmodel,carry, xlab="Ball model", ylab= "Carry") #boxplot of carry according to ball model type
#From the box plots, we can see that the carry dispersion is almost same for new and refurbished balls whereas it is considerably less for washed balls as compared to new and refurbished balls.

#ggplot(golf, aes(y=c_disp,x=ballmodel))+geom_boxplot()
plot(ballmodel,c_disp, xlab="Ball model", ylab= "Carry Dispersion") #boxplot of carry dispersions according to ball model type
#From the box plots, we can see that the carry dispersion is almost same for new and refurbished balls whereas it is considerably less for washed balls as compared to new and refurbished balls.

#correlation between the continuous variables
pcorr = cor(as.data.frame(cbind(carry,c_disp,total_distance,t_disp,traj)))
corrplot(pcorr)
# Total distance has a high positive correlation with carry but a large negative correlation with trajectory.
# Total dispersion has a high positive correlation with carry dispersion.
# Trajectory has a high negative correlation with carry and total distance which seems logical.



#################Ancova
# Here we treat the total distance as the dependent variable (y)
# We treat the ball model as treatment (tau)
# We treat the block as block effect (beta)
# We treat trajectory as a covariate
# We will use ancova effect

# In order to apply a covariate model, we need to check if there is a linear relationship between the dependent variable and covariate
plot(total_distance,traj)
lines(lowess(total_distance,traj), col="blue")
# The relationship looks fairly linear, so can proceed with ancova model

# Ancova to test whether the means of a dependent variable (total distance)
# are equal across levels of a treatment (ball models), 
# while statistically controlling for the effects of other covariates (trajectory) 


#Two methods to do ANCOVA

c.traj <- traj - mean(traj) #centering the variable

fit.1 <- aov(lm(total_distance ~ factor(ballmodel) + factor(block) + c.traj , data=golf))
summary(fit.1)

fit.2 <- anova(lm(total_distance ~ factor(ballmodel) + factor(block) + c.traj , data=golf))
fit.2
plot(fit.2)


#The ANCOVA Table is as follows

# SOV               df Sum Sq Mean Sq F value   Pr(>F)    
#factor(ballmodel)   2   5803  2901.3  117.45  < 2e-16 ***
#factor(block)      41  11749   286.6   11.60  < 2e-16 ***
#traj                1   1810  1809.8   73.26 2.56e-15 ***
#Residuals         207   5114    24.7
#Total             251  24476   

# Hence there is a significant effect of ball model, block and trajectory on the total distance.

#diagnostic plots
par(mfrow=c(2,2))
plot(fit.1)

#outliers
golf[c(93,193,161),] #do not seem to be unusual values for any co variate.
#looks good. since no leverage point, not to worry and analysis seems correct.


#################Check for different dependent variable

#Total dispersion
fit.3 <- aov(t_disp ~ factor(ballmodel) + factor(block) + c.traj , data=golf)
summary(fit.3)
#Significant impact of block on total dispersion but no significant impact of treatment or trajectory on total dispersion

#Carry
fit.4 <- aov(carry ~  factor(ballmodel) + factor(block) + c.traj , data=golf)
summary(fit.4)
#Significant impact of block, treatment and covariate on carry

#Carry dispersion
fit.5 <- aov(c_disp ~ factor(ballmodel) + factor(block) + c.traj, data=golf)
summary(fit.5)
#Significant impact of block on carry dispersion but no significant impact of treatment or trajectory on total dispersion
