# Final Project: CO2 Emissions 
########### ALL VARIABLES CONVERTED TO NUMERICAL VALUES ########################
################ AFTER THE PRESENTATION #################
#DATA IS LINKED IN REPORT #######3

emission4B <- read.csv("C:/Users/AlexisWoo/Downloads/CO2 Emissions_Canada 3B.csv")

library(dplyr)
emission4B$Vehicle.Class <- as.factor(emission4B$Vehicle.Class)
emission4B$Transmission <- as.factor(emission4B$Transmission)
emission4B$Fuel.Type <- as.factor(emission4B$Fuel.Type)
emission4B$Make <- as.factor(emission4B$Make)

summary(emission4B)

summary(emission4B$Make)
summary(emission4B$Vehicle.Class)
summary(emission4B$Transmission)

boxplot(emission4B)
boxplot(emission4B$CO2.Emissions.g.km, main = "CO2 Emissions in Canada", xlab = "CO2 Emissions", ylab = "L/km")
boxplot(emission4B$Engine.Size.L., xlab = "Engine Size", ylab = "Length")
boxplot(emission4B$Cylinders, xlab = "Cylinders", ylab = "Length")
boxplot(emission4B$Fuel.Consumption.City..L.100.km., xlab = "Fuel Consumption: City", ylab = "L/km")
boxplot(emission4B$Fuel.Consumption.Hwy..L.100.km., xlab = "Fuel Consumption: Hwy", ylab = "L/km")
boxplot(emission4B$Fuel.Consumption.Comb..L.100.km., xlab = "Fuel Consumption: Comb", ylab = "L/km")
boxplot(emission4B$Fuel.Consumption.Comb..mpg., xlab = "Fuel Consumption: Comb", ylab = "mpg")

hist(emission4B$Cylinders)
hist(emission4B$Fuel.Consumption.Comb..L.100.km.)
hist(emission4B$Fuel.Consumption.Comb..mpg.)
hist(emission4B$CO2.Emissions.g.km.)
plot(density(emission4B$CO2.Emissions.g.km.))



subset <- sample(nrow(emission4B), nrow(emission4B) * 0.9) 
emission_train4B = emission4B[subset, ] 
emission_test4B = emission4B[-subset, ] 

###################### Linear Regression ##########################
emission.model4B <- lm(CO2.Emissions.g.km. ~., data = emission_train4B)
summary(emission.model4B)

####### Optimal Model ##############
emission.back4 <- step(emission.model4B)

###### IN Sample MSE ###########
emission_pi1 <- predict(emission.back4, emission_train4B)
mean((emission_pi1- emission_train4B$CO2.Emissions.g.km.)^2)

######## OUT of Sample MSE ###############
emission_pi2 <- predict(emission.back4, emission_test4B)
mean((emission_pi2 - emission_test4B$CO2.Emissions.g.km.)^2)

############## Diagnostic Plot Model Assumption ############3
plot(emission.back4)


######################## RANDOM FOREST #################################
library(randomForest)

RF.emission4 <- randomForest(formula = CO2.Emissions.g.km. ~., data= emission_train4B)
print(RF.emission4)
plot(RF.emission4)

############ variable importance ###############
var.imp4 <- importance(RF.emission4)
print(var.imp4)
plot(var.imp4)

############# MSE on RF ##################
emissionRF_pi1 <- predict(RF.emission4, emission_train4B)
mean((emissionRF_pi1 - emission_train4B$CO2.Emissions.g.km.)^2)

emissionRF_pi2 <- predict(RF.emission4, emission_test4B)
mean((emissionRF_pi2 - emission_test4B$CO2.Emissions.g.km.)^2)

##################################################################3


################### Regression Tree ###################################
library(rpart)
library(rpart.plot)

emission.RT <- rpart(formula = CO2.Emissions.g.km. ~., data = emission_train4B)
emission.RT
prp(emission.RT, digits = 4, extra = 1)
plotcp(emission.RT)

emission.traintreeB = predict(emission.RT)
emission.testtreeB = predict(emission.RT, emission_test4B)

#### MSE For RT #########
mean((emission.traintreeB - emission_train4B$CO2.Emissions.g.km.)^2)

mean((emission.testtreeB - emission_test4B$CO2.Emissions.g.km.)^2)


emission.prune <- prune(emission.RT, cp = 0.010)
prp(emission.prune)

emission_pi1 <- predict(emission.prune)
emission_pi2 <- predict(emission.prune, emission_test4B)

mean((emission_pi1 - emission_train4B$CO2.Emissions.g.km.)^2)
mean((emission_pi2 - emission_test4B$CO2.Emissions.g.km.)^2)
########################################################################
