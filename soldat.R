
#Initialization
soldat <- read.csv("SolarPrediction.csv")
View(soldat) 
summary(soldat)

#Loading of libraries
library(corrplot)
library(dplyr)
library(Hmisc)
library(ggplot2)
library(ISLR)
library(car)
library(gbm)

###Data cleaning### 
#Renaming the data column
colnames(soldat)
names(c("Data"))
soldat <- soldat %>% rename(Date = Data)

#Removing junk values
clean <- soldat$Date      
soldat$Date <- gsub("12:00:00 AM","",clean)
soldat$Date <- as.Date(soldat$Date, "%m/%d/%Y")
head(soldat)

#checking NA values
sapply(soldat, function(x) mean(is.na(soldat)))

#checking outliers
boxplot(soldat$Radiation)
boxplot(soldat$Temperature)
boxplot(soldat$Pressure)
boxplot(soldat$Humidity)
boxplot(soldat$WindDirection.Degrees.)
boxplot(soldat$Speed)

##Data Wrangling
soldat$Date = as.numeric(as.factor(soldat$Date))
soldat$Time = as.numeric(as.factor(soldat$Time))
soldat$TimeSunRise = as.numeric(as.factor(soldat$TimeSunRise))
soldat$TimeSunSet = as.numeric(as.factor(soldat$TimeSunSet))

##Data visualization
#Plotting the radiation data points with respect to month
by_month <- soldat %>%
  group_by(Date) %>%
  summarise(Total.Rad = sum(Radiation),
            Avg.Temp = mean(Temperature),
            Avg.Pre = mean(Pressure),
            Avg.Hum = mean (Humidity),
            Avg.Spe = mean(Speed))
ggplot(by_month)+
  geom_point(mapping = aes(x = Date, y = Total.Rad), colour = "darkblue",
             size = 2)+
  ylab("Total Radiation")+
  ggtitle("Total Radiation per Month")

######## creating a correlation matrix ###########
##creating a data matrix
soldat_mat <- rcorr(as.matrix(soldat))
soldat_mat

##calling the coefficients of correlation
coeff <- cor(soldat_mat$r)
coeff

##plotting the values of coefficients of correlation
library(corrplot)
corrplot(coeff)

#Removing additional columns that are irrelevant in the making of predictive model
soldat$UNIXTime <- NULL
soldat$Time <- NULL
soldat$Speed <- NULL
soldat$Date <- NULL
head(soldat)

#### linear modelling ####
##creating a training model and testing model
library(ISLR)
set.seed(152)
sample_size <- floor(0.80*nrow(soldat))
sample_size
train_data <- sample(seq_len(nrow(soldat)), size = sample_size)
train = soldat[train_data,]
test = soldat[-train_data,]

##training model
lm_modeltrain <- lm(Radiation ~ . , data = soldat)
summary(lm_modeltrain)
library(car)
vif(lm_modeltrain)
plot(lm_modeltrain)

##testing model
lm_modeltest <- lm(Radiation ~ ., data = test)
summary(lm_modeltest)
vif(lm_modeltest)
plot(lm_modeltest)

##creating predictive model
rad1 <- data.frame(Temperature = 68, Pressure = 30.48, Humidity = 84, WindDirection.Degrees. = 101.28, TimeSunRise = 7, TimeSunSet = 31)
Radiation <- predict(lm_modeltest, rad1)
Radiation

###model optimization###
#applying square root transformation to radiation variable to normalize the data
hist(soldat$Radiation)
soldat$Radiation <- sqrt(soldat$Radiation)
hist(soldat$Radiation)

#creating a new linear model
set.seed(767)
sample_size <- floor(0.80*nrow(soldat))
sample_size
train_data <- sample(seq_len(nrow(soldat)), size = sample_size)
train = soldat[train_data,]
test = soldat[-train_data,]
## train model
lm.new_train <- lm(Radiation~., data = train)
summary(lm.new_train)
vif(lm.new_train)

##testing model
lm.new_test <- lm(Radiation~., data = test)
summary(lm.new_test)
vif(lm.new_test)

##creating a predictive model for the new linear model
rad2 <- data.frame(Temperature = 68, Pressure = 30.48, Humidity = 84, WindDirection.Degrees. = 101.28, TimeSunRise = 7, TimeSunSet = 31)
Radiation <- predict(lm.new_test, rad2)
Radiation

##applying stepwise regression to remove unwanted variables
regmod <- step(lm.new_test, direction = "backward", trace = TRUE)

###gradient-boosting model for training data set###
grad_boost <- gbm(Radiation~.,data = train, distribution = "gaussian", n.trees = 10000, shrinkage = 0.01, interaction.depth = 4)
grad_boost
summary(grad_boost)

### calculating the perfect number of iterations
gbm.perf(grad_boost)

### predicting the value of gbm model with 10000 iterations
pred1 <- predict(grad_boost, n.trees = 10000)

### comparing the predicted values to the original value
summary(pred1)
summary(soldat$Radiation)

### gbm for test data set 
grad_boost_test <- gbm(Radiation~., data = test, distribution = "gaussian", n.trees = 10000, shrinkage = 0.01, interaction.depth = 4)
grad_boost_test
summary(grad_boost_test)

### checking the optimal number of iterations
gbm.perf(grad_boost_test)

### predicting the values of gbm based on optimal iterations
pred2 <- predict(grad_boost_test, n.trees = 2467)

### comparing the predicted values to the original values
summary(pred2)
summary(soldat$Radiation)

## creating predictive model for gradiant boosting model
rad2 <- data.frame(Temperature = 68, Pressure = 30.48, Humidity = 84, WindDirection.Degrees. = 101.28, TimeSunRise = 7, TimeSunSet = 31)
Radiation <- predict(grad_boost_test,  n.trees = 2467, rad2)
Radiation


