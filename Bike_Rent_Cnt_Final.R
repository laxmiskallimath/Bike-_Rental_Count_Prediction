#************************************************************************#
# Bike Rental Prediction --------------------------------------------------
#************************************************************************ #

# Clean the environment ---------------------------------------------------
rm(list=ls())

# Set working directory ---------------------------------------------------
setwd("E:/EDWISOR/Project_BikeRent")

# Cross Check current working directory -----------------------------------
getwd()


# Load the required libraries for analysis of data-------------------------
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced","C50",
      "dummies", "e1071", "Information", "MASS", "rpart", "gbm", "ROSE",
      'sampling', 'DataCombine', 'inTrees',"scales","psych","gplots")
#install.packages(x)
lapply(x, require, character.only = TRUE)
rm(x)


# Load the data --------------------------------------------------------
Bike_Rent = read.csv("day.csv",header = T,na.strings = c(""," ",NA))


# Explore the data --------------------------------------------------------
#Check the dimensions(no of rows and no of columns)
dim(Bike_Rent)

# Check names of dataset,in names we can see shortcuts like, hum for humidity
#yr for year ,mnth for month,cnt for count
names(Bike_Rent)

#Rename the variables-
names(Bike_Rent)[4]  = "year"
names(Bike_Rent)[5]  = "month"
names(Bike_Rent)[9]  = "weather"
names(Bike_Rent)[10] = "temperature"
names(Bike_Rent)[12] = "humidity"
names(Bike_Rent)[16] = "count"

#Check top(first) rows of dataset 
head(Bike_Rent)

#Check bottom(last) rows of dataset 
tail(Bike_Rent)

#Check structure of dataset(data structure of each variable)
str(Bike_Rent)

#Check summary of dataset 
summary(Bike_Rent)

# Variable Identification 
# In this dataset cnt is our target variable and it is continous variable 
str(Bike_Rent$count) 

# Remove these variables 
# instant variable, as it is index in dataset
# date variable as we have to predict count on seasonal basis not date basis
# casual and registered variable as count is sum of these two variables
# cnt = casual + registered 

Bike_Rent = subset(Bike_Rent,select=-c(instant,dteday,casual,registered))

# Lets check dimensions of data after removing some variables
dim(Bike_Rent)

# Make Seperate categorical and numerical variables dataframe 
# Continous Variables 
cnames= c("temperature","atemp","humidity","windspeed","count")

# Categorical varibles-
cat_cnames= c("season","year","month","holiday","weekday","workingday","weather")


# EDA or Data Preprocessing  ----------------------------------------------

# Duplicate Values --------------------------------------------------------
# duplicated(Bike_Rent)# No duplicates in dataset


# Missing Value anlysis ---------------------------------------------------
# Check missing values in dataset
sum(is.na(Bike_Rent))

# there is no missing values present in this dataset


# Outlier Analysis and treatment ----------------------------------------
# Lets save copy of dataset before preprocessing
df = Bike_Rent 
Bike_Rent = df 

# Lets use boxplot to detect the outliers 
# We use ggplot library to plot boxplot for each numeric variable 
for(i in 1:length(cnames))
{
  assign(paste0("gn",i),ggplot(aes_string(y=(cnames[i]),x = 'count'),
                               data=subset(Bike_Rent))+
           stat_boxplot(geom = "errorbar",width = 0.5) +
           geom_boxplot(outlier.color = "red",fill="grey",
                        outlier.shape = 18,outlier.size = 1,notch = FALSE)+
           theme(legend.position = "bottom")+
           labs(y = cnames[i],x='count')+
           ggtitle(paste("boxplot of count for",cnames[i])))
}

# using library(gridExtra)
gridExtra::grid.arrange(gn1,gn2,gn3,gn4,gn5,ncol = 2)


# Loop to remove outliers by capping upperfence and lower fence values
for(i in cnames){
  print(i)
  #Quartiles
  Q1 = quantile(Bike_Rent[,i],0.25)
  Q3 = quantile(Bike_Rent[,i],0.75)
  
  #Inter quartile range 
  IQR = Q3-Q1
  
  # Upperfence and Lower fence values 
  UL = Q3 + (1.5*IQR(Bike_Rent[,i]))
  LL = Q1 - (1.5*IQR(Bike_Rent[,i]))
  
  # No of outliers and inliers in variables 
  No_outliers = length(Bike_Rent[Bike_Rent[,i] > UL,i])
  No_inliers = length(Bike_Rent[Bike_Rent[,i] < LL,i])

  # Capping with upper and inner fence values 
  Bike_Rent[Bike_Rent[,i] > UL,i] = UL
  Bike_Rent[Bike_Rent[,i] < LL,i] = LL
 
}

# Lets plot boxplots after removing outiers 
for(i in 1:length(cnames))
{
  assign(paste0("gn",i),ggplot(aes_string(y=(cnames[i]),x = 'count'),
                               data=subset(Bike_Rent))+
           stat_boxplot(geom = "errorbar",width = 0.5) +
           geom_boxplot(outlier.color = "red",fill="grey",
                        outlier.shape = 18,outlier.size = 1,notch = FALSE)+
           theme(legend.position = "bottom")+
           labs(y = cnames[i],x='count')+
           ggtitle(paste("boxplot of count for",cnames[i])))
}

# using library(gridExtra)
gridExtra::grid.arrange(gn1,gn2,gn3,gn4,gn5,ncol = 2)


# Data understanding using visualization ----------------------------------

# Lets look at numeric and categorical variables 
# Continous Variables 
cnames= c("temperature","atemp","humidity","windspeed","count")

# Categorical varibles-
cat_cnames= c("season","year","month","holiday","weekday","workingday","weather")

# Univariate Analysis -----------------------------------------------------
# Histogram for continuous variables to check  distribution of each variable 
for(i in 1:length(cnames))
{
  assign(paste0("h",i),ggplot(aes_string(x=(cnames[i])),
                               data=subset(Bike_Rent))+
           geom_histogram(fill="darkslateblue",colour = "black")+geom_density()+
           scale_y_continuous(breaks =pretty_breaks(n=10))+
           scale_x_continuous(breaks = pretty_breaks(n=10))+
           theme_bw()+xlab(cnames[i])+ylab("Frequency")+
           ggtitle(paste("distribution of ",cnames[i])))
}

# using library(gridExtra)
gridExtra::grid.arrange(h1,h2,h3,h4,h5,ncol = 2)

# Bivariate Analysis ------------------------------------------------------
# Lets check impact of continous variables on target variable
for(i in 1:length(cnames))
{
  assign(paste0("s",i),ggplot(aes_string(y='count',x = (cnames[i])),
                               data=subset(Bike_Rent))+
           geom_point(alpha=0.5,color="DarkSlateBlue") +
           # labs(title = "Scatter Plot of count vs", x = (cnames[i]), y = "count")+
           ggtitle(paste("Scatter Plot of count vs",cnames[i])))
}

# using library(gridExtra)
gridExtra::grid.arrange(s1,s2,s3,s4,s5,ncol = 2)

# count vs temperature(atemp) : as temperature increase Bike rent count also increases 
# count vs humidity : humidity doesnt have any effect on bikerent count
# count vs windspeed : windspeed doesnt have any effect on bikerent count
# count vs count : please ignore this plot as it is our target variable 

options(scipen = 999)
# Let us check impact of categorical variables on count

for(i in 1:length(cat_cnames))
{
  assign(paste0("b",i),ggplot(aes_string(y='count',x = (cat_cnames[i])),
                              data=subset(Bike_Rent))+
           geom_bar(stat = "identity",fill = "DarkSlateBlue") +
           # labs(title = "Scatter Plot of count vs", x = (cnames[i]), y = "count")+
           ggtitle(paste("Number of bikes rented with respect to",cat_cnames[i])))+
           theme(axis.text.x = element_text( color="black", size=8))+
           theme(plot.title = element_text(face = "bold"))
}

# using library(gridExtra)
gridExtra::grid.arrange(b1,b2,b3,b4,ncol = 2)
gridExtra::grid.arrange(b5,b6,b7,ncol = 2)

# From barplot we can observe below points 
# Season:Bike rent count is high in season 3(fall) and low in season 1(springer)
aggregate(count ~ season ,sum,data = Bike_Rent)

# year : Bike rent count is high in year 1 (in 2012)
aggregate(count ~ year ,sum,data = Bike_Rent)

# month : Bike rent count is high in month of august and low in jan
aggregate(count ~ month,sum,data = Bike_Rent)

# holiday : Bike rent count is high on holidays ie 0
aggregate(count ~ holiday ,sum,data = Bike_Rent)

# weekday :From bar plot we can see maximum bikes rented on 5th day and least bikes on day 0.
aggregate(count ~ weekday ,sum,data = Bike_Rent)

# workingday : Bike rent count is high on working day  ie 1
aggregate(count ~ workingday,sum,data = Bike_Rent)

# weather : Bike rent count is high on weather 1: ie when the weather is 
# Clear, Few clouds, Partly cloudy, Partly cloudy
aggregate(count ~ weather,sum,data = Bike_Rent)


# Bikes rented with respect to temp and humidity--------------
ggplot(Bike_Rent,aes(temperature,count)) + 
  geom_point(aes(color=humidity),alpha=0.5) +
  labs(title = "Bikes rented with respect to variation in temperature and humidity", x = "temperature")+ theme_bw()
# maximum bike rented between temp 0.50 to 0.75 and humidity 0.50 to 0.75 

#Bikes rented with respect to temp and weather------------------
ggplot(Bike_Rent, aes(x = temperature, y = count))+
  geom_point(aes(color=weather))+
  labs(title = "Bikes rented with respect to temperature and weather", x = "temperature")+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
  theme_bw()

# maximum bike rented with windspeed and normalized temp between 0.50 to 0.75
# and when the weathersite is 1 

# Bikes rented with respect to temp and season--------------------
ggplot(Bike_Rent, aes(x = temperature, y = count))+
  geom_point(aes(color=season))+
  labs(title = "Bikes rented with respect to temperature and season", x = "temperature")+
   # theme(panel.background = element_rect("white"))+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
  theme_bw()
# From figure it is clear that maximum bike count is for season 2 and 3, 
# when the temp between 0.5 to 0.7

# Feature Selection -------------------------------------------------------
# Lets save dataset after outlier analysis 
df =  Bike_Rent
Bike_Rent = df

# Using corrplot library we do correlation analysis for numeric variables
# Let us derive our correlation matrix 
Correlation_matrix = cor(Bike_Rent[,cnames])
Correlation_matrix
# By looking at correlation matrix we can say temperature and atemp 
# are highly correlated (>0.99)

#Lets plot correlation plot using corrgram library 
corrgram(Bike_Rent[,cnames],order = F,upper.panel = panel.pie,
         text.panel = panel.txt,main="Correlation plot for numeric variables")

# From correlation analysis temp and atemp variables are highly correlated 
# so delete atemp variable 

# Lets find significant categorical variables usig ANOVA test 
# Anova analysis for categorical variable with target numeric variable
for(i in cat_cnames){
  print(i)
  Anova_result= summary(aov(formula = count~ Bike_Rent[,i],Bike_Rent))
  print(Anova_result)
}

# From the anova result, we can observe working day,weekday and holiday 
# has p value > 0.05, so delete this variable not consider in model.

# Dimension reduction  -------------------------------------------------
Bike_Rent = subset(Bike_Rent,select = -c(atemp,holiday,weekday,workingday))


# Lets check dimensions after dimension reduction 
dim(Bike_Rent)

head(Bike_Rent)

# Lets check column names after dimension reduction 
names(Bike_Rent)

# Lets define/update  continous and categorical variables after dimension reduction
# Continuous variable
cnames= c('temperature','humidity', 'windspeed', 'count')

# Categorical variables
cat_cnames = c('season', 'year', 'month','weather')


# Feature Scaling  --------------------------------------------------------
# Since as it is mentioned in data dictionary the values of 
# temp,humidity,windspeed variables are already normalized values so no 
# need to go for feature scaling instead we will visualize the variables 
# to see normality  

# Normality check using normal qq plot
for(i in cnames){
  print(i)
  qqplot= qqnorm(Bike_Rent[,i])
}

# Normality check using histogram plot(we already plotted hist in 
# data understanding)
gridExtra::grid.arrange(h1,h2,h3,h4,h5,ncol = 2)

#check summary of continuous variable to check the scaling- 
for(i in cnames){
  print(i)
  print(summary(Bike_Rent[,i]))
}

# From normal qq plot,histplot and by looking at summary of 
# numeric variables we can say data is normally distributed


# Model Development -------------------------------------------------------
# Let's clean R Environment, as it uses RAM which is limited
library(DataCombine)
rmExcept("Bike_Rent")

# Lets convert all categorical variables ito dummy variables 
# As we cant pass categorical variables directly in to regression problems
# Lets save our preprocessed data into df data set 
df = Bike_Rent
Bike_Rent = df

# Lets call Categorical varaibles after feature selection using ANOVA 
cat_cnames= c("season","year","month","weather")

# lets create dummy variables using dummies library
library(dummies)
Bike_Rent = dummy.data.frame(Bike_Rent,cat_cnames)
dim(Bike_Rent)
head(Bike_Rent)
# we can see dummy variables are created in Bike rent dataset 

# Divide data into train and test sets
set.seed(1234)
train.index = createDataPartition(Bike_Rent$count, p = .80, list = FALSE)
train = Bike_Rent[ train.index,]
test  = Bike_Rent[-train.index,]

# Function for Error metrics to calculate the performance of model
mape= function(y,yhat){
  mean(abs((y-yhat)/y))*100
}

# Function for r2 to calculate the goodness of fit of model
rsquare=function(y,yhat){
  cor(y,yhat)^2
}

# Function for RMSE value 
rmse = function(y,yhat){
  difference = y - yhat
  root_mean_square = sqrt(mean(difference^2))
  print(root_mean_square)
}

# Linear Regression model -------------------------------------------------

# Before building multiple linear regression model lets check the 
# vif for multicolinearity
# continous variables after feature selection using correlation analysis 
cnames= c("temperature","humidity","windspeed")
numeric_data= Bike_Rent[,cnames]

# VIF test  using usdm library
library(usdm)
vifcor(numeric_data,th=0.6)
# No variable from the 3 input variables has collinearity problem.

# Lets build multiple linear regression model on train data 
# we will use the lm() function in the stats package
LR_Model = lm(count ~.,data = train)

# Check summary
summary(LR_Model) # Adjus.Rsquare = 0.833

# Lets check the assumptins of ols regression 
# 1) Error should follow normal distribution - Normal qqplot
# 2) No heteroscadacity - Residual plot
par(mfrow = c(2, 2))# Change the panel layout to 2 x 2
plot(LR_Model)
# 3) No multicolinearity between Independent variables 
# 4) No autocorrelation between errors
library(car)
dwt(LR_Model)
# All Asumptions of regression are satisfied

# Lets predict on train data 
LR_train = predict(LR_Model,train[,-25])
# Now Lets predict on test data 
LR_test= predict(LR_Model,test[-25])

# Lets check performance of model
# MAPE For train data
LR_MAPE_Train =mape(train[,25],LR_train)
# MAPE For test data
LR_MAPE_Test=mape(test[,25],LR_test)

# Rsquare For train data
LR_r2_train=rsquare(train[,25],LR_train)
# Rsquare For test data
LR_r2_test=rsquare(test[,25],LR_test)

# rmse For train data
LR_rmse_train = rmse(train[,25],LR_train)
# rmse For test data
LR_rmse_test = rmse(test[,25],LR_test)


############################################################################
# Lets build some more models using different ml algorithms for more accuracy 
# and less prediction error
##############################################################################


# Desicision Tree ---------------------------------------------------------
# Lets Build decision tree model on train data using rpart library 
DT_model= rpart(count~.,train,method = "anova")
DT_model

# Lets plot the decision tree model using rpart.plot library 
library(rpart.plot)	
rpart.plot(DT_model,type=4,digits=3,fallen.leaves=T,tweak = 2)

# Prediction on train data
DT_train= predict(DT_model,train[-25])

# Prediction on test data
DT_test= predict(DT_model,test[-25])

# MAPE For train data
DT_MAPE_Train = mape(train[,25],DT_train)#55.91

# MAPE For train data test data
DT_MAPE_Test = mape(test[,25],DT_test)#28.09

# Rsquare  For train data
DT_r2_train= rsquare(train[,25],DT_train)#0.81

# Rsquare For test data       
DT_r2_test = rsquare(test[,25],DT_test)#0.72

# rmse For train data
DT_rmse_train = rmse(train[,25],DT_train)

# rmse For test data
DT_rmse_test = rmse(test[,25],DT_test)


# Random Search CV In Decision Tree ---------------------------------------
# Lets set parameters to pass into our decision tree model 
# Lets use caret package for the same 
control = trainControl(method="repeatedcv", number=5, repeats=1,search='random')
maxdepth = c(1:30)
tunegrid = expand.grid(.maxdepth=maxdepth)

# Lets build a model using above parameters on train data 
RDT_model = caret::train(count~., data=train, method="rpart2",trControl=control,tuneGrid= tunegrid)
print(RDT_model)

# Lets look into best fit parameters
best_fit_parameters = RDT_model$bestTune
print(best_fit_parameters)

# Again rebuild decision tree model using randomsearch best fit parameter ie
# with maximum depth = 9
RDT_best_model = rpart(count~.,train,method = "anova",maxdepth=9)

# Prediction on train data 
RDT_train = predict(RDT_best_model,train[-25])

# Prediction on test data 
RDT_test = predict(RDT_best_model,test[-25])

# Lets check Model performance on both test and train

# MAPE for train data
RDT_MAPE_Train =mape(train[,25],RDT_train)

# MAPE for test data 
RDT_MAPE_Test =mape(test[,25],RDT_test)

# Rsquare for train data
RDT_r2_train =  rsquare(train[,25],RDT_train)

# Rsquare for test data
RDT_r2_test = rsquare(test[,25],RDT_test)

# rmse For train data
RDT_rmse_train = rmse(train[,25],RDT_train)

# rmse For test data
RDT_rmse_test = rmse(test[,25],RDT_test)


# Grid Search CV in Decision Tree -----------------------------------------
control =trainControl(method="repeatedcv", number=5, repeats=2, search="grid")
tunegrid = expand.grid(.maxdepth=c(6:20))

# Lets build a model using above parameters on train data
GDT_model = caret::train(count~.,train, method="rpart2", tuneGrid=tunegrid, trControl=control)
print(GDT_model)

# Lets look into best fit parameters from gridsearch cv DT 
best_parameter = GDT_model$bestTune
print(best_parameter)

# Again rebuild decision tree model using gridsearch best fit parameter ie
# with maximum depth = 9
GDT_best_model = rpart(count ~ .,train, method = "anova", maxdepth =9)

# Prediction on train data 
GDT_train = predict(GDT_best_model,train[-25])

# Prediction on test data 
GDT_test = predict(GDT_best_model,test[-25])

# Mape for train data using gridsearch cv  DT
GDT_MAPE_Train = mape(train[,25],GDT_train)

# Mape for test data using gridsearch cv  DT
GDT_MAPE_Test = mape(test[,25],GDT_test)

# Rsquare for train data
GDT_r2_train= rsquare(train[,25],GDT_train)

# Rsquare for test data
GDT_r2_test=rsquare(test[,25],GDT_test)

# rmse For train data
GDT_rmse_train = rmse(train[,25],GDT_train)

# rmse For test data
GDT_rmse_test = rmse(test[,25],GDT_test)


# Random Forest -------------------------------------------
# Lets Build random forest model on train data using randomForest library 
RF_model= randomForest(count~.,train,ntree=100,method="anova")

# Prediction on train data
RF_train= predict(RF_model,train[-25])

# Prediction on test data
RF_test = predict(RF_model,test[-25])

# MAPE For train data
RF_MAPE_Train = mape(train[,25],RF_train)

# MAPE For test data
RF_MAPE_Test = mape(test[,25],RF_test)

# Rsquare  For train data
RF_r2_train=rsquare(train[,25],RF_train)

# Rsquare For test data       
RF_r2_test=rsquare(test[,25],RF_test)

# rmse For train data
RF_rmse_train = rmse(train[,25],RF_train)

# rmse For test data
RF_rmse_test = rmse(test[,25],RF_test)


# Random Search CV in Random Forest  --------------------------------------
control = trainControl(method="repeatedcv", number=5, repeats=1,search='random')
maxdepth = c(1:30)
tunegrid = expand.grid(.maxdepth=maxdepth)

# Lets build modelon train data using random search 
RRF_model = caret::train(count~., data=train, method="rf",trControl=control,tuneLength=10)
print(RRF_model)

# Best fit parameters
best_parameter = RRF_model$bestTune

print(best_parameter)

# Lets build model based on best fit parameters
RRF_best_model = randomForest(count ~ .,train, method = "anova", mtry=9,importance=TRUE)

# Prediction on train data
RRF_train= predict(RRF_best_model,train[-25])

# Prediction on test data
RRF_test= predict(RRF_best_model,test[-25])

# Mape for train data
RRF_MAPE_Train = mape(train[,25],RRF_train)

# Mape for test data
RRF_MAPE_Test = mape(test[,25],RRF_test) 

# Rsquare for train data
RRF_r2_train = rsquare(train[,25],RRF_train)

# Rsquare for test data
RRF_r2_test = rsquare(test[,25],RRF_test)

# rmse For train data
RRF_rmse_train = rmse(train[,25],RRF_train)

# rmse For test data
RRF_rmse_test = rmse(test[,25],RRF_test)


# Grid search CV in Random Forest  ----------------------------------------
control = trainControl(method="repeatedcv", number=5, repeats=2, search="grid")
tunegrid = expand.grid(.mtry=c(6:20))

# Lets build a model using above parameters on train data using random forest grid search cv 
GRF_model= caret::train(count~.,train, method="rf", tuneGrid=tunegrid, trControl=control)
print(GRF_model)

# Best fit parameters
best_parameter = GRF_model$bestTune
print(best_parameter)

# Build model based on Best fit parameters
GRF_best_model = randomForest(count ~ .,train, method = "anova", mtry=10)

# Prediction for train data
GRF_train= predict(GRF_best_model,train[-25])

# Prediction for test data
GRF_test= predict(GRF_best_model,test[-25])

# Mape for train data
GRF_MAPE_Train = mape(train[,25],GRF_train)

# Mape calculation of test data
GRF_MAPE_Test = mape(test[,25],GRF_test)

# Rsquare for train data
GRF_r2_train= rsquare(train[,25],GRF_train)

# Rsquare for test data
GRF_r2_test=rsquare(test[,25],GRF_test)

# rmse For train data
GRF_rmse_train = rmse(train[,25],GRF_train)

# rmse For test data
GRF_rmse_test = rmse(test[,25],GRF_test)


# Gradient Boosting -------------------------------------------------------
library(gbm)

# Lets build a Gradient Boosting model for regression problem
GB_model = gbm(count~., data = train,distribution = "gaussian", n.trees = 100, interaction.depth = 2)

# Model Prediction on train data
GB_train = predict(GB_model, train[-25], n.trees = 100)

# Model Prediction on test data
GB_test = predict(GB_model, test[-25], n.trees = 100)

# Mape for train data
GB_MAPE_Train=mape(train[,25],GB_train)

# Mape for test data
GB_MAPE_Test=mape(test[,25],GB_test)

# Rsqure for train data
GB_r2_train=rsquare(train[,25],GB_train)

# Rsquare for test data
GB_r2_test=rsquare(test[,25],GB_test)

# rmse For train data
GB_rmse_train = rmse(train[,25],GB_train)

# rmse For test data
GB_rmse_test = rmse(test[,25],GB_test)


# Random Search CV in Gradient Boosting -----------------------------------
control = trainControl(method="repeatedcv", number=5, repeats=1,search="random")
#maxdepth = c(1:30)
#tunegrid = expand.grid(.maxdepth=maxdepth)

# Model devlopment on train dat
RGB_model = caret::train(count~., data=train, method="gbm",trControl=control,tuneLength=10)

print(RGB_model)

# Best fit parameters
best_parameter = RGB_model$bestTune
print(best_parameter)

# Build model based on best fit
RGB_best_model = randomForest(count ~ .,train, method = "anova", n.trees=50,
                         interaction.depth=5,shrinkage=0.1,n.minobsinnode=10)

# Prediction on train data
RGB_train= predict(RGB_best_model,train[-25])

# Prediction on test data
RGB_test= predict(RGB_best_model,test[-25])

# Mape calculation of train data
RGB_MAPE_Train = mape(train[,25],RGB_train)

# Mape calculation of test data
RGB_MAPE_Test = mape(test[,25],RGB_test)

# Rsquare calculation for train data
RGB_r2_train= rsquare(train[,25],RGB_train)

# Rsquare calculation for test data
RGB_r2_test=rsquare(test[,25],RGB_test)

# rmse For train data
RGB_rmse_train = rmse(train[,25],GRF_train)

# rmse For test data
RGB_rmse_test = rmse(test[,25],GRF_test)


# Grid Search CV in Gradient Boosting -------------------------------------
control = trainControl(method="repeatedcv", number=5, repeats=2, search="grid")
tunegrid = expand.grid(n.trees = seq(2565,2575, by = 2),
                       interaction.depth = c(2:4), 
                       shrinkage = c(0.01,0.02),
                       n.minobsinnode = seq(18,22, by = 2))

# Model devlopment on train data
GGB_model= caret::train(count~.,train, method="gbm", tuneGrid=tunegrid, trControl=control)
print(GGB_model)

# Best fit parameters
best_parameter = GGB_model$bestTune
print(best_parameter)

# > print(best_parameter)
# n.trees interaction.depth shrinkage n.minobsinnode
# 2567                 3      0.01             18

# Build model based on best fit
GGB_best_model = randomForest(count ~ .,train, method = "anova", n.trees = 2567,
                         interaction.depth = 3,shrinkage = 0.01,n.minobsinnode = 18)

# Prediction on train data
GGB_train= predict(GGB_best_model,train[-25])

# Prediction on test data
GGB_test= predict(GGB_best_model,test[-25])

# Mape calculation of train data
GGB_MAPE_Train = mape(train[,25],GGB_train)

# Mape for test data
GGB_MAPE_Test = mape(test[,25],GGB_test)

# Rsquare for train data
GGB_r2_train= rsquare(train[,25],GGB_train)

# Rsquare for test data
GGB_r2_test=rsquare(test[,25],GGB_test)

# rmse For train data
GGB_rmse_train = rmse(train[,25],GGB_train)

# rmse For test data
GGB_rmse_test = rmse(test[,25],GGB_test)

# Results -----------------------------------------------------------------

Model = c('Linear Regression','Decision Tree for Regression',
        'Random Search in Decision Tree','Gird Search in Decision Tree',
        'Random Forest','Random Search in Random Forest',
        'Grid Search in Random Forest','Gradient Boosting',
        'Random Search in Gradient Boosting',
        'Grid Search in Gradient Boosting')

MAPE_Train = c(LR_MAPE_Train,DT_MAPE_Train,RDT_MAPE_Train,
               GDT_MAPE_Train,RF_MAPE_Train,RRF_MAPE_Train,GRF_MAPE_Train,
               GB_MAPE_Train,RGB_MAPE_Train,GGB_MAPE_Train)

MAPE_Test = c(LR_MAPE_Test,DT_MAPE_Test,RDT_MAPE_Test,
               GDT_MAPE_Test,RF_MAPE_Test,RRF_MAPE_Test,GRF_MAPE_Test,
               GB_MAPE_Test,RGB_MAPE_Test,GGB_MAPE_Test)

Rsquare_Train = c(LR_r2_train,DT_r2_train,RDT_r2_train,GDT_r2_train,
                 RF_r2_train,RRF_r2_train,GRF_r2_train,GB_r2_train,
                 RGB_r2_train,GGB_r2_train)

Rsquare_Test = c(LR_r2_test,DT_r2_test,RDT_r2_test,GDT_r2_test,
                  RF_r2_test,RRF_r2_test,GRF_r2_test,GB_r2_test,
                  RGB_r2_test,GGB_r2_test)

Rmse_Train = c(LR_rmse_train,DT_rmse_train,RDT_rmse_train,GDT_rmse_train,
               RF_rmse_train,RRF_rmse_train,GRF_rmse_train,GB_rmse_train,
               RGB_rmse_train,GGB_rmse_train)

Rmse_Test = c(LR_rmse_test,DT_rmse_test,RDT_rmse_test,GDT_rmse_test,
               RF_rmse_test,RRF_rmse_test,GRF_rmse_test,GB_rmse_test,
               RGB_rmse_test,GGB_rmse_test)

Final_results = data.frame(Model,MAPE_Train,MAPE_Test,Rsquare_Train,
                           Rsquare_Test,Rmse_Train,Rmse_Test)

Final_results

# From above results Random Forest model have optimum values and this
# algorithm is good for our data 

# Lets save the out put of finalized model (RF)
Pred_count_RF_test = predict(RF_model,test[-25])

# Exporting the output to hard disk for further use
test <- as.data.frame(cbind(test,Pred_count_RF_test))

Final_output <- as.data.frame(cbind(test$count,Pred_count_RF_test))

names(Final_output)[1] <- "Actual_Bike_Rent_Count"

write.csv(Final_output,"E:/EDWISOR/Project_BikeRent/RF_output_R.csv",
          row.names = FALSE)

# -------------------------------END--------------------------------------------#