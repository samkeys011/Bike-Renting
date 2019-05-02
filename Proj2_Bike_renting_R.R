rm(list=ls())
setwd('D:/Edwisor/Project 2 - Bike Renting')
getwd()

#importing libraries
library('corrgram')
#install.packages('dummyVars')
library('ggplot2')
library('DMwR')
library('caret')
library('rpart')
library('randomForest')

#loading the data
df_init = read.csv("day.csv")

#checking summary
summary(df_init)

#checking the datatype of each column
sapply(df_init, function(x) class(x))

#creating day variable from dteday
day = read.table(text = as.character(df_init$dteday), sep = "-", stringsAsFactors=FALSE)
df_init$day = day$V3

#checking count of missing values in each column
sapply(df_init, function(x) sum(is.na(x)))

#removing dteday as we already have yr,mnth and day. also removing instant as it is just an index number
df_init$dteday = NULL
df_init$instant = NULL

#barplots of categorical variables
barplot(table(df_init$season))
barplot(table(df_init$yr))
barplot(table(df_init$mnth))
barplot(table(df_init$holiday))
barplot(table(df_init$weekday))
barplot(table(df_init$workingday))
barplot(table(df_init$weathersit))
barplot(table(df_init$day))

#boxplot for outlier analysis
boxplot(df_init$temp)
boxplot(df_init$atemp)
boxplot(df_init$hum)
boxplot(df_init$windspeed)

#outlier treatment using KNN
out_cnames = c('hum','windspeed')
for (i in out_cnames){
  val = df_init[,i][df_init[,i] %in% boxplot.stats(df_init[,i])$out]
  df_init[,i][df_init[,i] %in% val] = NA
}

df_init = knnImputation(df_init, k = 3)

#histogram for continuous variables
hist(df_init$temp)
hist(df_init$atemp)
hist(df_init$hum)
hist(df_init$windspeed)

#correlation plot
cont_cnames = c('temp','atemp','hum','windspeed','registered','casual','cnt')
corrgram(df_init[,cont_cnames], order = F, upper.panel = panel.pie, text.panel = panel.txt, main = "Correlation Plot")

#dropping atemp, casual and registered due to correlation and data leakage
df_init$atemp = NULL
df_init$casual = NULL
df_init$registered = NULL
df_init = df_init1 

#dummy encoding
df_init$season = as.factor(df_init$season)
dmy = dummyVars("~ season", data = df_init)
df_append = data.frame(predict(dmy,newdata = df_init))
df_init = cbind(df_init,df_append)

df_init$mnth = as.factor(df_init$mnth)
dmy = dummyVars("~ mnth", data = df_init)
df_append = data.frame(predict(dmy,newdata = df_init))
df_init = cbind(df_init,df_append)

df_init$weekday = as.factor(df_init$weekday)
dmy = dummyVars("~ weekday", data = df_init)
df_append = data.frame(predict(dmy,newdata = df_init))
df_init = cbind(df_init,df_append)

df_init$weathersit = as.factor(df_init$weathersit)
dmy = dummyVars("~ weathersit", data = df_init)
df_append = data.frame(predict(dmy,newdata = df_init))
df_init = cbind(df_init,df_append)

df_init$day = as.factor(df_init$day)
dmy = dummyVars("~ day", data = df_init)
df_append = data.frame(predict(dmy,newdata = df_init))
df_init = cbind(df_init,df_append)

df_init$season = NULL
df_init$mnth = NULL
df_init$weathersit = NULL
df_init$day = NULL
df_init$weekday = NULL

df_init1 = df_init
#sampling
train.index = createDataPartition(df_init$cnt, p=0.8, list = FALSE)
train_df = df_init[train.index,]
test_df = df_init[-train.index,]

y_train = train_df$cnt
y_test = test_df$cnt
train_df$cnt = NULL
test_df$cnt = NULL

#defining error metric MAPE
MAPE = function(y,yhat){
  mean(abs((y-yhat)/y))*100
}

#Decision Tree Regression(MAPE = 22.40)
DT_reg = rpart(y_train ~ ., data = train_df, method = 'anova')
DT_pred = predict(DT_reg, test_df)

MAPE(y_test, DT_pred)

#Random Forest Regression(MAPE = 14.086)
RF_reg = randomForest(y_train ~ ., data = train_df, ntree = 300)
RF_pred = predict(RF_reg, test_df)

MAPE(y_test, RF_pred)

#Linear Regression(MAPE = 17.58)
Lin_reg = lm(y_train ~ ., data = train_df)
summary(Lin_reg)
LR_pred = predict(Lin_reg, test_df)

MAPE(y_test, LR_pred)

#KNN Regression(MAPE = 30.75)
KNN_reg = knnreg(train_df, y_train, k = 2)
KNN_pred = predict(KNN_reg, test_df)

MAPE(y_test, KNN_pred)
