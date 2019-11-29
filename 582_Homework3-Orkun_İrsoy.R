setwd("C:/Users/Orkun/Desktop/IE 582 - HW 3")

library(data.table)
library(dplyr)
library(tidyr)
library(glmnet)

#Import the data and arrange the column names
consumptiondata<-fread("Power Consumption.csv",header=TRUE)
setnames(consumptiondata, "Consumption (MWh)","Consumption")

#Change the data structure since it is hard to work with "chr" type
str(consumptiondata)
consumptiondata$Date<-as.Date(consumptiondata$Date,"%d.%m.%Y")
consumptiondata$Consumption<-gsub(",","",consumptiondata$Consumption)
consumptiondata$Consumption<-as.numeric(consumptiondata$Consumption)
consumptiondata$Hour<-as.numeric(gsub(":00","",consumptiondata$Hour))
str(consumptiondata)

#Add the Lagged Values for 48 hours lag and 168 hours lag
consumptiondata<-cbind(consumptiondata,"LAG48" = lag(consumptiondata$Consumption, n = 48), "LAG168" = lag(consumptiondata$Consumption, n = 168))

##Part a
## As the test data I chosed between 2019-11-01 and 2019-11-21 so that we have equal number of each day of the week as test data
testdata<-consumptiondata[Date >= "2019-11-01"]

## Calculate Absolute percentage errors for Lag48 and Lag168 values and add them to dataframe
testdata<-cbind(testdata,"APE48" = 0, "APE168"=0)
testdata$APE48<-100*abs(testdata$Consumption-testdata$LAG48)/testdata$Consumption
testdata$APE168<-100*abs(testdata$Consumption-testdata$LAG168)/testdata$Consumption

##Part b
## To train the regression models I excluded the dates with NA values for LAG48 and LAG168 
traindata<-consumptiondata[Date < "2019-11-01" & Date > "2016-01-07"]

fitB<-lm(Consumption~ LAG48+LAG168, data = traindata)
summary(fitB)
## Both regression features seem statistically significant with relatively ok R-squared value
lag_regression<-predict(fitB, newdata = data.frame("LAG48"=testdata$LAG48,"LAG168" =testdata$LAG168))

## Add the absolute percentage deviation values for part b as well
testdata<-cbind(testdata,"Lag_Regression"= lag_regression,"APE_R"=0)
testdata$APE_R<-100*abs(testdata$Consumption-testdata$Lag_Regression)/testdata$Consumption

##Part c

##Add necessary columns to test data
testdata<-cbind(testdata,"Hour_Regression"=0,"APE_HR"=0)


##For every hour create a linear model from the data corresponding that hour then,
##find predictions and update the table.
for(i in 0:23 ){

fitC<-lm(Consumption~ LAG48+LAG168, data = traindata[Hour == i])

##Once we build the model we calculate for every same hour in our test data.
for(j in 0:20 ){
testdata$Hour_Regression[24*j+i+1]<-predict(fitC, newdata = data.frame("LAG48"=testdata$LAG48[24*j+i+1],"LAG168" =testdata$LAG168[24*j+i+1]))
}

}

##After making predictions we can easily calculate APE values.
testdata$APE_HR<-100*abs(testdata$Consumption-testdata$Hour_Regression)/testdata$Consumption


##Part d

#Transform data into wide format
wideconsumption<-spread(consumptiondata[,1:3],Hour, Consumption)


#Add lagged values as columns for 2 day lag
for (i in 2:25) {
  
  wideconsumption<-cbind(wideconsumption,lag(wideconsumption[[i]], n = 2))  
}

#Add lagged values as columns for 7 day lag
for (i in 2:25) {
  
  wideconsumption<-cbind(wideconsumption,lag(wideconsumption[[i]], n = 7))  
}

#Change column names accordingly
names1<-paste("2Lag_",seq(0:23)-1,sep = "")
setnames(wideconsumption,26:49,names1)
names2<-paste("7Lag",seq(0:23)-1,sep = "")
setnames(wideconsumption,50:73,names2)


#Drop unnecessary columns, ie. columns without lags
wideconsumption<-cbind(wideconsumption[,1], wideconsumption[,26:73])

#Merge with hourly consumption data to create data table to train models
D_traindata<-merge(consumptiondata[,1:3],wideconsumption,by = "Date")

#Filter out the NA's in the first 7 days and limit until 1st of November
D_traindata<-D_traindata[Date < "2019-11-01" & Date > "2016-01-07"]

#And we construct the wide format of the test data as well. 
D_testdata<-merge(testdata[,1:3],wideconsumption,by = "Date")

#Add necessary columns to write predictions and APE values to test data
testdata<-cbind(testdata,"Lasso_Regression"=0,"APE_Lasso"=0)


#Create values to keep the coeeficients and lambda values of 24 model
Lasso_coef<-matrix(NA,nrow = 48,ncol = 24)
Lambda_values<-0



#For each Hour value build models with multiple lamda values
#By cross validation chose the best lambda and use that model to predict
#Meanwhile we keep the corresponding beta and lambda values
for(i in 0:23 ){
  
  df0<-D_traindata[Hour == i]
  y<-df0[[3]]
  x<-as.matrix(df0[,4:51])
  set.seed(702)
  fitD<-cv.glmnet(x,y,alpha = 1)
  bestmodel<-glmnet(x,y,alpha = 1,lambda = fitD$lambda.min)  
  
  Lambda_values[i+1]<-bestmodel$lambda
  Lasso_coef[,i+1]<-bestmodel$beta[1:48]
  summary(bestmodel)
  
  for(j in 0:20 ){
    predictx<-as.matrix(D_testdata[24*j+i+1,4:51])
    testdata$Lasso_Regression[24*j+i+1]<-predict(bestmodel, newx = predictx )
  }
}

#It seems like lambda values vary between 0.3-1.35 which are relatively close to each other
#it is reasonable since consumption values are not that much different
summary(Lambda_values)


##We can obtain how many features have been included for each model and absolute sums of coefficients
#For comparison we can use the same feature matrix with the simple linear regression and see the difference
Lasso_Models<-data.frame("Models"=paste("Lasso_coefs_",seq(0:23)-1,sep = ""),"Number of used features"= 48-colSums(Lasso_coef == 0),"Absolute sums of coefficients"=colSums(abs(Lasso_coef)))


#Applying same procedure for simple linear regression
Linear_Reg_coef<-matrix(NA,nrow = 48,ncol = 24)
for(i in 0:23 ){
  df0<-D_traindata[Hour == i]
  y<-df0[[3]]
  x<-as.matrix(df0[,4:51])
  fitLR<-lm(y~x)
  Linear_Reg_coef[,i+1]<-fitLR$coefficients[2:49]
}

#Add absolute sums of coefficients to the same dataframe
Lasso_Models<-cbind(Lasso_Models,"Abs. Sums for Linear Reg"=colSums(abs(Linear_Reg_coef)))
show(Lasso_Models)

#As it is observed coefficients are smaller in terms of magnitude compared to the linear regression
#By using Lasso Regression we shrinked magnitudes of our coefficients
#However number of used features is still high.




#Next we calculate APE values for Lasso regression predictions
testdata$APE_Lasso<-100*abs(testdata$Consumption-testdata$Lasso_Regression)/testdata$Consumption
           

#Part E
#Combine all APE values in one dataframe
E_plotdata<-testdata[,c(2,6,7,9,11,13)]

#Calculate means for each hour value
E_plotdata<-aggregate.data.frame(E_plotdata[,2:6], by = list(E_plotdata$Hour) , FUN = mean ,order =)
setnames(E_plotdata,1:6,c("Hour","MAPE48","MAPE168","MAPE_R","MAPE_HR","MAPE_Lasso"))
show(E_plotdata)


#Plot the MAPE values for each method
boxplot(E_plotdata[,2:6],ylab = "Mean Absolute Percentage Error",outline = F,names = c("2 Day", "7 Day", "S Reg", "HR Reg","Lasso Reg"),col = c("red","green","blue","purple","orange"))

#Mean, median, max and min MAPE values for each model
summary(E_plotdata)

#As we can see the most successful models are 7_Day_Lagged prediction and the Lasso Regression. 
#It seems like Lasso Regression is better in terms of bias since its median value is closer to zero 
#whereas 7_Day_Lag model has smaller variance which makes it still preferable. 
#Discrepancy between 2_Day and 7_Day_Lag models suggests that chosing the same day values as predictors creates a considerable difference.


##We can plot predicted values and realized consumption for each method. 
##Note that x axis represents the starting hour each day and black graph is the realized consumption line.

##Predicted values vs Real Values for 2_Day_Lag Model (Black is real consumptions)
plot(testdata$Consumption,axes = F,type ="l",lwd=1.5,ylab = "Consumption",main = "LAG48 Model Predictions",xlab = "Days of November")
lines(testdata$LAG48,col = "red" )
axis(1, at=seq(1, 504, by=24), labels=seq(1,21), lwd=0, lwd.ticks=1)
axis(2)
abline(v=(24*(seq(1,21)-1))+1, col="lightgray", lty="dotted")

##Predicted values vs Real Values for 7_Day_Lag Model (Black is real consumptions)
plot(testdata$Consumption,axes = F,type ="l",lwd=1.5,ylab = "Consumption",main = "LAG168 Model Predictions",xlab = "Days of November")
lines(testdata$LAG168,col = "green" )
axis(1, at=seq(1, 504, by=24), labels=seq(1,21), lwd=0, lwd.ticks=1)
axis(2)
abline(v=(24*(seq(1,21)-1))+1, col="lightgray", lty="dotted")

##Predicted values vs Real Values for Regression Model (Black is real consumptions)
plot(testdata$Consumption,axes = F,type ="l",lwd=1.5,ylab = "Consumption",main = "Regression Model Predictions",xlab = "Days of November")
lines(testdata$Lag_Regression,col = "blue" )
axis(1, at=seq(1, 504, by=24), labels=seq(1,21), lwd=0, lwd.ticks=1)
axis(2)
abline(v=(24*(seq(1,21)-1))+1, col="lightgray", lty="dotted")

##Predicted values vs Real Values for Hourly Regression Model (Black is real consumptions)
plot(testdata$Consumption,axes = F,type ="l",lwd=1.5,ylab = "Consumption",main = "Hourly Regression Model Predictions",xlab = "Days of November")
lines(testdata$LAG168,col = "purple" )
axis(1, at=seq(1, 504, by=24), labels=seq(1,21), lwd=0, lwd.ticks=1)
axis(2)
abline(v=(24*(seq(1,21)-1))+1, col="lightgray", lty="dotted")

##Predicted values vs Real Values for Lasso Regression Model (Black is real consumptions)
plot(testdata$Consumption,axes = F,type ="l",lwd=1.5,ylab = "Consumption",main = "Lasso Regression Model Predictions",xlab = "Days of November")
lines(testdata$LAG168,col = "orange" )
axis(1, at=seq(1, 504, by=24), labels=seq(1,21), lwd=0, lwd.ticks=1)
axis(2)
abline(v=(24*(seq(1,21)-1))+1, col="lightgray", lty="dotted")


##We can see that almost every model underestimates 4th and 5th of November which may be caused by
##their predictors 28th and 29th of October (National Holiday). Graphs clarifies the seasonality depending
##on weekdays since Saturdays and Sundays' consumptions are less compared to other days. Thus, LAG48
##model fails to catch that trend. 
##It seems reasonable to use LAG168 model since it is simple and has low variance in terms of MAPE values. 


##We can also consider hourly MAPE values for each method

plot(E_plotdata$Hour,E_plotdata$MAPE48,col="red", type = "l",ylim = c(0,12),ylab = "MAPE",xlab = "Hours",main = "Hourly MAPE Values")
lines(E_plotdata$MAPE168, col = "green")
lines(E_plotdata$MAPE_R, col = "blue")
lines(E_plotdata$MAPE_HR, col = "purple")
lines(E_plotdata$MAPE_Lasso, col = "orange")
legend(18,12,legend = c("Lag 2 Day","Lag 7 Day","Reg.","Hr Reg.","Lasso Reg."),cex=0.6,col = c("red","green","blue","purple","orange"),lwd = 1)

##Daily hours are harder to predict. Mpreover Lasso Regression seems to perform better for night hours.
##Thus,a hybrid model can be used to predict daily consumption, Lag 7 for day hours and Lasso Regression for nights. 