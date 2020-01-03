
require(TunePareto)
require(glmnet)
require(gbm)
require(rpart)
library(data.table)
library(dplyr)
library(tidyr)
library(randomForest)


source('data_preprocessing.r')
source('non-odd_featuresson.R')
source('ELO_calculations.R')
source('performance_metrics.r')
source('train_models.r')

# Train and test periods are defined beforehand
# trainstartELO refers to the period ELO ratings becomes meaningful

testStart=as.Date('2019-11-01')
testEnd=as.Date('2020-01-03')
trainStart=as.Date('2017-09-16')
trainStartELO=as.Date('2019-01-01')


# Import the necessary data 
goaldata<-fread("goals.csv",header=TRUE)
matchdata<-fread("matches.csv",header=TRUE)
betdata<-fread("bets.csv")  
bookingdata<-fread("booking.csv")
statdata<-fread("stats.csv")


# Clean the stat data
setnames(statdata,old = "match_id", new = "matchId")
statdata$matchId<-as.character(statdata$matchId)
statdata$home_BallPossession<-gsub("%","",statdata$home_BallPossession)
statdata$away_BallPossession<-gsub("%","",statdata$away_BallPossession)
statdata$home_BallPossession<-as.numeric(statdata$home_BallPossession)/100
statdata$away_BallPossession<-as.numeric(statdata$away_BallPossession)/100


# Convert match data in a suitable format for prepared functions
mraw<-data.table("leagueId"=0,"matchId"=matchdata$match_id,"home"=matchdata$match_hometeam_name,"away"=matchdata$match_awayteam_name,"score"=paste(matchdata$match_hometeam_score,matchdata$match_awayteam_score,sep = ":"),"date"=matchdata$epoch,"type"="soccer")
mraw$leagueId<-as.character(mraw$leagueId)
mraw$matchId<-as.character(mraw$matchId)
mraw$date<-as.numeric(mraw$date)

# Convert bet data in the corresponding format
braw<-data.table("matchId"=betdata$match_id,"betType"= "1x2","oddtype"=betdata$variable,"bookmaker"=betdata$odd_bookmakers,"date"=betdata$odd_epoch,"odd"=betdata$value,"totalhandicap"=NA)
braw[oddtype!="odd_1" & oddtype!="odd_2"& oddtype!="odd_x"]$betType<-"OtherBets"
braw$oddtype<-gsub("_","",braw$oddtype)
braw$oddtype<-gsub("x","X",braw$oddtype)
braw$matchId<-as.character(braw$matchId)
braw$date<-as.numeric(braw$date)
braw$totalhandicap<-as.character(braw$totalhandicap)

# Preprocess matches data
matches=matches_data_preprocessing(mraw)

# Add ELO measures 
matches=add_elo(matches,testStart,testEnd)

# Add goals scored and conceded for the home and away team in each ones last N games
matches_L_n=extract_last_ngames(matches,N=15)

# Record the matches info matrix for different n values
matches_L_5=matches_L_n
matches_L_10=matches_L_n
matches_L_15=matches_L_n

# Upon need execute following to compare for different n values
matches_L_n=matches_L_5
matches_L_n=matches_L_10
matches_L_n=matches_L_15


# Divide data based on the provided dates 
train_features=matches_L_n[Match_Date>=trainStart & Match_Date<testStart] 
test_features=matches_L_n[Match_Date>=testStart] 

# Use complete.cases function to clean the data off NA values
# In order to make predictions for future games, refrain from using complete.cases function on test features
train_features<-train_features[complete.cases(train_features)]
test_features<-test_features[complete.cases(test_features)]


## Model 1 - LASSO Regression

# Run glmnet on train data with tuning lambda parameter based on RPS and return predictions based on lambda with minimum RPS
# After few trial we eliminated some of the extracted feature in order to minimize RPS in test data
predictions_glm=train_glmnet(train_features, test_features,not_included_feature_indices=c(1:18,25,26,31,32), alpha=1,nlambda=50, tune_lambda=TRUE,nofReplications=2,nFolds=10,trace=T)

# Calculate mean RPS for test set
mean(RPS_matrix(predictions_glm$predictions[,3:5],res_matrix(predictions_glm$predictions[,2])))

# Obtain the predicted probabilities via matrix form 
View(predictions_glm$predictions)
pred<-cbind(predictions_glm$predictions[!is.na(Match_Result)&!is.na(Home)],res_matrix(predictions_glm$predictions[!is.na(Match_Result)&!is.na(Home)],3))
pred<-cbind(pred,RPS=RPS_matrix(pred[,3:5],pred[,6:8]))



#Before proceeding forward use train data from the trainStartELO to use reasonable ELO Ratings as features
train_features=matches_L_n[Match_Date>=trainStartELO & Match_Date<testStart] 
test_features=matches_L_n[Match_Date>=testStart] 
train_features<-train_features[complete.cases(train_features)]
test_features<-test_features[complete.cases(test_features)]


## Model 2 - Stochastic Gradient Boosting

noftrees=500
sampling_fraction=0.5
depth= c(1,3,5)
learning_rate= c(0.15,0.1,0.05)

#GBMperf=matrix(0,ncol=3,nrow=3)

# Create dataframe to store results of each combination
sgb_RPS=data.frame(matrix(NA,nrow=3*3,ncol=3))

Folds=10
Replication = 2
cvindices=generateCVRuns(train_features$Match_Result,nfold = Folds, ntimes = Replication, stratified = TRUE)
a=1


for(i in 1:length(depth)){
  
  for(j in 1:length(learning_rate)){
    sumRPS=0
    
    for(k in 1:Replication){
      indices=cvindices[[k]]
      
      for(l in 1:Folds){
        
        test=order(indices[[l]])
        cvtrain=train_features[-test,]    
        cvtest=train_features[test,]
    
              boosting_model=gbm(as.factor(cvtrain$Match_Result)~., data=cvtrain[,c(19:24,27:30)],distribution = "multinomial", n.trees = noftrees,
                       interaction.depth = depth[i], n.minobsinnode = 5, shrinkage =learning_rate[j] ,
                       bag.fraction = sampling_fraction, cv.folds = 5)
              bestno = gbm.perf(boosting_model,method = "cv")
              sgb_pred=cbind(cvtest[,c(2,10)],predict(boosting_model,cvtest,n.trees = bestno ,type="response")[,1:3,1])
              sgb_pred<- sgb_pred[,c(1,2,4,5,3)]
              sumRPS<- sumRPS + mean(rps_calc(sgb_pred,3))
              
      }
    }
    sgb_RPS[a,]<- c(j,i,sumRPS/20)
    a=a+1
    
  }
}

# Get the parameters giving minimum RPS
index=which.min(sgb_RPS$X3)
opt_learning_rate=learning_rate[sgb_RPS[index,1]]
opt_depth=depth[sgb_RPS[index,2]]



# Obtain final model and calculate RPS for SGB model
final_sgb= gbm(as.factor(train_features$Match_Result)~., data=train_features[,c(19:24,27:30)],distribution = "multinomial", n.trees = noftrees,
               interaction.depth = opt_depth, n.minobsinnode = 5, shrinkage =opt_learning_rate ,
               bag.fraction = sampling_fraction, cv.folds = 5)
bestno = gbm.perf(boosting_model,method = "cv")
final_sgb_pred=cbind(test_features[,c(2,10)],predict(final_sgb,test_features,n.trees = bestno ,type="response")[,1:3,1])
final_sgb_pred<- final_sgb_pred[,c(1,2,4,5,3)]
mean(rps_calc(final_sgb_pred,3))  
  




## Model 3 - Decision Tree

# 10 fold cross validation is conducted to tune minbucket and complexity parameters

# Create dataframe to store results of each combination
dt_RPS=data.frame(matrix(NA,nrow=9*15,ncol=3))
colnames(dt_RPS)=c("MinBucket","Cp","RPS")

# CV indices were obtained using generateCVRuns function
Folds=10
Replication = 2
cvindices=generateCVRuns(train_features$Match_Result,nfold = Folds, ntimes = Replication, stratified = TRUE)
a=1

# For each parameter combination calculate the mean RPS and write on dt_RPS
for(i in seq(0.001,0.015,by=0.001)) {
  for(j in seq(20,60,by=5)){
    sumRPS=0
    for(k in 1:Replication){
      indices=cvindices[[k]]
      for(l in 1:Folds){
        test=order(indices[[l]])
        cvtrain=train_features[-test,]    
        cvtest=train_features[test,]
        tree=rpart(Match_Result~.,cvtrain[,c(10,16:32)],method="class",
                   control=rpart.control(minbucket=j,cp=i))
        dtpred=predict(tree, cvtest[,c(10,16:32)], type = 'prob')
        dtpred<- cbind(cvtest[,c(2,10)],dtpred)
        dtpred<- dtpred[,c(1,2,4,5,3)]
        sumRPS<- sumRPS + mean(rps_calc(dtpred,3))
        
      }
    }
    dt_RPS[a,]<- c(j,i,sumRPS/20)
    a=a+1
  }
}

# Get the parameters giving minimum RPS
index=which.min(dt_RPS$RPS)
optbucket=dt_RPS[index,1]
optcp=dt_RPS[index,2]


# Obtain final model and calculate RPS for Decision Tree model
final_dt= rpart(Match_Result~.,train_features[,c(10,16:32)],method="class",
                    control=rpart.control(minbucket=optbucket,cp=optcp))
final_dtpred=predict(final_dt, test_features[,c(10,16:32)], type = 'prob')
final_dt<- cbind(test_features[,c(2,10)],final_dtpred)
final_dt<- final_dt[,c(1,2,4,5,3)]
final_dt_RPS<- rps_calc(final_dt,3)
mean(final_dt_RPS)

str(final_dt)
fancyRpartPlot(final_dt)
barplot(final_dt$variable.importance)
final_dt$variable.importance

## Model 4 - Random Forest

# Number of trees, number of folds and replications are predefined. 
NoOfTrees=500
Folds=10
Replication = 2

# Generate sequence of number of sampled features for each tree
Mrange = 1:17

# Create matrix to store RPS values of each m value
rf_RPSmean=matrix(NA,nrow=17,ncol=20)

# CV indices were obtained using generateCVRuns function
cvindices=generateCVRuns(train_features$Match_Result,nfold = Folds, ntimes = Replication, stratified = TRUE)
k=1

# 10 fold cross validation is conducted to tune m

for(i in 1:Replication) {
  indices=cvindices[[i]]
  for(j in 1:Folds){
    test=order(indices[[j]])
    cvtrain=train_features[-test,]    
    cvtest=train_features[test,]
      for(m in Mrange)
      {
        rffit = randomForest(formula = as.factor(Match_Result) ~.,data=cvtrain[,c(10,16:32)], ntrees=NoOfTrees, mtry=m)
        rfpred=predict(rffit, cvtest[,c(16:32)], type = 'prob')
        rfpred<- cbind(cvtest[,c(2,10)],rfpred)
        rfpred<- rfpred[,c(1,2,4,5,3)]
        rf_RPS<- rps_calc(rfpred,3)
        rf_RPSmean[m,k]=mean(rf_RPS)
        
      }
    k=k+1
  }
}


# Get optimal number of subsets and obtain the final model
best_m=match(min(rowMeans(rf_RPSmean)),rowMeans(rf_RPSmean))
final_rf= randomForest(formula = as.factor(Match_Result) ~.,data=train_features[,c(10,19:30)], ntrees=NoOfTrees, mtry=7)

# Calculate RPS for Random Forest Model
final_rfpred=predict(final_rf, test_features[,c(19:30)], type = 'prob')
final_rfpred<- cbind(test_features[,c(2,10)],final_rfpred)
final_rfpred<- final_rfpred[,c(1,2,4,5,3)]
final_rf_RPS<- rps_calc(final_rfpred,3)
mean(final_rf_RPS)


## Model 5 - Combined glm and rf

# Try different proportions of the models and choose the one with minimum RPS
p_seq<-seq(0,1,by=0.1)
combined_RPS<-c()

for (p in 1:length(p_seq)) {
  
  combined_prob=(p_seq[p])*final_rfpred[,3:5]+(1 - p_seq[p])*pred[,3:5]
  combined<-data.frame(matchId=final_rfpred$matchId,Match_Result=final_rfpred$Match_Result)
  combined<-cbind(combined,combined_prob)
  combined_RPS[p]<-mean(rps_calc(combined,3)) 
}

combined_RPS
optimum_p<-p_seq[which.min(combined_RPS)]
optimum_p
final_combined_RPS<-combined_RPS[which.min(combined_RPS)]
final_combined_RPS

# Following iteration carries out the task of testing the final model for certain matches
# Probabilities are predicted and RPS is calculated for matches of submission schedule

combined_prob=(optimum_p)*final_rfpred[,3:5]+(1 -optimum_p)*pred[,3:5]
combined<-cbind(test_features[,c(2,10)],combined_prob)
combined<-cbind(combined,RPS=rps_calc(combined,3))
mean(rps_calc(combined,3)) 
ProjectRounds=combined[(as.numeric(matchId)>273230 & as.numeric(matchId)<273271) | (as.numeric(matchId)>273280 & as.numeric(matchId)<273311)]
mean(ProjectRounds$RPS)
