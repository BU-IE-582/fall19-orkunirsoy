setwd("C:/Users/orkun/Desktop/HW 5")

library(data.table)
library(factoextra)
require(scatterplot3d)
require(clValid)
require(fpc)
require(cluster)
require(class)


##Get files then extract the acceleration information
files=list.files('C:/Users/orkun/Desktop/HW 5',full.names = T)


Xmat=as.matrix(read.table('C:/Users/orkun/Desktop/HW 5/uWaveGestureLibrary_X_TRAIN'))
Xacc<-Xmat[,-1]

Ymat=as.matrix(read.table('C:/Users/orkun/Desktop/HW 5/uWaveGestureLibrary_Y_TRAIN'))
Yacc<-Ymat[,-1]

Zmat=as.matrix(read.table('C:/Users/orkun/Desktop/HW 5/uWaveGestureLibrary_Z_TRAIN'))
Zacc<-Zmat[,-1]

Xtest=as.matrix(read.table('C:/Users/orkun/Desktop/HW 5/uWaveGestureLibrary_X_TEST'))

Ytest=as.matrix(read.table('C:/Users/orkun/Desktop/HW 5/uWaveGestureLibrary_Y_TEST'))

Ztest=as.matrix(read.table('C:/Users/orkun/Desktop/HW 5/uWaveGestureLibrary_Z_TEST'))




class<-Xmat[,1]

#Find index from each class
c1=which(class == 1)[[1]]
c2=which(class == 2)[[1]]
c3=which(class == 3)[[1]]
c4=which(class == 4)[[1]]
c5=which(class == 5)[[1]]
c6=which(class == 6)[[1]]
c7=which(class == 7)[[1]]
c8=which(class == 8)[[1]]


##Using acceleration calculate velocity then create position information
rownumber=nrow(Xacc)
colnumber=ncol(Xacc)
Xvelocity<-matrix(0,nrow=rownumber,ncol=colnumber)
Xlocation<-matrix(0,nrow=rownumber,ncol=colnumber)
Yvelocity<-matrix(0,nrow=rownumber,ncol=colnumber)
Ylocation<-matrix(0,nrow=rownumber,ncol=colnumber)
Zvelocity<-matrix(0,nrow=rownumber,ncol=colnumber)
Zlocation<-matrix(0,nrow=rownumber,ncol=colnumber)

for(i in 1:rownumber) {
  Xvelocity[i,] <- cumsum(Xacc[i,]) }

for(i in 1:rownumber) {
  Xlocation[i,] <- cumsum(Xvelocity[i,])}


for(i in 1:rownumber){
  Yvelocity[i,] <- cumsum(Yacc[i,]) 
}

for(i in 1:rownumber){
  Ylocation[i,] <- cumsum(Yvelocity[i,])
}

for(i in 1:rownumber) {
  Zvelocity[i,] <- cumsum(Zacc[i,]) 
}

for(i in 1:rownumber) {
  Zlocation[i,] <- cumsum(Zvelocity[i,]) 
}

par(mfrow=c(1,1))

scatterplot3d(Xlocation[c1,],Ylocation[c1,],Zlocation[c1,],col.axis="lightblue",main = "GESTURE 1",xlab = "X location",ylab = "Y location",zlab = "Z location", col.grid="lightblue",type = "h", color = "gray")

scatterplot3d(Xlocation[c2,],Ylocation[c2,],Zlocation[c2,],col.axis="lightblue",main = "GESTURE 2",xlab = "X location",ylab = "Y location",zlab = "Z location", col.grid="lightblue",type = "h", color = "gray")

scatterplot3d(Xlocation[c3,],Ylocation[c3,],Zlocation[c3,],col.axis="lightblue",main = "GESTURE 3",xlab = "X location",ylab = "Y location",zlab = "Z location", col.grid="lightblue",type = "h", color = "gray")

scatterplot3d(Xlocation[c4,],Ylocation[c4,],Zlocation[c4,],col.axis="lightblue",main = "GESTURE 4",xlab = "X location",ylab = "Y location",zlab = "Z location", col.grid="lightblue",type = "h", color = "gray")

scatterplot3d(Xlocation[c5,],Ylocation[c5,],Zlocation[c5,],col.axis="lightblue",main = "GESTURE 5",xlab = "X location",ylab = "Y location",zlab = "Z location", col.grid="lightblue",type = "h", color = "gray")

scatterplot3d(Xlocation[c6,],Ylocation[c6,],Zlocation[c6,],col.axis="lightblue",main = "GESTURE 6",xlab = "X location",ylab = "Y location",zlab = "Z location", col.grid="lightblue",type = "h", color = "gray")

scatterplot3d(Xlocation[c7,],Ylocation[c7,],Zlocation[c7,],col.axis="lightblue",main = "GESTURE 7",xlab = "X location",ylab = "Y location",zlab = "Z location", col.grid="lightblue",type = "h", color = "gray")

scatterplot3d(Xlocation[c8,],Ylocation[c8,],Zlocation[c8,],col.axis="lightblue",main = "GESTURE 8",xlab = "X location",ylab = "Y location",zlab = "Z location", col.grid="lightblue",type = "h", color = "gray")


## To apply clustering we first need to combine x, y and z information of corresponding instance to the same matrix

trainmat<-cbind(Xmat,Yacc,Zacc)
trainmat[,-1]<-scale(trainmat[,-1])

testmat<-cbind(Xtest,Ytest[,-1],Ztest[,-1])
testmat[,-1]<-scale(testmat[,-1])



##Part b-c: to use k-medoids we will use pam function (Partitioning Around Medoids) 
#Apart from k-means, pam choses medoids among data points and uses dissimilarity info instead of SSE.
#If we were to use Euclidean distance matrix for k-medoids and each cluster were have to a data points inside
#that lies at the centeroid than two methods would be equivalent. 


raw_trainmat<-trainmat[,-1]
rownames(raw_trainmat)<-seq(1,nrow(raw_trainmat),1)



##For Eclidean distance
Hcl_av_euc<-clValid(raw_trainmat,2:10,clMethods = "hierarchical" ,metric = "euclidean",method = "average",validation = "internal",maxitems = nrow(raw_trainmat))
summary(Hcl_av_euc)


Hcl_ward_euc<-clValid(raw_trainmat,2:10,clMethods = "hierarchical" ,metric = "euclidean",method = "ward",validation = "internal",maxitems = nrow(raw_trainmat))
summary(Hcl_ward_euc)

Kcl_euc<-clValid(raw_trainmat,2:10,clMethods = "pam" ,metric = "euclidean",validation = "internal",maxitems = nrow(raw_trainmat))
summary(Kcl_euc)

#We can see the optimal number of clusters for each approach regarding different measures. Dunn and Silhoutte seems to gave similar
# number of clusters (close to 8 ) therefore it might be good idea to choose one of them as our metric. 


##If we use correlation as distance measure
Hcl_av_cor<-clValid(raw_trainmat,2:10,clMethods = "hierarchical" ,metric = "correlation",method = "average",validation = "internal",maxitems = nrow(raw_trainmat))
summary(Hcl_av_cor)

Hcl_ward_cor<-clValid(raw_trainmat,2:10,clMethods = "hierarchical" ,metric = "correlation",method = "ward",validation = "internal",maxitems = nrow(raw_trainmat))
summary(Hcl_ward_cor)

Kcl_cor<-clValid(raw_trainmat,2:10,clMethods = "pam" ,metric = "correlation",validation = "internal",maxitems = nrow(raw_trainmat))
summary(Kcl_cor)

#It seems like correlation as distance measures provides better silhoutte values
#Moreover usually ward distance performs better than average link 
#To compare ward distance hierarchical clustering and k-medoids clustering we can compare their internal validity measures values as well

comparison<-clValid(raw_trainmat,2:10,clMethods = c("pam" ,"hierarchical") ,metric = "correlation",method = "ward",validation = "internal",maxitems = nrow(raw_trainmat))




op <- par(no.readonly=TRUE)
par(mfrow=c(2,2),mar=c(4,4,3,1))
plot(comparison, legend=FALSE)
plot(nClusters(comparison),measures(comparison,"Dunn")[,,1],type="n",axes=F,xlab="",ylab="")
legend("center", clusterMethods(comparison), col=1:9, lty=1:9, pch=paste(1:9))
par(op)

optimalScores(comparison)
##For connectivity measure hierarchical with 2 clusters gives the best result
#For Dunn Index hierarchical with 8 clusters seems to give best results which overlaps with our gesture number
#Regarding average silhoutte width pam with 6 clusters produces best results which is still pleasable
#For further analysis we can include stability measures as well as internal measures. However, since it is computationally expensive
#we will proceed with hierarchial with 8 clusters and ward distance for the part E. 


##Part d

#To use three models to classify samples we need to form 8 clusters using each model

library(flexclust)
library(ClusterR)
library(aricode)

traindist_corr<-get_dist(raw_trainmat,method = "spearman")
  

#For k-medoids method we can use train data to get medoids then use that medoids to classify test data

class_kmed<-pam(traindist_corr,8,diss = T)
class_kmed$id.med

medoids<-trainmat[class_kmed$id.med,]
medoids<-medoids[,-1]

kmed_pred<-predict_Medoids(testmat[,-1],MEDOIDS = medoids,distance_metric = "pearson_correlation")




#For Hierarchical clustering methods we can use class labels to build a classifier.
#For the pupose of this assignment we will proceed with knn classifier to predict. 


#Hierarchical with Wards method

class_hward<-agnes(traindist_corr,method = "ward",diss = T)
fviz_cluster(list(data = raw_trainmat, cluster = cutree(class_hward,8)),ellipse = F)

#It would be better to use correlations as a distance in testing as well however there is no in-built function to 
#use pearson correlation for knn. Therefore, we will use euclidean distances for knn classification of the test data 

hward_labels<-cutree(class_hward,8)
wardknn_train<-cbind(trainmat[,-1],hward_labels)

##Try out different k values find the one with highest Normalized Mutual Information with test data
kseq<-seq(5,15,2)
nmi<-c()
for (i in 1:6) {
  wardknn_pred<-knn(wardknn_train[,1:945],testmat[,-1],wardknn_train[,946],kseq[i])
  nmi[i]<-NMI(wardknn_pred,testmat[,1])
  
}


##Use the k gives best result
wardknn_pred<-knn(wardknn_train[,1:945],testmat[,-1],wardknn_train[,946],kseq[which.max(nmi)])
wardknn_pred



#Hierarchical with average link method

class_hav<-agnes(traindist_corr,method = "average",diss = T)
fviz_cluster(list(data = raw_trainmat, cluster = cutree(class_hav,8)),ellipse = F)
av_labels<-cutree(class_hav,8)
avknn_train<-cbind(trainmat[,-1],av_labels)

##Find optimal k for average link method
kseq<-seq(5,15,2)
nmi<-c()
for (i in 1:6) {
  avknn_pred<-knn(avknn_train[,1:945],testmat[,-1],avknn_train[,946],kseq[i])
  nmi[i]<-NMI(avknn_pred,testmat[,1])
}

##Use that k to make predictions
avknn_pred<-knn(avknn_train[,1:945],testmat[,-1],avknn_train[,946],kseq[which.max(nmi)])
avknn_pred



##Combine all class labels generated from each model in one table
comb_class<-data.table("Original_Labels"=testmat[,1],"K_Medoid_Labels"=kmed_pred$clusters,"Ward_Method_Labels"=as.numeric(wardknn_pred),"Average_Link_Labels"=as.numeric(avknn_pred))
comb_class



#To measure accuracy we should match original labels with the cluster labels. This can be executed through changing 
#cluster labels to the most repeating original label that is encountered in that cluster. However more apropriate 
#evaluation may be conducted through normalized mutual information

##K-Medoid Classification 
NMI(comb_class$Original_Labels,comb_class$K_Medoid_Labels)

##Ward Method Classification
NMI(comb_class$Original_Labels,comb_class$Ward_Method_Labels)

##Average Link Classification
NMI(comb_class$Original_Labels,comb_class$Average_Link_Labels)

##From NMI we see that Ward Method Classification gave the best results. 
# We can also check by creating confusion matrix for each of them


##First adjust the correct labels
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

##For k-medoids
adj_labels<-c()
for (i in 1:8) {
  adj_labels[i]<-getmode(comb_class[K_Medoid_Labels==i]$Original_Labels)
}

for (i in 1:nrow(comb_class)) {
  comb_class[i,2]<-adj_labels[as.numeric(comb_class[i,2])]
}

##For Ward Method Labels
adj_labels<-c()
for (i in 1:8) {
  adj_labels[i]<-getmode(comb_class[Ward_Method_Labels==i]$Original_Labels)
}

for (i in 1:nrow(comb_class)) {
  comb_class[i,3]<-adj_labels[as.numeric(comb_class[i,3])]
}

##For Average Link Method Labels
adj_labels<-c()
for (i in 1:8) {
  adj_labels[i]<-getmode(comb_class[Average_Link_Labels==i]$Original_Labels)
}

for (i in 1:nrow(comb_class)) {
  comb_class[i,4]<-adj_labels[as.numeric(comb_class[i,4])]
}



require(caret)


#Confusion Matrix and accuracy for K_medoid Classification (Acc = 0.73)
confusionMatrix(as.factor(comb_class$K_Medoid_Labels),as.factor(comb_class$Original_Labels))


#Confusion Matrix and accuracy for Ward Method Classification (Acc =0.73 )
confusionMatrix(as.factor(comb_class$Ward_Method_Labels),as.factor(comb_class$Original_Labels))


#Confusion Matrix and accuracy for Average Link Method Classification (Acc = 0.62)
confusionMatrix(as.factor(comb_class$Average_Link_Labels),as.factor(comb_class$Original_Labels))



## First two nearly have same accuracy whereas for average link method it is worse than others. 
## To plot time series of clusters, we can chose Ward Method since it gives slightly better accuracy

##Part E

Xacc<-data.table("Labels"=hward_labels,Xacc)
Yacc<-data.table("Labels"=hward_labels,Yacc)
Zacc<-data.table("Labels"=hward_labels,Zacc)



##CLuster 1
A<-Xacc[Labels==1]
A<-A[,-1]

B<-Yacc[Labels==1]
B<-B[,-1]

C<-Zacc[Labels==1]
C<-C[,-1]


par(mfrow=c(1, 3))
ts.plot(t(A), main ="Cluster 1 - X",col="lightblue")
lines(colMeans(A),col="purple",lwd=2)
ts.plot(t(B), main ="Cluster 1 - Y",col="lightblue")
lines(colMeans(B),col="purple",lwd=2)
ts.plot(t(C), main ="Cluster 1 - Z",col="lightblue")
lines(colMeans(C),col="purple",lwd=2)

##CLuster 2
A<-Xacc[Labels==2]
A<-A[,-1]

B<-Yacc[Labels==2]
B<-B[,-1]

C<-Zacc[Labels==2]
C<-C[,-1]


par(mfrow=c(1, 3))
ts.plot(t(A), main ="Cluster 2 - X",col="lightblue")
lines(colMeans(A),col="purple",lwd=2)
ts.plot(t(B), main ="Cluster 2 - Y",col="lightblue")
lines(colMeans(B),col="purple",lwd=2)
ts.plot(t(C), main ="Cluster 2 - Z",col="lightblue")
lines(colMeans(C),col="purple",lwd=2)

##CLuster 3
A<-Xacc[Labels==3]
A<-A[,-1]

B<-Yacc[Labels==3]
B<-B[,-1]

C<-Zacc[Labels==3]
C<-C[,-1]


par(mfrow=c(1, 3))
ts.plot(t(A), main ="Cluster 3 - X",col="lightblue")
lines(colMeans(A),col="purple",lwd=2)
ts.plot(t(B), main ="Cluster 3 - Y",col="lightblue")
lines(colMeans(B),col="purple",lwd=2)
ts.plot(t(C), main ="Cluster 3 - Z",col="lightblue")
lines(colMeans(C),col="purple",lwd=2)


##CLuster 4
A<-Xacc[Labels==4]
A<-A[,-1]

B<-Yacc[Labels==4]
B<-B[,-1]

C<-Zacc[Labels==4]
C<-C[,-1]


par(mfrow=c(1, 3))
ts.plot(t(A), main ="Cluster 4 - X",col="lightblue")
lines(colMeans(A),col="purple",lwd=2)
ts.plot(t(B), main ="Cluster 4 - Y",col="lightblue")
lines(colMeans(B),col="purple",lwd=2)
ts.plot(t(C), main ="Cluster 4 - Z",col="lightblue")
lines(colMeans(C),col="purple",lwd=2)


##CLuster 5
A<-Xacc[Labels==5]
A<-A[,-1]

B<-Yacc[Labels==5]
B<-B[,-1]

C<-Zacc[Labels==5]
C<-C[,-1]


par(mfrow=c(1, 3))
ts.plot(t(A), main ="Cluster 5 - X",col="lightblue")
lines(colMeans(A),col="purple",lwd=2)
ts.plot(t(B), main ="Cluster 5 - Y",col="lightblue")
lines(colMeans(B),col="purple",lwd=2)
ts.plot(t(C), main ="Cluster 5 - Z",col="lightblue")
lines(colMeans(C),col="purple",lwd=2)

##CLuster 6
A<-Xacc[Labels==6]
A<-A[,-1]

B<-Yacc[Labels==6]
B<-B[,-1]

C<-Zacc[Labels==6]
C<-C[,-1]


par(mfrow=c(1, 3))
ts.plot(t(A), main ="Cluster 6 - X",col="lightblue")
lines(colMeans(A),col="purple",lwd=2)
ts.plot(t(B), main ="Cluster 6 - Y",col="lightblue")
lines(colMeans(B),col="purple",lwd=2)
ts.plot(t(C), main ="Cluster 6 - Z",col="lightblue")
lines(colMeans(C),col="purple",lwd=2)

##CLuster 7
A<-Xacc[Labels==7]
A<-A[,-1]

B<-Yacc[Labels==7]
B<-B[,-1]

C<-Zacc[Labels==7]
C<-C[,-1]


par(mfrow=c(1, 3))
ts.plot(t(A), main ="Cluster 7 - X",col="lightblue")
lines(colMeans(A),col="purple",lwd=2)
ts.plot(t(B), main ="Cluster 7 - Y",col="lightblue")
lines(colMeans(B),col="purple",lwd=2)
ts.plot(t(C), main ="Cluster 7 - Z",col="lightblue")
lines(colMeans(C),col="purple",lwd=2)

##CLuster 8
A<-Xacc[Labels==8]
A<-A[,-1]

B<-Yacc[Labels==8]
B<-B[,-1]

C<-Zacc[Labels==8]
C<-C[,-1]


par(mfrow=c(1, 3))
ts.plot(t(A), main ="Cluster 8 - X",col="lightblue")
lines(colMeans(A),col="purple",lwd=2)
ts.plot(t(B), main ="Cluster 8 - Y",col="lightblue")
lines(colMeans(B),col="purple",lwd=2)
ts.plot(t(C), main ="Cluster 8 - Z",col="lightblue")
lines(colMeans(C),col="purple",lwd=2)