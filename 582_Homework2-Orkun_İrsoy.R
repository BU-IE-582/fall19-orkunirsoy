setwd("C:/Users/orkun/Desktop/IE 582 - HW 2")

require(MASS)
library(factoextra) 
library(data.table)

#Include necessary libraries
par(mfrow=c(1,1))

#Import the musk data and apply PCA
musk_data<-fread("Musk1.csv")
pca_musk<-princomp(musk_data[,c(3:168)])

#Summarize the PCA
summary(pca_musk)
fviz_eig(pca_musk)

#First three component covers 60% of the total variance. If we were to explain up to 90% we would use first 19 components to describe the data

#We can plot the corresponding groups with respect to first two dimensions. 1 represents the musk group whereas 0 is non-musk
p<-cbind(pca_musk$scores[,c(1:2)],musk_data[,1])
fviz_pca_ind(pca_musk, label="none", habillage=musk_data$V1)


#To use MDS we need the distance matrix. 
a<-dist(musk_data[,c(3:168)])

#Then using that matrix in cmdscale function we can get reduced graph
mds=cmdscale(a)
plot(mds[,1],mds[,2],col=musk_data$V1+1)

#To compare two graphs we can put them together 
par(mfrow=c(1,2))
plot(pca_musk$scores[,1],pca_musk$scores[,2],col=1+musk_data$V1,xlab = "PCA 1",ylab = "PCA 2", main = "Principal Component Analysis")
plot(mds[,1],mds[,2],col=musk_data$V1+1,xlab = "MDS 1",ylab = "MDS 2", main = "Multi Dimensional Scaling")

#The graphs are nearly symmetric, which indicates two methods actually works parallel.
#Reason behind the symmetry is the opposite signs of mds[,2] and pca_musk$scores[,2]
mds[,2]
pca_musk$scores[,2]


#We can take the mean using aggregate function
mean_musk<-aggregate.data.frame(musk_data,by=list(musk_data$V2),FUN = mean)
mean_musk_pca<-prcomp(mean_musk[4:169])

#Using the means of the observations in a bag, we can obtain the following graph
fviz_pca_ind(mean_musk_pca, label="none", habillage=mean_musk$V1)


#Following the same logic we can obtain mds from that result as well 
mean_mds=cmdscale(dist(mean_musk[,c(4:169)]))
par(mfrow=c(1,1))
plot(mean_mds[,1],mean_mds[,2],col=mean_musk$V1+1)

#Again we get similar results with MDS and PCA, It seems logical to use bag means since formed groups seems same for both.
#Instead of means we might use other parameters such as median, xth percentile etc since these will yield similar results.

##Task 2
library(tidyr)
library(imager)

#Using paint I got an image 256*256

#reading image
img<-load.image("Example.jpg")

#displaying image
par(mfrow=c(1,1))
plot(img)


#Define nimg as an image and then make the necessary adjustments for each pixel value to get the noisy image

nimg<-img

for (i in 1:256) {
  for (j in 1:256) {
    for (k in 1:3) {
      nimg[i,j,1,k]<-img[i,j,1,k] + runif(1,min = min(img[,,,k]),max = 0.1*(max(img[,,,k])))
    }
  }
}

par(mfrow=c(1,2))
plot(img,main = "Original Image")
plot(nimg, main = "Noised Image")

#I defined a function to rotate elements of a matrix 90 degrees clockwise 
rotate <- function(x) t(apply(x, 2, rev))


#I separated picture into rgb channels and rotated twice to use them with image function(it displays reverse) 
rimg<-rotate(rotate(nimg[1:256,1:256,1,1]))
gimg<-rotate(rotate(nimg[1:256,1:256,1,2]))
bimg<-rotate(rotate(nimg[1:256,1:256,1,3]))


#Display different channels with the corresponding colors
par(mfrow=c(1,3))
image(rimg, col=rgb(c(0:255)/255, 0,0), main = "Red")
image(gimg, col=rgb(0,c(0:255)/255,0), main= "Green")
image(bimg, col=rgb(0, 0,c(0:255)/255), main = "Blue")

#Transform image into grayscale
par(mfrow=c(1,1))
grayimg<-grayscale(nimg)
plot(grayimg, main = "Grayscale Image")

#To extract patches I used extract_patches function. Since function requires coordinates of the centers as a two vector,
#I created coordinates of the center pixels using expand.grid function. I chosed patch width as 5

patchvectors<-expand.grid(c(13:244),c(13:244))
patch_list<-extract_patches(grayimg,patchvectors$Var1,patchvectors$Var2,25,25)

#Then I converted patch lists to a dataframe. I updated to dataframe to get the appropriate layout. 

patch_df1<-as.data.frame(patch_list)
patch_df1<-cbind( patch_df1 ,c(1:625))
patch_df2<-patch_df1[,c(1,5,4)]
setnames(patch_df2,"c(1:625)","pixel ID")
patch_df2$im<-as.numeric(patch_df2$im)
patch_df3<-spread(patch_df2,"pixel ID","value")



#After converting the patch dataframe I used princomp function to analyze principal components

pca_img<-princomp(patch_df3[,2:626])
summary(pca_img)
fviz_eig(pca_img)

#We see that for the patch width 25, first componant describes the huge portion of the variance (nearly 50%).
#Together with the second and third principal components we would be able to explain about 75% of the total variance 
#which mean we can construct this image using the first three companents without losing too much information. 




#To plot images using the first three components, first I get the scores as a matrix for the corresponding component.
#Then I normalized them between [0,1] to use it on the grayscale. Finally I rotated them and plotted using rasterImage function.

#Initialize the graph layout
par(mfrow=c(1,3))


#First principal component
firstcomp<-matrix(pca_img$scores[,1],232,232)
firstcomp<-(firstcomp-min(firstcomp))/(max(firstcomp)-min(firstcomp))
com1_img<-rotate(firstcomp)
plot(c(0,1),c(0,1),t='n',xlab = "",ylab = "", main = "Principal Component 1")
rasterImage(com1_img,0,0,1,1)

#Second principal component
secondcomp<-matrix(pca_img$scores[,2],232,232)
secondcomp<-(secondcomp-min(secondcomp))/(max(secondcomp)-min(secondcomp))
com2_img<-rotate(secondcomp)
plot(c(0,1),c(0,1),t='n',xlab = "",ylab = "", main = "Principal Component 2")
rasterImage(com2_img,0,0,1,1)

#Third principal component
thirdcomp<-matrix(pca_img$scores[,3],232,232)
thirdcomp<-(thirdcomp-min(thirdcomp))/(max(thirdcomp)-min(thirdcomp))
com3_img<-rotate(thirdcomp)
plot(c(0,1),c(0,1),t='n',xlab = "",ylab = "", main = "Principal Component 3")
rasterImage(com3_img,0,0,1,1)


#Part C with patch width 5
par(mfrow=c(1,3))
eigv1<-matrix(pca_img$loadings[,1],nrow = 25)
eigv1=imager::renorm(eigv1, min = 0 , max = 1)
plot(c(0,1),c(0,1),t='n',xlab = "",ylab = "", main = "PC 1 Eigenvector")
rasterImage(rotate(eigv1),  0, 0, 1, 1, angle = 0, interpolate = F)

eigv2<-matrix(pca_img$loadings[,2],nrow = 25)
eigv2=imager::renorm(eigv2, min = 0 , max = 1)
plot(c(0,1),c(0,1),t='n',xlab = "",ylab = "", main = "PC 2 Eigenvector")
rasterImage(rotate(eigv2),  0, 0, 1, 1, angle = 0, interpolate = F)

eigv3<-matrix(pca_img$loadings[,3],nrow = 25)
eigv3=imager::renorm(eigv3, min = 0 , max = 1)
plot(c(0,1),c(0,1),t='n',xlab = "",ylab = "", main = "PC 3 Eigenvector")
rasterImage(rotate(eigv3),  0, 0, 1, 1, angle = 0, interpolate = F)

#When we compare PC 2 and 3 and their eigenvectors we can see that component 2 represents vertical lines better 
#whereas component 3 represents horisontal lines better. It is consistent with their eigenvectors.
