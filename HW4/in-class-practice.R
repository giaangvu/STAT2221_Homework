setwd("/Users/giangvu/Desktop/STAT 2221 - Advanced Applied Multivariate Analysis/Class Notes/Clustering Example")
library(stats)
library(mclust)
library(data.table)

data1 <- read.table("data1.txt", header = T)
dat1 <- fread("data1.txt")
data2 <- read.table("data2.txt", header = T)
data3 <- read.table("data3.txt", header = T)

#scatterplot

#kmeans -----
dat1.k <- kmeans(data1, centers = 3)

library(ggplot2)
library(dplyr)
library(scatterplot3d)
library(plot3D)
library(rgl)

#plot -----
#3d plot
plot3d(data1, col = dat1.k$cluster)

#gg plot plot the k mean clustering result for first 2 variables
dat1_new <- cbind(dat1, dat1.k$cluster)
colnames(dat1_new)[1] <- "obs"
colnames(dat1_new)[5] <- "cluster"
ggplot(dat1_new, aes(dat1_new$V1, dat1_new$V2, group = dat1_new$cluster, col = dat1_new$cluster)) + 
  geom_point()
plot(dat1$V1, dat1$V2,)

#base plot
plot(data1, col=dat1.k$cluster)


#mclust -----
dat1.MC <- Mclust(data1, G=3) #add verbose = FALSE to get rid of status bar
dat1.MC <- Mclust(data1) #without specifying, we get result of 9 components -> not really close to the truth
dat1.MC <- Mclust(data1[,1:2], verbose = F)
summary(dat1.MC, parameters = T)

plot3d(data1, col = dat1.MC$classification)


#compare ----- 
adjustedRandIndex(dat1.k$cluster, dat1.MC$classification) 
#ARI from 0.7 and above are considered large -> the two clusterings are similar

#dissimilarity matrix ----
D.data1 <- dist(data1)
dat1.HC <- hclust(D.data1)
plot(dat1.HC)
