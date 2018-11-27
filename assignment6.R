
library("vegan")
RealData <- df <- read.csv(file="C:/Users/Mustafa Murat Coþkun/Desktop/Veri Analizi/DataSet Competition/DataSet-2018-final.csv", header=FALSE, sep=";")


RealData = data.matrix(RealData)
#RealData = lapply(RealData, as.numeric)
Dist_RealData=dist(RealData) # use dist instead of as.dist since my data is not dist object, but normal data frame

summary(RealData)

# NOTE: DID NOT CONVERGE IN 10 ITERATIONS, THEREFORE MAX ITER NO IS INCREASED
kmeans_RealData_3=kmeans(RealData,centers = 3,algorithm="Lloyd",iter.max = 50) # Lloyd = Forgy
kmeans_RealData_4=kmeans(RealData,centers = 4,algorithm="Lloyd",iter.max = 50) # Lloyd = Forgy
kmeans_RealData_5=kmeans(RealData,centers = 5,algorithm="Lloyd",iter.max = 50) # Lloyd = Forgy
kmeans_resultingClusters_3=kmeans_RealData_3$cluster
kmeans_resultingClusters_4=kmeans_RealData_4$cluster
kmeans_resultingClusters_5=kmeans_RealData_5$cluster
plot(kmeans_resultingClusters_3,col=kmeans_resultingClusters_3)
#prcomp_well=prcomp(MyData$V1,retx=TRUE)
#plot(x = prcomp_well$x[,1], y = prcomp_well$x[,2],col=kmeans_resultingClusters)
#my_hclust_resultingClusters=cutree(hclustAverage_WellSeparatedCluster,k=5)

library("dbscan")
dbScan_RealData=dbscan(Dist_RealData,eps=10)
resulting_dbscanClusters=dbScan_RealData$cluster
plot(resulting_dbscanClusters,col=resulting_dbscanClusters)

# BELOW MCLUST TAKES SOOOOO MUCH TIME EVEN WITH MY PROCESSOR
# Comment out below rows, but it tries to fit everytime calling the function, the plots that are made looks beautiful
library(mclust)
mclust_RealData <- Mclust(RealData)
plot(mclust_RealData) # plot results 
summary(mclust_RealData)

################################################ VALIDATION FOR K-MEANS ######################################

# VALIDATION BY SILHOUETTE INDEX
library("cluster") # for silhouette function
sil_RealData_3=silhouette(kmeans_resultingClusters_3,Dist_RealData )
sil_RealData_4=silhouette(kmeans_resultingClusters_4,Dist_RealData )
sil_RealData_5=silhouette(kmeans_resultingClusters_5,Dist_RealData )
plot(x=c(sil_RealData_3,sil_RealData_4,sil_RealData_5),y=c(3,4,5),main = "Silhouette Index Well Separated",xlab="Silhoutte Values",ylab="Initial #of Centers")

sil_dbScan=silhouette(resulting_dbscanClusters,Dist_RealData )
sil_dbScan

sil_mclust=silhouette(mclust_RealData$classification,Dist_RealData )
sil_mclust
# VALIDATION BY DUNN and DAVIES-BOULDIN INDEX

library("clv")


intraclust = c("average")
interclust = c("average")

# compute Davies-Bouldin indicies (also Dunn indicies)

# compute intercluster distances and intracluster diameters
cls_scatt_k3 <- cls.scatt.data(RealData, kmeans_resultingClusters_3, dist="manhattan")
cls_scatt_k4 <- cls.scatt.data(RealData, kmeans_resultingClusters_4, dist="manhattan")
cls_scatt_k5 <- cls.scatt.data(RealData, kmeans_resultingClusters_5, dist="manhattan")

#DUNN INDEX
#for k=3

# once computed valuse use in both functions
dunn3 <- clv.Dunn(cls_scatt_k3, intraclust, interclust)
#for k=4
dunn4 <- clv.Dunn(cls_scatt_k4, intraclust, interclust)
#for k=5
dunn5 <- clv.Dunn(cls_scatt_k5, intraclust, interclust)

plot(y=c(dunn3,dunn4,dunn5),x=c(3,4,5), ylab="Dunn Index(Average)" , xlab="k" )
# DAVIES-BOULDIN INDEX
#for k=3
davies3 <- clv.Davies.Bouldin(cls_scatt_k3, intraclust, interclust)
#for k=4
davies4 <- clv.Davies.Bouldin(cls_scatt_k4, intraclust, interclust)
#for k=5
davies5 <- clv.Davies.Bouldin(cls_scatt_k5, intraclust, interclust)

plot(y=c(davies3,davies4,davies5),x=c(3,4,5), ylab="Davies-Bouldin Index(Average)" , xlab="k" )



################################################ VALIDATION FOR DBSCAN ######################################

cls_scatt_dbscan <- cls.scatt.data(RealData, resulting_dbscanClusters, dist="manhattan")
dunn_dbscan <- clv.Dunn(cls_scatt_dbscan, intraclust, interclust)
dunn_dbscan
davies_dbscan <- clv.Davies.Bouldin(cls_scatt_dbscan, intraclust, interclust)
davies_dbscan


################################################ VALIDATION FOR MCLUST ######################################

cls_scatt_ <- cls.scatt.data(RealData, mclust_RealData$classification, dist="manhattan")
dunn_dbscan <- clv.Dunn(cls_scatt_dbscan, intraclust, interclust)
dunn_dbscan
davies_dbscan <- clv.Davies.Bouldin(cls_scatt_dbscan, intraclust, interclust)
davies_dbscan
