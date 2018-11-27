library('fossil')
library(cluster)
library(kknn)

thyro <- read.csv('new-thyroid.data', header = FALSE)

pca = princomp(thyro[,2:6])
biplot(pca)


Thyro_Clust <- kmeans(thyro[,2:6],3)
clusplot(thyro[,2:6], Thyro_Clust$cluster)
rand.index(thyro[,1],Thyro_Clust$cluster)


Thyro_scaled <- scale(thyro[,2:6])
Thyro_Clust2 <- kmeans(Thyro_scaled,3)
clusplot(thyro[,2:6], Thyro_Clust2$cluster)
rand.index(thyro[,1],Thyro_Clust2$cluster)


clusplot(thyro[,2:6],thyro[,1])


Spec_thy <- specClust(thyro[,2:6], centers = 3)
clusplot(thyro[,2:6], Spec_thy$cluster)
rand.index(thyro[,1],Spec_thy$cluster)


Spec_thy2 <- specClust(Thyro_scaled, centers = 3)
clusplot(thyro[,2:6], Spec_thy2$cluster)
rand.index(thyro[,1],Spec_thy2$cluster)
