library(ggplot2)
library(tidyverse)
library(ggthemes)
library(ggcorrplot)
library("ggpubr")
library("cluster")
library("FactoMineR")
library("factoextra")  
library(reshape)


data <- read_csv("C:/Users/miles/GitHub/ark-analysis-implementation/data_processing/segmentation/cell_table/cell_table_size_normalized.csv")
markers_columns <- c("ANAX1","C-Caspase-3","CD31","CK5","CK8","E-Cadherin","Ki67","Pericentrin","SMA")

subset_dataset <- subset_dataset[,markers_columns]
scaled_data = as.data.frame(scale(subset_dataset, center = T, scale = F))

var(subset_dataset)



ggplot(data = sapply(subset_dataset, var), aes())

corr_matrix <- cor(scaled_data)
ggcorrplot(corr_matrix)


ggplot(data = data, mapping = aes(x = Pericentrin, y = ANAX1))+
  geom_point(aes(color = Pericentrin, size = cell_size))+
  theme_pubr()

ggplot(data = data, mapping = aes(x = Ki67, y = ANAX1))+
  geom_point(aes(color = label, size = cell_size))+
  theme_pubr()+
  labs(title = "ANAX1 and Ki67 Intensity", x = "Ki67 Intensity", y = "ANAX1 Intensity")


melted_data <- melt(data[,markers_columns])

ggplot(, aes(x = label, y = , fill = subset_dataset))
data[,markers_columns]





  
data.pca <- princomp(corr_matrix)
summary(data.pca)

data.pca$loadings[, 1:2]




fviz_eig(pca_data, addlabels = TRUE)

fviz_pca_var(pca_data, col.var = "black")

fviz_cos2(pca_data, choice = "var", axes = 1:2)

fviz_pca_ind(pca_data,invisible = "quali", repel = F) +
  theme_pubr(base_size = 9)


pca_data = prcomp(subset_dataset, center = TRUE, scale = TRUE)
summary(pca_iris)

fviz_eig(pca_iris, addlabels = TRUE)

data_transform = as.data.frame(-pca_data$x[,1:2])

fviz_nbclust(data_transform, kmeans, method = 'wss')

fviz_nbclust(data_transform, kmeans, method = 'silhouette')
fviz_nbclust(data_transform, kmeans, method = 'gap_stat')

k = 2
kmeans_iris = kmeans(data_transform, centers = k, nstart = 50)
fviz_cluster(kmeans_iris, data = data_transform, labelsize = 0)+
  theme_pubr()







#--------------------------------------------------#
som_clustering_channel_average <- 