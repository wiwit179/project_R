library(tidyverse)  
library(cluster)
library(factoextra)
library(ggplot2)

#import data csv
df <- read.table("C://Users//Wito_Randomo//Desktop//magang//clustering in R//baru//ZTE-A01-DAILY-TGL-01-30Sept.csv",header=TRUE,sep=",")
data <- df[,c(2,3,7,8,9,11,47,48,49)]

#scale kolom oasr, peak traffic, pocc
data1 <- scale(data[,7:9])
View(data1)

#gabungin kolom
data2 <- cbind(data,data1)
data2 <- data2[,c(1,2,3,4,5,6,10,11,12)]

str(data2)

data3 <- data2[,c(7,8,9)]


#Menemukan Cluster Optimal 
fviz_nbclust(data3, FUN = kmeans, method = "wss")# metode wss
fviz_nbclust(data3, FUN = kmeans, method = "silhouette")# metode silhouette

gap_stat <- clusGap(data3, FUN = kmeans, nstart = 25,
                    K.max = 20, B = 50) # metode gap statistic
fviz_gap_stat(gap_stat)

#model cluster kmeans
Clustering=kmeans(data3,centers=5,nstart=25)
Clustering

#ploting
fviz_cluster(Clustering, geom = "point", data = data3)+ggtitle("k=5")

#melihat data yang telah di cluster
final=data.frame(data3, Clustering$cluster)
View(final)

data_final <- cbind(data2,final)
data_final <- data_final[,c(1,2,3,4,5,6,10,11,12,13)]

#eksport data csv
write.csv(data_final,"final_kluster.csv")

#profilisasi tiap kelompok yang terbentuk
data3 %>%
  mutate(Cluster = Clustering$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")

#ekplorasi data

library(dplyr)
library(ggplot2)

kluster_1 = filter(data_final, Clustering.cluster == 1)
kluster_2 = filter(data_final, Clustering.cluster == 2)
kluster_3 = filter(data_final, Clustering.cluster == 3)
kluster_4 = filter(data_final, Clustering.cluster == 4)
kluster_5 = filter(data_final, Clustering.cluster == 5)

write.csv(kluster_5,"Kluster 5.csv")

table(kluster_1$sub_opr)

ggplot(kluster_5, aes(x = sub_opr)) + stat_count(width = 0.6, aes(fill = To.Xch)) + 
  labs(title = "Berdasarkan To.Xch dan sub operator") 



