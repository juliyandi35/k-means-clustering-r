library(tidyverse)  
library(cluster)    # Algoritma klastering
library(factoextra) # Algoritma klastering dan visualisasi
library(readxl)

# Jabatan
dataclus <- read_excel("Dataset.xlsx")
dataclus <- data.frame(dataclus)
str(dataclus)
head(dataclus)

dataclus1 <- na.omit(dataclus) #untuk menghilangkan data missing

# Analisis Deskriptif dan Visualisasi Frekuensi
# Analisis Deskriptif
summary(dataclus1) 

# Menghapus Kolom Jabatan dan Departemen
dataclus1 <- dataclus1[,-c(1)]
head(dataclus1)

# KMO test
kmo <- function(x){
  x <- subset(x, complete.cases(x))       # menghilangkan data kosong (NA)
  r <- cor(x)                             # Membuat matrix korelasi
  r2 <- r^2                               # nilai koefisien untuk r squared
  i <- solve(r)                           # Inverse matrix dari matrix korelasi
  d <- diag(i)                            # element diagonal dari inverse matrix
  p2 <- (-i/sqrt(outer(d, d)))^2          # koefisien korelasi Parsial kuadrat
  diag(r2) <- diag(p2) <- 0               # menghapus element diagonal 
  KMO <- sum(r2)/(sum(r2)+sum(p2))
  MSA <- colSums(r2)/(colSums(r2)+colSums(p2))
  return(list(KMO=KMO, MSA=MSA))
}
kmo(dataclus1)

# Ubah data frame menjadi matriks
set.seed(123)
dataclus2 <- as.matrix(dataclus1)
rownames(dataclus2) <- dataclus$DEPARTEMEN

# Corelation test
library(car)
cor(dataclus2) # Variabel Skor.BBKual punya korelasi > 0.8, maka harus dihapus
writexl::write_xlsx(data.frame(cor(dataclus2)),"Uji korelasi awal.xlsx")
dataclus2 <- dataclus2[,-4]
cor(dataclus2) # Setelah dicheck kembali, semua variabel sudah punya korelasi < 0.8
writexl::write_xlsx(data.frame(cor(dataclus2)),"Uji korelasi akhir.xlsx")

# Analisis Cluster with K-Means Clustering
datafix <- scale(dataclus2) #standarisasi data
datafix
writexl::write_xlsx(data.frame(datafix),"Data setelah normalisasi.xlsx")

# Penentuan jarak
jarak <- get_dist(datafix)

# Penentuan jumlah cluster
Elbow <- fviz_nbclust(datafix, kmeans, method = "wss") # metode elbow
Elbow
writexl::write_xlsx(Elbow$data,"Elbow.xlsx")

Silhouette <- fviz_nbclust(datafix, kmeans, method = "silhouette") # metode silhouette
Silhouette
writexl::write_xlsx(Silhouette$data,"Silhouette.xlsx")

Clust_model <- kmeans(datafix, 2)
print(Clust_model)
Clust_model$centers
writexl::write_xlsx(data.frame(Clust_model$centers),"Cluster center.xlsx")

fviz_cluster(Clust_model, data = datafix)

dataclus1 %>%
  mutate(Cluster = Clust_model$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")

df.cluster <- data.frame(dataclus,Clust_model$cluster)
df.cluster
writexl::write_xlsx(df.cluster,"Data setelah cluster.xlsx")
