

if(!require("corrplot")){install.packages("corrplot", repos = "https://cran.rstudio.com/")}
library(corrplot)

if(!require("pastecs")){install.packages("pastecs", repos = "https://cran.rstudio.com/")}
library(pastecs)  #korelasyon grafikleri icin

if(!require("factoextra")){install.packages("factoextra", repos = "https://cran.rstudio.com/")}
library(factoextra)

if(!require("ggrepel")){install.packages("ggrepel", repos = "https://cran.rstudio.com/")}
library(ggrepel)

if(!require("ggplot2")){install.packages("ggplot2", repos = "https://cran.rstudio.com/")}
library(ggplot2)

if(!require("cowplot")){install.packages("cowplot", repos = "https://cran.rstudio.com/")}
library(cowplot)

if(!require("cluster")){install.packages("cluster", repos = "https://cran.rstudio.com/")}
library(cluster)   # PAM  & clustering

if(!require("fpc")){install.packages("fpc", repos = "https://cran.rstudio.com/")}
library(fpc)  

if(!require("clustertend")){install.packages("clustertend", repos = "https://cran.rstudio.com/")}
library(clustertend)

if(!require("NbClust")){install.packages("NbClust", repos = "https://cran.rstudio.com/")}
library(NbClust)


if(!require("mclust")){install.packages("mclust", repos = "https://cran.rstudio.com/")}
library(mclust)

rawdata <- read.csv("C:/Users/LENOVO/Desktop/data_clean.csv") #orijinal verileri rawdata adli nesneye kaydettik
#dim(rawdata)
str(rawdata)
summary(rawdata)
View(rawdata)

# Eksik degerleri kontrol et
which(is.na(rawdata))


# Sutun adlarini basitlestirdik
colnames(rawdata) <- c( "Country","Score", 
                       "Safe_Secu", "Pers_Free", 
                       "Governance", "Social_Capital", 
                       "Invest_Envi", "Enterp_Condit", 
                       "Infr_Market_Access", "Econ_Quality","Living_Condit","Health",
                       "Education","Natur_Envi"
                      )

# ulke adlarini satir adi yaptik
rownames(rawdata) <- rawdata$Country
str(rawdata)

# Country sutununu kaldir 
rawdata$Country <- NULL

head(rawdata)
str(rawdata)


dev.off()

corr <- cor(rawdata, method = "pearson",use ="pairwise.complete.obs")
# Korelasyon 
corrplot(corr, 
         method = "color", 
         type = "upper", 
         tl.col = "black", 
         tl.cex = 0.6, 
         addCoef.col = "black", 
         number.cex = 0.6,
         col = colorRampPalette(c("darkblue", "white", "darkred"))(100),
         main = "Degiskenler Arasi Korelasyon Matrisi", 
         mar = c(0,0,1,0))


#degiskenleri sec:
clean_data <- rawdata[, c( "Safe_Secu", "Pers_Free", 
                           "Governance", "Social_Capital", 
                           "Invest_Envi", "Enterp_Condit", 
                           "Infr_Market_Access", "Econ_Quality","Living_Condit","Health",
                           "Education","Natur_Envi")]


head(clean_data,10)
summary(clean_data)

# ozet istatistikler

# Her degisken icin ayri hesaplama
ozet <- matrix(nrow = 5, ncol = ncol(clean_data))
colnames(ozet) <- names(clean_data)
rownames(ozet) <- c("Standart sapma","Varyans", "Degisim katsayisi", 
                    "Degisim araligi - Alt sinir", "Degisim araligi - ust sinir")

for (i in 1:ncol(clean_data)) {
  col <- clean_data[,i]
  ozet[1,i] <- round(sd(col), 1)  # Standart sapma
  ozet[2,i] <- round(var(col), 1) # Varyans
  ozet[3,i] <- round(sd(col)/mean(col)*100, 1) # Degisim katsayisi
  ozet[4,i] <- min(col) # Alt sinir
  ozet[5,i] <- max(col) # ust sinir
}

# Veri cercevesine donusturduk
ozet <- as.data.frame(ozet)
ozet



par(mar=c(4, 12, 2, 2))  
boxplot(scale(clean_data), 
        col="lightgreen", 
        cex.axis = 1,      
        horizontal=T, 
        las=1,             
        )



corr <- cor(clean_data, method = "pearson")
corr_s <- round(corr, digits = 3)
colnames(corr_s) <- c( 
  "Safe_Secu", "Pers_Free", 
  "Governance", "Social_Capital", 
  "Invest_Envi", "Enterp_Condit", 
  "Infr_Market_Access", "Econ_Quality","Living_Condit","Health",
  "Education","Natur_Envi")
rownames(corr_s) <- c( 
  "Safe_Secu", "Pers_Free", 
  "Governance", "Social_Capital", 
  "Invest_Envi", "Enterp_Condit", 
  "Infr_Market_Access", "Econ_Quality","Living_Condit","Health",
  "Education","Natur_Envi")
corr_s




panel.hist <- function(x, ...)
{
  usr <- par("usr")
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}
pairs(clean_data, panel = panel.smooth,
      cex = 0.8, pch = 24, bg = "light blue", horOdd=TRUE,
      diag.panel = panel.hist, cex.labels = 1, font.labels = 2)



corrplot.mixed(corr, lower="pie", upper="number")



#------------------------------------PCA-----------------------

data.cor <- cor(clean_data)
data.eigen <- eigen(data.cor)
data.eigen
eigenvalues <- data.eigen$values
#eigenvalue


prop.var <- data.eigen$values/sum(eigenvalues)
cum.prop.var <- cumsum(prop.var)
round(rbind(eigenvalues, prop.var, cum.prop.var),digits = 3) 


plot(prop.var, xlab="Principal Component", ylab="Proportion of Variance Explained", ylim=c(0,1), type="b")
lines(cum.prop.var, type="b", col="red") 


data.pca <- prcomp(clean_data, center = TRUE , scale = TRUE) #korelasyon matrisi uzerinden
summary(data.pca)


#scree plot 
fviz_eig(data.pca, addlabels = TRUE)


data.pca$scale   ##std of variables_original data


data.frame(lapply(clean_data,var)) #variance of variables_original data

data.pca$rotation  ##eigenvectors



loadings <- as.data.frame(data.pca$rotation)
loadings$varname <- rownames(loadings)

 


pca_scores <- as.data.frame(data.pca$x)

country_pca <- data.frame(
  Country = rownames(pca_scores),
  PC1 = pca_scores$PC1,
  PC2 = pca_scores$PC2
)

country_pca <- country_pca[order(country_pca$Country), ]  
head(country_pca, 10)  



# Degisken vektorleri
loadings <- as.data.frame(data.pca$rotation)
loadings$varname <- rownames(loadings)



#ggplot ile daha guzel cizim
library(ggplot2)
library(ggrepel)  # Otomatik etiket yerlestirme icin

# PCA skorlar??n?? ve loadings'leri alal??m
pca_scores <- as.data.frame(data.pca$x[,1:2])
pca_loadings <- as.data.frame(data.pca$rotation[,1:2])

# Degisken isimleri ile birlikte loadings
loadings_df <- pca_loadings
loadings_df$variable <- rownames(pca_loadings)

#ulke isimleri ile birlikte scores
scores_df <- pca_scores
scores_df$country <- rownames(pca_scores)

# ggplot ile biplot
ggplot() +
  # sehir noktalari
  geom_point(data = scores_df, aes(x = PC1, y = PC2), color = "#3366CC", size = 2) +
  # sehir etiketleri (cakismayi onleyen)
  geom_text_repel(data = scores_df, aes(x = PC1, y = PC2, label = country), 
                  color = "#3366CC", size = 3, max.overlaps = 15) +
  # Degisken oklari
  geom_segment(data = loadings_df, 
               aes(x = 0, y = 0, xend = PC1*5, yend = PC2*5), 
               arrow = arrow(length = unit(0.3, "cm")), color = "#E41A1C") +
  # Degisken etiketleri (cakismayi onleyen)
  geom_text_repel(data = loadings_df, 
                  aes(x = PC1*5.5, y = PC2*5.5, label = variable), 
                  color = "#E41A1C", size = 4) +
  # Eksen cizgileri ve basliklar
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  labs(title = "Temel Bilesen Analizi",
       x = paste0("PC1 (", round(100*summary(data.pca)$importance[2,1]), "%)"),
       y = paste0("PC2 (", round(100*summary(data.pca)$importance[2,2]), "%)")) +
  theme_bw() +
  theme(panel.grid = element_line(color = "gray90", linetype = 3),
        plot.title = element_text(hjust = 0.5, size = 16))






pca_scores <- as.data.frame(data.pca$x[,1:2])
pca_loadings <- as.data.frame(data.pca$rotation[,1:2])

# Degisken isimleri ile birlikte loadings
loadings_df <- pca_loadings
loadings_df$variable <- rownames(pca_loadings)

#ulke isimleri ile birlikte scores
scores_df <- pca_scores
scores_df$country <- rownames(pca_scores)


print(scores_df$country)



p <- ggplot() +
  geom_point(data = scores_df, aes(x = PC1, y = PC2), color = "#3366CC", size = 2) +
  geom_text_repel(data = scores_df, aes(x = PC1, y = PC2, label = country), color = "#3366CC", size = 3.5, max.overlaps = 15) +
  geom_segment(data = loadings_df, aes(x = 0, y = 0, xend = PC1 * 4, yend = PC2 * 4), arrow = arrow(length = unit(0.3, "cm")), color = "#E41A1C") +
  geom_text_repel(data = loadings_df, aes(x = PC1 * 4.4, y = PC2 * 4.4, label = variable), color = "#E41A1C", size = 4) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  labs(title = "Temel Bilesen Analizi Biplot",
       x = sprintf("PC1 (%.1f%% Varyans)", 100 * summary(data.pca)$importance[2, 1]),
       y = sprintf("PC2 (%.1f%% Varyans)", 100 * summary(data.pca)$importance[2, 2])) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))

p

#---------------------------------------

# Eigenvalues
eig.val <- get_eigenvalue(data.pca)


data_new <- as.data.frame(data.pca$x[, 1:2])

head(data_new)
View(data_new)


res.var <- get_pca_var(data.pca)


corrplot(res.var$cos2) # Quality of representation

res.var <- get_pca_var(data.pca)



# Contributions of variables to PC1
pc1 <- fviz_contrib(data.pca, choice = "var", axes = 1)
# Contributions of variables to PC2
pc2 <- fviz_contrib(data.pca, choice = "var", axes = 2)

plot_grid(pc1,pc2)


#Degiskenler ve bilesenlerin birbirleriyle iliskisi
fviz_pca_var(data.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
             
             
)



##Graph of individuals. Individuals with a similar profile are grouped together
fviz_pca_ind(data.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)


pc1_top5 <- head(country_pca[order(country_pca$PC1, decreasing = TRUE), ], 5)
pc1_bottom5 <- head(country_pca[order(country_pca$PC1), ], 5)

pc2_top5 <- head(country_pca[order(country_pca$PC2, decreasing = TRUE), ], 5)
pc2_bottom5 <- head(country_pca[order(country_pca$PC2), ], 5)


turkey_values <- country_pca[country_pca$Country == "Turkey", ]


# uzaklik / benzerlik olcumleri

#normalize edilmemis
summary(clean_data)
boxplot(clean_data, main="Normalize Edilmemis Veriler", horizontal=TRUE, cex.axis = 0.45)# Normalize edilmemi?? De??i??kenlerin varyanslar??
cat("Degiskenlerin varyanslari:\n")
apply(clean_data, 2, var)


#ozet istatistikler
summary(clean_data)

# Boxplot cizimi
boxplot(clean_data, 
        main="Normalize Edilmemis Veriler", 
        horizontal=TRUE, 
        cex.axis = 0.45)

# Varyans hesaplama 
cat("\nDegiskenlerin varyanslari:\n")
varyanslar <- numeric(ncol(clean_data))
names(varyanslar) <- names(clean_data)

for(i in 1:ncol(clean_data)) {
  varyanslar[i] <- var(clean_data[,i])
}


print(round(varyanslar, 2))


#Scale ile Normalize edilmis
data_scaled <- scale(clean_data)
summary(data_scaled)

# Boxplot cizimi
boxplot(data_scaled, 
        main="Normalize Edilmis Veriler", 
        horizontal=TRUE, 
        cex.axis = 0.45)

# Varyans hesaplama - her degisken icin 
cat("\nDegiskenlerin varyanslari:\n")
varyanslar <- numeric(ncol(data_scaled))
names(varyanslar) <- names(data_scaled)

for(i in 1:ncol(data_scaled)) {
  varyanslar[i] <- var(data_scaled[,i])
}

# Varyanslar
print(round(varyanslar, 2))


#Min-Max ile Normalize Edilmis Veriler
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

data_minmax <- as.data.frame(lapply(clean_data, min_max_norm))
summary(data_minmax)
boxplot(data_minmax, main="Min-Max ile Normalize Edilmis Veriler", horizontal=TRUE, cex.axis = 0.6)

cat("\nDegiskenlerin varyanslari:\n")
varyanslar <- numeric(ncol(data_minmax))
names(varyanslar) <- names(data_minmax)

for(i in 1:ncol(data_minmax)) {
  varyanslar[i] <- var(data_minmax[,i])
}

# Varyanslar
print(round(varyanslar, 2))


#tum durumlar
par(mfrow=c(1,3))
boxplot(clean_data, main="Orijinal Veri", horizontal=TRUE)
boxplot(data_scaled, main="Scale Edilmis Veri", horizontal=TRUE)
boxplot(data_minmax, main="Min-Max Normalize Veri", horizontal=TRUE)


## Uzaklik olcusunun Hesaplanmasi

#Scale edilmis veri icin oklid uzaklik (ilk 6x6)
dist_eucl <- dist(data_scaled, method="euclidean")
dist_euc <- round(as.matrix(dist_eucl)[1:6, 1:6],1)


#Scale manhattan  (ilk 6x6)
dist_man <- dist(data_scaled, method="manhattan")
dist_mat <- round(as.matrix(dist_man)[1:6, 1:6],1)


#Pearson korelasyon tabanli (ilk 6x6)
dist_cor2 <- get_dist(data_scaled, method="pearson")
dist_cor <- round(as.matrix(dist_cor2)[1:6, 1:6],1)

#Spearman korelasyonu tabanli (ilk 6x6)
dist_spe <- get_dist(data_scaled, method="spearman")
dist_sp <- round(as.matrix(dist_spe)[1:6, 1:6],1)



#ham oklid
dist_eucl_raw <- dist(clean_data, method="euclidean")
fviz_dist(dist_eucl_raw, order = TRUE, gradient = list(low = "red", mid = "white", high = "blue"), 
          show_labels = FALSE)


#scale oklid
??
fviz_dist(dist_eucl, order = TRUE, gradient = list(low = "red", mid = "white", high = "blue"), 
          show_labels = FALSE)


fviz_dist(dist_man, order = TRUE, gradient = list(low = "red", mid = "white", high = "blue"), 
          show_labels = FALSE)



fviz_dist(dist_cor2, order = TRUE, gradient = list(low = "red", mid = "white", high = "blue"), 
          show_labels = FALSE)



fviz_dist(dist_spe, order = TRUE, gradient = list(low = "red", mid = "white", high = "blue"), 
          show_labels = FALSE)




#ek min max veri ile ##########################

#Tum verilerin heatmap uzerindeki gorunusu minmax
dist_minmax_eucl <- dist(data_minmax, method="euclidean")

fviz_dist(dist_minmax_eucl, order = TRUE, gradient = list(low = "red", mid = "white", high = "blue"), 
          show_labels = FALSE)


#ek pcali veri ile
data.pca <- prcomp(clean_data, center = TRUE, scale. = TRUE)
data_pca <- predict(data.pca)[,1:2]
dist_eucl_pca <- dist(data_pca, method="euclidean")
#Tum verilerin heatmap uzerindeki gorunusu pca
fviz_dist(dist_eucl_pca, order = TRUE, gradient = list(low = "red", mid = "white", high = "blue"), 
          show_labels = FALSE)


## Uzaklik olcusunun Hesaplanmasi

#Scale edilmis veri icin oklid uzaklik (ilk 6x6)
dist_eucl_pca <- dist(data, method="euclidean")
dist_euc_pca <- round(as.matrix(dist_eucl_pca)[1:6, 1:6],1)


#Scale edilmis veri icin manhattan  (ilk 6x6)
dist_man_pca <- dist(data, method="manhattan")
dist_mat <- round(as.matrix(dist_man)[1:6, 1:6],1)


#Pearson korelasyon tabanli (ilk 6x6)
dist_cor2 <- get_dist(data_scaled, method="pearson")
dist_cor <- round(as.matrix(dist_cor2)[1:6, 1:6],1)

#Spearman korelasyonu tabanli (ilk 6x6)
dist_spe <- get_dist(data_scaled, method="spearman")
dist_sp <- round(as.matrix(dist_spe)[1:6, 1:6],1)


################################################################


turkey_index <- which(rownames(clean_data) == "Turkey")
if(length(turkey_index) > 0) {
  cat("Turkiye'ye en yakin 5 sehir (oklid uzakligi):\n")
  turkey_dist <- as.matrix(dist_eucl_pca)[turkey_index, ]
  closest <- sort(turkey_dist)[2:6]  # ??lk eleman T??rkiye'nin kendisi (0)
  names(closest)
}
print(closest)


#En benzer ulke cifti

dist_matrix <- as.matrix(dist_eucl)
# En yak??n (benzer) ??ift
min_dist <- which(dist_matrix == min(dist_matrix[dist_matrix > 0]), arr.ind = TRUE)
cat("En benzer ulke cifti (oklid):", rownames(dist_matrix)[min_dist[1,1]], "ve", rownames(dist_matrix)[min_dist[1,2]], "\n")
cat("Aralarindaki uzaklik:", dist_matrix[min_dist[1,1], min_dist[1,2]], "\n")


#Bu ulkelerin verilerini karsilastiralim

if(length(min_dist) > 0 && nrow(min_dist) > 0) {
  most_similar_countries <- clean_data[c(rownames(dist_matrix)[min_dist[1,1]], rownames(dist_matrix)[min_dist[1,2]]),]
  cat("\nEn benzer sehirlerin degerleri:\n")
  print(most_similar_countries)
}


#En uzakcift

max_dist <- which(dist_matrix == max(dist_matrix), arr.ind = TRUE)
cat("En farkli ulke cifti (oklid):", rownames(dist_matrix)[max_dist[1,1]], "ve", rownames(dist_matrix)[max_dist[1,2]], "\n")
cat("Aralarindaki uzaklik:", dist_matrix[max_dist[1,1], max_dist[1,2]], "\n")


#Bu verileri karsilastiralim


if(length(max_dist) > 0 && nrow(max_dist) > 0) {
  most_different_countries <- clean_data[c(rownames(dist_matrix)[max_dist[1,1]], rownames(dist_matrix)[max_dist[1,2]]),]
  cat("\nEn farkli ulkelerin degerleri:\n")
  print(most_different_countries)
}




turkey_index <- which(rownames(clean_data) == "Turkey")
if(length(turkey_index) > 0) {
  cat("Turkiye'ye en yakin 5 ulke (oklid uzakligi):\n")
  turkey_dist <- as.matrix(dist_eucl)[turkey_index, ]
  closest <- sort(turkey_dist)[2:6]  # ??lk eleman T??rkiye'nin kendisi (0)
  names(closest)
}


#En benzer ulke cifti
dist_matrix <- as.matrix(dist_eucl)
# En yakin (benzer) cift
min_dist <- which(dist_matrix == min(dist_matrix[dist_matrix > 0]), arr.ind = TRUE)
cat("En benzer ulke cifti (oklid):", rownames(dist_matrix)[min_dist[1,1]], "ve", rownames(dist_matrix)[min_dist[1,2]], "\n")
cat("Aralarindaki uzaklik:", dist_matrix[min_dist[1,1], min_dist[1,2]], "\n")


#Bu ulke verilerini karsilastiralim
if(length(min_dist) > 0 && nrow(min_dist) > 0) {
  most_similar_countries <- clean_data[c(rownames(dist_matrix)[min_dist[1,1]], rownames(dist_matrix)[min_dist[1,2]]),]
  cat("\nEn benzer ulkelerin degerleri:\n")
  print(most_similar_countries)
}


#En uzak (farkli) cift
max_dist <- which(dist_matrix == max(dist_matrix), arr.ind = TRUE)
cat("En farkli ulke cifti (oklid):", rownames(dist_matrix)[max_dist[1,1]], "ve", rownames(dist_matrix)[max_dist[1,2]], "\n")
cat("Aralarindaki uzaklik:", dist_matrix[max_dist[1,1], max_dist[1,2]], "\n")


#Bu ulkelerin verilerini karsilastiralim
if(length(max_dist) > 0 && nrow(max_dist) > 0) {
  most_different_countries <- clean_data[c(rownames(dist_matrix)[max_dist[1,1]], rownames(dist_matrix)[max_dist[1,2]]),]
  cat("\nEn farkli ulkelerin degerleri:\n")
  print(most_different_countries)
}

###################################### Kumeleme Analizi ############################################
library(factoextra)

#kume gecerliligi

random_data <- apply(clean_data, 2,
                     function(x){runif(length(x), min(x), (max(x)))})
random_data <- as.data.frame(random_data)
dim(random_data)
head(random_data)
summary(random_data)
boxplot(random_data)


data_scale=scale(clean_data) ## orjinal veri scale et
random_data=scale(random_data) 

boxplot(data_scale,horizontal=TRUE)
boxplot(random_data,horizontal=TRUE)


fviz_pca_ind(prcomp(data_scale), title = "PCA - final data",
             palette = "jco",
             geom = "point", ggtheme = theme_classic(),
             
             
             legend = "bottom")

#kaydettigim veri 3 kume  
set.seed(123)
# K-means on dataset
km_data <- kmeans(data_scale, 3)
fviz_cluster(list(data_scale = data_scale, cluster = km_data$cluster),
             ellipse.type = "convex", geom = "point", stand = FALSE,
             palette = "jco", ggtheme = theme_classic())

# K-means on the random dataset
km_random_data <- kmeans(random_data, 3)
fviz_cluster(list(data = random_data, cluster = km_random_data$cluster),
             ellipse.type = "convex", geom = "point", stand = FALSE,
             palette = "jco", ggtheme = theme_classic())
# Hierarchical clustering 
fviz_dend(hclust(dist(data_scale)), k = 3, 
          as.ggplot = TRUE, show_labels = FALSE)

# Hierarchical clustering on the random dataset
#fviz_dend(hclust(dist(random_data)), k = 3, 
#as.ggplot = TRUE, show_labels = FALSE)

##### Hopkins 

set.seed(123)
h_data=hopkins(data_scale, nrow(data_scale)-1)
h_random_data=hopkins(random_data, nrow(random_data)-1)
cbind(h_data,h_random_data)



fviz_dist(dist(data_scale), show_labels = FALSE )+
  labs(title = "final data")

fviz_dist(dist(random_data), show_labels = FALSE)+
  labs(title = "Random data")


p1=fviz_nbclust(data_scale, kmeans, nstart = 25, iter.max = 200, method = "wss") +
  labs(subtitle = "Elbow method")
p1




p2=fviz_nbclust(data_scale, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")
p2

p2=fviz_nbclust(data_scale, kmeans, nstart = 25, iter.max = 200,  method = "silhouette")+
  labs(subtitle = "Silhouette method")
p2


set.seed(123)
p3=fviz_nbclust(data_scale, kmeans, nstart = 25, method = "gap_stat", nboot = 500)+
  labs(subtitle = "Gap statistic method")
p3

#
# Display plots side by side
gridExtra::grid.arrange(p1, p2, p3, nrow = 1)


# Display plots by rows
gridExtra::grid.arrange(p1, p2, p3, nrow = 3)


#30 indices for choosing the best number of clusters
nb <- NbClust(data_scale, distance = "euclidean", min.nc = 2,
              max.nc = 9, method = "kmeans")
help("NbClust")

nb$All.index
nb$All.CriticalValues
nb$Best.nc
nb$Best.partition

fviz_nbclust(nb)


# K-means clustering
library(factoextra)
#Computing cluster validation statistics
# K-means clustering
km_data <- eclust(data_scale, "kmeans", k=3 , nstart = 25, graph = TRUE)
km_data
# Visualize k-means clusters
fviz_cluster(km_data, geom = "point", ellipse.type = "norm",
             palette = "jco", ggtheme = theme_minimal())

# Hierarchical clustering
hc_data <- eclust(data_scale, "hclust", k = 4, hc_metric = "euclidean",hc_method = "ward.D2", graph = FALSE)
# Visualize dendrograms
fviz_dend(hc_data, show_labels = FALSE,
          palette = "jco", as.ggplot = TRUE)

##Silhouette plot
fviz_silhouette(km_data, palette = "jco",
                ggtheme = theme_classic())

silinfo <- km_data$silinfo
silinfo
names(silinfo)
fviz_cluster(list(data = data_scale, cluster = km_data$cluster), data = data_scale,
             ellipse.type = "euclid", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal()
)


# Silhouette widths of each observation
head(silinfo$widths[, 1:3], 10)
# Average silhouette width of each cluster
silinfo$clus.avg.widths
# The total average (mean of all individual silhouette widths)
silinfo$avg.width 
# The size of each clusters
km_data$size
# Silhouette width of observation
sil <- km_data$silinfo$widths[, 1:3]
# Objects with negative silhouette
neg_sil_index <- which(sil[, 'sil_width']<0)
sil[neg_sil_index, , drop = FALSE]



# PCA
data.pca <- prcomp(clean_data, center = TRUE, scale. = TRUE)

data <- predict(data.pca)[,1:2]

set.seed(123)
km_res <- kmeans(data, 3, nstart=25) 
km_res
fviz_cluster(km_res, data = data,
             ellipse.type = "convex", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal()
)


library(cluster)
sil <- silhouette(km_res$cluster, dist(data))

# Silhouette grafigi
fviz_silhouette(sil, palette = "jco", ggtheme = theme_classic())

sil_summary <- summary(sil)
print("Her K??me i??in Ortalama Silhouette De??erleri:")
print(sil_summary$clus.avg.widths)
print(paste("Genel Ortalama Silhouette Geni??li??i:", sil_summary$avg.width))



# Alternatif olarak, ortalama silhouette degerlerini manuel hesaplama
avg_sil_width_by_cluster <- aggregate(sil[,"sil_width"], by=list(Cluster=sil[,"cluster"]), mean)
colnames(avg_sil_width_by_cluster)[2] <- "Ortalama_Silhouette_Genisligi"
print("Her K??me i??in Ortalama Silhouette De??erleri (alternatif):")
print(avg_sil_width_by_cluster)

###EK

summary(clean_data)
#aggregate(clean_data, by=list(cluster=km_data$cluster), mean) 
aggregate(clean_data, by=list(cluster=km_res$cluster), mean) 



# Silhouette degerlerini hesapla
sil <- silhouette(km_res$cluster, dist(data))


fviz_cluster(km_res, data = data,
             ellipse.type = "convex", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal()
)

km_sil <- silhouette(km_res$cluster, dist(data))
mean(km_sil[,3])
km_dunn <- cluster.stats(dist(data), km_res$cluster)$dunn
print(km_dunn)

summary(clean_data)
aggregate(clean_data, by=list(cluster=km_res$cluster), mean) 

# Silhouette grafigini ciz
fviz_silhouette(sil, palette = "jco", print.summary = TRUE)

# Her kume ortalama silhouette genislikleri
avg_sil_width_by_cluster <- aggregate(sil[,"sil_width"], by=list(Cluster=sil[,"cluster"]), mean)
colnames(avg_sil_width_by_cluster)[2] <- "Ortalama_Silhouette_Genisligi"




#Computing Dunn index and other cluster validation statistics
install.packages("fpc")
library(fpc)
# Statistics for k-means clustering
km_stats <- cluster.stats(dist(data_scale), km_data$cluster)
# Dun index
km_stats$dunn



# Stability measures

#clmethods <- c("kmeans","pam")
clmethods <- c("kmeans","pam","hierarchical")
stab <- clValid(data_scale, nClust = 2:6, clMethods = clmethods,
                validation = "stability")
summary(stab)
# Display only optimal Scores
optimalScores(stab)


# Display plots side by side
gridExtra::grid.arrange(p1, p2, p3, nrow = 1)
####
library("NbClust")
# Display plots by rows
gridExtra::grid.arrange(p1, p2, p3, nrow = 3)


#30 indices for choosing the best number of clusters
nb <- NbClust(data_scale, distance = "euclidean", min.nc = 2,
              max.nc =9, method = "kmeans")
help("NbClust")
library("NbClust")
library("factoextra")

nb$All.index
nb$All.CriticalValues
nb$Best.nc
nb$Best.partition

fviz_nbclust(nb)

##random data
nb_rd <- NbClust(random_data, distance = "euclidean", min.nc = 2,
                 max.nc = 10, method = "kmeans")

# K-means  

#Computing cluster validation statistics
# K-means clustering
km_data <- eclust(data_scale, "kmeans", k=4 , nstart = 25, graph = TRUE)
km_data
# Visualize k-means clusters
fviz_cluster(km_data, geom = "point", ellipse.type = "norm",
             palette = "jco", ggtheme = theme_minimal())

# Hierarchical clustering
hc_data <- eclust(data_scale, "hclust", k = 4, hc_metric = "euclidean",hc_method = "ward.D2", graph = FALSE)
# Visualize dendrograms
fviz_dend(hc_data, show_labels = FALSE,
          palette = "jco", as.ggplot = TRUE)

##Silhouette plot
fviz_silhouette(km_data, palette = "jco",
                ggtheme = theme_classic())

silinfo <- km_data$silinfo
silinfo
names(silinfo)
fviz_cluster(list(data = clean_data, cluster = km_data$cluster), data = data_scale,
             ellipse.type = "euclid", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal()
)


# Silhouette widths of each observation
head(silinfo$widths[, 1:3], 10)
# Average silhouette width of each cluster
silinfo$clus.avg.widths
# The total average (mean of all individual silhouette widths)
silinfo$avg.width
# The size of each clusters
km_data$size
# Silhouette width of observation
sil <- km_data$silinfo$widths[, 1:3]
# Objects with negative silhouette
neg_sil_index <- which(sil[, 'sil_width']<0)
sil[neg_sil_index, , drop = FALSE]



#Compare clustering algorithms 
install.packages("clValid")
library(clValid)
clmethods <- c("kmeans","pam")
#clmethods <- c("kmeans","pam","hierarchical")
intern <- clValid(data_scale, nClust = 2:6,  
                  clMethods = clmethods, validation = "internal")
# Summary
summary(intern)


# Stability measures

clmethods <- c("kmeans","pam")
#clmethods <- c("kmeans","pam","hierarchical")
stab <- clValid(clean_data, nClust = 2:6, clMethods = clmethods,
                validation = "stability")
summary(stab)
# Display only optimal Scores
optimalScores(stab)


#####################################kumeleme analizi ##########################################################  

install.packages("cluster")
library(cluster)
data <- scale(clean_data, center = TRUE, scale = TRUE)
par(mfrow=c(1,2))

boxplot(data)
summary(clean_data)
apply(clean_data,2, var)
summary(data)
apply(data,2, var)

library(factoextra)
set.seed(123)
fviz_nbclust(data,kmeans,method = "wss") #for total within sum of square
fviz_nbclust(data,kmeans,method = "wss",nstart = 25)+
  labs(subtitle = "Elbow method") #3 gibi
fviz_nbclust(data,kmeans,method = "silhouette")+ #for average silhouette width
  labs(subtitle = "Silhoutte method")  # 
fviz_nbclust(data, kmeans, nstart = 25,  method = "gap_stat", nboot = 500)+
  labs(subtitle = "Gap statistic method") #or gap statistics # 5 verdi




set.seed(123)
p1=fviz_nbclust(data, kmeans, nstart = 25, iter.max = 200, method = "wss") +
  labs(subtitle = "Elbow method")
p1


# Silhouette method

p2=fviz_nbclust(data, kmeans, nstart = 25, iter.max = 200,  method = "silhouette")+
  labs(subtitle = "Silhouette method")
p2


# Gap statistic
# nboot = 50 to keep the function speedy.
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
set.seed(123)
p3=fviz_nbclust(data, kmeans, nstart = 25, method = "gap_stat", nboot = 500)+
  labs(subtitle = "Gap statistic method")
p3



# Display plots side by side
gridExtra::grid.arrange(p1, p2, p3, nrow = 1)



set.seed(123)
km_res <- kmeans(data, 3, nstart=25) 
print(km_res)


km_sil <- silhouette(km_res$cluster, dist(data))
mean(km_sil[,3])
km_dunn <- cluster.stats(dist(data), km_res$cluster)$dunn


km_res <- eclust(data, "kmeans", k = 3, nstart = 25) 


silinfo <- km_res$silinfo
silinfo
names(silinfo)


# Silhouette widths of each observation
head(silinfo$widths[, 1:3], 10)
# Average silhouette width of each cluster
silinfo$clus.avg.widths
# The total average (mean of all individual silhouette widths)
silinfo$avg.width
# The size of each clusters
km_data$size
# Silhouette width of observation
sil <- km_data$silinfo$widths[, 1:3]
# Objects with negative silhouette
neg_sil_index <- which(sil[, 'sil_width']<0)
sil[neg_sil_index, , drop = FALSE]



summary(clean_data)
aggregate(clean_data, by=list(cluster=km_res$cluster), mean) #orjinal veri kullanilarak kumelere gore her degisken icin ortalamasi
dd=cbind(clean_data, cluster=km_res$cluster)
head(dd,10)

#3 kume cizdirmesi scale edilmis uzerinden
fviz_cluster(km_res, data = data,
             ellipse.type = "convex", # Concentration ellipse -  euclid di degistirdim
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal()
)


#kume sayisi 3 olarak belirlenirse:
km_res <- kmeans(data, 3, nstart=25) 
print(km_res)
km_sil <- silhouette(km_res$cluster, dist(data))
mean(km_sil[,3])
km_dunn <- cluster.stats(dist(data), km_res$cluster)$dunn

fviz_cluster(km_res, data = data,
             ellipse.type = "convex", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal()
)

summary(clean_data)
aggregate(clean_data, by=list(cluster=km_res$cluster), mean) 
dd=cbind(clean_data, cluster=km_res$cluster)
head(dd,10)



##kume sayisi 7 olarak belirlenirse:
km_res <- kmeans(data, 7, nstart=25) 
print(km_res)
km_sil <- silhouette(km_res$cluster, dist(data))
mean(km_sil[,3])
km_dunn <- cluster.stats(dist(data), km_res$cluster)$dunn

fviz_cluster(km_res, data = data,
             ellipse.type = "convex", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal()
)

summary(clean_data)
aggregate(clean_data, by=list(cluster=km_res$cluster), mean) 
dd=cbind(clean_data, cluster=km_res$cluster)
head(dd,10)


km_res <- eclust(data, "kmeans", k = 7, nstart = 25) 


silinfo <- km_res$silinfo
silinfo
names(silinfo)


# Silhouette widths of each observation
head(silinfo$widths[, 1:3], 10)
# Average silhouette width of each cluster
silinfo$clus.avg.widths
# The total average (mean of all individual silhouette widths)
silinfo$avg.width
# The size of each clusters
km_data$size
# Silhouette width of observation
sil <- km_data$silinfo$widths[, 1:3]
# Objects with negative silhouette
neg_sil_index <- which(sil[, 'sil_width']<0)
sil[neg_sil_index, , drop = FALSE]

# Silhouette grafi??ini ??iz
fviz_silhouette(km_data, palette = "jco", ggtheme = theme_classic())


############# pca+kmeans##################
data.pca <- prcomp(clean_data, center = TRUE, scale. = TRUE)
data <- predict(data.pca)[,1:2]
summary(data.pca)
fviz_nbclust(data,kmeans,method = "wss", nstart=25) #for total within sum of square   4
fviz_nbclust(data,kmeans,method = "silhouette") #for average silhouette width  4 gibi
fviz_nbclust(data, kmeans, nstart = 25,  method = "gap_stat", nboot = 500) #4  or gap statistics

# kume gecerliliginden ek 
set.seed(123)
p1=fviz_nbclust(data, kmeans, nstart = 25, iter.max = 200, method = "wss") +
  labs(subtitle = "Elbow method")
p1


# Silhouette method

p2=fviz_nbclust(data, kmeans, nstart = 25, iter.max = 200,  method = "silhouette")+
  labs(subtitle = "Silhouette method")
p2


# Gap statistic
# nboot = 50 to keep the function speedy.
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
set.seed(123)
p3=fviz_nbclust(data, kmeans, nstart = 25, method = "gap_stat", nboot = 500)+
  labs(subtitle = "Gap statistic method")
p3


# Display plots side by side
gridExtra::grid.arrange(p1, p2, p3, nrow = 1)
####


set.seed(123)
km_res <- kmeans(data, 3, nstart=25) 
print(km_res)
fviz_cluster(km_res, data = data,
             ellipse.type = "convex", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal()
)



# PCA
data.pca <- prcomp(clean_data, center = TRUE, scale. = TRUE)
data <- predict(data.pca)[,1:2]
# eclust() fonksiyonu ile K-means k??meleme
library(factoextra)
set.seed(123)
km_res_ext <- eclust(data, "kmeans", k = 3, nstart = 25, graph = TRUE)
print(km_res_ext)


fviz_cluster(km_res_ext, data = data,
             ellipse.type = "convex", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal()
)

# Silhouette bilgileri
silinfo <- km_res_ext$silinfo
print(silinfo)
print(names(silinfo))

# Silhouette grafi??ini ??iz
fviz_silhouette(km_res_ext, palette = "jco", ggtheme = theme_classic())

# Her k??me i??in ortalama silhouette de??erleri
print("Her Kume icin Ortalama Silhouette Degerleri:")
print(silinfo$clus.avg.widths) 
print(paste("Genel Ortalama Silhouette Genisligi:", silinfo$avg.width))


summary(clean_data)
aggregate(clean_data, by=list(cluster=km_res$cluster), mean) #orjinal veri kullanilarak 




data.pca <- prcomp(clean_data, center = TRUE, scale. = TRUE)
data <- predict(data.pca)[,1:2]
summary(data.pca)
fviz_nbclust(data,kmeans,method = "wss", nstart=25) #for total within sum of square  
fviz_nbclust(data,kmeans,method = "silhouette") #for average silhouette width
fviz_nbclust(data, kmeans, nstart = 25,  method = "gap_stat", nboot = 500) #or gap statistics

set.seed(123)
km_res <- kmeans(data, 3, nstart=25) 
print(km_res)
fviz_cluster(km_res, data = data,
             ellipse.type = "convex", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal()
)



# PCA 
data.pca <- prcomp(clean_data, center = TRUE, scale. = TRUE)
data <- predict(data.pca)[,1:2]
# eclust() fonksiyonu ile K-means kumeleme
library(factoextra)
set.seed(123)
km_res_ext <- eclust(data, "kmeans", k = 3, nstart = 25, graph = TRUE)
print(km_res_ext)

# Kumeleri gorsellestir
fviz_cluster(km_res_ext, data = data,
             ellipse.type = "convex", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal()
)

# Silhouette bilgileri
silinfo <- km_res_ext$silinfo
print(silinfo)
print(names(silinfo))

# Silhouette grafi??ini ??iz
fviz_silhouette(km_res_ext, palette = "jco", ggtheme = theme_classic())

# Her k??me i??in ortalama silhouette de??erleri
print("Her Kume icin Ortalama Silhouette Degerleri:")
print(silinfo$clus.avg.widths) 
print(paste("Genel Ortalama Silhouette Genisligi:", silinfo$avg.width))
#Genel Ortalama Silhouette Genisligi: 

# K??me b??y??kl??kleri
print("Kume Buyuklukleri:")
print(km_res_ext$size) 


##
km_sil <- silhouette(km_res_ext$cluster, dist(data))
mean(km_sil[,3]) 
km_dunn <- cluster.stats(dist(data), km_res_ext$cluster)$dunn
print(km_dunn) 


km_sil <- silhouette(km_res$cluster, dist(data))
mean(km_sil[,3])

km_dunn <- cluster.stats(dist(data), km_res$cluster)$dunn



#--------------------- sil width vs hesaplanmasi 2-ek 4 kume -------------------------
# PCA uygula
# ??lk iki bile??eni al
data.pca <- prcomp(clean_data, center = TRUE, scale. = TRUE)
data <- predict(data.pca)[,1:2]
# eclust() fonksiyonu ile K-means kumeleme
library(factoextra)
set.seed(123)
km_res_ext <- eclust(data, "kmeans", k = 4, nstart = 25, graph = TRUE)
print(km_res_ext)

# Kumeleri gorsellestir
fviz_cluster(km_res_ext, data = data,
             ellipse.type = "convex", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal()
)

# Silhouette bilgileri
silinfo <- km_res_ext$silinfo
print(silinfo)
print(names(silinfo))

# Silhouette grafi??ini ??iz
fviz_silhouette(km_res_ext, palette = "jco", ggtheme = theme_classic())

# Her k??me i??in ortalama silhouette de??erleri
print("Her Kume icin Ortalama Silhouette Degerleri:")
print(silinfo$clus.avg.widths) 
print(paste("Genel Ortalama Silhouette Genisligi:", silinfo$avg.width))
#Genel Ortalama Silhouette Genisligi: 

# K??me b??y??kl??kleri
print("Kume Buyuklukleri:")
print(km_res_ext$size) 


##
km_sil <- silhouette(km_res_ext$cluster, dist(data))
mean(km_sil[,3]) 
km_dunn <- cluster.stats(dist(data), km_res_ext$cluster)$dunn
print(km_dunn) 


km_sil <- silhouette(km_res$cluster, dist(data))
mean(km_sil[,3])

km_dunn <- cluster.stats(dist(data), km_res$cluster)$dunn



#-------------------- sil width vs hesaplanmasi -- ek 5 kume ---------------------------
# PCA uygula
# ??lk iki bile??eni al
data.pca <- prcomp(clean_data, center = TRUE, scale. = TRUE)
data <- predict(data.pca)[,1:2]
# eclust() fonksiyonu ile K-means kumeleme
library(factoextra)
set.seed(123)
km_res_ext <- eclust(data, "kmeans", k = 5, nstart = 25, graph = TRUE)
print(km_res_ext)

# Kumeleri gorsellestir
fviz_cluster(km_res_ext, data = data,
             ellipse.type = "convex", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal()
)

# Silhouette bilgileri
silinfo <- km_res_ext$silinfo
print(silinfo)
print(names(silinfo))

# Silhouette grafi??ini ??iz
fviz_silhouette(km_res_ext, palette = "jco", ggtheme = theme_classic())

# Her k??me i??in ortalama silhouette de??erleri
print("Her Kume icin Ortalama Silhouette Degerleri:")
print(silinfo$clus.avg.widths) 
print(paste("Genel Ortalama Silhouette Genisligi:", silinfo$avg.width))
#Genel Ortalama Silhouette Genisligi: 

# K??me b??y??kl??kleri
print("Kume Buyuklukleri:")
print(km_res_ext$size) 


##
km_sil <- silhouette(km_res_ext$cluster, dist(data))
mean(km_sil[,3]) 
km_dunn <- cluster.stats(dist(data), km_res_ext$cluster)$dunn
print(km_dunn) 


km_sil <- silhouette(km_res$cluster, dist(data))
mean(km_sil[,3])

km_dunn <- cluster.stats(dist(data), km_res$cluster)$dunn


#-------------------- sil width vs hesaplanmasi   ek 7 kume ------------------------------
# PCA uygula
# ??lk iki bile??eni al
data.pca <- prcomp(clean_data, center = TRUE, scale. = TRUE)
data <- predict(data.pca)[,1:2]
# eclust() fonksiyonu ile K-means kumeleme
library(factoextra)
set.seed(123)
km_res_ext <- eclust(data, "kmeans", k = 7, nstart = 25, graph = TRUE)
print(km_res_ext)

# Kumeleri gorsellestir
fviz_cluster(km_res_ext, data = data,
             ellipse.type = "convex", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal()
)

# Silhouette bilgileri
silinfo <- km_res_ext$silinfo
print(silinfo)
print(names(silinfo))

# Silhouette grafi??ini ??iz
fviz_silhouette(km_res_ext, palette = "jco", ggtheme = theme_classic())

# Her k??me i??in ortalama silhouette de??erleri
print("Her Kume icin Ortalama Silhouette Degerleri:")
print(silinfo$clus.avg.widths) 
print(paste("Genel Ortalama Silhouette Genisligi:", silinfo$avg.width))
#Genel Ortalama Silhouette Genisligi: 

# K??me b??y??kl??kleri
print("Kume Buyuklukleri:")
print(km_res_ext$size) 


km_sil <- silhouette(km_res_ext$cluster, dist(data))
mean(km_sil[,3]) 
km_dunn <- cluster.stats(dist(data), km_res_ext$cluster)$dunn
print(km_dunn) 


km_sil <- silhouette(km_res$cluster, dist(data))
mean(km_sil[,3])

km_dunn <- cluster.stats(dist(data), km_res$cluster)$dunn



#------------------------------


#5 kume
data.pca <- prcomp(clean_data, center = TRUE, scale. = TRUE)
data <- predict(data.pca)[,1:2]
summary(data.pca)
fviz_nbclust(data,kmeans,method = "wss", nstart=25) #for total within sum of square  
fviz_nbclust(data,kmeans,method = "silhouette") #for average silhouette width
fviz_nbclust(data, kmeans, nstart = 25,  method = "gap_stat", nboot = 500) #or gap statistics
set.seed(123)
km_res <- kmeans(data, 5, nstart=25) 
print(km_res)
fviz_cluster(km_res, data = data,
             ellipse.type = "convex", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal()
)

km_sil <- silhouette(km_res$cluster, dist(data))
mean(km_sil[,3])

km_dunn <- cluster.stats(dist(data), km_res$cluster)$dunn

summary(clean_data)
aggregate(clean_data, by=list(cluster=km_res$cluster), mean) #orjinal veri kullan?larak k?melere g?re her de?i?ken i?in ortalams?


#-----------------------------------------------k medoids--------------------------------------


##K-medoids
install.packages("cluster","factoextra","fpc") #"fpc" paketi pamk fonksiyonu icin gerekli
install.packages("cluster")
library(cluster)
install.packages("factoextra")
library(factoextra)
install.packages("fpc")
library(fpc)

data<-scale(clean_data)
boxplot(data)

fviz_nbclust(data, pam, method= "silhouette") 
fviz_nbclust(data, pam, method= "wss") 
fviz_nbclust(data, pam, method= "gap") 


dev.off()

### kume gecerliliginden ek 
set.seed(123)
p1 <- fviz_nbclust(data, pam, method = "wss") +
  labs(subtitle = "Elbow method")
p1

# Silhouette method
p2 <- fviz_nbclust(data, pam, nstart = 25, iter.max = 200,  method = "silhouette")+
  labs(subtitle = "Silhouette method")
p2

# Gap statistic
# nboot = 50 to keep the function speedy.
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
set.seed(123)
p3 <- fviz_nbclust(data, pam, nstart = 25, method = "gap_stat", nboot = 500)+
  labs(subtitle = "Gap statistic method")
p3



# Display plots side by side
gridExtra::grid.arrange(p1, p2, p3, nrow = 1)
####



p1 <- fviz_nbclust(data, pam, method = "wss") +
  labs(subtitle = "Elbow method")


p2 <- fviz_nbclust(data, pam, method = "silhouette") +
  labs(subtitle = "Silhouette method")


set.seed(123)
p3 <- fviz_nbclust(data, pam, method = "gap_stat", nboot = 50) + # D??KKAT: nboot = 50
  labs(subtitle = "Gap statistic method")


grid.arrange(p1, p2, p3, nrow = 1)


#----------------------------
set.seed(123)
pam_data_2 <- pam(data,2)
print(pam_data_2)

km_sil <- silhouette(pam_data_2$cluster, dist(data))
mean(km_sil[,3])
km_dunn <- cluster.stats(dist(data), pam_data_2$cluster)$dunn

pam_data_2$medoids
pam_data_2$clustering
pam_data_2$clusinfo

fviz_cluster(pam_data_2,
             ellipse.type = "convex", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_classic()
)

summary(clean_data)
aggregate(clean_data, by=list(pam_data_2$clustering), mean)  
aggregate(clean_data, by=list(pam_data_2$clustering), sd)

pamk_data <- pamk(data) #bu fonksiyonda k degeri belirlemesine gerek yoktur.
print(pamk_data)
pamk_data$pamobject


fviz_nbclust(data, pam, method= "wss") 
pam_data_4 <- pam(data,4)
print(pam_data_4)
pam_data_4$medoids
pam_data_4$clustering
pam_data_4$clusinfo

fviz_cluster(pam_data_4,
             ellipse.type = "convex", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_classic()
)

km_sil <- silhouette(pam_data_4$cluster, dist(data))
mean(km_sil[,3])
km_dunn <- cluster.stats(dist(data), pam_data_4$cluster)$dunn

pam_data_3 <- pam(data,3)
print(pam_data_3)
pam_data_3$medoids
pam_data_3$clustering
pam_data_3$clusinfo

fviz_cluster(pam_data_3,
             ellipse.type = "convex", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_classic()
)
sil3 <- silhouette(pam_data_3)

km_sil <- silhouette(pam_data_3$cluster, dist(data))
mean(km_sil[,3])
km_dunn <- cluster.stats(dist(data), pam_data_3$cluster)$dunn


aggregate(clean_data, by=list(pam_data_3$clustering), mean) #orjinal veri kullanilarak kumelere gore her degisken icin ortalamasi
aggregate(clean_data, by=list(pam_data_3$clustering), sd)

#--------------------------------------------------------------


pam_data_4 <- pam(data,4)
print(pam_data_4)
fviz_cluster(pam_data_4,
             ellipse.type = "convex", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_classic()
)
pam_data_4$clusinfo
sil4 <- silhouette(pam_data_4)


km_sil <- silhouette(pam_data_4$cluster, dist(data))
mean(km_sil[,3])
km_dunn <- cluster.stats(dist(data), pam_data_4$cluster)$dunn


aggregate(clean_data, by=list(pam_data_4$clustering), mean) #orjinal veri kullanilarak kumelere gore her degisken icin ortalamasi
aggregate(clean_data, by=list(pam_data_4$clustering), sd)

#-----------------------------------------------------------

pam_data_5 <- pam(data,5)
print(pam_data_5)
fviz_cluster(pam_data_5,
             ellipse.type = "convex", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_classic()
)
pam_data_5$clusinfo
sil5 <- silhouette(pam_data_5)

pam_data_6 <- pam(data,6)
print(pam_data_6)
fviz_cluster(pam_data_6,
             ellipse.type = "convex", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_classic()
)

pam_data_6$clusinfo


pam_data_10 <- pam(data,10)
print(pam_data_10)
fviz_cluster(pam_data_10,
             ellipse.type = "convex", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_classic()
)
pam_data_10$clusinfo



#------------------------------------------------------------------
pam_data_7 <- pam(data,7)
print(pam_data_7)
fviz_cluster(pam_data_7,
             ellipse.type = "convex", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_classic()
)
pam_data_7$clusinfo
sil7 <- silhouette(pam_data_7)


km_sil <- silhouette(pam_data_7$cluster, dist(data))
mean(km_sil[,3])
km_dunn <- cluster.stats(dist(data), pam_data_7$cluster)$dunn


aggregate(clean_data, by=list(pam_data_7$clustering), mean) #orjinal veri kullanilarak kumelere gore her degisken icin ortalamasi
aggregate(clean_data, by=list(pam_data_7$clustering), sd)



#------------------------------- pca+kmedoids--------------------------

data.pca <- prcomp(clean_data, center = TRUE, scale. = TRUE)
summary(data.pca)
data <- predict(data.pca)[,1:2]
summary(data)

p1 <- fviz_nbclust(data, pam, method= "silhouette") #2
p2 <-fviz_nbclust(data, pam, method= "wss") #4
p3 <-fviz_nbclust(data, pam, method= "gap") #2 en buyuk 3 4

grid.arrange(p1,p2,p3, nrow=1)



pam_data_4 <- pam(data,4)
print(pam_data_4)
fviz_cluster(pam_data_4,
             ellipse.type = "convex", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_classic()
)

pam_data_4$clusinfo

# Silhouette ve Dunn indeksi hesaplama
# dist() fonksiyonunu do??ru ??ekilde kullanma
d <- dist(data) # ??nce uzakl??k matrisini hesapla
sil <- silhouette(pam_data_4$clustering, d)
mean_sil <- mean(sil[,3]) # Ortalama silhouette de??eri
dunn <- cluster.stats(d, pam_data_4$clustering)$dunn

# Sonu??lar?? yazd??rma
print(paste("Ortalama Silhouette de??eri:", round(mean_sil, 3)))
print(paste("Dunn indeksi:", round(dunn, 3)))


pam_data_3 <- pam(data,3)
print(pam_data_3)
fviz_cluster(pam_data_3,
             ellipse.type = "convex", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_classic()
)

d <- dist(data) # ??nce uzakl??k matrisini hesapla
sil <- silhouette(pam_data_3$clustering, d)
mean_sil <- mean(sil[,3]) # Ortalama silhouette de??eri
dunn <- cluster.stats(d, pam_data_3$clustering)$dunn
# Sonuclari yazdirma
print(paste("Ortalama Silhouette degeri:", round(mean_sil, 3)))
print(paste("Dunn indeksi:", round(dunn, 3)))


pam_data_2 <- pam(data,2)
print(pam_data_2)
fviz_cluster(pam_data_2,
             ellipse.type = "t", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_classic()
)

d <- dist(data) # ??nce uzakl??k matrisini hesapla
sil <- silhouette(pam_data_3$clustering, d)
mean_sil <- mean(sil[,3]) # Ortalama silhouette de??eri

dunn <- cluster.stats(d, pam_data_3$clustering)$dunn

# Sonu??lar?? yazd??rma
print(paste("Ortalama Silhouette de??eri:", round(mean_sil, 3)))
print(paste("Dunn indeksi:", round(dunn, 3)))


pam_data_5 <- pam(data,5)
print(pam_data_5)
fviz_cluster(pam_data_5,
             ellipse.type = "convex", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_classic()
             
)
d <- dist(data) # ??nce uzakl??k matrisini hesapla
sil <- silhouette(pam_data_5$clustering, d)
mean_sil <- mean(sil[,3]) # Ortalama silhouette de??eri

dunn <- cluster.stats(d, pam_data_5$clustering)$dunn

# Sonu??lar?? yazd??rma
print(paste("Ortalama Silhouette de??eri:", round(mean_sil, 3)))
print(paste("Dunn indeksi:", round(dunn, 3)))

pam_data_5$clusinfo

summary(clean_data)

aggregate(clean_data, by=list(pam_data_2$clustering), mean)  #orjinal veri kullan?larak k?melere g?re her de?i?ken i?in ortalamas?
aggregate(clean_data, by=list(pam_data_2$clustering), sd)

aggregate(clean_data, by=list(pam_data_3$clustering), mean) #orjinal veri kullan?larak k?melere g?re her de?i?ken i?in ortalamas?
aggregate(clean_data, by=list(pam_data_3$clustering), sd)

aggregate(clean_data, by=list(pam_data_4$clustering), mean) #orjinal veri kullan?larak k?melere g?re her de?i?ken i?in ortalamas?
aggregate(clean_data, by=list(pam_data_4$clustering), sd)

aggregate(clean_data, by=list(pam_data_5$clustering), mean) #orjinal veri kullan?larak k?melere g?re her de?i?ken i?in ortalamas?
aggregate(clean_data, by=list(pam_data_5$clustering), sd)



#-------------------------------hiyerarsik-------------------------------------------------

library("cluster")
data <- scale(clean_data)
head(data)
dist_euc <- dist(data, method="euclidean")
dist_man <- dist(data, method="manhattan")

##ward.D2 baglanti fonksiyonu ile
hc_e <- hclust(d=dist_euc, method="ward.D2")
plot(hc_e)

fviz_dend(hc_e, k = 3, # Cut in four groups
          cex = 0.5, # label size
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
)

hc_m <- hclust(d=dist_man, method="ward.D2")
plot(hc_m)

library("factoextra")
fviz_dend(hc_e,cex=.5) #cex yazi buyuklugu icindir

fviz_dend(hc_m,cex=.5) #cex yazi buyuklugu icindir

coph_e <- cophenetic(hc_e)
as.matrix(coph_e)[1:6,1:6]
as.matrix(dist_euc)[1:6,1:6]
cor(dist_euc,coph_e)

coph_m <- cophenetic(hc_m)
as.matrix(coph_m)[1:6,1:6]
as.matrix(dist_man)[1:6,1:6]
cor(dist_man,coph_m)

###avarage baglanti fonksiyonu ile
hc_e2 <- hclust(d=dist_euc, method="average") #kotu degil
plot(hc_e2)
fviz_dend(hc_e2,cex=.5) 
hc_m2 <- hclust(d=dist_man, method="average")
plot(hc_m2)

fviz_dend(hc_e2, k = 3, # Cut in four groups
          cex = 0.5, # label size
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
)


hc_e3 <- hclust(d=dist_euc, method="centroid")  
plot(hc_e3)
fviz_dend(hc_e3,cex=.5) 

fviz_dend(hc_e3, k = 3, # Cut in four groups
          cex = 0.5, # label size
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
)


hc_e4 <- hclust(d=dist_euc, method="complete")
plot(hc_e4)
fviz_dend(hc_e4,cex=.5) 

fviz_dend(hc_e4, k = 3, # Cut in four groups
          cex = 0.5, # label size
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
)



hc_e5 <- hclust(d=dist_euc, method="single")
plot(hc_e5)
fviz_dend(hc_e5,cex=.5) 

fviz_dend(hc_e5, k = 3, # Cut in four groups
          cex = 0.5, # label size
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
)


####
#oklid ve average baglamti fonk ile = 
coph_e2 <- cophenetic(hc_e2)
as.matrix(coph_e2)[1:6,1:6]
as.matrix(dist_euc)[1:6,1:6]
cor(dist_euc,coph_e2)


coph_m2 <- cophenetic(hc_m2)
as.matrix(coph_m2)[1:6,1:6]
as.matrix(dist_man)[1:6,1:6]
cor(dist_man,coph_m2)


##ek

install.packages("dendextend")
library(dendextend)
library(dextend)
library(magrittr)  # pipe operat??r?? i??in



# Farkli baglanti fonksiyonlarina gore karsilastirma yapmak uzere


hc_c <- data %>% dist %>% hclust("complete") %>% cophenetic 
hc_s <- data %>% dist %>% hclust("single") %>% cophenetic 
hc_a <- data %>% dist %>% hclust("average") %>% cophenetic 
hc_cen <- data %>% dist %>% hclust("centroid") %>% cophenetic 
hc_w <- data %>% dist %>% hclust("ward.D2") %>% cophenetic 
cor(dist_euc, hc_c)   #  complete
cor(dist_euc, hc_s)   #  single
cor(dist_euc, hc_a)   #  average
cor(dist_euc, hc_cen) #  centroid
cor(dist_euc, hc_w)   #  ward.D2


hc_cm <- data %>% dist("manhattan") %>% hclust("complete") %>% cophenetic
hc_sm <- data %>% dist("manhattan") %>% hclust("single") %>% cophenetic
hc_am <- data %>% dist("manhattan") %>% hclust("average") %>% cophenetic
hc_cenm <- data %>% dist("manhattan") %>% hclust("centroid") %>% cophenetic
hc_wm <- data %>% dist("manhattan") %>% hclust("ward.D2") %>% cophenetic
cor(dist_man, hc_cm)
cor(dist_man, hc_sm)
cor(dist_man, hc_am)
cor(dist_man, hc_cenm)
cor(dist_man, hc_wm)




#### cut tree in 4 groups
#euclid_ward
grup <- cutree(hc_e, k=3)
grup
table(grup)
#grup
#1   2   3   
#44  74 49 
rownames(data)[grup==1]  
rownames(data)[grup==2]  
rownames(data)[grup==3] 
 
library(factoextra)
fviz_dend(hc_e, k = 3, # Cut in four groups
          cex = 0.5, # label size
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
)




fviz_cluster(list(data = data, cluster = grup),
             palette = c("#2E9FDF", "#00FF00", "#E7B800", "#FC4E07"),
             ellipse.type = "convex", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             show.clust.cent = FALSE, ggtheme = theme_minimal())




# Silhouette hesaplamas??
sil <- silhouette(grup, dist_euc)
print(summary(sil))

# Her k??me i??in ortalama silhouette geni??li??i
avg_sil_by_cluster <- aggregate(sil[,"sil_width"], by=list(Cluster=sil[,"cluster"]), mean)
colnames(avg_sil_by_cluster)[2] <- "Ort_Silhouette_Genisligi"
print("Her Kume icin Ortalama Silhouette Degerleri:")
print(avg_sil_by_cluster)
print(paste("Genel Ortalama Silhouette Genisligi:", mean(sil[,"sil_width"])))

# Silhouette grafi??i
fviz_silhouette(sil, palette = "jco", ggtheme = theme_classic())

# Dunn indeksi (y??ksek de??erler daha iyi k??melemeyi g??sterir)
dunn_index <- dunn(dist_euc, grup)
print(paste("Dunn indeksi:", dunn_index))



###########  tum linkage fonskiyonlarinin (oklid) sil widt dunn karsilastirmasi

# Gerekli paketler
library(cluster)
library(factoextra)
library(fpc)

# Veriyi haz??rla
data <- scale(clean_data)
dist_euc <- dist(data, method="euclidean")

# Ba??lant?? fonksiyonlar??
linkage_methods <- c("ward.D2", "complete", "average", "single", "centroid")

# Sonu??lar?? saklamak i??in tablo olu??tur
results <- matrix(NA, nrow = length(linkage_methods), ncol = 10)
rownames(results) <- linkage_methods
colnames(results) <- c(paste0("Sil_k", 2:6), paste0("Dunn_k", 2:6))

# T??m ba??lant?? y??ntemleri ve k??me say??lar?? i??in d??ng??
for (i in 1:length(linkage_methods)) {
  method <- linkage_methods[i]
  
  # Hiyerar??ik k??meleme uygula
  hc <- hclust(dist_euc, method = method)
  
  for (k in 2:6) {
    # A??ac?? kes
    clusters <- cutree(hc, k)
    
    # Silhouette de??eri hesapla
    sil <- silhouette(clusters, dist_euc)
    avg_sil <- mean(sil[, "sil_width"])
    
    # Dunn indeksi hesapla
    dunn_val <- dunn(dist_euc, clusters)
    
    # Sonu??lar?? tabloya kaydet
    results[i, paste0("Sil_k", k)] <- avg_sil
    results[i, paste0("Dunn_k", k)] <- dunn_val
  }
}

# Tabloyu g??ster
print("Average Silhouette Width De??erleri:")
print(round(results[, 1:5], 3))

print("\nDunn Indeksi De??erleri:")
print(round(results[, 6:10], 3))

# En iyi Silhouette de??erini bul
best_sil <- which(results[, 1:5] == max(results[, 1:5]), arr.ind = TRUE)
cat("\nEn y??ksek Silhouette de??eri:", round(max(results[, 1:5]), 3), "\n")
cat("Metod:", rownames(results)[best_sil[1]], "K??me Say??s??:", best_sil[2] + 1, "\n")

# En iyi Dunn de??erini bul
best_dunn <- which(results[, 6:10] == max(results[, 6:10]), arr.ind = TRUE)
cat("\nEn y??ksek Dunn indeksi:", round(max(results[, 6:10]), 3), "\n")
cat("Metod:", rownames(results)[best_dunn[1]], "K??me Say??s??:", best_dunn[2] + 1, "\n")

# Ward.D2 metodu ve k=4 i??in do??rulama
hc_ward <- hclust(dist_euc, method = "ward.D2")
grup_ward_4 <- cutree(hc_ward, k = 4)
sil_ward_4 <- silhouette(grup_ward_4, dist_euc)
avg_sil_ward_4 <- mean(sil_ward_4[, "sil_width"])
dunn_ward_4 <- dunn(dist_euc, grup_ward_4)

cat("\nWard.D2, k=4 i??in de??erler:\n")
cat("Average Silhouette Width:", round(avg_sil_ward_4, 3), "\n")
cat("Dunn ??ndeksi:", round(dunn_ward_4, 3), "\n")

# Ward.D2, k=4 i??in grup b??y??kl??kleri
cat("\nWard.D2, k=4 i??in grup b??y??kl??kleri:\n")
print(table(grup_ward_4))


#------------------------- ek hc_e2

grup <- cutree(hc_e2, k=3)
grup

table(grup)
rownames(data)[grup==1]  
rownames(data)[grup==2]  
rownames(data)[grup==3] 




fviz_dend(hc_e, k = 4, # Cut in four groups
          cex = 0.5, # label size
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
)


fviz_dend(hc_e2, k = 4, # Cut in four groups
          cex = 0.5, # label size
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
)

fviz_dend(hc_e5, k = 4, # Cut in four groups
          cex = 0.5, # label size
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
)

fviz_cluster(list(data = data, cluster = grup),
             palette = c("#2E9FDF", "#00FF00", "#E7B800", "#FC4E07"),
             ellipse.type = "convex", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             show.clust.cent = FALSE, ggtheme = theme_minimal())

#### cut tree in 3 groups
grup <- cutree(hc_e, h=15)
grup


table(grup)
rownames(data)[grup==1]  
rownames(data)[grup==2]  
rownames(data)[grup==3] 



fviz_dend(hc_e, k = 4, # Cut in four groups
          cex = 0.5, # label size
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
)

# burasi  9 kume verdi
fviz_cluster(list(data = data, cluster = grup),
             ellipse.type = "convex", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             show.clust.cent = FALSE, ggtheme = theme_minimal())

## ek euclid average 
grup <- cutree(hc_e2, k=3)
grup
table(grup)
rownames(data)[grup==1]  
rownames(data)[grup==2]  
rownames(data)[grup==3] 



fviz_dend(hc_e2, k = 3, # Cut in four groups
          cex = 0.5, # label size
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
)


#
fviz_cluster(list(data = data, cluster = grup),
             ellipse.type = "convex", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             show.clust.cent = FALSE, ggtheme = theme_minimal())



###################### dendrogram karsilastirmasi  ######################################

library("cluster")
library("dendextend")
data=scale(clean_data)

dist_euc=dist(data, method="euclidean")

hc_e_w=hclust(d=dist_euc, method="ward.D2")
plot(hc_e_w)
dend1=as.dendrogram(hc_e_w)

hc_e_a=hclust(d=dist_euc, method="average") 
plot(hc_e_a)
dend2=as.dendrogram(hc_e_a)

dist_man=dist(data, method="manhattan")

hc_m_w=hclust(d=dist_man, method="ward.D2")
plot(hc_m_w)
dend3=as.dendrogram(hc_m_w)

hc_m_a=hclust(d=dist_man, method="average")
plot(hc_m_a)
dend4=as.dendrogram(hc_m_a)


tanglegram(dend1,dend2)
dend_list=dendlist(dend1,dend2)
tanglegram(dend1, dend2,  #0.41 cikti
           highlight_distinct_edges = FALSE, # Turn-off dashed lines
           common_subtrees_color_lines = FALSE, # Turn-off line colors
           common_subtrees_color_branches = TRUE, # Color common branches
           main = paste("entanglement =", round(entanglement(dend_list), 2)))

tanglegram(dend1,dend3)
dend_list=dendlist(dend1,dend3)
tanglegram(dend1, dend3, #0.8 cikti
           highlight_distinct_edges = FALSE, # Turn-off dashed lines
           common_subtrees_color_lines = FALSE, # Turn-off line colors
           common_subtrees_color_branches = TRUE, # Color common branches
           main = paste("entanglement =", round(entanglement(dend_list), 2)))

dend_list=dendlist(dend1,dend1)
tanglegram(dend1,dend1)
tanglegram(dend1, dend1, #0 cikti tam match 
           highlight_distinct_edges = FALSE, # Turn-off dashed lines
           common_subtrees_color_lines = FALSE, # Turn-off line colors
           common_subtrees_color_branches = TRUE, # Color common branches
           main = paste("entanglement =", round(entanglement(dend_list), 2)))

dend_list=dendlist(dend3,dend4)
tanglegram(dend3,dend4)
tanglegram(dend3, dend4,
           highlight_distinct_edges = FALSE, # Turn-off dashed lines
           common_subtrees_color_lines = FALSE, # Turn-off line colors
           common_subtrees_color_branches = TRUE, # Color common branches
           main = paste("entanglement =", round(entanglement(dend_list), 2)))

#https://cran.r-project.org/web/packages/dendextend/vignettes/dendextend.html

set.seed(3958)
x <- dendlist(dend1,dend2) %>% untangle(method = "random", R = 10) 
x %>% plot(main = paste("entanglement =", round(entanglement(x), 2))) 

set.seed(3958)
x <- dendlist(dend1,dend2) %>% untangle(method = "step2side") 
x %>% plot(main = paste("entanglement =", round(entanglement(x), 2)))  

x <- dendlist(dend3,dend4) %>% untangle(method = "random", R = 10) 
x %>% plot(main = paste("entanglement =", round(entanglement(x), 2))) 

x <- dendlist(dend3,dend4) %>% untangle(method = "step2side") 
x %>% plot(main = paste("entanglement =", round(entanglement(x), 2))) 


x <- dendlist(dend1,dend3) %>% untangle(method = "random", R = 10) 
x %>% plot(main = paste("entanglement =", round(entanglement(x), 2))) 

x <- dendlist(dend1,dend3) %>% untangle(method = "step2side") 
x %>% plot(main = paste("entanglement =", round(entanglement(x), 2))) 

####correlation
cor_cophenetic(dend1, dend2) 
z

cor_bakers_gamma(dend1, dend2)

dend_list=dendlist(dend1,dend2,dend3, dend4)
cor.dendlist(dend_list, method = "cophenetic") 

cor.dendlist(dend_list, method = "baker")

###farkli baglanti fonksyonlarina gore karsilastirma yapmak uzere 
dend1 <- data %>% dist %>% hclust("complete") %>% as.dendrogram
dend2 <- data %>% dist %>% hclust("single") %>% as.dendrogram
dend3 <- data %>% dist %>% hclust("average") %>% as.dendrogram
dend4 <- data %>% dist %>% hclust("centroid") %>% as.dendrogram
dend5 <- data %>% dist %>% hclust("ward.D2") %>% as.dendrogram
# Compute correlation matrix
dend_list <- dendlist("Complete" = dend1, "Single" = dend2,
                      "Average" = dend3, "Centroid" = dend4, "ward.D2" = dend5)
cors <- cor.dendlist(dend_list)
cors
# Print correlation matrix
round(cors, 2)
# Visualize the correlation matrix using corrplot package
install.packages("corrplot")
library(corrplot)
par(mfrow=c(1,1))
corrplot(cors, "pie", "lower")

##### visualizing dendrograms

#dist_euc=dist(data, method="euclidean")
#hc_e_w=hclust(d=dist_euc, method="ward.D2")
#plot(hc_e_w)
#dend1=as.dendrogram(hc_e_w)
library(factoextra)

fviz_dend(hc_e_w, cex=0.5)

fviz_dend(hc_e_w, cex = 0.5,
          main = "Dendrogram - ward.D2",
          xlab = "Objects", ylab = "Distance", sub = "")

fviz_dend(hc_e_w, cex = 0.5, horiz = TRUE) #yan ceviriyor

#renklendirme icin
fviz_dend(hc_e_w, k = 4, # Cut in four groups kume say??s?? 4
          cex = 0.5, # label size
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE, # Add rectangle around groups
          rect_fill = TRUE)

fviz_dend(hc_e_w, k = 4, # Cut in four groups
          cex = 0.5, # label size
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE, # color labels by groups
          ggtheme = theme_classic()# Change theme)
)


# ---  Hiyerarsik K-Means (hkmeans) Analizi ---

# a) HK-Means Modelini Olu??turma 

final_hk <- hkmeans(pca_scores, k = 3, hc.method = "ward.D2")

# b) Dendrogram G??rselle??tirmesi
fviz_dend(final_hk, cex = 0.7, palette = "jco",
          rect = TRUE, rect_border = "jco", rect_fill = TRUE,
          main = "Hiyerarsik K-Means Dendrogrami")

# c) Final K??me G??rselle??tirmesi
fviz_cluster(final_hk,
             repel = TRUE,
             ggtheme = theme_minimal(),
             main = "Hiyerarsik K-Means Sonuclari (K=3)")

# d) Ge??erlilik ??statisti??i
sil_hk <- silhouette(final_hk$cluster, dist(pca_scores))
avg_sil_hk <- mean(sil_hk[, 3])
print(paste("Hiyerarsik K-Means (K=3) Ortalama Siluet Genisligi:", round(avg_sil_hk, 4)))




# --- 7. Model Temelli Kumeleme (Mclust) ---


library(mclust)
library(factoextra)

final_mc <- Mclust(pca_scores)
summary(final_mc) # En iyi modeli ve k??me say??s??n?? g??rmek i??in

# b) Model Se??im Grafi??i (BIC)
fviz_mclust(final_mc, "BIC", palette = "jco")

# c) K??me G??rselle??tirmesi (Paket ad??yla birlikte)
factoextra::fviz_cluster(final_mc,
                         ellipse.type = "norm",
                         repel = TRUE,
                         ggtheme = theme_minimal(),
                         main = paste("Model Temelli K??meleme Sonucu (", final_mc$G, " K??me)"))



# d) Belirsizlik Grafi??i
fviz_mclust(final_mc, "uncertainty", palette = "jco")



##k-means
df <- scale(clean_data)
# Compute hierarchical k-means clustering
library(factoextra)
set.seed(123)
fviz_nbclust(df, kmeans,nstart = 25, iter.max = 200, method = "wss") +
  labs(subtitle = "Elbow method")
km_data=kmeans(df, 4) 
print(km_data)
fviz_cluster(km_data, data = df,
             ellipse.type = "euclid", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal()
)

##hierarchical k-means clustering (hkmeans)
res.hk <-hkmeans(df, 3)
# Print the results
res.hk
# Visualize the tree  3 kume oldu
fviz_dend(res.hk, cex = 0.6, palette = "jco",
          rect = TRUE, rect_border = "jco", rect_fill = TRUE)

# Visualize the hkmeans final clusters
fviz_cluster(res.hk, palette = "jco", repel = TRUE,
             ggtheme = theme_classic())

km_data$centers
res.hk$centers



# --- Hiyerar??ik K-Means Ge??erlilik Analizi ( 'res.hk' nesnesi ??zerinden) ---


library(cluster) 

# 1. Siluet Bilgilerini Hesapla

sil_hk_tam_veri <- silhouette(res.hk$cluster, dist(df))

# 2. Ortalama Siluet Geni??li??ini (Average Silhouette Width) Hesapla ve Yazd??r
avg_sil_hk_tam_veri <- mean(sil_hk_tam_veri[, 3])
print(paste("Hiyerarsik K-Means (K=3, Tam Veri) Ortalama Siluet Genisligi:", round(avg_sil_hk_tam_veri, 3)))

# 3. Daha Detayl?? ??nceleme i??in Siluet Grafi??i
fviz_silhouette(sil_hk_tam_veri, palette = "jco", ggtheme = theme_classic())

# 4. K??me Profillerini Yorumlamak ????in Ortalamalar?? Hesapla

data_with_clusters_hk <- cbind(clean_data, Cluster = res.hk$cluster)

# Her k??me i??in de??i??ken ortalamalar??n?? hesapla
cluster_profiles_hk <- aggregate(. ~ Cluster, data = data_with_clusters_hk, mean)

print("--- Hiyerarsik K-Means Kumelerinin Profil Ortalamalari (Tam Veri) ---")
print(round(cluster_profiles_hk, 2))


#------------------------


##hc.metric; hc.method
res.hk <-hkmeans(df, 3,  hc.metric = "euclidean",hc.method = "average")
res.hk
fviz_dend(res.hk, cex = 0.6, palette = "jco", 
          rect = TRUE, rect_border = "jco", rect_fill = TRUE)
# Visualize the hkmeans final clusters
fviz_cluster(res.hk, palette = "jco", repel = TRUE,
             ggtheme = theme_classic())

km_data$centers
res.hk$centers



# --- Hiyerarsik K-Means Gecerlilik Analizi ( 'res.hk' nesnesi uzerinden) ---


library(cluster) # silhouette fonksiyonu icin

# 1. Siluet Bilgilerini Hesapla

sil_hk_tam_veri <- silhouette(res.hk$cluster, dist(df))

# 2. Ortalama Siluet Genisligini (Average Silhouette Width) Hesapla ve Yazdir
avg_sil_hk_tam_veri <- mean(sil_hk_tam_veri[, 3])
print(paste("Hiyerarsik K-Means (K=3, Tam Veri) Ortalama Siluet Genisligi:", round(avg_sil_hk_tam_veri, 3)))

# 3. Daha Detayli inceleme icin Siluet Grafigi 
fviz_silhouette(sil_hk_tam_veri, palette = "jco", ggtheme = theme_classic())

# 4. Kume Profillerini Yorumlamak icin Ortalamalari Hesapla

data_with_clusters_hk <- cbind(clean_data, Cluster = res.hk$cluster)

# Her kume icin degisken ortalamalarini hesapla
cluster_profiles_hk <- aggregate(. ~ Cluster, data = data_with_clusters_hk, mean)

print("--- Hiyerarsik K-Means Kumelerinin Profil Ortalamalari (Tam Veri) ---")
print(round(cluster_profiles_hk, 2))



#-----------------model-based clustering scale veri ile-------------------------------


library(MASS)
data <- clean_data
dim(data)
summary(data)

head(data)


library(mclust)
data <- clean_data
data <- scale(clean_data)
mc <- Mclust(data)
summary(mc)
#View(mc)
head(mc$z)
head(mc$classification,10)

dev.off() 
library(factoextra)
# BIC values used for choosing the number of clusters
fviz_mclust(mc, "BIC", palette = "jco")

#---muadil

library(mclust)

# Temel BIC grafi??i 
data <- scale(clean_data)
mc <- Mclust(data)
plot(mc) #SELECT ile 1 2 3 4 sec BIC, classification, uncertainty, density
plot(mc, what = "BIC")

# En iyi modeli ve k??me say??s??n?? g??rmek i??in:
summary(mc)



fviz_mclust_bic(mc)

fviz_mclust(mc,'uncertainty')
fviz_mclust(mc,'classification')
mc$parameters

#-----------------------------------------

# Hangi model oldu??unu net g??rmek i??in:
cat("En iyi model:", mc$modelName)
cat("Optimal kume sayisi:", mc$G)
cat("En yuksek BIC degeri:", max(mc$BIC, na.rm = TRUE))



# Classification: plot showing the clustering
fviz_mclust(mc, "classification", geom = "point",
            pointsize = 1.5, palette = "jco")
# Classification uncertainty
fviz_mclust(mc, "uncertainty", palette = "jco",pos = FALSE)



# Model-Based i??in Siluet
sil_mclust <- silhouette(mc$classification, dist_matrix)
avg_sil_mclust <- mean(sil_mclust[, 3])
print(avg_sil_mclust)

#G=3
mc <- Mclust(data, G=3)
summary(mc)
head(mc$classification,10)
mc$z

# Classification: plot showing the clustering
fviz_mclust(mc, "classification", geom = "point",
            pointsize = 1.5, palette = "jco")
# Classification uncertainty
fviz_mclust(mc, "uncertainty", palette = "jco",pos = FALSE)

fviz_cluster(mc, data = data,
             ellipse.type = "norm", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal()
)

####kmeans
set.seed(123)
fviz_nbclust(data, kmeans, nstart = 25, iter.max = 200, method = "wss") +
  labs(subtitle = "Elbow method")
km_data=kmeans(data, 3) 
print(km_data)
fviz_cluster(km_data, data = data,
             ellipse.type = "convex", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal()
)



# Model-Based i??in Siluet
sil_mclust <- silhouette(mc$classification, dist_matrix)
avg_sil_mclust <- mean(sil_mclust[, 3])
print(avg_sil_mclust)




#-------------------- model-based clustering pca veri ile ------------------------


library(MASS)
data <- data_pca
dim(data)
summary(data)

head(data)


library(mclust)
data <- data_pca

mc <- Mclust(data)
summary(mc)
#View(mc)
head(mc$z)
head(mc$classification,10)

dev.off() 
library(factoextra)
# BIC values used for choosing the number of clusters
fviz_mclust(mc, "BIC", palette = "jco")



fviz_mclust_bic(mc)

fviz_mclust(mc,'uncertainty')
fviz_mclust(mc,'classification')
mc$parameters

#-----------------------------------------

# Hangi model oldu??unu net g??rmek i??in:
cat("En iyi model:", mc$modelName)
cat("Optimal kume sayisi:", mc$G)
cat("En yuksek BIC degeri:", max(mc$BIC, na.rm = TRUE))

#---------

dev.off()

# Classification: plot showing the clustering
fviz_mclust(mc, "classification", geom = "point",
            pointsize = 1.5, palette = "jco")
# Classification uncertainty
fviz_mclust(mc, "uncertainty", palette = "jco",pos = FALSE)


# Model-Based i??in Siluet
sil_mclust <- silhouette(mc$classification, dist_matrix)
avg_sil_mclust <- mean(sil_mclust[, 3])
print(avg_sil_mclust)


#--------- G=3 -----------------
#G=3
mc <- Mclust(data, G=3)
summary(mc)
head(mc$classification,10)
mc$z

# Classification: plot showing the clustering
fviz_mclust(mc, "classification", geom = "point",
            pointsize = 1.5, palette = "jco")
# Classification uncertainty
fviz_mclust(mc, "uncertainty", palette = "jco",pos = FALSE)

fviz_cluster(mc, data = data,
             ellipse.type = "norm", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal()
)

####kmeans
set.seed(123)
fviz_nbclust(data, kmeans, nstart = 25, iter.max = 200, method = "wss") +
  labs(subtitle = "Elbow method")
km_data=kmeans(data, 3) 
print(km_data)
fviz_cluster(km_data, data = data,
             ellipse.type = "convex", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal()
)


#G=3
mc <- Mclust(data, G=3)
summary(mc)
head(mc$classification,10)
mc$z

# Classification: plot showing the clustering
fviz_mclust(mc, "classification", geom = "point",
            pointsize = 1.5, palette = "jco")
# Classification uncertainty
fviz_mclust(mc, "uncertainty", palette = "jco",pos = FALSE)

fviz_cluster(mc, data = data,
             ellipse.type = "norm", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal()
)

####kmeans
set.seed(123)
fviz_nbclust(data, kmeans, nstart = 25, iter.max = 200, method = "wss") +
  labs(subtitle = "Elbow method")
km_data=kmeans(data, 3) 
print(km_data)
fviz_cluster(km_data, data = data,
             ellipse.type = "convex", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal()
)



# Model-Based i??in Siluet pca 3 kume
sil_mclust <- silhouette(mc$classification, dist_matrix)
avg_sil_mclust <- mean(sil_mclust[, 3])
print(avg_sil_mclust)




###################################
#### Density-Based Clustering
###################################
library(factoextra)
summary(multishapes)
head(multishapes)
str(multishapes)
as.factor(multishapes[,3])
data("multishapes")
df <- multishapes[, 1:2]
dim(df)
par(mfrow=c(1,1))
plot(df, col=c("red","blue","green","black","purple","pink")[multishapes[, 3]])
set.seed(123)
fviz_nbclust(df, kmeans, nstart = 25, iter.max = 200, method = "wss") +
  labs(subtitle = "Elbow method")
km.res <- kmeans(df, 5, nstart = 25)
km.res
fviz_cluster(km.res, df, geom = "point",
             ellipse= FALSE, show.clust.cent = FALSE,
             palette = "jco", ggtheme = theme_classic())

install.packages("fpc")
library(fpc)
install.packages("dbscan")
library(dbscan)
dev.off()
# Compute DBSCAN using fpc package
set.seed(123)

dbscan::kNNdistplot(df, k = 3)
abline(h = 2.2, lty = 2) # nerde keskin yukselis onun oldugu yerden kesikli cizgi  icin

db <- fpc::dbscan(df, eps = 2.2, MinPts = 3) 

print(db)

#----------------------
dbscan::kNNdistplot(df, k = 10) 
abline(h = 2.2, lty = 2) 

db <- fpc::dbscan(df, eps = 2.2, MinPts = 10) #

print(db) 
#----------------------
dbscan::kNNdistplot(df, k = 5)
abline(h = 2.2, lty = 2) 

db <- fpc::dbscan(df, eps = 2.2, MinPts = 5) 

print(db) 


#----------------------
dbscan::kNNdistplot(df, k = 5)
abline(h = 1.0, lty = 2) 

db <- fpc::dbscan(df, eps = 1.0, MinPts = 5) 
print(db) #1 gurultu yakalad??


#-------------------------
# Plot DBSCAN results
fviz_cluster(db, data = df, stand = FALSE,
             ellipse = FALSE, show.clust.cent = FALSE,
             geom = "point",palette = "jco", ggtheme = theme_classic())





library(fpc)
library(dbscan)
library(factoextra)


df_pca <- as.data.frame(data.pca$x[, 1:2])


head(df_pca)
plot(df_pca, main = "PCA Sonras?? ??lke Verileri (PC1 vs PC2)")




dbscan::kNNdistplot(df_pca, k = 3)


abline(h = 0.6, lty = 2, col = "red") 



set.seed(123)

.
db <- fpc::dbscan(df_pca, eps = 0.6, MinPts = 3)


print(db)


# fviz_cluster ile DBSCAN sonu??lar??n?? g??rselle??tirme
fviz_cluster(db, data = df_pca, stand = FALSE,
             ellipse = FALSE,
             show.clust.cent = FALSE,
             geom = "point",
             palette = "jco",
             ggtheme = theme_classic(),
             main = "DBSCAN Kumeleme (eps = 0.6, MinPts = 5)")

# 3. Sonucu g??rselle??tir
fviz_cluster(db, data = df_pca, geom = "point")

#---------------------



# dbcv() fonksiyonu g??r??lt??y?? kendisi ay??rabilir
library(fpc)
validation_score <- dbcv(df_pca, db$cluster)
print(validation_score) 
# Not: DBCV'nin yorumu biraz daha karma????kt??r, ancak farkl?? eps/MinPts 
# kombinasyonlar??n?? kar????la??t??rmak i??in kullan??labilir.





df_pca <- as.data.frame(data.pca$x[, 1:2])

head(df_pca)
plot(df_pca, main = "PCA Sonras?? ??lke Verileri (PC1 vs PC2)")



dbscan::kNNdistplot(df_pca, k = 2)

abline(h = 0.7, lty = 2, col = "red") 


set.seed(123)


db <- fpc::dbscan(df_pca, eps = 0.7, MinPts = 2)

# Modelin ??zet sonu??lar??n?? g??relim
print(db)


# fviz_cluster ile DBSCAN sonu??lar??n?? g??rselle??tirme
fviz_cluster(db, data = df_pca, stand = FALSE,
             ellipse = FALSE,
             show.clust.cent = FALSE,
             geom = "point",
             palette = "jco",
             ggtheme = theme_classic(),
             main = "DBSCAN Kumeleme (eps = 0.6, MinPts = 5)")

# 3. Sonucu g??rselle??tir
fviz_cluster(db, data = df_pca, geom = "point")
#---------------------

# dbcv() fonksiyonu g??r??lt??y?? kendisi ay??rabilir
library(fpc)
validation_score <- dbcv(df_pca, db$cluster)
print(validation_score) 




###################################
#### Density-Based Clustering
###################################
library(factoextra)
summary(multishapes)
head(multishapes)
str(multishapes)
as.factor(multishapes[,3])
data("multishapes")
df <- clean_data
dim(df)
par(mfrow=c(1,1))
plot(df, col=c("red","blue","green","black","purple","pink")[multishapes[, 3]])
set.seed(123)
fviz_nbclust(df, kmeans, nstart = 25, iter.max = 200, method = "wss") +
  labs(subtitle = "Elbow method")
km.res <- kmeans(df, 5, nstart = 25)
km.res
fviz_cluster(km.res, df, geom = "point",
             ellipse= FALSE, show.clust.cent = FALSE,
             palette = "jco", ggtheme = theme_classic())

install.packages("fpc")
library(fpc)
install.packages("dbscan")
library(dbscan)
dev.off()
# Compute DBSCAN using fpc package
set.seed(123)

dbscan::kNNdistplot(df, k = 3)
abline(h = 12, lty = 2) # nerde keskin yukselis onun oldugu yerden kesikli cizgi  icin

db <- fpc::dbscan(df, eps = 12, MinPts = 3) 

print(db)

#----------------------
dbscan::kNNdistplot(df, k = 10) #ky?? 10 yap
abline(h = 2.2, lty = 2) # nerde keskin yukselis onun oldugu yerden kesikli cizgi  icin

db <- fpc::dbscan(df, eps = 2.2, MinPts = 10)

print(db) # 5 gurultu yakalad??
#----------------------
dbscan::kNNdistplot(df, k = 5)
abline(h = 2.2, lty = 2) # nerde keskin yukselis onun oldugu yerden kesikli cizgi  icin

db <- fpc::dbscan(df, eps = 2.2, MinPts = 5) 

print(db) #4 gurultu yakalad??


#----------------------
dbscan::kNNdistplot(df, k = 5)#ky?? 10 yapt?? denedi
abline(h = 1.0, lty = 2) 

db <- fpc::dbscan(df, eps = 1.0, MinPts = 5)
print(db) #1 gurultu yakalad??

#----------------------
#-------------------------
# Plot DBSCAN results
fviz_cluster(db, data = df, stand = FALSE,
             ellipse = FALSE, show.clust.cent = FALSE,
             geom = "point",palette = "jco", ggtheme = theme_classic())

#---------son ek scale ile
# 
df_scaled <- scale(clean_data)
df_scaled <- as.data.frame(df_scaled) 

# 1.  D??ST PLOT (??l??eklenmi?? veriyle!)
dbscan::kNNdistplot(df_scaled, k = 5) 

abline(h = 1.8, lty = 2, col = "red")


db_yeni <- fpc::dbscan(df_scaled, eps = 1.8, MinPts = 5) 
print(db_yeni)


fviz_cluster(db_yeni, data = df_scaled, geom = "point")
#----------------