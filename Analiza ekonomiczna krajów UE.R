library(tidyverse)
library(ggrepel)
library(GGally)       
library(fpc)          
library(DataExplorer) 
library(plotly)       
library(FactoMineR)   
library(factoextra)
library(psych)
library(corrplot)
library(pander)
library(ggplot2)
library(viridis)
library(ggplot2)
library(cluster)
library(leaps)

proj <- read.csv2("./data/PROJEKT.csv")
summary(proj)

# Plotting boxplots for various variables
proj %>% 
  ggplot(aes(x = PKB)) +
  geom_boxplot(fill= "lightblue") +
  xlab("GDP") +
  theme_bw()

proj %>% 
  ggplot(aes(x = Zwolnienia.ze.szpitala)) +
  geom_boxplot(fill= "paleturquoise4") +
  xlab("Discharges Due to Cardiac Reasons") +
  theme_bw()

proj %>% 
  ggplot(aes(x = Użycie.Internetu)) +
  geom_boxplot(fill= "lightyellow") +
  xlab("Internet Usage") +
  theme_bw()

proj %>% 
  ggplot(aes(x = Budżet.na.rozwój)) +
  geom_boxplot(fill= "lightpink") +
  xlab("Research and Development Budget") +
  theme_bw()

proj %>% 
  ggplot(aes(x = Produkcja.energii)) +
  geom_boxplot(fill= "lightsalmon1") +
  xlab("Energy Production") +
  theme_bw()

proj %>% 
  ggplot(aes(x = Wydajność.pracowników)) +
  geom_boxplot(fill= "wheat3") +
  xlab("Employee Productivity") +
  theme_bw()

proj %>% 
  ggplot(aes(x = Zarobki.roczne)) +
  geom_boxplot(fill= "lightpink3") +
  xlab("Annual Earnings") +
  theme_bw()

proj %>% 
  ggplot(aes(x = Wskaźnik.konsumpcji)) +
  geom_boxplot(fill= "honeydew3") +
  xlab("Consumption Index") +
  theme_bw()


# Effect 4: Correlation Analysis
proj <- read.csv2("./data/PROJEKT.csv")
proj <- proj[-2]
cor(proj[, 2:8])
corrplot(cor(proj[, 2:8]), order = "hclust", tl.cex = 0.7, col = COL1('Purples'), tl.col = 'black')
cortest.bartlett(cor(proj[, 2:8]), n = nrow(proj))
KMO(cor(proj[, 2:8]))
pr1 <- principal(proj[, 2:8], nfactors = 2, rotate = "none")

# Scree plot
plot(pr1$values, type = "b") 
abline(h = 1, col = "red")

# Variance explained plot
plot(cumsum(pr1$values) / 8, type = "b") 

proj <- proj %>% select(-Kraj)
pr2 <- PCA(proj, graph = FALSE) 
fviz_screeplot(pr2, addlabels = TRUE, barfill = "turquoise4", barcolor = "goldenrod", xlab = "", ylab = "") + theme_bw() 


# Effect 5: Cluster Analysis
proj <- read.csv2("./data/PROJEKT.csv")
proj <- proj[-2]
proj.scaled <- scale(proj[, 2:8]) %>% as.data.frame()

# Hierarchical clustering using Ward's method
clusterboot(proj.scaled, B = 500, clustermethod = hclustCBI, method = "ward.D2", k = 2)
clusterboot(proj.scaled, B = 500, clustermethod = hclustCBI, method = "ward.D2", k = 4)

# Determine the optimal number of clusters
fviz_nbclust(proj.scaled, clara, method = "wss") #2
fviz_nbclust(proj.scaled, clara, method = "silhouette") #2
fviz_nbclust(proj.scaled, clara, method = "gap_stat") #1

# Conclusion: Two clusters are optimal
clara1 <- clara(proj.scaled, k = 2)
fviz_cluster(clara1, data = proj.scaled, repel = TRUE, labelsize = 8, ggtheme = theme_bw())

# According to the clara algorithm, the first dimension has negative values for highly developed countries and positive values for less developed ones.
summary(proj.scaled) 
rownames(proj.scaled) <- proj$Kraj
p <- dist(proj.scaled, method = "euclidean")
hc1 <- hclust(p, method = "ward.D2")

# Dendrogram plot
fviz_dend(hc1, cex = 0.7, lwd = 0.7, main = "Dendrogram of Countries", xlab = "Countries", k = 2, k_colors = c("darkorchid", "mediumaquamarine"), ggtheme = theme_bw(), rect = TRUE)

# The closest countries are Czech Republic and Estonia.
# The distance between these two variables on the plot is the smallest.
# This is based on the dendrogram and the lowest line connecting pairs.
# The dendrogram clearly identifies two groups. We interpret it using the "bottom-up" method.
# We divide our dataset into two groups and interpret which group predominates for each variable.

# Visualization of Countries on the Biplot
cutree(hc1, k = 2)
proj$cluster.w <- cutree(hc1, k = 2) %>% as.factor()
plot_boxplot(proj[, 2:9], by = "cluster.w", geom_boxplot_args = list(fill = "turquoise4", "outlier.color" = "goldenrod"), ggtheme = theme_bw())

pr <- proj %>% select(-Kraj)
rownames(pr) <- proj$Kraj
pc1 <- PCA(pr, quali.sup = 8, graph = FALSE)
fviz_pca_biplot(pc1, habillage = 8, legend = "bottom", col.var = "turquoise4", labelsize = 3)
fviz_pca_ind(pc1, habillage = 8, legend = "bottom", labelsize = 3, addEllipses = TRUE)

# According to Ward's method, unlike the clara algorithm, clustering assigned positive values of dimension 1 to more developed countries.
# The outlined groups are, however, very close to each other.
# Larger deviations can be observed in group 1, while most countries in group 2 have similar results.

# Effect 6: Regression Analysis

proj <- read.csv2("./data/PROJEKT.csv")
proj <- proj[-1]
str(proj)

# Seed for reproducibility
set.seed(20)

# Test set size
p.test <- 0.2

# Randomly selecting rows for the test set
test.ind <- sample.int(nrow(proj), size = trunc(p.test * nrow(proj)))

# Creating test (proj.test) and training (proj.train) sets
proj.test <- proj[test.ind, ]
proj.train <- proj[-test.ind, ]
rs <- regsubsets(PKB ~ ., data = proj, nvmax = 8, method = "seqrep")
plot(rs, scale = "adjr2", col = c("darkgreen", "green4", "palegreen3"), labels = FALSE)
text(x = 1:length(proj), y = par("usr")[3] - 0.45, labels = names(proj), xpd = NA, srt = 18, cex = 0.8)

lm01 <- lm(PKB ~ ., data = proj)
lm01

# Regression Analysis with Dimensions

proj <- read.csv2("./data/PROJEKT.csv")
proj <- proj[c(-1, -2)]
summary(proj)

pr2 <- principal(proj, nfactors = 2, rotate = "none") 
pr2$scores 
df <- as.data.frame(pr2$scores) 
proj <- read.csv2("./data/PROJEKT.csv")
df$y1 = proj$PKB 
df

model1 <- lm(df$y1 ~ df$PC1)
summary(model1)

# Dimension 1 (PC1) explains 90% of the variance in the target variable GDP.
# Model parameters are significant as both p-values are less than 0.05.

model2 <- lm(df$y1 ~ df$PC1 + df$PC2)
summary(model2) 
# The obtained model F statistic is significant and explains 90% of the variance in GDP. However, PC2 turned out to be insignificant.

anova(model1, model2)
# We accept the hypothesis
# H0: Both models are equally useful for predicting the outcome.
# Conclusion: Both models can be used, as they are very similar to each other.

