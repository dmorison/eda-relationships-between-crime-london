library(stats)
library(reshape2)
library(ggplot2)

df <- read.csv("Data/crime-types.csv")

# summary of data
summary(df)

# prepare data for boxplots
df2 <- melt(df, id.vars = "LSOA_name")

# boxplot of crime types
bp1 <- ggplot(df2, aes(x = variable, y = value))
bp1 <- bp1 + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 13, face = "bold")) +
  labs(x = "Crime type")
print(bp1)
# excluding outliers
bp2 <- ggplot(df2, aes(x = variable, y = value))
bp2 <- bp2 + geom_boxplot(outlier.colour = NA) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 13, face = "bold")) +
  coord_cartesian(ylim = c(0, 125)) +
  labs(x = "Crime type")
print(bp2)

# find number of zeros in each crime
df3 <- df[, -1]
df.zeros <- apply(df3, 2, function(x) {sum(x == 0)})
print(df.zeros)
# find the upper inner fence value for each crime
maxval <- apply(df3, 2, function(x) { (IQR(x) * 1.5) + quantile(x, probs = 0.75) })
print(maxval)
# find number of outliers in each crime
df4 <- data.frame(matrix(ncol = 15, nrow = 0))
maxvals <- NULL
num.outliers <- NULL
num.zeros <- NULL
for (i in 2:15) {
  dfsub <- df[which(df[, i] > maxval[[i - 1]]), ]
  print(paste0(colnames(df)[i], ": ", nrow(dfsub)))
  maxvals <- c(maxvals, maxval[[i - 1]])
  num.outliers <- c(num.outliers, nrow(dfsub))
  num.zeros <- c(num.zeros, df.zeros[[i - 1]])
  df4 <- rbind(df4, dfsub)
  print(nrow(df4))
}
# create table
outliers.table <- data.frame(cbind(colnames(df)[2:15], maxvals, num.outliers, num.zeros))
colnames(outliers.table) <- c("Crime type", "Upper inner fence value", "Number outliers", "Number zeros")
print(outliers.table)
write.csv(outliers.table, file = "Output/outliers-table-full.csv", row.names = FALSE)

# find variable describing the most variance
df3.var <- apply(df3, 2, var)
print(df3.var)
max(df3.var)
which(df3.var == max(df3.var))
min(df3.var)
which(df3.var == min(df3.var))

# histograms of least and most variance variables
h1 <- ggplot(df, aes(df$anti_social_behaviour))
h1 <- h1 + geom_histogram(breaks = seq(0, 500, by = 10), col = "black", fill = "light blue") +
  xlim(c(0, 500)) +
  ylim(c(0, 1000)) +
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 13, face = "bold")) +
  labs(x = "anti social behaviour")
print(h1)

h2 <- ggplot(df, aes(df$possession_of_weapons))
h2 <- h2 + geom_histogram(breaks = seq(0, 15, by = 1), col = "black", fill = "green") +
  xlim(c(0, 15)) +
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 13, face = "bold")) +
  labs(x = "possession of weapons")
print(h2)

# correlation matrix
cor.m <- cor(df3)
print(cor.m)
cor.test(df3$criminal_damage_and_arson, df3$violence_and_sexual_offences)

# PCA
pca <- prcomp(df3, center = TRUE, scale. = TRUE)
print(pca)
summary(pca)
# flip values to positives
pca2.1 <- pca
pca2.1$rotation <- -pca2.1$rotation
pca2.1$x <- -pca2.1$x
print(pca2.1)
summary(pca2.1)

# cumulative variance plot of components
plot(pca, type = "lines")
# PCA basic biplot
biplot(pca2.1, scale = 0)

# PCA scatter plot
pc12 <- data.frame(pca2.1$rotation[, 1:2])
pc12$crimes <- rownames(pc12)
pt1 <- ggplot(pc12, aes(x = PC1, y = PC2))
pt1 <- pt1 + geom_point(size = 3) +
  geom_text(aes(label = crimes), vjust = 1) +
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 13, face = "bold"))
print(pt1)

# k-means on principal components 1 and 2
set.seed(7)
pc.km <- kmeans(pc12[, 1:2], 3, nstart = 100)
# ggplot clusters
pt.km <- ggplot(pc12, aes(x = PC1, y = PC2))
pt.km <- pt.km + geom_point(aes(colour = factor(pc.km$cluster)), size = 3) +
  scale_colour_manual(values = c("red", "blue", "green")) +
  geom_text(aes(label = crimes), vjust = 1) +
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 13, face = "bold")) +
  labs(colour = "Cluster")
print(pt.km)

# finding the areas with the highest crime recording for each type of crime
crimes <- colnames(df3)
hc <- NULL
for (i in 2:(length(crimes) + 1)) {
  area <- df[which(df[, i] == max(df[, i])), ]
  hc <- rbind(hc, data.frame(area))
}
print(hc)
write.csv(hc, file = "Output/areas-with-max-crime-value-per-type.csv", row.names = FALSE)
