# Phiwokuhle Somdaka
# Cluster Analysis
# 01 August 2021

# load libraries
library(tidyverse) 
library(cluster)
library(ggcorrplot)
library(factoextra)
library(vegan)
library(ggpubr)


# load data
SDGs <- read_csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/SDG_complete.csv")
SDGs[1:5, 1:8]

# the parent locations
unique(SDGs$ParentLocation)

# the number of countries
length(SDGs$Location)

# correlation plot matrix
corr <- round(cor(SDGs[3:ncol(SDGs)]), 1)
ggcorrplot(corr, type = 'upper', outline.col = "white", 
           colors = c("navy", "white", "#FC4E07"), 
           lab = TRUE)

SDGs_std <- decostand(SDGs[3:ncol(SDGs)], method = "standardize")
rownames(SDGs_std) <- SDGs$Location # carry location names into output

# different methods to see the number of clusters to be used
# using silhouette analysis
plt1 <- fviz_nbclust(SDGs_std, cluster::pam, method = "silhouette") + theme_grey()

# total within cluster sum of square / elbow analysis
plt2 <- fviz_nbclust(SDGs_std, cluster::pam, method = "wss") + theme_grey()

# gap statistics
plt3 <- fviz_nbclust(SDGs_std, cluster::pam, method = "gap_stat") + theme_grey()

ggarrange(plt1, plt2, plt3, nrow = 3)

# Opting to use three clusters
SDGs_pam <- pam(SDGs_std, metric = "euclidean", k = 3)

fviz_cluster(SDGs_pam, geom = "point", ellipse.type = "convex", palette = c("#FC4E07", "violetred3", "deepskyblue3"), ellipse.alpha = 0.05) +
  geom_text(aes(label = SDGs$Location), size = 2.5)

# Clearer plot to see SA
# scale SA bigger for plotting
SDGs <- SDGs |> 
  mutate(col_vec = ifelse(Location == "South Africa", "black", "grey50"),
         scale_vec = ifelse(Location == "South Africa", 3.5, 2.5))

fviz_cluster(SDGs_pam, geom = "point", ellipse.type = "convex",
             palette = c("#FC4E07", "violetred3", "deepskyblue3"),
             ellipse.alpha = 0.05, pointsize = 2.0) +
  geom_text(aes(label = SDGs$Location), size = SDGs$scale_vec, col = SDGs$col_vec)

# a starplot
fviz_cluster(SDGs_pam, palette = c("#FC4E07", "violetred3", "deepskyblue3"), ellipse.type = "euclid", 
             star.plot = TRUE, repel = TRUE, pointsize = SDGs$scale_vec * 0.8) + # SA, no 147, plotted slightly bigger
  theme_grey()

# silhoullette analysis to check cluster fidelity
fviz_silhouette(SDGs_pam, palette = c("#FC4E07", "violetred3", "deepskyblue3"), ggtheme = theme_grey())

# finding the median value for each cluster
SDGs_centroids <- SDGs |> 
  mutate(cluster = SDGs_pam$clustering) |> 
  group_by(cluster) |> 
  summarise_at(vars(other_1:SDG3.b_5), median, na.rm = TRUE)
SDGs_centroids

# the most representative countries of each cluster
SDGs_pam$medoids

# pairwise scatterplot for the first 7 columns
pairs(SDGs[, 3:10], col = c("#FC4E07", "violetred3", "deepskyblue3")[SDGs_pam$clustering])

# Answers
# 1, Let us try and see
SDGs_pam <- pam(SDGs_std, metric = "euclidean", k = 4)

fviz_cluster(SDGs_pam, geom = "point", ellipse.type = "convex", palette = c("#FC4E07", "violetred3", "deepskyblue3", "BLUE"), ellipse.alpha = 0.05) +
  geom_text(aes(label = SDGs$Location), size = 2.5)
# In the above diagram, countries are further divided to make the clusters more accurate
# in terms of attaining the SDGs.

# 2. In my opinion, it would be 2. Based on plt1, 2 and 3, two of the methods yield 
# 2 is the number of clusters needed (Silhouette and elbow analysis) and I am more inclined to the former. The silhouette
# method computes the average silhouette of observations for different k values. The
# optimal number of clusters k is the one that maximize the average silhouette over a 
# range of possible values of k.

# 3.
SDG <- dplyr::select(SDGs, -1, -2)
SDGs_log <- log1p(SDG)
rownames(SDGs_log) <- SDGs$Location

# distance matrix
SDGs_dist <- vegdist(SDGs_log, method = 'bray', na.rm = TRUE)
SDGs_dist


# calculating ward's algorithm
clust <- agnes(sqrt(SDGs_dist), method = 'ward')

# dendogram
plot(clust, which.plot = 2)

# transform clust
clust.hclust <- as.hclust(clust)
plot(clust.hclust)

# cut cluster
groups <- cutree(clust, k = 3)
groups

# which sample belongs where
clust.hclust$order
group.order <- groups[clust.hclust$order]
group.order
group.in.cluster <- unique(group.order)
group.in.cluster

# display the above in a plot
plot(clust.hclust)
rect.hclust(clust.hclust, border = group.in.cluster, k = 3)
legend('topleft', legend = paste('Cluster', 1:3, pch = 22, col = 1:3
                                 , bty = 'n'))
# pam() visualises the data better (clearer) so I will proceed with it.
# They are not different because in the dendogram, African countries are still
# mostly found in one cluster, with the American and European in one cluster as well.

# 4. The clusters are divided according to how different countries are achieving
# the SDGs. The SDG Index and Dashboard shows a scale from 0 to 100, where 0 is 
# the worst level of implementation and 100 means full compliance with the SDG
# targets. Most African countries are found in one cluster (on the left). This is 
# because they share lacks in  all aspects, especially poverty, hunger, education,
# peace and justice.For example, the scales of some African countries in the cluster
# are: Central African Republic (26.1), Liberia (30.5), the Democratic Republic of
# the Congo (31.3) and Niger (31.4). In the far right, most American and European 
# countries are found in the cluster. These countries are mostly found in the top 
# 20 or 30. for example, countries like the United States is in the 25th place with 72.7,
# Canada is 13th with 76.8, Australia is 20th with 74.5 and the United Kingodm is 10th 
# with 78.1. These are well-developed countries where the economy allows for the implementation
# of the SDGs. In the middle cluster, you'll find a mix of countries, from some developing
# African countries (e.g South Africa), Asian and all other continents. For example, you will
# find , Spain ranks 30th with 72.2, Chile is 42nd with 67.2, Mexico is 56th with 63.4, Peru
# is 81st with 58.4 and Colombia is 91st (57.2).

# 5. South Africa is one of the few countries in Africa that are developing. With its
# development, South Africa launched the National Development Plan in 2012 - to eliminate
# poverty and reduce inequality by 2030. The plan aims to achieve this through uniting South
# Africans, unleashing the energies of its citizens, growing an inclusive economy, building
# capabilities, enhancing the capabilities of state, and promoting leadership and partnerships
# throughout society. This plan has helped the country stand out from some African countries
# which still fall behind due to political and economic issues. However, the country is still
# not performing at its best due to issues like political corruption and inequality. 
# South Africa is, thus, found in the edge of under-developed and developed countries.
