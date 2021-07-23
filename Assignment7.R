# Phiwokuhle Somdaka
# PCA, PCA, PCoA and nMDS
# 23 July 2021

# load libraries
library(vegan)
library(tidyverse)

# load data
ls()
data(mite.env)
head(mite.env)
mite.env <- dplyr::select(mite.env, -3, -4, -5)


# do the PCA
mite.env_pca <- rda(mite.env, scale = TRUE)
mite.env_pca

# extracting the first eigenvalue
round(mite.env_pca$CA$eig[1], 3)

# Total inertia (total variables)
sum(mite.env_pca$CA$eig)

# sum of the variation produced by the first PCA in percentage
round(mite.env_pca$CA$eig[1] / sum(mite.env_pca$CA$eig) * 100, 1)

summary(mite.env_pca)

# Graphical representations of ordinations
biplot(mite.env_pca, scaling = 1, main = "PCA scaling 1", choices = c(1, 2))
biplot(mite.env_pca, scaling = 2, main = "PCA scaling 2", choices = c(1, 2))

# creating biplots using clean.pca()
source("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/Num_Ecol_R_book_ed1/cleanplot.pca.R")
cleanplot.pca(mite.env_pca, scaling = 1)
cleanplot.pca(mite.env_pca, scaling = 2)

# Do the Correspondence Analysis
# load data
data(mite)
mite_ca <- cca(mite)
mite_ca

# The more informative summary output
summary(mite_ca)

# Total inertia
round(sum(mite_ca$CA$eig), 3)

# Inertia for the first axis
round(mite_ca$CA$eig[1], 3)

# Inertia for first to sixth axis
round(sum(mite_ca$CA$eig[1:6]), 3)

# Fraction of the variance explained by the first sixth axes
round(sum(mite_ca$CA$eig[1:6]) / sum(mite_ca$CA$eig) * 100, 2) # result in %

# Ordination diagrams
plot(mite_ca, scaling = 1, main = "Mite data - biplot scaling 1")
plot(mite_ca, scaling = 2, main = "Mite data - biplot scaling 2")

# Plotting individual species separately
require('viridis')
palette(viridis(8))
par(mar = c(4, 4, 0.9, 0.5) + .1, mfrow = c(2, 2))
with(mite, tmp <- ordisurf(mite_ca ~ LCIL, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "LCIL"))
abline(h = 0, v = 0, lty = 3)
with(mite, tmp <- ordisurf(mite_ca ~ Trhypch1, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Tryhpch1"))
abline(h = 0, v = 0, lty = 3)
with(mite, tmp <- ordisurf(mite_ca ~ PLAG2, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "PLAG2"))
abline(h = 0, v = 0, lty = 3)
with(mite, tmp <- ordisurf(mite_ca ~ Trimalc2, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Trimalc2"))
abline(h = 0, v = 0, lty = 3)

# a posteriori projection of environmental variables in a CA
data("mite.env")
mite.env <- dplyr::select(mite.env, -3, -4, -5)

(mite_ca_env <- envfit(mite_ca, mite.env, scaling = 2))
plot(mite_ca_env, col = "grey40")
plot(mite_ca_env, p.max = 0.05, col = "red")

# Results and discussion

# In the Principal component analysis, the two variables (Substrate density and
# Water Content of substrate density) do not go over the radius of the circle, so they 
# do not contribute more than average. There is a gradient from the left to right
# beginning with most sites between 1 and 30 having a low value on the Water Content 
# of the Substrate. Sites like 3, 4, 6, 9, 15, 26 etc having a high value in Substrate
# density. Most sites between 50 and 60 have a high value in Water Content of Substrate.
# Some sites between 40 and 50 and between 60 and 70 have a low value in Substrate Density
# Water Content of Substrate and Substrate density are positively correlated.

# In the Correspondence analysis, the first axis opposes most sites from 1 to 30 to 
# most sites from 31 to 70. Many species appear close to the first 20 or so sites.
# The second axis contrasts sites in a similar suite to axis 1. The scaling 2 biplot 
# shows how group of species are distributed among sites. The species PLAG2, NCOR and
# Trimalc are found in most of the last 20 sites, while most species are found in the
# first 20 sites.

# ANALYSIS ON THE DUNE DATA
# load data
data(dune)
?dune

dune_nmds <- metaMDS(dune, distance = "jaccard")
dune_nmds

# calculating stress
dune_nmds$stress

# Ordination diagrams
par(mfrow = c(2, 2))
stressplot(dune_nmds, main = "Shepard plot") 
ordiplot(dune_nmds, type = "t", cex = 1.5, main = paste0("nMDS stress = ", round(dune_nmds$stress, 2)))
gof = goodness(dune_nmds)
plot(dune_nmds, type = "t", main = "Goodness of fit")
points(dune_nmds, display = "sites", cex = gof * 200)

# Ordination plots from scratch
pl <- ordiplot(dune_nmds, type = "none", main = "nMDS Dune data ")
points(pl, "sites", pch = 21, cex = 1.75, col = "grey80", bg = "grey80")
points(pl, "species", pch = 21, col = "turquoise", arrows = TRUE)
text(pl, "species", col = "blue4", cex = 0.9)
text(pl, "sites", col = "red4", cex = 0.9)

# fit response surfaces and project environmental drivers
require('viridis')
palette(viridis(8))
par(mar = c(4, 4, 0.9, 0.5) + .1, mfrow = c(2, 2))
with(dune, tmp <- ordisurf(dune_nmds ~ Scorautu, bubble = 3,
                           family = quasipoisson, knots = 2, col = 6,
                           display = "sites", main = "Scrorautu"))
abline(h = 0, v = 0, lty = 3)
with(dune, tmp <- ordisurf(dune_nmds ~ Trifrepe, bubble = 3,
                           family = quasipoisson, knots = 2, col = 6,
                           display = "sites", main = "Trifrepe"))
abline(h = 0, v = 0, lty = 3)
with(dune, tmp <- ordisurf(dune_nmds ~ Bracruta, bubble = 3,
                           family = quasipoisson, knots = 2, col = 6,
                           display = "sites", main = "Bracruta"))
abline(h = 0, v = 0, lty = 3)
with(dune, tmp <- ordisurf(dune_nmds ~ Elymrepe, bubble = 3,
                           family = quasipoisson, knots = 2, col = 6,
                           display = "sites", main = "Elymrepe"))
abline(h = 0, v = 0, lty = 3)

# a posteriori projection of environmental variables in a CA
data("dune.env")
dune.env <- dplyr::select(dune.env, -3, -4)

(dune_nmds_env <- envfit(dune_nmds, dune.env)) 
plot(dune_nmds_env, col = "grey40")
plot(dune_nmds_env, p.max = 0.05, col = "red")
     
# Calculate dissimilarity that is suitable
dune_bray <- vegdist(dune)
     
# Do the PCoA
dune_pcoa <- capscale(dune ~ 1, distance = "bray")
dune_pcoa
     
# percentage inertia explained by the first three axes
round(sum(dune_pcoa$CA$eig[1:3]) / sum(dune_pcoa$CA$eig) * 100, 2)
     
# Ordination diagrams
plot(dune_pcoa, scaling = 1, main = "PCoA Dune data- biplot scaling 1")
plot(dune_pcoa, scaling = 2, main = "PCoA Dune data - biplot scaling 2")
     
# We can build plots from scratch
pl1 <- ordiplot(dune_pcoa, type = "none", scaling = 1, main = "PCoA ?? - biplot scaling 1")
points(pl1, "sites", pch = 21, cex = 1.75, col = "grey80", bg = "grey80")
points(pl1, "species", pch = 21, col = "turquoise", arrows = TRUE)
text(pl1, "species", col = "blue4", cex = 0.9)
text(pl1, "sites", col = "red4", cex = 0.9)
     
pl2 <- ordiplot(dune_pcoa, type = "none", scaling = 2, main = "PCoA ??? - biplot scaling 2")
points(pl2, "sites", pch = 21, cex = 1.75, col = "grey80", bg = "grey80")
points(pl2, "species", pch = 21, col = "turquoise", arrows = TRUE)
text(pl2, "species", col = "blue4", cex = 0.9)
text(pl2, "sites", col = "red4", cex = 0.9)
     
# fit response surfaces
require('viridis')
palette(viridis(8))
par(mar = c(4, 4, 0.9, 0.5) + .1, mfrow = c(2, 2))
with(dune, tmp <- ordisurf(dune_nmds ~ Scorautu, bubble = 3,
                           family = quasipoisson, knots = 2, col = 6,
                           display = "sites", main = "Scrorautu"))
abline(h = 0, v = 0, lty = 3)
with(dune, tmp <- ordisurf(dune_nmds ~ Trifrepe, bubble = 3,
                           family = quasipoisson, knots = 2, col = 6,
                           display = "sites", main = "Trifrepe"))
abline(h = 0, v = 0, lty = 3)
with(dune, tmp <- ordisurf(dune_nmds ~ Bracruta, bubble = 3,
                           family = quasipoisson, knots = 2, col = 6,
                           display = "sites", main = "Bracruta"))
abline(h = 0, v = 0, lty = 3)
with(dune, tmp <- ordisurf(dune_nmds ~ Elymrepe, bubble = 3,
                           family = quasipoisson, knots = 2, col = 6,
                           display = "sites", main = "Elymrepe"))
abline(h = 0, v = 0, lty = 3)

# a posteriori projection of environmental variables in a CA
data("dune.env")
dune.env <- dplyr::select(dune.env, -3, -4)

(dune_pcoa_env <- envfit(dune_pcoa, dune.env, scaling = 2)) 
plot(dune_pcoa_env, col = "grey40")
plot(dune_pcoa_env, p.max = 0.05, col = "red")

# The stress value of the data is 0.12, less than 0.2, meaning that the ordination
# is reliable. On the Shepard plot, the amount of scatter up and down the red line is
# low, signifying that indeed the stress value is less than 0.2. he larger circles are
# representing plots that don’t have as strong as a fit with the original dissimilarity
# matrix, for example around site 19. 
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     

     