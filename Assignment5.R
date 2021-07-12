# Phiwokuhle Somdaka
# Principal Component Analysis
# 12 July 2021


# load libraries
library(vegan)
library(tidyverse)

# load data
env <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/Num_Ecol_R_book_ed1/DoubsEnv.csv")
env <- dplyr::select(env, -1)
head(env)

# do the PCA
env_pca <- rda(env, scale = TRUE) # Scale = True calls for the standardization
# of the variables and creates a correlation matrix.
env_pca

# extracting the first eigenvalue
round(env_pca$CA$eig[1], 3)

# Total inertia (total variables)
sum(env_pca$CA$eig)

# sum of the variation produced by the first PCA in percentage
round(env_pca$CA$eig[1] / sum(env_pca$CA$eig) * 100, 1)

# Question A
# PCA is not a statistical test, but a heuristic procedure: it aims at representing the 
# major features of the data along a reduced number of axes (hence, the expression 
# "ordination in reduced space"). Usually, the user examines the eigenvalues, and 
# decides how many axes are worth representing and displaying on the basis of the 
# amount of variance explained. The decision can be completely arbitrary (for 
# instance, interpret the number of axes necessary to represent 75% of the variance 
# of the data), or assisted by one of several procedures proposed to set a limit 
# between the axes that represent interesting variation of the data and axes that 
# merely display the remaining, essentially random variance.

summary(env_pca)

# Graphical representations of ordinations
biplot(env_pca, scaling = 1, main = "PCA scaling 1", choices = c(1, 2))
biplot(env_pca, scaling = 2, main = "PCA scaling 2", choices = c(1, 2))

# creating biplots using clean.pca()
source("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/Num_Ecol_R_book_ed1/cleanplot.pca.R")
cleanplot.pca(env_pca, scaling = 1)
cleanplot.pca(env_pca, scaling = 2)

# plotting underlying environmental gradients (for altitude and biological oxygen demand)
biplot(env_pca, type = c("text", "points"), col = c("black", "black"))
ordisurf(env_pca ~ bod, env, add = TRUE, col = "turquoise", knots = 1)
ordisurf(env_pca ~ alt, env, add = TRUE, col = "salmon", knots = 1)

# Question B

# bird communities data
ybirds.env <- read.delim ('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/ybirds_env.txt', row.names = 1)
ybirds.env <- dplyr::select(ybirds.env, -1, -2) #removing the first and second column about vegetation type(categorical)
head(ybirds.env)

# Do PCA
ybirds.env_pca <- rda(ybirds.env, scale = TRUE)
ybirds.env_pca

# extracting the second eigenvalue
round(ybirds.env_pca$CA$eig[2], 3)

# Total inertia (total variables)
sum(ybirds.env_pca$CA$eig)

# sum of the variation produced by the second PCA in percentage
round(ybirds.env_pca$CA$eig[2] / sum(ybirds.env_pca$CA$eig) * 100, 1)

summary(ybirds.env_pca)

# Graphical representations of ordinations
biplot(ybirds.env_pca, scaling = 1, main = "PCA scaling 1", choices = c(1, 2))
biplot(ybirds.env_pca, scaling = 2, main = "PCA scaling 2", choices = c(1, 2))

# creating biplots using clean.pca()
source("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/Num_Ecol_R_book_ed1/cleanplot.pca.R")
cleanplot.pca(ybirds.env_pca, scaling = 1)
cleanplot.pca(ybirds.env_pca, scaling = 2)

# plotting underlying environmental gradients (for conifer percentage and canopy height)
biplot(ybirds.env_pca, type = c("text", "points"), col = c("black", "black"))
ordisurf(ybirds.env_pca ~ CP, ybirds.env, add = TRUE, col = "turquoise", knots = 1)
ordisurf(ybirds.env_pca ~ CH, ybirds.env, add = TRUE, col = "salmon", knots = 1)


# The proportion of variance accounted for by the first two axes is 71.23%, and this
# value proves that interpretation of the first pair of axes extracts the most
# important information from the table. 
# In scaling 1 biplot (from the cleanplot), variables that have vectors longer than
# the radius of the circle make a higher contribution than average, these are mean tree 
# DBH, Standard deviation of tree DBH, tree basal area, elevation and foliage height
# diversity index. This scaling 1 biplot shows a gradient from left to right, beginning
# with a group formed by sites 17 to 31 and 33 which display the highest values of 
# tree basal area, standard deviation of tree DBH, foliage height diversity index,
# canopy cover, total foliage volume and canopy height. These sites show the lowest values
# in shrub cover and ground cover. The second group of sites (1 to 16) show the highest
# values in tree density, tree species diversity, secondary tree cover, aspect and
# herb cover, with the lowest values in slope, elevation, conifer percentage and exposure.
# Site 32 to 45 (except 33) show the highest values in slope, elevstion, conifer percentage
# and exposure, with the lowest values in tree density, tree species diversity, secondary
# tree cover, aspect and herb cover. The last group of sites (46 to 50) have the highest
# values in ground cover and shrub cover, with the lowest values in tree basal area, 
# standard deviation of tree DBH, foliage height diversity index, canopy cover, total 
# foliage volume and canopy height.
# The scaling 2 biplot shows that the variables are organized in groups. The lower left
# part of the biplot shows that tree basal area and standard deviation of tree DBH
# are very highly positively correlated and that these two are very highly negatively
# correlated with shrub cover and also negatively (intermediate) correlated with 
# ground cover. Tree basal area and standard deviation of tree DBH are also positively
# correlated with foliage height index, canopy cover, canopy height and total foliage
# volume (these four are strongly positively correlated and negatively correlated with 
# shrub cover and ground cover). The top left part of the biplot shows secondary tree 
# cover, herb cover, aspect, tree density and tree species diversity being strongly
# positively correlated and also positively correlated with tree basal area, standard
# deviation of tree DBH, foliage height index, canopy cover, canopy height, and total
# foliage volume. These top left variables are strongly negatively correlated with
# exposure, elevation, slope and conifer percentage (which are positively correlated 
# with each other). Mean tree DBH is nearly an orthogonal arrow, indicating a correlation 
# close to 0. Shrub cover shows a shorter arrow, indicating it is least important for
# the ordination of the sites in the ordination plane. 


# The sites were found in an elevation range or 1400 to 3700 mm. Sites 1 to 16 are found
# at the lowest elevation from 1400 m to less than 2000 m. Thus, it is not surprising
# that they have the lowest values in correlation as depicted by the biplot. Sites 17 to 
# to 31 follow these with elevations ranging from 2500 m to 3000m, these intermediate values
# of elevation because they are found a little higher than the sites before them.
# Sites 32 to 50 are found at the highest peak of Taiwan, from 3000 upwards, hence
# they have higher elevation values.

# Standard deviation of tree DBH and tree basal area are strongly positively correlated.
# The basal area of a tree is defined as the cross-sectional area (usually in square feet)
# of a single tree at breast height, or 4. feet above ground. DBH is a standard method
# of expressing the diameter of the trunk or bole of a standing tree and is also measured
# 4 feet above the ground. When measuring DBH, the measurement of the circumference of
# the tree at 4 feet is taken and that is divided by 3.14 to find the diameter. With basal
# area, a tree is cut and the diameter is measured directly from the cut tree. These
# variables are similar, thus they will have a strong positive correlation.
# Shrub cover and standard deviation of tree DBH plus basal area have a strong negative
# correlation. Shrubs are small trees (which can or cannot be more than 4ft, enough to
# measure DBH) and these are bound to have smaller circumferences and diameters especially
# at 4 feet. Thus, areas dominated by shrubs will have smaller standard deviations of tree 
# DBH and basal area.
# Conifer percentage and elevation are strongly positively correlated because conifers
# are generally the most common trees at higher elevations. They are adapted to these
# elevations because they have needles instead of trees which help them not lose much water,
# their conical shape helps keep them from falling over in winter because of snow collecting
# on them and they are flexible, keeping them from breaking under heavy loads of snow.

# Alpine plant communities data
aravo <- read.csv("aravo.csv") #here I downloaded the entire aravo dataset in xlsx
# format, and only converted the environmental dataset to csv format which is the one
# I just loaded
aravo <- dplyr::select(aravo, -1, -6) # remove non-numeric columns
head(aravo)

# do the PCA
aravo_pca <- rda(aravo, scale = TRUE)

# extracting the first eigenvalue
round(aravo_pca$CA$eig[1], 3)

# Total inertia (total variables)
sum(aravo_pca$CA$eig)

# sum of the variation produced by the first PCA in percentage
round(aravo_pca$CA$eig[1] / sum(aravo_pca$CA$eig) * 100, 1)

summary(aravo_pca)

# Graphical representations of ordinations
biplot(aravo_pca, scaling = 1, main = "PCA scaling 1", choices = c(1, 2))
biplot(aravo_pca, scaling = 2, main = "PCA scaling 2", choices = c(1, 2))

# creating biplots using clean.pca()
source("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/Num_Ecol_R_book_ed1/cleanplot.pca.R")
cleanplot.pca(aravo_pca, scaling = 1)
cleanplot.pca(aravo_pca, scaling = 2)

# plotting underlying environmental gradients (for physical disturbances and 
# mean snowmelt date)
biplot(aravo_pca, type = c("text", "points"), col = c("black", "black"))
ordisurf(aravo_pca ~ PhysD, aravo, add = TRUE, col = "turquoise", knots = 1)
ordisurf(aravo_pca ~ Snow, aravo, add = TRUE, col = "salmon", knots = 1)

# In scaling 1 biplot, slope inclination and microtopographic landform index have vectors
# longer than the radius, meaning they have a contribution higher than average. Sites vary in 
# their values with some sites (e.g 8, 13, 25, 43 and 46) having the highest values in
# slope inclination and relative south aspect, with lower values in mean snowmelt date. Other sites
# like 45, 50 and 39 have the highest microtopographic landform index with the lowest values in
# physical distubance. Other sites like 59 and 60 have higher values in mean snowmelt date
# with lower values in slope inclination and relative south aspect. The last group of sites
# including 1, 10, 42 and 75 have higher vales in physical disturbances with lower values in 
# microtopographic landform index.
# In the scaling 2 biplot, the lower left of the biplot shows physical disturbance 
# which is negatively correlated to microtopographic landform index and positively 
# correlated to slope inclination and relative south aspect. Bottom right of the 
# biplot is mean snowmelt date which is negatively correlated to slope inclination
# and relative south aspect and positively correlated to microtopographic landform index.
# slope inclination and relative south aspect are strongly positively correlated.

# Slope inclination and relative south aspect are strongly positively correlated
# because aspect denotes the compass direction in which the slope of a mountain faces.
# This means that the aspect of an area depends on its slope.
