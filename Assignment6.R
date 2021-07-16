# Phiwokuhle Somdaka
# Correspondence Analysis
# 16 July 2021

# load libraries
library(tidyverse)
library(vegan)

# load data
spe <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/Num_Ecol_R_book_ed1/DoubsSpe.csv")
spe <- dplyr::select(spe, -1) #remove the first column
head(spe, 8) #view data with the first 8 species

# Do the Correspondence Analysis
spe_ca <- cca(spe) # this gives an error because one or more rows sums up to 0

# Find which row is that by first calculating sums for each row
apply(spe, 1, sum)

# ommiting rows equal to 0
spe <- spe[rowSums(spe) > 0,]
head(spe, 8)

# Try again the CA
spe_ca <- cca(spe)
spe_ca

# The more informative summary output
summary(spe_ca)

# Total inertia
round(sum(spe_ca$CA$eig), 5)

# Inertia for the first axis
round(spe_ca$CA$eig[1], 5)

# Inertia for both first and section axis
round(sum(spe_ca$CA$eig[1:2]), 5)

# Fraction of the variance explained by the first two axes
round(sum(spe_ca$CA$eig[1:2]) / sum(spe_ca$CA$eig) * 100, 2) # result in %

# Ordination diagrams
plot(spe_ca, scaling = 1, main = "CA fish abundances - biplot scaling 1")
plot(spe_ca, scaling = 2, main = "CA fish abundances - biplot scaling 2")

# Plotting individual species seperately
require('viridis')
palette(viridis(8))
par(mar = c(4, 4, 0.9, 0.5) + .1, mfrow = c(2, 2))
with(spe, tmp <- ordisurf(spe_ca ~ Satr, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Satr"))
abline(h = 0, v = 0, lty = 3)
with(spe, tmp <- ordisurf(spe_ca ~ Scer, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Scer"))
abline(h = 0, v = 0, lty = 3)
with(spe, tmp <- ordisurf(spe_ca ~ Teso, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Teso"))
abline(h = 0, v = 0, lty = 3)
with(spe, tmp <- ordisurf(spe_ca ~ Cogo, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Cogo"))
abline(h = 0, v = 0, lty = 3)

# a posteriori projection of environmental variables in a CA
env <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/Num_Ecol_R_book_ed1/DoubsEnv.csv")
env <- dplyr::select(env, -1) # remove the first column

# we removed the 8th row in spe, so do it here too
env <- dplyr::slice(env, -8)

# the last plot produced (CA scaling 2) must be active
# scaling 2 is default
(spe_ca_env <- envfit(spe_ca, env, scaling = 2))
plot(spe_ca_env, col = "grey40")
plot(spe_ca_env, p.max = 0.05, col = "red") # plot significant variables with a different colour


# Answers
# The 'bubbles' indicate how abundant a species is. The green lines indicate places
# where the species are going to be most abundant. Satr becomes more abundant on the right 
# hand side of the plot (there is a gradient of increasing Satr abundance from left to right).
# For Scer, most of the species seem to be concentrated on the left side of the plot
# so all the sites in the same area will be heavily influenced by this species. 
# Teso is most abundant in the lower right part of the plot and near the middle, 
# the few sites (e.g 14 and 15) found there will be greatly influenced by the species.
# Cogo is mostly abundant on the bottom right, similar suite of sites affected by Teso
# will be affected by Cogo. Looking at the environmental variables, we could say
# there is a high abundance of Cogo in the bottom right because they need a high oxygen
# concentration (as it is indicated by the arrow that oxygen increases that side). 
# Satr maybe has a preference for a steeper slope and a higher altitude, hence it is 
# most abundant where the slope and altitude arrows point. The area where Scer is 
# present has a high biological oxygen demand, nitrate etc. The red arrows indicate
# variables that are significant, and those are where there is significant correlation
# across space. The arrows in ble do not correlate significantly, their influence is not
# much significant.

# Bird communities data
ybirds.spe <- read.delim ('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/ybirds_spe.txt', row.names = 1)
head(ybirds.spe, 8)

# Do the Correspondence Analysis
ybirds.spe_ca <- cca(ybirds.spe)
ybirds.spe_ca

# The more informative summary output
summary(ybirds.spe_ca)

# Total inertia
round(sum(ybirds.spe_ca$CA$eig), 5)

# Inertia for the first axis
round(ybirds.spe_ca$CA$eig[1], 5)

# Inertia for both first and section axis
round(sum(ybirds.spe_ca$CA$eig[1:2]), 5)

# Fraction of the variance explained by the first two axes
round(sum(ybirds.spe_ca$CA$eig[1:2]) / sum(ybirds.spe_ca$CA$eig) * 100, 2) # result in %

# Ordination diagrams
plot(ybirds.spe_ca, scaling = 1, main = "Bird communities - biplot scaling 1")
plot(ybirds.spe_ca, scaling = 2, main = "Bird communities - biplot scaling 2")

# Plotting individual species seperately (edit this please)
require('viridis')
palette(viridis(8))
par(mar = c(4, 4, 0.9, 0.5) + .1, mfrow = c(2, 2))
with(ybirds.spe, tmp <- ordisurf(ybirds.spe_ca ~ JAY, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "JAY"))
abline(h = 0, v = 0, lty = 3)
with(ybirds.spe, tmp <- ordisurf(ybirds.spe_ca ~ MLB, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "MLB"))
abline(h = 0, v = 0, lty = 3)
with(ybirds.spe, tmp <- ordisurf(ybirds.spe_ca ~ PGW, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "PGW"))
abline(h = 0, v = 0, lty = 3)
with(ybirds.spe, tmp <- ordisurf(ybirds.spe_ca ~ SWP, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "SWP"))
abline(h = 0, v = 0, lty = 3)

# a posteriori projection of environmental variables in a CA
ybirds.env <- read.delim ('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/ybirds_env.txt', row.names = 1)
ybirds.env <- dplyr::select(ybirds.env, -1, -2) # remove the first two columns

# the last plot produced (CA scaling 2) must be active
# scaling 2 is default
(ybirds.spe_ca_env <- envfit(ybirds.spe_ca, ybirds.env, scaling = 2))
plot(ybirds.spe_ca_env, col = "grey40")
plot(ybirds.spe_ca_env, p.max = 0.05, col = "red")

# Explaining the biplot with reference to environmental variables
# A few bird species (for example ALA, WRN and VRF) dominate the upper left part of
# the biplot where you find sites 40 to 50. This area has a high ground cover and exposure.
# Thus only a few bird species can dominate an area  with little to no trees (not enough habitats).
# On the lower left part of the biplot, bird species are getting clustered on the sites (20 to 40).
# In that area, the Mean DBH is high, meaning there are a lot of trees for birds to
# build nests and the conifer percentage increases, the food crops of conifers are essential
# food sources for some birds. On the bottom right of the biplot, more bird species 
# dominate on the sites 15 to 20. There, the tree density is high and there is a variety
# of tree species (tree species diversity) for bird habitats. The total foliage volume
# is high meaning birds can drink the water collected on leaves. On the last part, top right,
# it is where most birds are found, in sites 1 to 15. In that area, there is also a high
# shrub cover and dense shrubs provide great shelter for birds and if thorny, can deter predators.

# Alpine plant communities data
aravo_spe <- read.csv("aravo_spe.csv") # downloaded the species dataset in my computer
aravo_spe <- dplyr::select(aravo_spe, -1)
head(ybirds.spe, 8)

# Do the Correspondence Analysis
aravo.spe_ca <- cca(aravo_spe)
aravo.spe_ca

# The more informative summary output
summary(aravo.spe_ca)

# Total inertia
round(sum(aravo.spe_ca$CA$eig), 5)

# Inertia for the first axis
round(aravo.spe_ca$CA$eig[1], 5)

# Inertia for both first and section axis
round(sum(aravo.spe_ca$CA$eig[1:2]), 5)

# Fraction of the variance explained by the first two axes
round(sum(aravo.spe_ca$CA$eig[1:2]) / sum(aravo.spe_ca$CA$eig) * 100, 2) # result in %

# Ordination diagrams
plot(aravo.spe_ca, scaling = 1, main = "Alpine plants - biplot scaling 1")
orditorp(aravo.spe_ca, display = 'species')
orditorp(aravo.spe_ca, display = 'sites')

plot(aravo.spe_ca, scaling = 2, main = "Alpine plants- biplot scaling 2")
orditorp(aravo.spe_ca, display = "species")
orditorp(aravo.spe_ca, display = "sites")


# Plotting individual species seperately (edit this please)
require('viridis')
palette(viridis(8))
par(mar = c(4, 4, 0.9, 0.5) + .1, mfrow = c(2, 2))
with(aravo_spe, tmp <- ordisurf(aravo.spe_ca ~ Heli.sede, bubble = 3,
                                 family = quasipoisson, knots = 2, col = 6,
                                 display = "sites", main = "Heli.sede"))
abline(h = 0, v = 0, lty = 3)
with(aravo_spe, tmp <- ordisurf(aravo.spe_ca ~ Care.foet, bubble = 3,
                                 family = quasipoisson, knots = 2, col = 6,
                                 display = "sites", main = "Care.foet"))
abline(h = 0, v = 0, lty = 3)
with(aravo_spe, tmp <- ordisurf(aravo.spe_ca ~ Fest.laev, bubble = 3,
                                 family = quasipoisson, knots = 2, col = 6,
                                 display = "sites", main = "Fest.laev"))
abline(h = 0, v = 0, lty = 3)
with(aravo_spe, tmp <- ordisurf(aravo.spe_ca ~ Kobr.myos, bubble = 3,
                                 family = quasipoisson, knots = 2, col = 6,
                                 display = "sites", main = "Kobr.myos"))
abline(h = 0, v = 0, lty = 3)

# a posteriori projection of environmental variables in a CA
aravo_env <- read.csv ("aravo.env.csv") #downloaded in my computer
aravo_env <- dplyr::select(aravo_env, -1, -6) # remove the first two columns

# the last plot produced (CA scaling 2) must be active
# scaling 2 is default
(aravo.spe_ca_env <- envfit(aravo.spe_ca, aravo_env, scaling = 2))
plot(aravo.spe_ca_env, col = "grey40")
plot(aravo.spe_ca_env, p.max = 0.05, col = "red")

# Explaining the biplot with reference to environmental variables
# On the top left part of the biplot, there is a lower proportion of plant species
# in comparison to other parts. The sites there include sites between 49  and 75
# with the variables that mostly influence there being microtopographic landform
# and mean snowmelt date. An early snowmelt date with warming leads to negative population
# growth, driven especially by changes in seedling establishment and seed production. Thus,
# only a few species can dominate those sites. From the bottom left site to the top right site,
# there is a gradual increase in the number of sites and species , with the variables
# affecting the right side being slope and physical disturbance.
