# Phiwokuhle Somdaka
# Correlation and Association matrices
# 06 July 2021


# load libraries

library(tidyverse)
library(Hmisc) #for rcorr()
library(ade4)
library(vegan)
library(gclus)
library(cluster)
library(FD)
library(BioStatR)

# load data
env <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/Num_Ecol_R_book_ed1/DoubsEnv.csv")
head(env, 1) #view the first row of the data

# remove the first column of the data
env <- dplyr::select(env, -1)
head(env)

# calculating correlation
env_pearson <- round(cor(env), 2) # method Pearson not specified as it is the default

# to see the associated p-values
rcorr(as.matrix(env))

# Answers A
1.
# reorder the variables before plotting
env_o <- order.single(env_pearson)

dev.new(title = "Linear correlation matrix", width = 9, height = 9)
op <- par(mfrow = c(1,1), pty = "s")
pairs(env[,env_o], lower.panel = panel.smooth, upper.panel = panel.cor,
      diag.panel = panel.hist, main = "Pearson Correlation Matrix")
par(op)



# 2. Positive: 0.97 between ammonium and phosphate concentrations & 0.99 between
# mean minimum discharge and distance from the source.
# Negative: -0.94 between altitude and distance from the source & -0.87 between 
# mean minimum discharge and altitude.

# association between species
# load data
spp <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/Num_Ecol_R_book_ed1/DoubsSpe.csv")
spp <- dplyr::select(spp, -1) #remove column 1
head(spp)

# transpose the data
spp_t <- t(spp)

# Answers B
# 1. Transposing is done to study the relationship among objects after the relationship
# among descriptors has been analysed. In this case, we need to analyse the relationship
# between the species and so the columns and interchanged with the rows.
# 2. The rows become the species and and the columns become sites. The minimum value
# is zero, so all values are positive.

# calculate association matrix
spp_assoc1 <- vegdist(spp_t, method = "jaccard")
as.matrix((spp_assoc1))[1:10, 1:10]

spp_assoc2 <- vegdist(spp_t, method = "jaccard", binary = TRUE)
as.matrix((spp_assoc2))[1:10, 1:10]

# Answers C
# 1 .Properties of an association matrix include: the number of rows are equal to
# the number of columns, they are symmetric with the elements in the upper right 
# triangle being equal to those in the lower left triangle. Association matrices
# properties are similar to dissimilarity matrices but different to correlation matrices
# because the latter can have a number less than zero (negative correlation).

# 2. spp_assoc1 takes into account the actual abundances of the species present to 
# calculate association, whereas, spp_assoc2 only accounts for if a species is present
# or absent. The information is not really different.

# 3. A species association matrix shows if two species can survive the same environmental
# conditions or if they can co-exist. Species which occur under the same environmental
# conditions are abundant on the same sites and have a lower value in the matrix.
