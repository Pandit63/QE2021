# Phiwokuhle Somdaka
# Working with Doubs data Assignment
# 05 July 2021


# libraries
library(tidyverse)
library(betapart)
library(vegan)
library(gridExtra)
library(BiodiversityR)
library(grid)
library(gridBase)
library(tidyr)
library(readr)

# load data
Doubs_spe <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/Num_Ecol_R_book_ed1/DoubsSpe.csv")
Doubs_spe <- dplyr::select(Doubs_spe, -1)

# View how many rows and columns
dim(Doubs_spe)

# 1. The dataset is an abundance dataset consisting of 27 fish species and in 30 sites.

# 2. Bray-Curtis dissimilarity

# 3. 
#First remove empty row 8
Doubs_spe <- Doubs_spe[-8,]

# Calculate Bray-Curtis
Doubs_spe_bc <- vegdist(Doubs_spe, method = "bray", binary = FALSE, diag = TRUE)
Doubs_spe_bc1 <- round(as.matrix(Doubs_spe_bc), 4)

# 4. Sites with more or less the same species present have a lower dissimilarity,
# for example, site 2 and 3 have the first three species present with site 3 only
# having one other species that site 2 does not, tus their dissimilarity is low
# (0.1429). The more difference there is in the number of species present between 
# two sites, the more dissimilar the sites are. For example, sites 1 and 19 do not
# share any common species and site 19 has more species than site 1, thus their
# dissimilarity is a perfect 1.

# 5. The less further sites are from each other, the more dissimilar they are.

# 6. 
Doubs_spe_ds <- as.data.frame(as.matrix(Doubs_spe_bc))

Doubs <- Doubs_spe_ds[1:29, 1]
Site <- (1:29)
Data <- data.frame(Site, Doubs)

ggplot(data = Data , (aes(x = Site, y = Doubs))) + # x=1:58 means that use the rows inbetween those numbers
geom_line() + xlab("Sites") + ylab("Bray-Curtis dissimilarity") + ggtitle("Dissimilarity between fish sites")

# 7. The further sites are from each other, the less fish species they share. This 
# may be because of different environmental factors acting upon the sites as you move
# further away from the starting point. Thus, dissimilarity increases from site 1 to 30.

# 8. 
# Convert data to presence-absence data
Doubs_spe_pa <- decostand(Doubs_spe, method = "pa", )

# 9
#Calculate Sorensen dissimilarity
Doubs_spe_sor <- vegdist(Doubs_spe_pa, binary = TRUE)
Doubs_sor_df <- round(as.matrix(Doubs_spe_sor), 4) # make a matrix
Doubs_sor_df[1:20, 1:20]

# plotting Sorensen dissimilarity
sor <- Doubs_sor_df[1, 1:29] # cutting the data to show only the first row
Site <- (1:29)
Values <- sor
Soren <- data.frame(Site, Values) # creating a dataframe to make a plot, so that I can have x and y axis

ggplot(data = Soren, (aes(x = Site, y = Values))) +
  geom_line(col = 'salmon') +
  xlab("Sites") + ylab("Dissimilarity Index") + ggtitle("Dissimilarity in fish sites")

# From site 1 to 6, the dissimlarity increases because with each sites there, there
# is a different kind of species added and others from a previous site being removed.
# Site 19 to 28 have more or less the same number of species presence and thus their
# graph maintain a straight line.
