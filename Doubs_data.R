# Working with Doubs data
# Phiwokuhle Somdaka
# 03 July 2021

# load libraries
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
spe <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/Num_Ecol_R_book_ed1/DoubsSpe.csv")
spe <- dplyr::select(spe, -1) # removing column 1

dim(spe) #How many rows and columns in the data

# Alpha diversity: species richness
spe_richness <- diversityresult(spe, index = 'richness', method = 'each site')

# plot alpha diversity
ggplot(data = spe_richness, (aes(x = 1:30, y = richness))) + # x=1:58 means that use the rows inbetween those numbers
  geom_line() + xlab("Section") + ylab("Species richness")

# let us convert this abundance data to presence-absence data to calculate Sorensen
spe_pa <- decostand(spe, method = "pa", )

# Calculate Sorensen dissimilarity
sor <- vegdist(spe_pa, binary = TRUE)
sor_df <- round(as.matrix(sor), 4) # make a matrix
sor_df[1:20, 1:20]

# plotting Sorensen dissimilarity
sor <- sor_df[1, 1:30] # cutting the data to show only the first row
Section_number <- (1:30)
Values <- sor
Soren <- data.frame(Section_number, Values) # creating a dataframe to make a plot, so that I can have x and y axis

ggplot(data = Soren, (aes(x = Section_number, y = Values))) +
  geom_line(col = 'salmon') +
  xlab("Section") + ylab("Dissimilarity Index")
