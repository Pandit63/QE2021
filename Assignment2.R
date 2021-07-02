# Gamma diversity
# Phiwokuhle Somdaka
# 02 July 2021

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

# data
spp <- read_csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/diversity/SeaweedsSpp.csv") # it was not running using the code given by Prof, I then opted for dowloading the data and reading it from my computer
spp <- dplyr::select(spp, -1) # removing column 1

# True beta diversity: gamma diversity for the region/ alpha diverstit for a
# specific coastal section
true_beta <- data.frame(
  beta = ncol(spp)/ specnumber(spp, MARGIN = 1),
  section_no = c(1:58)
)
# plotting true beta
ggplot(data = true_beta, (aes(x = section_no, y = beta))) +
  geom_line() + xlab("Coastal section, west to east") + ylab("True beta-diversity")


#  Absolute species turnover: subtract alpha diversity for each section from the region's 
# gamma diversity
abs_beta <- data.frame(
  beta = ncol(spp) - specnumber(spp, MARGIN = 1),
  section_no = c(1:58)
)
# Plotting absolute species turnover
ggplot(data = abs_beta, (aes(x = section_no, y = beta))) +
  geom_line() + xlab("Coastal section, west to east") + ylab("Absolute beta-diversity")


# Calculating turnover and nestednessness resultant
# First decompose the total Sorensen dissimilarity into turnover and nestedness 
# resultant
Y.core <- betapart.core(spp)
Y.pair <- beta.pair(Y.core, index.family = "sor")

# Let Y1 be the turnover component (beta-sim):
Y1 <- as.matrix(Y.pair$beta.sim)

# Let Y2 be the nestedness-resultant component (beta-sne):
Y2 <- as.matrix(Y.pair$beta.sne)

# View turnover component, first 10 rows and columns (rounded off to 4 decimals in this case)
Y3 <- round(Y1[1:10, 1:10], 4)

# View nestedness-resultant (same way as turnover)
Y4 <- round(Y2[1:10, 1:10], 4)


# Exercise
# 1. 
# creating a dataframe for plotting
Turnover <- Y1[1, 1:58]
Section_number <- (1:58)
Data <- data.frame(Section_number, Turnover)

# plot
ggplot(data = Data, (aes(x = Section_number, y = Turnover))) +
  geom_line(col = "blue") + xlab("Coastal section, west to east") + ylab("Species turnover") +
  ggtitle("Species turnover between sections from the west to east coastal sections")

# The graph shows that section 1 to 4 there is no turnover in species, a steady
# increase in turnover from 4 to 6, no turnover from 6 to 9 and increase from 9
# to 10 and so forth. When the turnover is 0, it means that the sections have the
# same number of species (alpha diversity) and the same kind of species. For example, 
# sections 1 to 4 may have all have only species A, B,C,D,E. When turnover is greater
# than 0, this means that the sections may have the same alpha diversity (or not),
# but the kind of species inside are not all similar. 

# 2. According to a definition by Patterson and Atmar (1986), nestedness describes 
# a pattern where the species present at species-poor sites form proper subsets 
# of the species in species-richer sites. This definition implies, explicitly a 
# that there is gradient in species richness across sites. Therefore, nestedness 
# is necessarily linked to ordered species loss and therefore to a certain degree 
# of b-diversity. Implicitly, the definition also implies differences in species 
# incidences. 
# In a study of benthic macroalgae of the Red Sea in Egypt, from 3 sites showed
# varying alpha diversity values. Alpha diversity ranged from 4 to 9 species in 
# Hurghada, 3 to 9 species in Safaga, and 4 to 17 species in Al-Quseir. There was
# a high species nestedness between sites. A possible explanation of the existence of 
# significant differences between Safaga and the other two studied sites is the 
# environmental disturbance caused by phosphate mining port at Safaga that was very 
# close to the study site but relatively far from the other two sites.  This environmental 
# disturbance induced fluctuation in the macroalgal abundance and diversity and 
# may have contributed to the lower species richness than in the other two sites. 
# than in the other two study sites.
