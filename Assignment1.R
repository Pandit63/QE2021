# Somdaka Phiwokuhle
# Quantitative Ecology Assignment 1
# 30 June 2021

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
spp <- read_csv("data/SeaweedsSpp.csv") # it was not running using the code given by Prof, I then opted for dowloading the data and reading it from my computer
spp <- dplyr::select(spp, -1) # removing column 1

# Lets look at the data:
dim(spp) # determines number of rows and columns, respectively.

# what is in the first five rows and columns
spp[1:5, 1:5] # frist rows then coumns

# Alpha diversity: species richness
spp_richness <- diversityresult(spp, index = 'richness', method = 'each site') # we are calculating species richness per site

# plot species richness
ggplot(data = spp_richness, (aes(x = 1:58, y = richness))) + # x=1:58 means that use the rows inbetween those numbers
  geom_line() + xlab("Coastal section, west to east") + ylab("Species richness")

# Alpha diverity: diversity indices
# fictitious data (for abundace data since we cannot use presence-absence)
light <- read_csv("data/light_levels.csv") # loaded the same way

# calculating species richness and diversity indices
light_div <- data.frame(
  site = c("low_light", "mid_light", "high_light"), # creates a dataframe of the diversity indices diversity indices
  richness = specnumber(light[, 2:7], MARGIN = 1),
  shannon = round(diversity(light[, 2:7], MARGIN = 1, index = "shannon"), 2),
  simpson = round(diversity(light[, 2:7], MARGIN = 1, index = "simpson"), 2)
)
light_div

# Dissimilarity indices
# Sorensen for the presence-absence data (Seaweed)
sor <- vegdist(spp, binary = TRUE) # binary = TRUE sets to presence/absence data
sor_df <- round(as.matrix(sor), 4) # make a matrix
sor_df[1:20, 1:20] # the first 20 rows and columns


# Answers
# 1. The matrix is square because it is same as its transpose. The number of rows
# and columns is determined by the code, (in this case), "sor_df[1:20, 1:20]"
# 2. The diagonal is the distance between each section and itself
# 3. The non-diagonal elements are the distances between two sections
# 5. plot
sor <- sor_df[1, 1:20] # cutting the data to show only the first row
Section_number <- (1:20)
Values <- sor
Soren <- data.frame(Section_number, Values) # creating a dataframe to make a plot, so that I can have x and y axis

ggplot(data = Soren, (aes(x = Section_number, y = Values))) +
  geom_line(col = 'salmon') +
  xlab("Coastal section, west to east") + ylab("Dissimilarity Index")

# 6. The graph increases in dissimilarity from the west coastal sections to the
# east sections because the west and east coast of South Africa have different temperatures
# thus the abundance and type of vegetation will vary. Sections in these coasts
# will have increasing dissimilarity as you move further east because of this 
# difference in vegetation.
