# Phiwokuhle Somdaka
# Environmental distances
# 30 June 2021

# load libraries
library(vegan) #allows multivariate analysis
library(ggplot2) #used for most data analyses
library(geodist) #used to take a series of geographical
# coordinates measured as localities in geograpihcal space
# and calculate spatial distances between them
library(ggpubr) #arranges ggplot panelss

xyz <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/diversity/Euclidian_distance_demo_data_xyz.csv")

# first look at how many rows and columns using dimensions
dim(xyz)

# what is now in those 7 rows and columns?
xyz # column1 are sites (places where data was collected), and x. y,z are the 
# coordinates in space of the different sites

# Calculating Euclidena distances
xyz_euc <- round(vegdist(xyz[2:4], method = "euclidean", upper = FALSE, diag = TRUE),4)
# so above here we are calculating distances ofr only rows 2, 3 and 4, using
# euclidean method (Environmental data only uses this), we do not want the upper
# right triangle and we want the diagonal. The results will be rounded off to 4
# decimal places
xyz_euc # result indicates the straightest line distance between two points
# located in a 3D space, the number can be any number greater than 0, no negatives

# checking the class of something
class(xyz_euc) # shows that it is a distance matrix, this is not useful for 
# calculations

# convert it to a dataframe
xyz_df <- as.data.frame((as.matrix(xyz_euc))
xyz_df

# "Actual" environmental data
env_fict <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/diversity/Euclidian_distance_demo_data_env.csv")
# x, y, z are now temperature depth and light (environmental variables). These 
# indicate something about the nature of the place
env_fict

# calculating the above's euclidean distance
fict_euc <- round(vegdist(env_fict[2:4], method = "euclidean", upper = FALSE, diag = TRUE),4)
fict_euc

# Back to the seaweed data
load("SeaweedEnv.RData") #from my device

# what is inside?
dim(env) #there is corresponding environmental data for every one of the 58 sites
# the columns have means, max, median etc of each months.

# top 5 columns and rows
round(env[1:5, 1:5], 4)

# we can view names of different variables in the dataset
colnames(env) # now you know what the columns are

# select some variables to be analysed to explain why seaweeds differ across space in SA
env1 <- dplyr::select(env, febMean, febRange, febSD, augMean,
                      augRange, augSD, annMean, annRange, annSD)

# NOw we do standardisation, where all measurements are brought down to a mean of 
# 0 and SD of 1. From all values, overall mean of the datapoints is subtracted and
# divide by the overall SD. decostand applies standardisation automatically
E1 <- round(decostand(env1, method = "standardize"), 4)
# Above, all variables measured across different units are brought down to the 
# same scale. so that those variables with large measurements do not have an overbearing
# influence on distance between places.

E1[1:5, 1:5]

# Calculating euclidean distance
E1_euc <- round(vegdist(E1, method = "euclidean", upper = TRUE), 4)
E1_df <- as.data.frame(as.matrix(E1_euc)) # convert matric to a dataframe
E1_df[1:10, 1:10] #view the 1st 10 rows and columns of the dataframe

# make a plot
ggplot(data = E1_df, (aes(x = 1:58, y = E1_df[,1]))) +  #note how you chose row 1
  geom_line() + xlab("Coastal section, west to east") + ylab("Environmental distance")

# Euclidean distance of geographical coordinates
geo <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/diversity/sites.csv")
dim(geo) #columns are latitude and longitude
head(geo)

# calculate geogrsphic distance between sections
dists <- geodist(geo, paired = TRUE, measure = "geodesic")
dist_df <- as.data.frame(as.matrix(dists))
dist_df[1:5, 1:5]

# plot
ggplot(data = dist_df, (aes(x = 1:58, y = dist_df[,1]))) +
  geom_line() + xlab("Coastal section, west to east") + ylab("Distance") +
  ggtitle("Euclidean distance")

# Euclidean distance calculations can also be used
dists_euc <- vegdist(geo, method = "euclidean")
dists_euc_df <- as.data.frame(as.matrix(dists_euc)) #these were taken as arbitrary number not decimal degrees 
# hence the difference

# plot
ggplot(data = dists_euc_df, (aes(x = 1:58, y = dists_euc_df[,1]))) +
  geom_line() + xlab("Coastal section, west to east") + ylab("Distance") +
  ggtitle("Euclidean distance")

