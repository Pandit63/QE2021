# Phiwokuhle Somdaka
# PCA on SDGs
# 25 July 2021

# load libraries
library(tidyverse)
library(vegan)
library(missMDA) #for missing values
library(ggcorrplot) #for correlation

# load data
# Domestic general government health expenditure as a percentage of general govermnent
# expenditure
SDG1.a <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG1.a_domestic_health_expenditure.csv") %>%
  filter(Period == 2016) %>% #removing all other periods except 2016
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>% #selecting only these columns to use for analysis
  mutate(SDG = "SDG1.a") #creating a new column titles "SDG" with the rows having "SDG1"

# Maternal mortality ratio (per 100 000 live births)
SDG3.1_1 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.1_maternal_mort.csv") %>%
  filter(Period == 2016,
         Indicator == "Maternal mortality ratio (per 100 000 live births)") %>% # leave only this indicators in the rows
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>% #select only these required columns
  mutate(SDG = "SDG3.1_1") #create a new columns called SDG with the observations being SDG 3.1_1

# SDG 3.1 Births attended by skilled health personnel (%))
SDG3.1_2 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.1_skilled_births.csv") %>%
  filter(Period == 2016) %>% 
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.1_2")

# SDG 3.2 Number of neonatal deaths (Child mortality)
SDG3.2_1 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.2_neonatal_deaths.csv") %>%
  filter(Period == 2016,
         Dim1 == "Both sexes") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.2_1")

# SDG 3.2 Number of under-five deaths (Child mortality)
SDG3.2_2 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.2_under_5_deaths.csv") %>%
  filter(Period == 2016,
         Dim1 == "Both sexes") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.2_2")

# SDG 3.2 Number of infant deaths (Child mortality)
SDG3.2_3 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.2_infant_deaths.csv") %>%
  filter(Period == 2016,
         Dim1 == "Both sexes") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.2_3")

# SDG 3.3 New HIV infections (per 1000 uninfected population))
SDG3.3_1 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.3_new_HIV_infections.csv") %>%
  filter(Period == 2015,
         Dim1 == "Both sexes") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.3_1")

# SDG 3.3 Incidence of tuberculosis (per 100 000 population per year))
SDG3.3_2 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.3_TB.csv") %>%
  filter(Period == 2016) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.3_2")

# SDG 3.3 Malaria incidence (per 1 000 population at risk))
SDG3.3_3 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.3_malaria.csv") %>%
  filter(Period == 2016) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.3_3")

# SDG 3.3 Hepatitis B surface antigen (HBsAg) prevalence among children under 5 years-prevalence-among-children-under-5-years)
SDG3.3_4 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.3_hepatitis_B.csv") %>%
  filter(Period == 2015) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.3_4")

# SDG 3.3 Reported number of people requiring interventions against NTDs
SDG3.3_5 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.3_NCD_interventions.csv") %>%
  filter(Period == 2016) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.3_5")

# SDG 3.4 Adult mortality rate (probability of dying between 15 and 60 years per 1000 population))
SDG3.4_1 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.4_adult_death_prob.csv") %>%
  filter(Period == 2016,
         Dim1 == "Both sexes") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.4_1")

# SDG 3.4 Number of deaths attributed to non-communicable diseases, by type of disease and sex
SDG3.4_2 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.4_NCD_by_cause.csv") %>%
  filter(Period == 2016,
         Dim1 == "Both sexes",
         Dim2 == "Diabetes mellitus") %>%
  mutate(Indicator = Dim2) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.4_2")

SDG3.4_3 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.4_NCD_by_cause.csv") %>%
  filter(Period == 2016,
         Dim1 == "Both sexes",
         Dim2 == "Cardiovascular diseases") %>%
  mutate(Indicator = Dim2) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.4_3")

SDG3.4_4 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.4_NCD_by_cause.csv") %>%
  filter(Period == 2016,
         Dim1 == "Both sexes",
         Dim2 == "Respiratory diseases") %>%
  mutate(Indicator = Dim2) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.4_4")

# SDG 3.4 Crude suicide rates (per 100 000 population) (SDG 3.4.2))
SDG3.4_5 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.4_suicides.csv") %>%
  filter(Period == 2016,
         Dim1 == "Both sexes") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.4_5")

# SDG3.4 Total NCD Deaths (in thousands)
SDG3.4_6 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.4_NCD_data_total.csv") %>%
  filter(Period == 2016,
         Dim1 == "Both sexes") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.4_6")

# SDG 3.5 Alcohol, total per capita (15+) consumption (in litres of pure alcohol) (SDG Indicator 3.5.2)-alcohol-per-capita-(15-)-consumption)
SDG3.5 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.5_alcohol_consumption.csv") %>%
  filter(Period == 2015,
         Dim1 == "Both sexes") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.5")

#SDG 3.6 Estimated road traffic death rate (per 100 000 population))
SDG3.6 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.6_traffic_deaths_prop.csv") %>%
  filter(Period == 2016,
         Dim1 == "Both sexes") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.6")

#SDG 3.7 Adolescent birth rate (per 1000 women aged 15-19 years))
SDG3.7 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.7_adolescent_births.csv") %>%
  filter(Period == 2016) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.7")

# SDG 3.8 UHC Index of service coverage (SCI)
SDG3.8_1 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.8_UHC_data_availability.csv") %>%
  filter(Period == "2013-2017") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.8_1")

# SDG 3.8 Data availability for UHC index of essential service coverage (%))
SDG3.8_2 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.8_UHC_data_availability.csv") %>%
  filter(Period == 2017) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.8_2")

#SDG 3.9 Poison control and unintentional poisoning
SDG3.9_1 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.9_unintentional_poisoning_prop.csv") %>%
  filter(Period == 2016,
         Dim1 == "Both sexes") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.9_1")

# SDG 3.9 Mortality rate attributed to exposure to unsafe WASH services (per 100 000 population) (SDG 3.9.2)-(sdg-3-9-2))
SDG3.9_3 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.9_WASH_mortalities.csv") %>%
  filter(Period == 2016,
         Dim1 == "Both sexes") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.9_3")

# SDG 16.1 Estimates of rate of homicides (per 100 000 population)
SDG16.1 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG16.1_homicides.csv") %>%
  filter(Period == 2016,
         Dim1 == "Both sexes") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG16.1")

# SDG 3.a Prevalence of current tobacco use among persons aged 15 years and older (age-standardized rate)
SDG3.a <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.a_tobacco_control.csv") %>%
  filter(Period == 2016,
         Dim1 == "Both sexes") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.a")

# SDG 3.b Total net official development assistance to medical research and basic health sectors per capita (US$), by recipient country
SDG3.b_1 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.b_dev_assistence_for_med_research.csv") %>%
  filter(Period == 2016) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.b_1")

# SDG 3.b Measles-containing-vaccine second-dose (MCV2) immunization coverage by the nationally recommended age (%)-immunization-coverage-by-the-nationally-recommended-age-(-))
SDG3.b_2 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.b_measles_vaccine.csv") %>%
  filter(Period == 2016) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.b_2")

#SDG 3.b Diphtheria tetanus toxoid and pertussis (DTP3) immunization coverage among 1-year-olds (%)-immunization-coverage-among-1-year-olds-(-))
SDG3.b_3 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.b_diphtheria_vaccine.csv") %>%
  filter(Period == 2016) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.b_3")

#SDG 3.b Pneumococcal conjugate vaccines (PCV3) immunization coverage among 1-year-olds (%)-immunization-coverage-among-1-year-olds-(-))
SDG3.b_4 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.b_pneumococcal_vaccine.csv") %>%
  filter(Period == 2016) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.b_4")

# SDG 3.b Girls aged 15 years old that received the recommended doses of HPV vaccine
SDG3.b_5 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.b_HPV_vaccine.csv") %>%
  filter(Period == 2016) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.b_5")

# SDG 3.c SDG Target 3.c | Health workforce: Substantially increase health financing and the recruitment, development, training and retention of the health workforce in developing countries, especially in least developed countries and small island developing States
SDG3.c_1 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.c_health_workforce.csv")  %>%
  filter(Period == 2016,
         Indicator == "Medical doctors (per 10,000)") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.c_1")

SDG3.c_2 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.c_health_workforce.csv")  %>%
  filter(Period == 2016,
         Indicator == "Nursing and midwifery personnel (per 10,000)") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.c_2")

SDG3.c_3 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.c_health_workforce.csv")  %>%
  filter(Period == 2016,
         Indicator == "Dentists (per 10,000)") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.c_3")

SDG3.c_4 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.c_health_workforce.csv")  %>%
  filter(Period == 2016,
         Indicator == "Pharmacists  (per 10,000)") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.c_4")

# SDG 3.d Average of 13 International Health Regulations core capacity scores, SPAR version
SDG3.d_1 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_SDG3.d_health_risks.csv")  %>%
  filter(Period == 2016) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.d_1")

# Other Life expectancy at birth (years))
other_1 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_Other_life_expectancy.csv") %>%
  filter(Period == 2015,
         Dim1 == "Both sexes",
         Indicator == "Life expectancy at birth (years)") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "other_1")

#Other Life expectancy at age 60 (years))
other_2 <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_Other_life_expectancy.csv") %>%
  filter(Period == 2015,
         Dim1 == "Both sexes",
         Indicator == "Life expectancy at age 60 (years)") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "other_2")

# Rbind the data
health <- do.call("rbind", lapply(ls(),get))
# Above we are joining all datasets with every individual observation and variables, 
# because we need to do analyses on one dataset.
head(health)

# List of all SDGs used
unique(health[, c(5, 1)])
# we are removing duplicates from the health dataset (to avoid SDGs repeating themselves) 
# and only want to show column 1 and 5 so that we know what the indicators are and what 
# SDG number or symbol they are for explanation of trends after PCA.

# Pivot wider
health_wide <- health %>%
  arrange(Location) %>%
  select(-Indicator) %>%
  pivot_wider(names_from = SDG, values_from = FactValueNumeric) %>%
  as_tibble()
# Based on location, we remove all indicators, make the observations under the variable 
# "SDG" to be columns. Under these columns, we put FactValueNumeric as the observations
# for the different locations. We do this because our focus will be on the Sustainable
# Development Goals and the percentage or how much each country has achieved of a goal.

# Add world population data
popl <- read_csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/WHO/WHO_population.csv") %>%
  filter(Year == 2016) %>%
  rename(popl_size = `Population (in thousands) total`,
         Location = Country) %>%
  select(Location, popl_size) %>%
  mutate(popl_size = as.numeric(gsub("[[:space:]]", "", popl_size)) * 1000)
# Above we read in the World population data, we only have an interest in the year 2016
# hence it is filtered. We rename 'country' to 'location' because the health_wide
# data has the former not the latter. 'Population (in thousands) total'is also renamed to
# 'popl_size' because under mutate(), the population size is multiplied by a 1000, giving
# the exact values, thus it is not necessary to specify that it is in thousands.
# Our interest is in just two variables; location and popl size, hence only those are selected.

# Now join the two data
health_wide <- health_wide %>%
  left_join(popl)
# the two data are joined by location so we can have variables and observations
# for every country.

# Express some variables to unit of population size
health_wide <- health_wide %>%
  mutate(SDG3.4_4 = SDG3.4_4 / popl_size * 100000,
         SDG3.4_3 = SDG3.4_3 / popl_size * 100000,
         SDG3.4_2 = SDG3.4_2 / popl_size * 100000,
         SDG3.4_6 = SDG3.4_6 / 100,
         SDG3.2_2 = SDG3.2_2 / popl_size * 100000,
         SDG3.2_3 = SDG3.2_3 / popl_size * 100000,
         SDG3.2_1 = SDG3.2_1 / popl_size * 100000)

# calculate histograms
health_wide$na_count <- apply(health_wide[, 3:(ncol(health_wide) - 1)], 1, function(x) sum(is.na(x)))
hist(health_wide$na_count, breaks = 14, plot = TRUE)
# firstly we create a new column where the number of NAs in each row will be counted
# and written, then create a frequency histogram of these NA values

# remove rows where there are more than 10 NAs
health_wide <- health_wide %>%
  filter(na_count <= 10) %>%
  select(-na_count)
# rows with more than 10 NAs are removed. We filter our data so that it includes 
# only  the rows with less than 10 NAs (...<=10) and then remove the column for NAs
# since we do not need it for the analysis anymore. NAs make the calculations inaccurate.

# calculate pairwise correlations
corr <- round(cor(health_wide[, 3:(ncol(health_wide) - 1)]), 1)
# Correlation between the SDGs is being calculated to see how the SDGs affect each other.

# visualization of the correlation matrix
ggcorrplot(corr, type = 'upper', outline.col = "grey60",
           colors = c("#1679a1", "white", "#f8766d"),
           lab = TRUE)
# We visualise the correlation matrix, showing only the upper part, no diagonal.
# different colours are chosen for the correlation scale.

# Impute remaining NAs
health_wide_complete <- imputePCA(health_wide[, 3:(ncol(health_wide) - 1)])$completeObs
# The dataset is incomplete because of NAs. The NAs are imputed with the means of each variable.

# Do the PCA
health_wide_complete_std <- decostand(health_wide_complete, method = "standardize")
health_pca <- rda(health_wide_complete_std)
health_pca
# the data is first standardized because not all SDGs have the same form of measurement.
# PCA is done after that

# More informative output
summary(health_pca)

# Graphical representations
biplot(health_pca, scaling = 1, main = "PCA scaling 1", choices = c(1, 2))
biplot(health_pca, scaling = 2, main = "PCA scaling 2", choices = c(1, 2))

# Create plots from scratch
pl1 <- ordiplot(health_pca, type = "none", scaling = 1, main = "PCA WHO/SDG")
points(pl1, "sites", pch = 21, cex = 1.0, col = "grey20", bg = "grey80")
points(pl1, "species", pch = 21, col = "turquoise", arrows = TRUE)
text(pl1, "species", col = "blue4", cex = 0.9)
text(pl1, "sites", col = "red4", cex = 0.9)
# text(pl1, "sites", col = "red4", cex = 0.9)
# A plot is created from scratch using scaling 1, under the heading 'PCA WHO/SDG'.
# The sites (Countries) are plotted as points in colour 'grey20', in size 1.0 with point symbol 21.
# the species (SDGs) are plotted in point symbol 21, in a turqoise colour of arrows.
# The SDG names are plotted in blue4 colour, countries in red with size 1 and text formats.

pl2 <- ordiplot(health_pca, type = "none", scaling = 2, main = "PCA WHO/SDG")
points(pl2, "sites", pch = 21, cex = 1.75, col = "grey80", bg = "grey80")
points(pl2, "species", pch = 21, col = "turquoise", arrows = TRUE)
text(pl2, "species", col = "blue4", cex = 0.9)
text(pl2, "sites", col = "red4", cex = 0.9)
# Same as above but in scaling 2

# Other ways to create plots using ggplot
site_scores <- tibble(ParentLocation = health_wide$ParentLocation,
                      Location = health_wide$Location)
site_scores <- tibble(cbind(site_scores, scores(health_pca, display = "sites", choices = c(1:7))))
species_scores <- data.frame(scores(health_pca, display = "species", choices = c(1:7)))
species_scores$species <- rownames(species_scores)
species_scores <- tibble(species_scores)
# A new data table is created to include the Parent location and location from the 
# healthwide data. This is joined with the PCA data, choosing only seven axes for each country (site)
# A dataframe is created from the species scores of the pca choosing only seven axes.
# We extract the rownames from the species_scores dataframe.

ggplot(data = site_scores, aes(x = PC1, y = PC2)) +
  geom_point(aes(col = ParentLocation)) +
  geom_segment(data = species_scores, 
               aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = arrow(length = unit(0.4, "cm"), type = "closed"), 
               color = "lightseagreen", alpha = 1, size = 0.3) +
  geom_text(data = species_scores, 
            aes(x = PC1, y = PC2, label = species),
            color = "black") +
  xlab("PC1") + ylab("PC2") + 
  ggtitle("WHO SDGs, Scaling 2")
# Question 2

# Most African countries have a high value in Malaria  incidences, number of under five deaths
# Hepatitis prevalence among under 5 years children, road traffic accidents etc.
# European countries mostly have high values in Data availability for UHC index of essential
# service coverage, births attended by skilled health personnels, Dentists, Pharmacists, 
# life expectancy at age 60 and at birth etc. Most American countries follow a similar trend.
# South East Asian countries are at an intermediate with high values in other indicators like
# Diabetes mellitus and Total NCD deaths.
# Western Pacific countrieshave high values in crude suicide rates, diabetes mellitus, with others
# scattered around the values of American and European countries

# Select countries required for analysis
health_wide1 <- health_wide %>% 
  slice(23, 32, 35, 50, 58, 74, 94, 110, 114, 147, 153, 168)

# Impute the NAs
health_wide_complete1 <- imputePCA(health_wide1[, 3:(ncol(health_wide1) - 1)])$completeObs

# Do the PCA
health_wide_complete_std1 <- decostand(health_wide_complete1, method = "standardize")
health_pca1 <- rda(health_wide_complete_std1)
health_pca1

summary(health_pca1)

# Graphical representation
site_scores <- tibble(ParentLocation = health_wide1$ParentLocation,
                      Location = health_wide1$Location)
site_scores <- tibble(cbind(site_scores, scores(health_pca1, display = "sites", choices = c(1:7))))
species_scores <- data.frame(scores(health_pca1, display = "species", choices = c(1:7)))
species_scores$species <- rownames(species_scores)
species_scores <- tibble(species_scores)

ggplot(data = site_scores, aes(x = PC1, y = PC2)) +
  geom_point(aes(col = Location)) +
  geom_segment(data = species_scores, 
               aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = arrow(length = unit(0.4, "cm"), type = "closed"), 
               color = "lightseagreen", alpha = 1, size = 0.3) +
  geom_text(data = species_scores, 
            aes(x = PC1, y = PC2, label = species),
            color = "black") +
  xlab("PC1") + ylab("PC2") + 
  ggtitle("WHO SDGs, Scaling 2")

# Africa
# South Africa has high values in New HIV infections Diabetes mellitus, TB incidences,
# crude suicide rates etc.
# Central African Republic has high values in road traffic deaths, number of infant deaths etc.
# South East Asia
# India has high values in births by skilled personnels, reported number of people requiring
# interventions against NTDs etc. Myanmar is the same as India and close to Central African 
# Republic.
# East Mediterrenean 
# Egypt has high values in measles containing vaccine second dose, total NCD deaths,
# cardiovascular diseases etc. Libya is the same.
# West Pacific
# China has high values in measles, average of 13 interaction Health Regulations core capacity 
# score, cardiovascular diseases and not too far from Egypt.
# New Zealand has high values in pneumoccocal conjugate vaccines, immunization coverage
# among 1 year olds, data availability for UHC Index of essential service coverage.
# Europe
# France has high values in those similar to New Zealand.
# Swtizerland has high values similar to France plus diphthreria tetanus toxoid, pertussis
# immunization coverage among 1 year olds and pharmacists.
# Americas
# United States of America is similar to Switzerland. Brazil is at an intermediate between 
# the high values of New Zealand and crude suicide rates.

# Question 3
# Globally:
# European and American countries and some West Pacific are well developed (high income countries). They
# have access to higher grade technology, can purchase vast amount of medications and
# vaccines, can build high quality health facilities and can afford to hire and pay a 
# huge number healthcare workers. African countries on the other hand are mostly under-
# developed with only a few (e.g South Africa) developing. They are still trying to rectify
# the consequences of past segregations and thus inequality deters most parts of the countries
# from having access to healthcare systems. SouthEast Asia and East Meditteranean countries
# are better than African countries but political wars and corruption block them from being
# similar to American/European countries.
# Some regional causes:
# Immediately after the adoption of the 2030 Agenda in 2015, the Federal Council
# of Switzerland commissioned a comprehensive baseline assessment and gap analysis
# of the implementation status at federal level. The analysis concerned all 169 
# targets and covered both Switzerland's domestic and international contributions.
# Switzerland is a developed country, thus it is able to cater (economically) for
# the Federal Council to work on these SDGs.
# A decade of inclusive growth in Brazil between 2003 and 2013 raised millions from poverty and reduced 
# inequality, earning its leaders praise for its social progress and leading some to ask 
# whether Brazil is a "role model" for other developing countries. Since 2014 however, Brazil's 
# economy has been mired in a deep recession and its political system paralyzed by a wide-ranging 
# corruption scandal that has claimed the political lives of former Presidents. The combined economic and 
# political situation in Brazil has revealed a number of structural challenges facing the country, including: 
# an aging population, low and declining growth in economic productivity, rising unemployment, 
# dilapidated infrastructure, and a political situation which has shaken confidence in Brazil's public 
# institutions. This is why Brazil is at an intermediate between poor African countries and West Pacific and
# European countries.
# South Africa has played a key role in the negotiations and processes that led to the development of the 2030 
# Agenda for Sustainable Development, including its 17 SDGs, and Agenda 2063. Aspects of these negotiations were
# informed by the priorities of South Africa's National Development Plan. In working towards realizing the vision of 
# both the NDP and the SDGs, South Africa has made several important steps forward, but also faces considerable 
# challenges regarding implementation, capacity-building, financing, and engagement. In Africa, South Africa is one of 
# the countries that are developing, however, one of the issues that deter it from fully developing their healthcare
# systems is the issue of political corruption. There has been cases of political leaders who are given tenders for 
# vaccines for diseases where it was discovered that the politicians in charge did not utilise the funds for the vaccines
# but for personal use. You rarely here of a new hospital being built in the country and the level of employment for
# healthcare workers in low (as the unemployment rate of the country is high).
