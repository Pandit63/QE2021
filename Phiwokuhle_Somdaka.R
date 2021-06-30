# Phiwokuhle SOmdaka
# Covid-19 Assignment
# Due: 28 June 2021

# load libraries
library(tidyverse)
library (readr)
library(rvest)
library(rgeos)
library(rworldmap)
library(car)
library(corrplot)
library(ggmap)
library(magrittr)
library(stringr)
library(Hmisc)

# Datasets

# JHU CSSE dataset
urlfile <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"

confirmed <- read_csv(url(urlfile)) %>% 
  gather(key = "time", value = "confirmed", -`Province/State`, -`Country/Region`, -Lat, -Long) %>%
  group_by(`Province/State`, `Country/Region`, Lat, Long) %>%
  mutate(day_zero = dplyr::row_number() - 1) %>%
  filter(confirmed > 0) %>%
  mutate(day_zero_country = dplyr::row_number() - 1) %>%
  ungroup() %>%
  mutate(time = as.Date(time, format = "%m/%d/%y"))

# Update the names
confirmed$`Country/Region`[confirmed$`Country/Region` == "Congo (Kinshasa)"] <- "DR Congo" 
confirmed$`Country/Region`[confirmed$`Country/Region` == "Congo (Brazzaville)"] <- "DR Congo"
confirmed$`Country/Region`[confirmed$`Country/Region` == "Kosovo"] <- "Serbia"

# rename Country/Region to just Country
confirmed2 <- confirmed %>%
  rename('Country' = 'Country/Region')

# Our World in data
urlfile1 <- "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv"
owid_covid_data <- read_csv(url(urlfile1))

# rename the location to Country and rename the column total cases
Owid_cov <- owid_covid_data %>% 
  rename('Country' = 'location') %>% 
  rename('confirmed' = 'total_cases')


# World pop data
webpage <- read_html("https://www.worldometers.info/world-population/population-by-country/")
tbls <- html_nodes(webpage, "table")
head(tbls)

world_pop <- webpage %>%
  html_nodes("table") %>%
  html_table(fill = TRUE, trim = TRUE) %>%
  as.data.frame() %>%
  select(-X.) %>%
  as_tibble() %>%
  rename(`Country` = `Country..or.dependency.`,
         Population_2020 = `Population..2020.`,
         Yearly_change = `Yearly.Change`,
         Net_change = `Net.Change`,
         Density = `Density..P.Km².`,
         Land_area = `Land.Area..Km².`,
         Migrants = `Migrants..net.`,
         Fertility_rate = `Fert..Rate`,
         Median_age = `Med..Age`,
         Urban_population = `Urban.Pop..`,
         World_share = `World.Share`) %>%
  mutate(Population_2020 = as.numeric(gsub(",", "", Population_2020)),
         Yearly_change = as.numeric(gsub(" %", "", Yearly_change)),
         Net_change = as.numeric(gsub(",", "", Net_change)),
         Density = as.numeric(gsub(",", "", Density)),
         Land_area = as.numeric(gsub(",", "", Land_area)),
         Migrants = as.numeric(gsub(",", "", Migrants)),
         Fertility_rate = as.numeric(Fertility_rate),
         Median_age = as.numeric(Median_age),
         Urban_population = as.numeric(gsub(" %", "", Urban_population)),
         World_share = as.numeric(gsub(" %", "", World_share)))

# Edit names of countries/regions to match that in the JHU data
world_pop[world_pop == "South Korea"] <- "Korea, South"
world_pop[world_pop == "Taiwan"] <- "Taiwan*"
world_pop[world_pop == "United States"] <- "US"
world_pop[world_pop == "Czech Republic (Czechia)"] <- "Czechia"
# West Bank and Gaza ???
# 'Congo (Kinshasa)' in JSU data changed to 'DR Congo'
world_pop[world_pop == "CÃ´te d'Ivoire"] <- "Cote d'Ivoire"
world_pop[world_pop == "St. Vincent & Grenadines"] <- "Saint Vincent and the Grenadines"
# 'Congo (Brazzaville)' in JSU data changed to 'DR Congo'
world_pop[world_pop == "Saint Kitts & Nevis"] <- "Saint Kitts and Nevis"
# 'Kosovo' and 'Serbia' combined in JSU data
world_pop[world_pop == "Myanmar"] <- "Burma"
world_pop[world_pop == "Sao Tome & Principe"] <- "Sao Tome and Principe"
# Update % urban population from https://data.worldbank.org/indicator/SP.URB.TOTL.in.zs
# except where noted otherwise
world_pop %>%
  select(`Country`, Urban_population) %>%
  filter(is.na(Urban_population))
world_pop$Urban_population[world_pop$`Country/Region` == "Venezuela"] <- 88
world_pop$Urban_population[world_pop$`Country/Region` == "Hong Kong"] <- 100
world_pop$Urban_population[world_pop$`Country/Region` == "Singapore"] <- 100
world_pop$Urban_population[world_pop$`Country/Region` == "Kuwait"] <- 100
world_pop$Urban_population[world_pop$`Country/Region` == "Puerto Rico"] <- 94
world_pop$Urban_population[world_pop$`Country/Region` == "Macao"] <- 100
world_pop$Urban_population[world_pop$`Country/Region` == "Guadeloupe"] <- 98.4 # updated from http://data.un.org/
world_pop$Urban_population[world_pop$`Country/Region` == "Monaco"] <- 100
world_pop$Urban_population[world_pop$`Country/Region` == "Gibraltar"] <- 100
world_pop$Urban_population[world_pop$`Country/Region` == "Palau"] <- 80
world_pop$Urban_population[world_pop$`Country/Region` == "Anguilla"] <- 100 # updated from https://www.cia.gov/
world_pop$Urban_population[world_pop$`Country/Region` == "Nauru"] <- 100
world_pop$Urban_population[world_pop$`Country/Region` == "Holy See"] <- 100 # updated from https://www.cia.gov/


# Join some data
confirmed_norm <- confirmed2%>%
  group_by(`Country`) %>%
  mutate(Lat = mean(Lat)) %>%
  group_by(`Country`, time, day_zero, Lat) %>%
  summarise(confirmed = sum(confirmed)) %>%
  mutate(day_since_start = dplyr::row_number() - 1) %>%
  right_join(world_pop, by = 'Country') %>%
  select(-Yearly_change, -Net_change, -World_share) %>%
  mutate(Population_2020 = as.numeric(Population_2020)) %>%
  filter(time != is.na(time)) %>%
  mutate(infect_per_mil = (confirmed / Population_2020) * 1000000,
         infect_per_cent = (confirmed / Population_2020) * 100,
         pop_dens = round(Population_2020 / Land_area), 1) %>%
  ungroup()

# removing dates after vaccine for all datasets (first vaccine on the 8th of December 2020)
Owid <- Owid_cov %>% 
  filter(date < "2020-12-08")

confirmed_norm2 <- confirmed_norm %>% 
  filter(time < "2020-12-08")

# Total number of cases per country day before vaccine
Cases <- confirmed_norm %>% # first filter for only that day
  filter( time == "2020-12-07")

sum(Cases$confirmed) # Now calculate the total number of cases

# Range of the cases
range(confirmed_norm2$confirmed)

# SD of the cases
sd(confirmed_norm2$confirmed)

# mean and standard deviation of the indicators
Confirmed_stats1 <- confirmed_norm2 %>% 
  summarise(mean_Lat = mean(Lat, na.rm = T),
            sd_Lat = sd(Lat, na.rm = T),
            mean_Migrants = mean(Migrants, na.rm = T),
            sd_Migrants = sd(Migrants, na.rm = T),
            mean_Fertility = mean(Fertility_rate, na.rm = T),
            sd_Fertility = sd(Fertility_rate, na.rm = T),
            mean_Med.age = mean(Median_age, na.rm = T),
            sd_Med.age = sd(Median_age, na.rm = T),
            mean_Urban.pop = mean(Urban_population, na.rm = T),
            sd_Urban.pop = sd(Urban_population, na.rm = T),
            mean_pop.dens = mean(pop_dens, na.rm = T),
            sd_pop.dens = sd(pop_dens, na.rm = T))

confirmed_stats2 <- Owid %>% 
  summarise(mean_gdp = mean(gdp_per_capita, na.rm = T),
            sd_gdp = sd(gdp_per_capita, na.rm = T),
            mean_stringency = mean(stringency_index, na.rm = T),
            sd_stringency = sd(stringency_index, na.rm = T))


# Correlation (univariate analysis)
# Remove categories
Owid2 <- Owid %>% 
  select(confirmed, stringency_index, gdp_per_capita)

confirmed_norm3 <- confirmed_norm2 %>% 
  select(confirmed, Lat, Median_age, Urban_population, pop_dens, Migrants, Fertility_rate)

# Calculating correlation using Pearson method
Pearson1 <- cor(Owid2, use = "complete.obs")
Pearson2 <- cor(confirmed_norm3, use = "complete.obs")

# Now plot the correlations

Corr <- corrplot(Pearson1, tl.col = "red", tl.srt = 45, bg = "White",
                 title = "Correlation of number of COVID-cases and indicators affecting them",
                 addCoef.col = "black", type = "lower")

Corr1 <- corrplot(Pearson2, tl.col = "red", tl.srt = 45, bg = "White",
                  title = "Correlation of number of COVID-cases and indicators affecting them",
                  addCoef.col = "black", type = "lower")

# multivariate analysis
model <- lm(confirmed ~ Migrants + Lat + Fertility_rate + Median_age + Urban_population, data = confirmed_norm2)
summary(model)  #thus p value is highly significant, so one predictor variable is significant

model2 <- lm(confirmed ~ stringency_index + gdp_per_capita, data = Owid)

# Determine significant variable(s)
summary(model)$coefficient

summary(model2)$coefficient

# remove the insignificant (Latitude in this case)
model <- lm(confirmed ~ Migrants + Lat + Fertility_rate + Median_age + Urban_population, data = confirmed_norm2)
summary(model)

# Confidence interval of the model coefficient
confint(model)

confint(model2)

# plot 
avPlots(model)

avPlots(model2)

# Since the variable Migration is the greatest influence, choose 10 countries to work on
Mig_data <- confirmed_norm2 %>% # selecting only the migrants data
  select(Country, Migrants, confirmed)

Mig_summaries <- Mig_data %>%  # first calculate the average of each country before finding the top 10
  group_by(Country) %>% 
  summarise(Migrants = mean(Migrants, na.rm = T))

Mig <- Mig_summaries %>% #arrange in descending order
  arrange(-Migrants)

Final_mig <- Mig %>% #select top 10
  slice(1:10)

# plot to show influence of migrants in these 10 countries
data1 <- Mig_data %>% 
  filter(Country == c("US", "Germany", "Turkey", "United Kingdom", "Canada", "Colombia", "Russia", "Uganda", "Australia", "Italy"))

e <- ggplot(data1, aes(x = Migrants, y = confirmed))

e2 <- e + geom_boxplot(aes(fill = Country),
  position = position_dodge(0.9)) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07", "#A4A4A4", "#999999", "#E69F00", "#56B4E9", "#E7B800", "#FC4E07", "#00AFBB"))
e2









