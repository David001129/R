install.packages("FSA")
install.packages("FSAdata")
install.packages("magrittr")
install.packages("dplyr")
install.packages("tidyr")
install.packages("plyr")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("knitr")
library("FSA")
library("FSAdata")
library("magrittr")
library("dplyr")
library("tidyr")
library("plyr")
library("tidyverse")
library("ggplot2")
library("knitr")

nasa_exoplanets <- read_csv("C:/Users/张天羽/Desktop/cleaned_5250.csv")
View(nasa_exoplanets)
summary(nasa_exoplanets)
str(nasa_exoplanets)
# install and load the openxlsx package
install.packages("openxlsx")
library(openxlsx)

# create a data frame with the summary statistics
summary_df <- summary(nasa_exoplanets)

# write the data frame to an Excel file
write.xlsx(summary_df, "summary.xlsx")

nasa_exoplanets$detection_method


str(nasa_exoplanets$detection_method)
count(nasa_exoplanets$detection_method)
count(nasa_exoplanets$planet_type)
# Count the number of exoplanets by detection method
detection_counts <- table(nasa_exoplanets$detection_method)

# Create a barplot with rotated x-axis labels
barplot(detection_counts, 
        main = "Number of Exoplanets Discovered by Detection Method",
        xlab = "Detection Method", 
        ylab = "Count",
        names.arg = names(detection_counts),
      las =2, cex.names = 0.5)

planet_counts <- table(nasa_exoplanets$planet_type)

# Create a bar plot
barplot(planet_counts, 
        main = "Number of Exoplanets by Type",
        xlab = "Planet Type", 
        ylab = "Count",
        col = "steelblue")

ggplot(nasa_exoplanets, aes(x = distance)) +
  geom_histogram(binwidth = 50) +
  labs(title = "Distribution of Exoplanet Distances",
       x = "Distance (parsecs)",
       y = "Count") +
  theme_minimal()

ggplot(nasa_exoplanets, aes(x = mass_wrt, y = radius_wrt)) +
  geom_point(alpha = 0.5) +
  labs(title = "Exoplanet Mass vs. Radius",
       x = "Mass (Earth Masses)",
       y = "Radius (Earth Radii)") +
  theme_minimal()



















# Create histogram of planet masses
nasa_exoplanets %>%
  filter(!is.na(pl_bmassj)) %>%
  ggplot(aes(x = pl_bmassj)) +
  geom_histogram(bins = 50, color = "white") +
  labs(title = "Distribution of Planet Masses",
       x = "Planet Mass (Jupiter masses)",
       y = "Count") +
  theme_minimal()

# Create scatterplot of planet radius vs. equilibrium temperature
nasa_exoplanets %>%
  filter(!is.na(pl_radj) & !is.na(pl_eqt)) %>%
  ggplot(aes(x = pl_radj, y = pl_eqt, color = pl_discmethod)) +
  geom_point() +
  labs(title = "Planet Radius vs. Equilibrium Temperature",
       x = "Planet Radius (Jupiter radii)",
       y = "Equilibrium Temperature (K)",
       color = "Discovery Method") +
  theme_minimal()

# Clean data by removing out-of-range values for planet mass and radius
nasa_exoplanets_cleaned <- nasa_exoplanets %>%
  filter(pl_bmassj >= 0 & pl_bmassj <= 25 &
           pl_radj >= 0 & pl_radj <= 10)

# Repeat descriptive statistics on cleaned data
summary(nasa_exoplanets_cleaned)

# Repeat histogram of planet masses with cleaned data
nasa_exoplanets_cleaned %>%
  ggplot(aes(x = pl_bmassj)) +
  geom_histogram(bins = 50, color = "white") +
  labs(title = "Distribution of Planet Masses",
       x = "Planet Mass (Jupiter masses)",
       y = "Count") +
  theme_minimal()

# Repeat scatterplot of planet radius vs. equilibrium temperature with cleaned data
nasa_exoplanets_cleaned %>%
  filter(!is.na(pl_radj) & !is.na(pl_eqt)) %>%
  ggplot(aes(x = pl_radj, y = pl_eqt, color = pl_discmethod)) +
  geom_point() +
  labs(title = "Planet Radius vs. Equilibrium Temperature",
       x = "Planet Radius (Jupiter radii)",
       y = "Equilibrium Temperature (K)",
       color = "Discovery Method") +
  theme_minimal()

