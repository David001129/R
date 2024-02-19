#Print your name at the top of the script and load these libraries: FSA, FSAdata, magrittr,
#dplyr, tidyr plyr and tidyverse
print("Tianyu Zhang")
install.packages("FSA")
install.packages("FSAdata")
install.packages("magrittr")
install.packages("dplyr")
install.packages("tidyr")
install.packages("plyr")
install.packages("tidyverse")
install.packages("ggplot2")
library("FSA")
library("FSAdata")
library("magrittr")
library("dplyr")
library("tidyr")
library("plyr")
library("tidyverse")
library("ggplot2")

years <- c(2018, 2019, 2020, 2021, 2022)
demand_healthcare_products <- c(12, 13, 14, 15, 16)
demand_healthcare_information_technology <- c(13, 14, 15, 16, 17)
demand_healthcare_support_services <- c(13, 14, 15, 16, 18)

# Create a data frame
df <- data.frame(years, demand_healthcare_products, demand_healthcare_information_technology, demand_healthcare_support_services)

# Create the line graph
ggplot(df, aes(years)) +
  geom_line(aes(y = demand_healthcare_products, color = "Healthcare Products")) +
  geom_line(aes(y = demand_healthcare_information_technology, color = "Healthcare Information & Technology")) +
  geom_line(aes(y = demand_healthcare_support_services, color = "Healthcare Support Services")) +
  labs(x = "Years", y = "Demand") +
  scale_color_manual(name = "Industry", values = c("Healthcare Products" = "red", "Healthcare Information & Technology" = "blue", "Healthcare Support Services" = "green")) +
  theme_classic()

years<-c(2022,2023) 
register_total_market_value<-c(162.25,194.7)
df<-data.frame(years,register_total_market_value)    
ggplot( df, aes(years)) +  geom_line(aes(y = register_total_market_value  , color = "Market Value")) + 
  labs(x = "Years/yrs", y = "Demand/bn.") +  
  scale_color_manual(name = "Industry", values = c("Market Value" = "red"))
  theme_classic()
  
  years<-c(2022,2023) 
  register_total_market_value<-c(162.25,194.7)
  df<-data.frame(years,register_total_market_value)
  ggplot(df, aes(x=years, y=register_total_market_value)) +
    geom_col(fill="red", color="black") +
    labs(x = "Years/yrs", y = "Demand/bn.") +
    theme_classic()
#. Import the inchBio.csv and name the table <tBio>
data_analysis <- read.csv("C:/Users/张天羽/Desktop/data.CSV")
jobs_posts <- read.csv("C:/Users/张天羽/Desktop/jobs_posts.CSV")
library(ggplot2)

jobs_posts <- c(1367491, 374948, 817375, 724928, 15891, 298613, 2722229, 396934, 238009, 467371, 358050, 77818)
jobs <- c("Internet·Game·Software", "Electronics, communications, and hardware", "Real estate·construction", "Finance", "Consumer goods", "Automobile and machinery manufacturing", "Healthcare", "Energy", "Advertising·Media", "Transportation", "Government", "Other")

df <- data.frame(jobs, jobs_posts)

ggplot(df, aes(x = "", y = jobs_posts, fill = jobs)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  ggtitle("Jobs Posts") +
  scale_fill_discrete(guide = guide_legend(reverse = TRUE)) +
  theme_void()


#Create a barplot of <tSpec>
par(mgp = c(4, 1, 0))
barplot(data_analysis$salary, col = "red", ylab = "Salaries($)",
        names.arg = data_analysis$companies, 
        main = "Average salary for the role", ylim = c(0,150000),las = 2, cex.names = 0.64 )


#Create a barplot of <tSpecPct>

barplot(tSpecPct, col = "yellow", ylab = "%",
        main = "Plot 2: Fish Relative Frequency",ylim = c(0,0.35), las = 2, cex.names = 0.55)

#Rearrange the <dfSP> data frame in descending order of relative frequency. Save the
#rearranged data frame as the object <data>
data <- dfSP %>% arrange(desc(dfSP$Freq))
data

#Rename the <data> columns Var 1 to Species, and Freq to RelFreq
colnames(data) <- c("Species", "RelFreq")
data

#Add new variables to <data> and call them cumFreq, cts, and cumCts
data <- data %>%
  mutate(cumFreq = cumsum(RelFreq), cts = cts , cumCts = cumsum(cts))
data

#Create a parameter variable <varPar> to store graphical parameters 
varPar <- list()
varPar
#Create a barplot for data$cts, <pc>,
barplot(data$cts, width = 1, space = .1, border = NA, axes = F, 
        ylim = c(0, 3.05*max(data$cts, na.rm=T)), ylab = "Cumulative Counts", 
        xaxs = "i", cex.names = 0.55, names.arg = data$Species, 
        main = "Plot 3: Species Pareto", las = 2)


#Add a cumulative counts line to the <pc> plot 
lines(data$cumCts, type = "b", col = "blue", lty = 2, lwd = 2)

#Place a grey(grey62) box around the pareto plot 
box(col="grey62", lty=1, lwd=2)

#Add a left side axis with the following specifications 
axis(side = 2, at = data$cts, col = "grey62", col.axis = "grey62", cex.names = 0.75)

#Add axis details on right side of box 
axis(side = 4, at = data$cumCts, labels = data$cumFreq, las = 2, col.axis = "cyan4", col.lab = "cyan4", cex.name = 0.75, tick = TRUE)

#Display the finished Species Pareto Plot
text(x = nrow(data), y = max(data$cts)*2.5, col = "red", labels = "Zhang", cex = 0.75)
