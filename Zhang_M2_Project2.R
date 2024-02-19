#Print your name at the top of the script. Include the prefix: “Plotting Basics:” such that it
#appears “Plotting Basics: Firstname Lastname ”
print("Plotting Basics : Tianyu Zhang")
#Import libraries including: FSA, FSAdata, magrittr, dplyr, plotrix, ggplot2, and moments
#NOTE: You must use R version 3.6.3 to gain access to the FSA data set. If you installed a
#later version of R, you must uninstall Rstudio and R. Then reinstall R version 3.6.3; then
#reinstall Rstudio
install.packages("FSA")
install.packages("FSAdata")
install.packages("magrittr")
install.packages("dplyr")
install.packages("plotrix")
install.packages("ggplot2")
install.packages("moments")
library(FSA)
library(FSAdata)
library(magrittr)
library(dplyr)
library(plotrix)
library(ggplot2)
library(moments)
#Load the BullTroutRML2 dataset (BullTroutRML2.csv)
data(BullTroutRML2)
# Print the first 5 records
print(head(BullTroutRML2, 5))

# Print the last 5 records
print(tail(BullTroutRML2, 5))

#Remove all records except those from Harrison Lake (hint: use dplyr::filter() function)
#NOTE: From this point forward any reference to BullTroutRML2 always refers to the
#filtered dataset (Harrison Lake only data is used). You may choose to rename the
#dataset at this point.
BullTroutRML2_filtered <- dplyr::filter(BullTroutRML2, lake == "Harrison")

#Display the first 4 and last 4 records from the filtered BullTroutRML2 dataset
# Display the first 4 records
print(head(BullTroutRML2_filtered, 4))

# Display the last 4 records
print(tail(BullTroutRML2_filtered, 4))

#Display the structure of the filtered BullTroutRML2 dataset
str(BullTroutRML2_filtered)
#Display the summary of the filtered BullTroutRML2 dataset
summary(BullTroutRML2_filtered)

#Create a scatterplot for “Age (yrs)” (y variable) and “Fork Length (mm)” (x variable)
#with the following specifications:
#• Limit of x axis is (0,500)
#• Limit of y axis is (0,15)
#• Title of graph is “Plot 1: Harrison Lake Trout”
#• Y axis label is “Age (yrs)”
#• X axis label is “Fork Length (mm)”
#• Use small solid circles for the plotted data points
plot(BullTroutRML2_filtered$fl, BullTroutRML2_filtered$age,
     xlim = c(0,500), ylim = c(0,15),
     main = "Plot 1 : Harrison Lake Trout",
     xlab = "Fork Length (mm))", ylab = "Age(yrs)",
     pch = 21, cex = 0.5)
#Plot an “Age” histogram with the following specifications
#• Y axis label is “Frequency”
#• X axis label is “Age (yrs)”
#• Title of the histogram is “Plot 2: Harrison Fish Age Distribution”
#• The color of the frequency plots is “lightblue”
#• The color of the Title is “red”


hist(BullTroutRML2_filtered$age, xlab = "Age (yrs)", ylab = "Frequency",
     main = "Plot 2: Harrison Fish Age Distribution",
     col = "lightblue")
title(main = "Plot 2: Harrison Fish Age Distribution", col.main = "red")

#Create a plot using the same specifications as the previous scatterplot. But,
#• Title the plot “Plot 3: Harrison Density Shaded by Era”
#• Y axis label is “Age (yrs)”
#• Y axis limits are 0 to 15
#• X axis label is “Fork Length (mm)”
#• X axis limits are 0 to 500
#• include two levels of shading of blue for the data points based on era values.
#• Plot solid diamonds as data points


ggplot(BullTroutRML2_filtered, aes(x = fl, y = age, color = era)) +
  geom_point(shape = 4, size = 3) +
  ggtitle("Plot 3: Harrison Density Shaded by Era")+
  xlab("Fork Length (mm)") +
  ylab("Age (yrs)") +
  scale_color_manual(values = c("darkblue","lightblue")) +
  xlim(0,500) + ylim(0,15) 

#Create a new object called “tmp” that includes the first 3 and last 3 records of the
#BullTroutRML2 dataset.

head(BullTroutRML2, 3)
tail(BullTroutRML2, 3)
tmp <- rbind(head(BullTroutRML2, 3), tail(BullTroutRML2, 3))

#Display the “era” column (variable) in the new “tmp” object

tmp$era
#Create a pchs vector with the argument values for + and x.
#Create a cols vector with the two elements “black” and “red”
pchs <- c("+", "x")
cols <- c("black", "red")

# Convert the tmp era values to numeric values.
# Initialize the pchs and cols vector conditional on the tmp era values

tmp$era <- as.numeric(tmp$era) 
tmp$pch <- pchs[tmp$era]
tmp$col <- cols[tmp$era]

#Create a plot of “Age (yrs)” (y variable) versus “Fork Length (mm)” (x variable) with the 
#following specifications: 
 # • Title of graph is “Plot 4: Symbol & Color by Era” 
#• Limit of x axis is (0,500) 
#• Limit of y axis is (0,15) 
#• Y axis label is “Age (yrs)” 
#• X axis label is “Fork Length (mm)” 
#• Set pch equal to pchs era values 
#• Set col equal to cols era values 
ggplot(tmp, aes(x = fl, y = age, pch = pch, col = col)) +
  geom_point() +
  xlim(0, 500) +
  ylim(0, 15) +
  xlab("Fork Length (mm)") +
  ylab("Age (yrs)") +
  ggtitle("Plot 4: Symbol & Color by Era")

#Plot a regression line (blue color) overlay on Plot 4 and title the new graph “Plot 5: 
#Regression Overlay”. 
ggplot(tmp, aes(x = fl, y = age)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  xlim(0, 500) +
  ylim(0, 15) +
  xlab("Fork Length (mm)") +
  ylab("Age (yrs)") +
  ggtitle("Plot 5: Regression Overlay")

#Place a legend of on Plot 5 and call the new graph “Plot 6: Legend Overlay” 
ggplot(tmp, aes(x = fl, y = age, pch = pch, col = col)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  xlim(0, 500) +
  ylim(0, 15) +
  xlab("Fork Length (mm)") +
  ylab("Age (yrs)") +
  ggtitle("Plot 6: Legend Overlay")
 