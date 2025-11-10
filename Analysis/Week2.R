# This is a comment. The code is not run; it is used for annotation.

# This script was used to generate the plots used in the lecture on exploratory spatial analysis.

# It is a useful exercise to work through the code alongside the lecture slides and insert comments that state what each
# block of code does.

# Note that these data have already been converted into a spatial object
# using the geocoded postcodes.


# setwd("C:/Users/cege/Dropbox/Spatial Analysis & Geocomputation/Tutorials/SpatialAnalysisandGeocomputation")
# Put the location of the week 2 data here
# install libraries if necessary
#install.packages("tmap")
#install.packages("ggplot2")
library(tmap)
library(tidyverse)
load("Data/House Prices/housepricesshp")
load("Data/Boundaries/London/LondonLSOA")
load("Data/Boundaries/London/LondonWards")
ls()

#EDA: Open the data and look at it...
nrow(housepricesshp)
ncol(housepricesshp)
colnames(housepricesshp)

## 1. What is an average house price?
# Draw a histogram
housepricesshp%>%
  ggplot(aes(Price)) +
  geom_histogram(breaks=seq(0,2000000,100000))

# Add mean and median price
housepricesshp%>%
  ggplot(aes(Price)) +
  geom_histogram(breaks=seq(0,2000000,100000)) +
  geom_vline(aes(xintercept = mean(Price)),col='red',size=2) +
  geom_vline(aes(xintercept = median(Price)),col='blue',size=2)

## 2. Exploring the variation in prices paid
# Add interquartile range
housepricesshp%>%
  ggplot(aes(Price)) +
  geom_histogram(breaks=seq(0,2000000,50000)) +
  geom_vline(aes(xintercept = median(Price)),col='blue',size=1.5)+
  geom_vline(aes(xintercept = quantile(Price)[2]),col='blue',size=1.5)+
  geom_vline(aes(xintercept = quantile(Price)[4]),col='blue',size=1.5)

# Calculate summary statistics
housepricesshp$Price%>%summary()

min(housepricesshp$Price)
max(housepricesshp$Price)
mean(housepricesshp$Price)
median(housepricesshp$Price)
sd(housepricesshp$Price)
mean(housepricesshp$Price)-sd(housepricesshp$Price)
quantile(housepricesshp$Price)


## 3. Exploring variations in prices of properties of different types

# Draw boxplots broken down by property type
housepricesshp%>%
  ggplot(aes(x=Property_type, y=Price)) +
  geom_boxplot()

# Remove 'other' type from boxplot
housepricesshp%>%filter(Property_type!="O")%>%
  ggplot(aes(x=Property_type, y=Price)) +
  geom_boxplot()

# Draw boxplot with truncated y-axis
housepricesshp%>%filter(Property_type!="O")%>%
  ggplot(aes(x=Property_type, y=Price)) +
  geom_boxplot()+
  coord_cartesian(ylim = quantile(housepricesshp$Price, c(0, 0.97)))

## 4. Exploring geographic variations in prices
# Count the number of unique counties
length(unique(housepricesshp$County))

# Draw a boxplot broken down by county
housepricesshp%>%
  ggplot(aes(x=County, y=Price)) +
  geom_boxplot()+
  coord_cartesian(ylim = quantile(housepricesshp$Price, c(0, 0.97)))

# Mapping the raw house price data
# These examples may take a few minutes run
library(sf)

# Make the data sampling select the same 'random' sample
# This ensures you will get the same results
set.seed(123)
smp <- sample(1:nrow(housepricesshp), 10000)
housepricesshp_sample <- housepricesshp[smp,]

# Map the data using Jenks breaks - default in ArcGIS
tmap_mode("view")
# Map the data using quantile breaks
housepricesshp_sample%>%
  tm_shape()+
  tm_dots(col="Price", style="quantile") # jenks


Counties <- st_read("Data/GB/English Ceremonial Counties.shp")%>%st_transform(st_crs(housepricesshp))
medianhpcounties <- aggregate(housepricesshp, Counties, median)
medianhpcounties%>%
  tm_shape()+
  tm_polygons(col="Price")

# Ensure the county and point data use the same scale
# Use breaks from the county data
brks <- medianhpcounties$Price%>%quantile(probs=seq(0, 1, 0.2))
medianhpcounties%>%
  tm_shape()+
  tm_polygons(col="Price", style="quantile")+
  tm_shape(housepricesshp[smp,])+
  # breaks the price into 5 equal parts, if you want the color filled, use breaks
  tm_dots(col="Price", style="fixed", breaks=brks)

# Use breaks from the point data
brks <- housepricesshp_sample$Price%>%quantile(probs=seq(0, 1, 0.2))
medianhpcounties%>%
  tm_shape()+
  tm_polygons(col="Price", style="fixed", breaks=brks)+
  tm_shape(housepricesshp[smp,])+
  tm_dots(col="Price", style="quantile")

# Create a new dataset containing only the transactions within Greater Manchester
manchester <- Counties%>%
  filter(NAME=="Greater Manchester")
GreaterManchester <- housepricesshp[manchester,]

# Plot all transactions within Greater Manchester
manchester%>%
  tm_shape()+
  tm_polygons()+
  tm_shape(GreaterManchester)+
  # use the national level breaks
  tm_dots(col="Price", style="fixed", breaks=brks)

# Load the England Wards polygon data
Wards <- st_read("Data/GB/england_wa_2011.shp")%>%
  st_transform(st_crs(housepricesshp))
# Extract the wards data for Greater Manchester
GrManWards <- Wards[manchester,]

#Plot the Greater Manchester wards and counties
GrManWards%>%
  tm_shape()+
  tm_polygons()+
  tm_shape(manchester)+
  tm_polygons()

# Aggregate the house prices at ward level using the median
medianhpgrmanc <- aggregate(housepricesshp, GrManWards, median)

#Plot the median house prices at ward level using national level breaks
medianhpgrmanc%>%
  tm_shape()+
  tm_polygons(col="Price", style="fixed", breaks=brks)

# Plot a histogram of Greater Manchester house prices
GreaterManchester%>%
  ggplot(aes(Price)) +
  geom_histogram(breaks=seq(0,2000000,50000)) +
  geom_vline(aes(xintercept = median(Price)),col='blue',size=1.5)+
  geom_vline(aes(xintercept = quantile(Price)[2]),col='blue',size=1.5)+
  geom_vline(aes(xintercept = quantile(Price)[4]),col='blue',size=1.5)
  quantile(GreaterManchester$Price)

