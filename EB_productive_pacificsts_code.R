###### Midterm Exam ######
###### Elijah Bakam IR 412 ######
###### Created: 11/11/2023 ######
###### Last Modified: 11/11/2023 ######
###### In this script, I will be creating a scatter plot, making it pretty, and saving it as a pdf ######

# Step 0 I am going to clean my working environment so I can operate without clutter
rm(list=ls())

# Load all my packages 
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)

# Next we will set our working directory so R knows where to get data from in the Training Data folder
setwd("/Users/elijahbakam/Library/CloudStorage/GoogleDrive-bakam@usc.edu/.shortcut-targets-by-id/19DfrFWKARGNjNlJ0nheNeauY-8DKul1a/412_413 shared AY23-24/Training Data Fall 23")

# Step 1 we will load the data in. We are using ProductivePacifists_Data.RDATA

# I had an issue with error messages loading in the data so I went to open the file in R first so it could give me the exact filepath I needed to make it show up in my console
load("/Users/elijahbakam/Library/CloudStorage/GoogleDrive-bakam@usc.edu/.shortcut-targets-by-id/19DfrFWKARGNjNlJ0nheNeauY-8DKul1a/412_413 shared AY23-24/Training Data Fall 23/ProductivePacifists_Data.RDATA")

# Step 2 subset the data to choose 4 years from the data (1925,1950,1975,2000) and 4 countries of choice

# I will first create a vector to use to lump my selected countries together

# Note: countries are identified by gwno and as numbers, not names
countries <- c(90, 91, 92, 93) # I chose (in order), Guatemala, Honduras, El Salvador, and the Nicaragua

# I will do the same for my years
years <- c(1925, 1950, 1975, 2000)

# Now when we filter, I can just plug in the countries/years vector
dat1 <- dat %>%
  filter(gwno %in% countries,
         year %in% c(1925,1950,1975,2000)) # using %in% means that R will comb through the data and only pull what I've designated in my vectors

# Step 3 create a scatter plot with the log of gdp per capita on the x axis
# and land orientation on the Y axis
ggplot(dat1, aes(x = WorldBank_gdppc_2010_con_estimate, y = land_oriented_medium_continuous_trim)) + # puts each variable on the right axis
  geom_point(color = "indianred4", size = 3) + # this is just a test color to see if I like it going forward
  labs(title = "Log of GDP Compared to Land Orientation",
       subtitle = "Seen via 4 Central American States ", # Titles, can change these afterwards but for now these will be fine
       x = "Log of GDP",
       y = "Land Orientation") 

# Step 4 Give each year a different color on the plot
# + BONUS create a legend that matches the years to the colors
ggplot(dat1, aes(x = WorldBank_gdppc_2010_con_estimate, y = land_oriented_medium_continuous_trim, color = as.factor(year))) + # as.factor(year) will allow the colors to be changed for each separate year
  geom_point(size = 3) + # size of our dot
  labs(title = "Log of GDP Compared to Land Orientation", # these are all titles and labels for my plot
       subtitle = "Observed in 4 Central American States",
       x = "Log of GDP",
       y = "Land Orientation",
       color = "Year") +
  scale_color_manual(values = c("1925" = "deeppink", "1950" = "mediumblue", "1975" = "darkgoldenrod1", "2000" = "gray0")) # this gives us the chance here to pick whatever color you'd like by making a vector for each year to be assigned a color
# 1950 does not have any data for land continuous so that's why this plot only returns dots for 1925, 1975, and 2000

# BONUS 2 Give each country a different dot shape on the plot
dat1$shape_num <- as.numeric(as.factor(dat1$country)) # creates a numeric variable for the shape based on country

ggplot(dat1, aes(x = WorldBank_gdppc_2010_con_estimate, y = land_oriented_medium_continuous_trim, color = as.factor(year), shape = as.factor(shape_num))) + # shape as factor does the same effect as colors where you can choose each country with a unique shape
  geom_point(size = 3) +
  labs(title = "Log of GDP Compared to Land Orientation",
       subtitle = "Observed in 4 Central American States",
       x = "Log of GDP",
       y = "Land Orientation", 
       color = "Year",
       shape = "Country") + 
  scale_color_manual(values = c("1925" = "violet", "1950" = "mediumblue", "1975" = "darkgoldenrod1", "2000" = "gray0")) +
  scale_shape_manual(values = seq_along(unique(dat1$gwno)),
                     breaks = seq_along(unique(dat1$gwno)), # breaks is set to the numeric value assigned to each country
                     labels = unique(dat1$gwno)) # labels is set to unique country name that ensures my legend will show the gwno code for each country

# Now I like this table but I want to change the subtitle to explain the gwno codes because this dataset doesn't code them as their names
ggplot(dat1, aes(x = WorldBank_gdppc_2010_con_estimate, y = land_oriented_medium_continuous_trim, color = as.factor(year), shape = as.factor(shape_num))) + # shape as factor does the same effect as colors where you can choose each country with a unique shape
  geom_point(size = 3) +
  labs(title = "Log of GDP Compared to Land Orientation",
       subtitle = "90 = Guatemala, 91 = Honduras, 92 = El Salvador, 93 = Nicaragua",
       x = "Log of GDP",
       y = "Land Orientation", 
       color = "Year",
       shape = "Country") + 
  scale_color_manual(values = c("1925" = "violet", "1950" = "mediumblue", "1975" = "darkgoldenrod1", "2000" = "gray0")) +
  scale_shape_manual(values = seq_along(unique(dat1$gwno)),
                     breaks = seq_along(unique(dat1$gwno)), # breaks is set to the numeric value assigned to each country
                     labels = unique(dat1$gwno))

# I'd also like to make my background white and I can do that with a theme_minimal() command
ggplot(dat1, aes(x = WorldBank_gdppc_2010_con_estimate, y = land_oriented_medium_continuous_trim, color = as.factor(year), shape = as.factor(shape_num))) + # shape as factor does the same effect as colors where you can choose each country with a unique shape
  geom_point(size = 3) +
  labs(title = "Log of GDP Compared to Land Orientation",
       subtitle = "90 = Guatemala, 91 = Honduras, 92 = El Salvador, 93 = Nicaragua",
       x = "Log of GDP",
       y = "Land Orientation", 
       color = "Year",
       shape = "Country") + 
  scale_color_manual(values = c("1925" = "violet", "1950" = "mediumblue", "1975" = "darkgoldenrod1", "2000" = "gray0")) +
  scale_shape_manual(values = seq_along(unique(dat1$gwno)),
                     breaks = seq_along(unique(dat1$gwno)), # breaks is set to the numeric value assigned to each country
                     labels = unique(dat1$gwno)) +
  theme_minimal() + # gives me a minimal theme
  theme_bw() # this is the easiest way to do a black and white background

# I would also like to make my title + axis labels stand out by being bolded which I can doi by changing the theme
p <- ggplot(dat1, aes(x = WorldBank_gdppc_2010_con_estimate, y = land_oriented_medium_continuous_trim, color = as.factor(year), shape = as.factor(shape_num))) + # shape as factor does the same effect as colors where you can choose each country with a unique shape
  geom_point(size = 3) +
  labs(title = "Log of GDP with Land Orientation",
       subtitle = "90 = Guatemala, 91 = Honduras, 92 = El Salvador, 93 = Nicaragua",
       x = "Log of GDP",
       y = "Land Orientation", 
       color = "Year",
       shape = "Country") + 
  scale_color_manual(values = c("1925" = "violet", "1950" = "mediumblue", "1975" = "darkgoldenrod1", "2000" = "gray0")) +
  scale_shape_manual(values = seq_along(unique(dat1$gwno)),
                     breaks = seq_along(unique(dat1$gwno)), # breaks is set to the numeric value assigned to each country
                     labels = unique(dat1$gwno)) +
  theme_minimal() + # gives me a minimal theme
  theme_bw() +
  theme(plot.title = element_text(face = "bold"),
         axis.text.x = element_text(face = "bold"), # You can make anything bolded from within the theme function and just add which facet of the plot you want bolded
        axis.text.y = element_text(face = "bold")) 

# To save this plot as a PDF I can use ggsave function
download_path <- file.path(Sys.getenv("HOME"), "Downloads", "EB_midterm_plot.pdf") # this specifies the file path of where I want my pdf to be saved
ggsave(download_path, plot = p, width = 8, height = 6, units = "in", device = "pdf")










































