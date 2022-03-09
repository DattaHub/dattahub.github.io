## ----import data---------------------------------------------------------
library(dplyr)
cats <- read.csv("data/herding-cats.csv")


## ----scatter-plot-----------------------------------
# scatter plot : graphical view of relationship between two sets of numbers.
# scatterplot of birth weight by mother's age.

plot(x = cats$age, y = cats$weight, pch = 15, main = "age vs. weight")

# For any plot you can customize many features of your graphs 
# (fonts, colors, axes, titles) through graphic options (see pdf)


## ----ggplot-----------------------------------------##
# From base graphic to `ggplot2` : adds a lot of functionality 
# The syntax is different but it's extremely powerful and flexible.

install.packages("ggplot2")


## ----load ggplot2--------------------------------------------------------
library(ggplot2)

#`ggplot()` initializes the basic graph structure, then we add to it.
# The basic idea is that you specify different parts of the plot, 
# and add them together using the + operator.


## ---- Blank plot -------------------------------------------------
# Start with a blank plot 

ggplot(cats)

# Will produce no points/lines, so you need to add layers.


## ---- Geometric objects---------------------------------------------------------
# Geometric objects are the actual marks we put on a plot. 
# Examples include:
# * points (geom_point, for scatter plots, dot plots, etc)
# * lines (geom_line, for time series, trend lines, etc)
# * boxplot (geom_boxplot, for, well, boxplots!)

ggplot(cats) +
    geom_point()

# Still an error 
# Each type of geom usually has a required set of aesthetics to be set,
# and usually accepts only a subset of all aesthetics.

?geom_point

ggplot(cats) + geom_point(aes(x = weight, y = wander_dist))

## ---- scatterplot-------------------------------------------------
ggplot(cats) +
    geom_point(aes(x = age, y = weight),
               color = "red",
               alpha = 0.5,
               shape = 1,
               size = 3)


## -------Scales -----------------------------------------------------------------
# Scales control the mapping between data and aesthetics.

ggplot(cats) +
    geom_point(aes(x = age, y = weight)) +
    scale_x_continuous(name = "Age",
                       breaks = c(1, 2, 3),
                       limits = c(-5, 15)) +
    scale_y_continuous("Weight", trans = "log") + 
    ggtitle(label = "Scatterplot")+
    theme_bw()


## ---- Themes-------------------------------------------------
# The ggplot2 theme system handles non-data plot elements such as:
    
# * Axis labels
# * Plot background
# * Facet label backround
# * Legend appearance

ggplot(cats) +
    geom_point(aes(x = age, y = weight)) +
    theme_minimal()


## ----Faceting-----------------------------------------------##

# Facets display subsets of the dataset in different panels. 
# Let's use the `facet_grid` function to lay out panels in a grid.

ggplot(cats) +
    geom_point(aes(x = age, y = weight)) +
    xlab("Mother's age") +
    ylab("Birth weight") +
    # facet_grid(. ~ coat) +
    theme_bw()

## How to save? 
# 1. export directly from the RStudio 'Plots' panel
# 2. use R functions in the console, allowing you the flexibility 
#    to specify the size and resolution of the output image.

ggsave("cats_facet.pdf", height = 5, width = 7)

## where is it? 

## Comments:
# We have just scratched the surface (pun intended)
# See the pdf file for more resources. 

## Read the airline-safety data 

dat <- read.table("data/airline-safety.csv", 
                  sep = ",", header = TRUE)


## Boxplot 

ggplot(dat) +
    geom_boxplot(aes(x = airline, y = avail_seat_km_per_week)) +
    ggtitle("Available seats vs Airline") +
    xlab("Airline") + ylab("Available Seats") +
    theme_minimal()

## Boxplot

# Fix angles of X-axis texts? 
# Try the following: 

ggplot(dat) +
    geom_boxplot(aes(x = airline, y = avail_seat_km_per_week)) +
    ggtitle("Available seats vs Airline") +
    xlab("Airline") + ylab("Available Seats") +
    theme_minimal()+
    theme(axis.text = element_text(angle = 45, size = 5))

## Scatterplot (with colours)

ggplot(dat) +
    geom_point(aes(y = fatalities_00_14, x = avail_seat_km_per_week)) +
    ggtitle("Fatalities vs. Available seats") +
    xlab("Available Seats") + ylab("Fatalities") + 
    theme_minimal()+
    theme(axis.text = element_text(angle = 45, size = 10))


## Exercise 

# Recreate the above plot but add a straight line
# to it using `geom_smooth()`

