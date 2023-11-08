
### read the simulated class data from open intro website

classdata <- read.csv("https://www.openintro.org/data/csv/classdata.csv")

View(classdata) ## opens in a new tab

## we'll select only two sections: a and b for our comparison.

classdata_ab <- classdata[classdata$lecture %in% c("a", "b"),]

## look at the structure of the data 

str(classdata_ab)

## visual comparison: do they look different? 

boxplot(m1~lecture, data = classdata_ab)

## summarize by group 

library(dplyr)

# %>% (ctrl + shift + M)

classdata %>% 
  group_by(lecture) %>% 
  summarize(mean = mean(m1), median = median(m1), 
                                                 sd = sd(m1), n = n())

## two-sample tests (nonparametric vs. parametric)

wilcox.test(m1 ~ lecture, data = classdata_ab, alternative = "greater")

t.test(m1 ~ lecture, data = classdata_ab, alternative = "greater")


#### Another example 
## ACS Data

acs12 <- read.csv("https://www.openintro.org/data/csv/acs12.csv")
View(acs12)

### pick one categorical variable: 

table(acs12$married)

## summarize by group 

acs12 %>% group_by(married) %>% summarize(mean = mean(income), 
                                          median = median(income),
                                          sd = sd(income))
## the NA's appear because missing data. 
## We can consider only the complete cases i.e. rows without any missingness

acs12 <- acs12[complete.cases(acs12),]

## look at the summary again

acs12 %>% group_by(married) %>% summarize(mean = mean(income), 
                                          median = median(income),
                                          sd = sd(income))


### visual
boxplot(income ~ married, data = acs12)

library(ggplot2)

acs12 %>% ggplot(aes(y = income, group = married)) + 
  geom_boxplot()+theme_bw()

acs12 %>% ggplot(aes(x = income, group = married)) +
  geom_histogram()+facet_wrap(married~.)+theme_bw()

### now the test

wilcox.test(income ~ married, data = acs12)

t.test(income ~ married, data = acs12)
