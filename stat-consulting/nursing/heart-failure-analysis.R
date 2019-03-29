rm(list=ls())
setwd("C:/Users/jd033/OneDrive/Documents/R/Nursing")
heart <- read.csv("Heart-failure-project-data.csv",sep=",",header=T)
str(heart)
library(tidyverse)
# heart.wide.1 <- spread(heart,Question,Score)

library(reshape2)
colnames(heart)[1] <- "subject"
heart.new <- heart[,c(1:3,5)]
heart.wide <-  dcast(heart.new, subject + Group ~ Question, sum, value.var="Score")
head(heart.wide)

recode(heart.wide$`Forget Meds`, `1` = 4, `2` = 3, `3`= 2, `4` = 1, `0` = 0)

heart.wide[is.na(heart.wide)] <- 0 

# (heart.wide$sec.A.score = Weigh+Swelling+Sick+Activity+Appointment+`Low Salt Diet`+
  # Exercise+`Forget Meds`+`Ask for  low Salt`+`Pill box`)

heart.wide$sec.A.score = with(heart.wide, Weigh+Swelling+Sick+Activity+Appointment+`Low Salt Diet`+
       Exercise+`Forget Meds`+`Ask for  low Salt`+`Pill box`)
Max.sec.A.score = 4*10
Min.sec.A.score = 0
(heart.wide$normalized.sec.A.score = (heart.wide$sec.A.score - Min.sec.A.score)/(Max.sec.A.score - Min.sec.A.score)*100)

# attach(heart.wide)

fit <- lm(normalized.sec.A.score ~ Group, data = heart.wide)
summary(fit)

fit <- lm(`Ask for  low Salt` ~ Group, data = heart.wide)
summary(fit)


# library(lme4)
# gm2 <- lmer(`Low Salt Diet` ~ Group + (1 | subject), data = heart.wide)
